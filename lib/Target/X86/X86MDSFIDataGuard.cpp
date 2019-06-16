#include "X86.h"
#include "X86InstrBuilder.h"
#include "X86InstrInfo.h"
#include "X86RegValueTracking.h"
#include "X86Subtarget.h"

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Analysis/LoopIterator.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineLoopInfo.h"

#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/Debug.h"

#include "llvm/Analysis/LoopIterator.h"

using namespace llvm;

static cl::opt<bool>
    enableX86MDSFIDGOpt("enable-x86-mdsfidg-opt", cl::init(true), cl::Hidden,
                        cl::desc("Enable X86 MDSFI data guard optimization."));

static cl::opt<bool> enableX86MDSFIDGLoopOpt(
    "enable-x86-mdsfidg-loop-opt", cl::init(true), cl::Hidden,
    cl::desc("Enable X86 MDSFI data guard loop optimization."));

static cl::opt<bool> enableX86MDSFIDG("enable-x86-mdsfidg", cl::init(true),
                                      cl::Hidden,
                                      cl::desc("Enable X86 MDSFI data guard."));

namespace llvm {
void initializeX86MDSFIDataGuardPass(PassRegistry &);
}

namespace {

class X86MDSFIDataGuard : public MachineFunctionPass {
public:
  static char ID;
  const X86Subtarget *STI;
  const TargetInstrInfo *TII;
  const TargetRegisterInfo *TRI;
  /* std::vector<const MachineBasicBlock *>RPOTBlocks; */
  /* using rpot_iterator = MachineBasicBlock* std::vector<const
   * MachineBasicBlock*>::const_iterator; */
  /* rpot_iterator rpot_begin() const {return RPOTBlocks.begin();} */
  /* rpot_iterator rpot_end() const {return RPOTBlocks.end();} */

  X86RegValueTracking RT;
  std::map<MachineInstr *, bool> EliminableList;

  MachineLoopInfo *MLI;
  X86MDSFIDataGuard() : MachineFunctionPass(ID) {
    initializeX86MDSFIDataGuardPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &Fn) override;
  bool OptimizeCheck(MachineFunction &Fn, bool canOpt);
  bool OptimizeLoop(MachineFunction &Fn);
  bool OptimizeMBB(MachineBasicBlock &MBB, X86RegValueTracking &RT);
  bool LoweringMI(MachineFunction &Fn);
  void getAnalysisUsage(AnalysisUsage &AU) const override;
  bool isLoopBody(MachineBasicBlock *MBB, MachineLoop *L);

  bool checkRSP(MachineFunction &Fn);
  bool computeLoop(MachineLoop *ML, MachineLoopInfo *MLI,
                   X86RegValueTracking &UpLevelRT);
  bool hoist(MachineLoop *ML, MachineLoopInfo *MLI, X86RegValueTracking &RT);
  void LoopHoist();

  bool hasIndirectCall(MachineFunction &Fn);
  unsigned GuardZoneSize = 4 * 1024;
};
char X86MDSFIDataGuard::ID = 0;
} // namespace

#define DEBUG_TYPE "MDSFIDG"

void X86MDSFIDataGuard::getAnalysisUsage(AnalysisUsage &AU) const {
  MachineFunctionPass::getAnalysisUsage(AU);
  AU.addRequired<MachineLoopInfo>();
}
bool X86MDSFIDataGuard::hasIndirectCall(MachineFunction &Fn) {
  for (auto &MBB : Fn) {
    if (MBB.empty())
      continue;

    for (MachineBasicBlock::instr_iterator I = MBB.instr_begin();
         I != MBB.instr_end(); I++) {
      MachineInstr &MI = *I;
      switch (MI.getOpcode()) {
      default:
        break;
      case X86::JMP16m:
      case X86::JMP32m:
      case X86::JMP64m:
      case X86::CALL16r:
      case X86::CALL16m:
      case X86::CALL32r:
      case X86::CALL32m:
      case X86::CALL64r:
      case X86::CALL64m:
        return true;
      }
    }
  }
  return false;
}

// if changed RSP ,should check RSP after that instructions
bool X86MDSFIDataGuard::checkRSP(MachineFunction &Fn) {
  bool ret = false;
  for (auto &MBB : Fn) {
    if (MBB.empty())
      continue;
    MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();

    while (MBBI != E) {
      MachineInstr *MI = &*MBBI;
      DebugLoc DL = MI->getDebugLoc();
      MachineBasicBlock::iterator NMBBI = std::next(MBBI);

      for (auto &MO : MI->defs()) {
        if (MO.isReg() && MO.getReg() == X86::RSP) {
          MachineInstr *NewMI = addDirectMem(
              BuildMI(Fn, DL, TII->get(X86::checkstore64m)), X86::RSP);
          MBB.insertAfter(MBBI, NewMI);
          LLVM_DEBUG(dbgs() << "Insert a check of RSP\n" << *MI << "\n");
          ret = true;
        }
      }
      MBBI = NMBBI;
    }
  }
  return ret;
}

bool X86MDSFIDataGuard::runOnMachineFunction(MachineFunction &Fn) {
  STI = &static_cast<const X86Subtarget &>(Fn.getSubtarget());
  TII = STI->getInstrInfo();
  TRI = STI->getRegisterInfo();
  MLI = &getAnalysis<MachineLoopInfo>();

  // if there are any indirectCall, then we can not optimize loops
  // in this function because any program may jump before any basicblock
  bool canOptimize = !hasIndirectCall(Fn);

  enableX86MDSFIDGLoopOpt = enableX86MDSFIDGOpt && enableX86MDSFIDGLoopOpt;

  // compute for LoopHoist
  if (canOptimize && enableX86MDSFIDGOpt) {
    checkRSP(Fn);
    RT.init(Fn);
    RT.computeRange(Fn);
  }

  if (canOptimize && enableX86MDSFIDGLoopOpt) {
    LoopHoist();
  }

  // after loophoist, compute it again
  if (canOptimize && enableX86MDSFIDGOpt) {
    RT.init(Fn);
    RT.computeRange(Fn);
  }

  if (enableX86MDSFIDGOpt) {
    OptimizeCheck(Fn, canOptimize);
  }
  LoweringMI(Fn);
  return true;
}
bool X86MDSFIDataGuard::OptimizeMBB(MachineBasicBlock &MBB,
                                    X86RegValueTracking &RT) {
  RangeInfo ReadRegion(InRead, GuardZoneSize, -GuardZoneSize);
  RangeInfo WriteRegion(InWrite, GuardZoneSize, -GuardZoneSize);
  // copy Range in BasicblockRanges
  RegsRange Ranges = RT.InRanges.at(&MBB);

  LLVM_DEBUG(dbgs() << "In Range for: ");
  LLVM_DEBUG(MBB.printAsOperand(dbgs(), false));
  LLVM_DEBUG(dbgs() << "\n" << Ranges);
  LLVM_DEBUG(dbgs() << "Out Range for: ");
  LLVM_DEBUG(MBB.printAsOperand(dbgs(), false));
  LLVM_DEBUG(dbgs() << "\n" << RT.OutRanges.at(&MBB));
  LLVM_DEBUG(MBB.printAsOperand(dbgs(), false));
  LLVM_DEBUG(dbgs() << " Has Predesuccess: ");

  for (auto &it : MBB.predecessors()) {
    LLVM_DEBUG(it->printAsOperand(dbgs(), false));
    LLVM_DEBUG(dbgs() << ",");
  }
  LLVM_DEBUG(dbgs() << "\n\n");

  MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
  while (MBBI != E) {
    MachineInstr *MI = &*MBBI;

    MachineBasicBlock::iterator NMBBI = std::next(MBBI);

    switch (MI->getOpcode()) {
    case X86::checkstore64m: {
      /* LLVM_DEBUG(dbgs() << "guard: " << *MI); */
      X86AddressMode AM = getAddressFromInstr(MI, 0);
      EliminableList[MI] = RT.isInRange(AM, WriteRegion, Ranges);
    } break;
    case X86::checkload64m: {
      /* LLVM_DEBUG(dbgs() << "guard: " << *MI); */
      X86AddressMode AM = getAddressFromInstr(MI, 0);
      EliminableList[MI] = RT.isInRange(AM, ReadRegion, Ranges);
    } break;
    default:
      break;
    }

    // If eliminable, we will delete this guard instruction.
    // However, we can still use step function here
    // because eliminable means MI is in Ranges.
    // So this instruction can't bring any information, InferenceCheck in step
    // won't have any side effect
    RT.step(MI, Ranges);
    MBBI = NMBBI;
  }
}

// OptimizeCheck will store every eliminable MI in EliminableList
bool X86MDSFIDataGuard::OptimizeCheck(MachineFunction &Fn, bool canOpt) {
  LLVM_DEBUG(dbgs() << "Global Optimizing\n");
  MachineBasicBlock &MBB = Fn.front();
  MachineBasicBlock::iterator MBBI = MBB.getFirstNonPHI();
  DebugLoc DL;
  if (MBBI != MBB.end())
    DL = MBBI->getDebugLoc();

  for (MachineBasicBlock &MBB : Fn) {
    if (MBB.empty())
      continue;
    // if can't Opt, only scan inside a bb
    // so init RT at the begining of every MBB
    if (!canOpt) {
      RT.init(Fn);
    }
    OptimizeMBB(MBB, RT);
  }
  // Loop Optimization is definitely between BBs
  if (canOpt && enableX86MDSFIDGLoopOpt) {
    OptimizeLoop(Fn);
  }

  return false;
}

bool X86MDSFIDataGuard::isLoopBody(MachineBasicBlock *MBB, MachineLoop *L) {
  return L->contains(MBB) && MBB != L->getHeader();
}

void X86MDSFIDataGuard::LoopHoist() {
  for (auto &L : MLI->getBase()) {
    ReversePostOrderTraversal<MachineLoop *> RPOT(L);
    for (MachineLoop *ML : RPOT) {
      if (ML->getLoopDepth() != 1) {
        continue;
      }
      hoist(ML, MLI, RT);
    }
  }
}

bool X86MDSFIDataGuard::hoist(MachineLoop *ML, MachineLoopInfo *MLI,
                              X86RegValueTracking &UpLevelRT) {

  MachineLoopBlocksRPO RPOTBlocks(ML);
  RPOTBlocks.perform(MLI);
  X86RegValueTracking CurLoopRT;
  MachineBasicBlock *Header = ML->getHeader();
  CurLoopRT.init(*(Header->getParent()));

  for (auto &it : Header->predecessors()) {
    MachineBasicBlock *beforeMBB = &*it;
    if (ML->contains(beforeMBB)) {
      continue;
    }
    CurLoopRT.OutRanges.at(beforeMBB) = UpLevelRT.OutRanges.at(beforeMBB);
  }

  // In nature loop, except header, ie, for BB in loop body, should not have
  // any predecessor outside of the loop
  for (MachineBasicBlock *MBB : RPOTBlocks) {
    for (auto &it : MBB->predecessors()) {
      MachineBasicBlock *beforeMBB = &*it;
      CurLoopRT.InRanges.at(MBB).merge(CurLoopRT.OutRanges.at(beforeMBB));
    }
    CurLoopRT.transfer(MBB);
  }

  // who needs to be hoisted?
  // the baseregister of check function and it's indexreg is inRange in one
  // compute
  std::set<unsigned> hoistreg;
  for (MachineBasicBlock *MBB : ML->blocks()) {
    for (MachineBasicBlock::instr_iterator I = MBB->instr_begin();
         I != MBB->instr_end(); I++) {
      MachineInstr *MI = &*I;
      switch (MI->getOpcode()) {
      case X86::checkload64m:
      case X86::checkstore64m: {
        X86AddressMode AM = getAddressFromInstr(MI, 0);
        if (AM.BaseType == X86AddressMode::RegBase) {
          if (AM.IndexReg != 0) {
            if (CurLoopRT.InRanges.at(MBB)
                    .getRegRange(AM.IndexReg)
                    .RangeClass == SmallNum)
              hoistreg.insert(AM.Base.Reg);
          } else {
            hoistreg.insert(AM.Base.Reg);
          }
        }
      }
      }
    }
  }

  // and who can be hoisted?
  // not modified register
  for (MachineBasicBlock *MBB : ML->blocks()) {
    for (MachineBasicBlock::instr_iterator I = MBB->instr_begin();
         I != MBB->instr_end(); I++) {
      MachineInstr *MI = &*I;
      for (auto &MO : MI->defs()) {
        if (MO.isReg()) {
          auto Reg = MO.getReg();
          hoistreg.erase(Reg);
        }
      }
    }
  }

  MachineBasicBlock *Preheader = ML->getLoopPreheader();
  // TODO if preheader not exist, insert one
  if (Preheader == nullptr) {
    return false;
  }
  LLVM_DEBUG(dbgs() << "Loop Preheader is: ");
  LLVM_DEBUG(Preheader->printAsOperand(dbgs(), false));
  LLVM_DEBUG(dbgs() << "\n");

  MachineBasicBlock &preheader = *Preheader;
  MachineBasicBlock::iterator MBBI = Preheader->getLastNonDebugInstr();
  DebugLoc DL;
  if (MBBI != Preheader->end()) {
    DL = MBBI->getDebugLoc();
  } else {
  }

  LLVM_DEBUG(dbgs() << "Loop Hoist Register: ");
  for (unsigned reg : hoistreg) {
    LLVM_DEBUG(dbgs() << TRI->getRegAsmName(reg) << "\t");
    addDirectMem(BuildMI(preheader, MBBI, DL, TII->get(X86::checkstore64m)),
                 reg);
  }
  LLVM_DEBUG(dbgs() << "\n");

  for (MachineLoop *SubLoop : *ML) {
    hoist(SubLoop, MLI, CurLoopRT);
  }
}

bool X86MDSFIDataGuard::computeLoop(MachineLoop *ML, MachineLoopInfo *MLI,
                                    X86RegValueTracking &UpLevelRT) {

  MachineLoopBlocksRPO RPOTBlocks(ML);
  RPOTBlocks.perform(MLI);
  X86RegValueTracking CurLoopRT;
  MachineBasicBlock *Header = ML->getHeader();
  CurLoopRT.init(*(Header->getParent()));

  for (auto &it : Header->predecessors()) {
    MachineBasicBlock *beforeMBB = &*it;
    if (ML->contains(beforeMBB)) {
      continue;
    }
    CurLoopRT.OutRanges.at(beforeMBB) = UpLevelRT.OutRanges.at(beforeMBB);
  }

  // In nature loop, except header, ie, for BB in loop body, should not have
  // any predecessor outside of the loop
  for (MachineBasicBlock *MBB : RPOTBlocks) {
    for (auto &it : MBB->predecessors()) {
      MachineBasicBlock *beforeMBB = &*it;
      LLVM_DEBUG(dbgs() << "OutRanges of header's predecessors\n");
      LLVM_DEBUG(beforeMBB->printAsOperand(dbgs()));
      LLVM_DEBUG(dbgs() << "\n" << CurLoopRT.OutRanges.at(beforeMBB) << "\n");
      CurLoopRT.InRanges.at(MBB).merge(CurLoopRT.OutRanges.at(beforeMBB));
    }
    LLVM_DEBUG(dbgs() << "InRanges of MBB before transfer in loop\n");
    LLVM_DEBUG(MBB->printAsOperand(dbgs()));
    LLVM_DEBUG(dbgs() << "\n" << CurLoopRT.InRanges.at(MBB) << "\n");

    CurLoopRT.transfer(MBB);
  }

  LLVM_DEBUG(dbgs() << "Loop Optimizing \n");
  for (MachineBasicBlock *MBB : RPOTBlocks) {
    OptimizeMBB(*MBB, CurLoopRT);
  }

  LLVM_DEBUG(dbgs() << "Loop: " << *ML);
  for (MachineLoop *SubLoop : *ML) {
    computeLoop(SubLoop, MLI, CurLoopRT);
  }
}

#if 0
RegsRange collectLoopPattern(RegsRange r1, RegsRange r2) {
  RegsRange pattern;
  for (auto &RegIt : r1.Ranges) {
    if (RegIt.second.equal(r2.Ranges[RegIt.first]) &&RegIt.second.RangeClass =
            r2.Ranges[RegIt.first].RangeClass) {
      RangeInfo r = RegIt.second.min(r2.Ranges[RegIt.first]);
      if (r.UpRange < GuardZoneSize && r.LowRange < GuardZoneSize)
        pattern.setRegRange(RegIt.first, RegIt.second);
    }
  }
  return pattern;
}
bool computeLoop(MachineLoop *ML, MachineLoopInfo *MLI,
                 X86RegValueTracking &UpLevelRT) {

  MachineLoopBlocksRPO RPOTBlocks(ML);
  RPOTBlocks.perform(MLI);
  X86RegValueTracking CurLoopRT;
  MachineBasicBlock *Header = ML->getHeader();
  for (auto &it : Header->predecessors()) {
    MachineBasicBlock *beforeMBB = &*it;
    if (isLoopBody(beforeMBB, ML)) {
      continue;
    }
    CurLoopRT.InRanges.at(Header).merge(UpLeverlRT.OutRanges.at(beforeMBB));
  }

  RegsRange RoundRange[2];
  for (int i = 0; i < 2; i++) {

    // In nature loop, except header, ie, for BB in loop body, should not have
    // any predecessor outside of the loop
    for (MachineBasicBlock *MBB : RPOTBlocks) {
      for (auto &it : MBB->predecessors()) {
        MachineBasicBlock *beforeMBB = &*it;
        if (!ML->contains(beforeMBB)) {
          continue;
        }
        CurLoopRT.InRanges.at(MBB).merge(CurLoopRT.OutRanges.at(beforeMBB));
      }

      CurLoopRT.transfer(MBB);
    }

    // collect round result
    for (auto &it : Header->predecessors()) {
      MachineBasicBlock *beforeMBB = &*it;
      if (!ML->contains(beforeMBB)) {
        continue;
      }
      RoundRange[i].merge(CurLoopRT.OutRanges.at(beforeMBB));
    }
  }
  // compare two round result, and get Loop Pattern
  RegsRange LoopPattern = collectLoopPattern(RoundRange[0], RoundRange[1]);

  // with LoopPattern, check if any guard is eliminable
  for (auto &it : Header->predecessors()) {
    MachineBasicBlock *beforeMBB = &*it;
    CurLoopRT.InRanges.at(Header).merge(UpLeverlRT.OutRanges.at(beforeMBB));
  }

  LLVM_DEBUG(dbgs() << "Loop: " << *ML);
  for (MachineLoop *SubLoop : *ML) {
    computeLoop(SubLoop, MLI, CurLoopRT);
  }
}
#endif

// TODO currently we can only handle nature loop
// no opt on irreducible loop will not cause security problem
bool X86MDSFIDataGuard::OptimizeLoop(MachineFunction &Fn) {
  for (auto &L : MLI->getBase()) {
    // FIXME ReversePostOrder to scan subloop
    // currently this API works weird (seems does not work fine with sub loops)
    // so we natively use Loop's API to scan subloops
    ReversePostOrderTraversal<MachineLoop *> RPOT(L);
    for (MachineLoop *ML : RPOT) {
      if (ML->getLoopDepth() != 1) {
        continue;
      }
      computeLoop(ML, MLI, RT);
    }
  }
}

bool X86MDSFIDataGuard::LoweringMI(MachineFunction &Fn) {
  for (MachineBasicBlock &MBB : Fn) {
    if (MBB.empty())
      continue;
    MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();

    while (MBBI != E) {
      MachineInstr *MI = &*MBBI;
      DebugLoc DL = MI->getDebugLoc();
      MachineInstr *Upcheck, *Lowcheck;

      MachineBasicBlock::iterator NMBBI = std::next(MBBI);

      switch (MI->getOpcode()) {
      case X86::checkstore64m:
      case X86::checkload64m: {
        if (!enableX86MDSFIDG) {
          MBBI->eraseFromParent();
          break;
        }
        if (EliminableList[MI]) {
          LLVM_DEBUG(dbgs() << "eliminable guard " << *MI);
          MBBI->eraseFromParent();
          break;
        }

        if (MI->getOpcode() == X86::checkstore64m) {
          Upcheck = BuildMI(MBB, MBBI, DL, TII->get(X86::BNDCU64rm), X86::BND1);
          Lowcheck =
              BuildMI(MBB, MBBI, DL, TII->get(X86::BNDCL64rm), X86::BND1);
        } else if (MI->getOpcode() == X86::checkload64m) {
          Upcheck = BuildMI(MBB, MBBI, DL, TII->get(X86::BNDCU64rm), X86::BND0);
          Lowcheck =
              BuildMI(MBB, MBBI, DL, TII->get(X86::BNDCL64rm), X86::BND0);
        }
        for (const MachineOperand &MO : MBBI->operands()) {
          Upcheck->addOperand(MO);
          Lowcheck->addOperand(MO);
        }
        MBBI->eraseFromParent();
        break;
      }
      }
      MBBI = NMBBI;
    }
  }
  return true;
}

#undef DEBUG_TYPE

INITIALIZE_PASS_BEGIN(X86MDSFIDataGuard, "x86-constraint-check",
                      "X86 MDSFI data guard", false, false)
INITIALIZE_PASS_DEPENDENCY(MachineLoopInfo)
INITIALIZE_PASS_END(X86MDSFIDataGuard, "x86-constraint-check",
                    "X86 MDSFI data guard", false, false)

FunctionPass *llvm::createX86MDSFIDataGuard() {
  return new X86MDSFIDataGuard();
}
