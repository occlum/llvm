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

static cl::opt<bool>
    NoSFI("disable-SFI", cl::desc("do not check memory operations"), cl::Hidden);

static cl::opt<bool> enableX86MDSFIDGLoopOpt(
    "enable-x86-mdsfidg-loop-opt", cl::init(true), cl::Hidden,
    cl::desc("Enable X86 MDSFI data guard loop optimization."));

static cl::opt<bool> enableX86MDSFIDG("enable-x86-mdsfidg", cl::init(true),
                                      cl::Hidden,
                                      cl::desc("Enable X86 MDSFI data guard."));

static cl::opt<bool> CheckStoreOnly("check-store-only", cl::init(true),
                                     cl::desc("only check store operation"),
                                     cl::Hidden);

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

  // This function scan the whole machine function and insert proper 
  // data guards for each instruction that may load or store.
  bool InsertDataGuards(MachineFunction &Fn) ;

  // This function take AM as the target address and build one data guard
  // isLoad means a guard for load or for store
  // if check store only is used, only insert store guards
  bool insertOneDG(MachineInstr &MI, X86AddressMode &AM, bool isLoad);

  // This function take AM as the target address and build one data guard
  // isLoad means a guard for load or for store
  // if check store only is used, only insert store guards
  bool insertOneDG(MachineInstr &MI, unsigned BaseReg, bool isLoad);
  
  // handle a instruction that may load or may store. I.e. this instruction 
  // may load but won't store or vice verse.
  bool handleLoadOrStore(MachineInstr &MI, bool isLoad);

  // Handle instruction store and load 
  bool handleLoadAndStore(MachineInstr &MI);

  bool OptimizeCheck(MachineFunction &Fn, bool canOpt);
  bool OptimizeLoop(MachineFunction &Fn);
  bool OptimizeMBB(MachineBasicBlock &MBB, X86RegValueTracking &RT);
  bool LoweringMI(MachineFunction &Fn);
  bool getX86AddressFromInstr(const MachineInstr &MI, X86AddressMode &AM);
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
          /* LLVM_DEBUG(dbgs() << "Insert a check of RSP\n" << *MI << "\n"); */
          /* errs() << "Insert a check of RSP\n" << *MI << "\n"; */
          ret = true;
        }
      }
      MBBI = NMBBI;
    }
  }
  return ret;
}

// This function will extract Address mode from an instruction
// If there is no memory operands in MI, return false, otherwise return true
bool X86MDSFIDataGuard::getX86AddressFromInstr(const MachineInstr &MI, X86AddressMode &AM){
  const MCInstrDesc &Desc = MI.getDesc();
  int MemRefBegin = X86II::getMemoryOperandNo(Desc.TSFlags);
  if(MemRefBegin < 0)
    return false;
  MemRefBegin += X86II::getOperandBias(Desc);
  AM = getAddressFromInstr(&MI, MemRefBegin);
  return true;
}

bool X86MDSFIDataGuard::insertOneDG(MachineInstr &MI, X86AddressMode &AM, bool isLoad){
  MachineBasicBlock &MBB= *MI.getParent();
  const DebugLoc &DL = MI.getDebugLoc();
  if (AM.BaseType == X86AddressMode::RegBase) {
    if (AM.Base.Reg == X86::RIP){
      return false;
    }
  }
  if(isLoad == true ) {
    if(CheckStoreOnly)
      return false;
    addFullAddress(BuildMI(MBB, MI, DL, TII->get(X86::checkload64m)), AM);
  } else {
    addFullAddress(BuildMI(MBB, MI, DL, TII->get(X86::checkstore64m)), AM);
  }
  return true;
}

bool X86MDSFIDataGuard::insertOneDG(MachineInstr &MI, unsigned BaseReg, bool isLoad){
  MachineBasicBlock &MBB= *MI.getParent();
  const DebugLoc &DL = MI.getDebugLoc();
  if (BaseReg == X86::RIP){
    return false;
  }
  if(isLoad == true) {
    if(CheckStoreOnly)
      return false;
    addDirectMem(BuildMI(MBB, MI, DL, TII->get(X86::checkload64m)), BaseReg);
  } else {
    addDirectMem(BuildMI(MBB, MI, DL, TII->get(X86::checkstore64m)), BaseReg);
  }
  return true;
}

// This function tackle a instruction only load or only store.
bool X86MDSFIDataGuard::handleLoadOrStore(MachineInstr &MI, bool isLoad) {
  const MCInstrDesc &MCID = MI.getDesc();
  unsigned NumOfPotentialPointer = 0;
  // instructions only store
  X86AddressMode AM;
  if(getX86AddressFromInstr(MI, AM) == true) {
    //instructions that mayStore or mayLoad and have one memory operand
    insertOneDG(MI, AM, isLoad);
    NumOfPotentialPointer++;
  }
  const MCPhysReg *ImplicitUses = MCID.getImplicitUses();
  // If it has implicit uses, we scan the implicit uses list and check if there
  // are any potential pointer.
  if(ImplicitUses){
    for (unsigned i = 0; ImplicitUses[i]; ++i){
      if(TRI->getPointerRegClass(*MI.getMF())->contains(ImplicitUses[i])){
        if(ImplicitUses[i] == X86::RSP){
          // we don't care RSP, just check it and omit it as a potential pointer
          // Since we assumed that the RSP is used to access variable on stack, so we 
          // do not treat it as an potential pointer.
          insertOneDG(MI, X86::RSP, isLoad);
        } else
          NumOfPotentialPointer++;
      }
    }
  }
  // If this instruciton have use two potential pointers(we omit RSP)
  // then there must be one implicitly used pointer.
  // Since we don't konw whether should we check the implicitly used 
  // register (maybe it's just an normal register but not a pointer)
  // So we have to handle this kind of instructions ad-hoc(one by one).
  // Every time the compiler encounter an instruction potentially implicitly 
  // access memory but we can't handle, add it here like DIVs.
  if(NumOfPotentialPointer > 1) {
    switch(MI.getOpcode()){
      default:
        errs() << "mayStore or mayLoad: " <<MI;
        break;
        // mayLoad instructions we handle here
        // mayStore instructions we handle here
      case X86::DIV8m:
      case X86::IDIV8m:
      case X86::DIV16m:
      case X86::IDIV16m:
      case X86::DIV32m:
      case X86::IDIV32m:
      case X86::DIV64m:
      case X86::IDIV64m:
        insertOneDG(MI, AM, true);
        break;
    }
  }
  return true;
}

bool X86MDSFIDataGuard::handleLoadAndStore(MachineInstr &MI) {
  // Even if the instruction may load and store at the same time,
  // if it only have one potential pointer, we can check it
  const MCInstrDesc &MCID = MI.getDesc();
  unsigned NumOfPotentialPointer = 0, ImplicitReg = 0;
  bool haveMemOp = false;
   
  X86AddressMode AM;
  if(getX86AddressFromInstr(MI, AM) == true) {
    haveMemOp = true;
    NumOfPotentialPointer++;
  }
  const MCPhysReg *ImplicitUses = MCID.getImplicitUses();
  // If it has implicit uses, we scan the implicit uses list and check if there
  // are any potential pointer.
  if(ImplicitUses){
    for (unsigned i = 0; ImplicitUses[i]; ++i){
      if(TRI->getPointerRegClass(*MI.getMF())->contains(ImplicitUses[i])){
          NumOfPotentialPointer++;
          ImplicitReg = ImplicitUses[i];
      }
    }
  }

  // Every time the compiler encounter an instruction potentially implicitly 
  // access memory but we can't handle, add it here.
  if(NumOfPotentialPointer == 1){
    if(haveMemOp){
      insertOneDG(MI, AM, false);
      insertOneDG(MI, AM, true);
    } else  {
      insertOneDG(MI, ImplicitReg, false);
      insertOneDG(MI, ImplicitReg, true);
    }
    return true;
  }
  
  // If one instruction Both Load and Store and have more than one pointer, 
  // we don't know which pointer is used for store and which for load.
  // Hence we have to handle it ad-hoc:
  // That is, once more instructions is reported, we handle it.
  // Notice that we can't trusted the report. Here we are trying to construct 
  // a benign compiler. This report code is just help us. we still may not 
  // cover all instrutions.
  switch(MI.getOpcode()) {
    default:
      errs() << "mayStore and mayLoad: " <<MI;
      break;
    case X86::checkstore64m:
    case X86::checkload64m:
      break;
    case X86::PUSH64rmm:
      // push read from AM and push into stack
      insertOneDG(MI, AM, true);
      insertOneDG(MI, X86::RSP, false);
      break;
    case X86::POP64rmm:
      // pop read from stack and write to AM
      insertOneDG(MI, AM, false);
      insertOneDG(MI, X86::RSP, true);
      break;
  }
  return true;
}

bool X86MDSFIDataGuard::InsertDataGuards(MachineFunction &Fn) {
  for (MachineBasicBlock &MBB : Fn) {
    if (MBB.empty())
      continue;
    for(auto &MI: MBB) {
      // insert store guards
      if(!MI.mayLoadOrStore()) {
        // instructions won't access memory according to it's 
        // attribute mayLoad and may Store. For in case, we dump the instruction.
        LLVM_DEBUG(dbgs()<< "MI won't access memory" <<MI);
      } else if(MI.mayStore() && !MI.mayLoad()) {
        handleLoadOrStore(MI, false);
      } else if(MI.mayLoad() && !MI.mayStore()) {
        // instructions only load
        handleLoadOrStore(MI, true);
      } else if(MI.mayLoad() && MI.mayStore() ) {
        handleLoadAndStore(MI);
      }
    } //end of MBB
  } // end of Fn
  return true;
}

bool X86MDSFIDataGuard::runOnMachineFunction(MachineFunction &Fn) {
  if(NoSFI)
    return false;
  STI = &static_cast<const X86Subtarget &>(Fn.getSubtarget());
  TII = STI->getInstrInfo();
  TRI = STI->getRegisterInfo();
  MLI = &getAnalysis<MachineLoopInfo>();
  InsertDataGuards(Fn);

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
  return true;
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
  return true;
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
  return true;
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
  return true;
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
