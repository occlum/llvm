#include "X86.h"
#include "X86InstrBuilder.h"
#include "X86InstrInfo.h"
#include "X86RegValueTracking.h"
#include "X86Subtarget.h"

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/Support/Debug.h"

using namespace llvm;

static cl::opt<bool>
    enableX86BoundcheckerOpt("enable-x86-boundchecker-opt", cl::init(true),
                             cl::Hidden,
                             cl::desc("Enable X86 Boundchecker optimization."));

static cl::opt<bool>
    enableX86Boundchecker("enable-x86-boundchecker", cl::init(true), cl::Hidden,
                          cl::desc("Enable X86 Boundchecker."));
namespace {

class X86ConstraintCheck : public MachineFunctionPass {
public:
  static char ID;
  const X86Subtarget *STI;
  const TargetInstrInfo *TII;
  const TargetRegisterInfo *TRI;
  X86RegValueTracking RT;

  X86ConstraintCheck() : MachineFunctionPass(ID) {}

  bool runOnMachineFunction(MachineFunction &Fn) override;
  bool LoweringCheck(MachineFunction &Fn, bool canOpt);
  bool LoweringMI(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
                  bool eliminable);

  bool hasIndirectCall(MachineFunction &Fn);
  unsigned GuardZoneSize = 4 * 1024;
};
char X86ConstraintCheck::ID = 0;
} // namespace

/* //read metadata and extract constraint. */
/* bool X86ConstraintCheck::ExtracatConstraint(){ */
/*   return false; */
/* } */

#define DEBUG_TYPE "Boundchecker"
bool X86ConstraintCheck::hasIndirectCall(MachineFunction &Fn) {
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

bool X86ConstraintCheck::runOnMachineFunction(MachineFunction &Fn) {
  STI = &static_cast<const X86Subtarget &>(Fn.getSubtarget());
  TII = STI->getInstrInfo();
  TRI = STI->getRegisterInfo();

  // if there are any indirectCall, then we can not optimization this function
  // because it can jump before basicblock
  bool canOptimizate = !hasIndirectCall(Fn);
  if (canOptimizate == true && enableX86BoundcheckerOpt)
    RT.computeRange(Fn);
  LoweringCheck(Fn, canOptimizate);

  return true;
}
bool X86ConstraintCheck::LoweringCheck(MachineFunction &Fn, bool canOpt) {

  RangeInfo ReadRegion(InRead, GuardZoneSize, -GuardZoneSize);
  RangeInfo WriteRegion(InWrite, GuardZoneSize, -GuardZoneSize);

  for (MachineBasicBlock &MBB : Fn) {
    if (MBB.empty())
      continue;
    MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();

    // if opt is not enabled Lowering MI and continue to next MBB
    if (!enableX86BoundcheckerOpt ) {
      while (MBBI != E) {
        MachineBasicBlock::iterator NMBBI = std::next(MBBI);
        LoweringMI(MBB, MBBI, false);
        MBBI = NMBBI;
      }
      continue;
    }
    // if can't Opt, only scan inside a bb
    // so init RT at the begining of every MBB
    if (!canOpt) {
      RT.init(Fn);
    }

    // copy Range in BasicblockRanges
    RegsRange Ranges = RT.InRanges.at(&MBB);
    LLVM_DEBUG(dbgs() << "In Range for: "<< &MBB << "\n"<< Ranges);
    LLVM_DEBUG(dbgs() << "Out Range for: "<< &MBB << "\n"<< RT.OutRanges.at(&MBB));

    while (MBBI != E) {
      LLVM_DEBUG(dbgs() << *MBBI);
      MachineInstr *MI = &*MBBI;

      MachineBasicBlock::iterator NMBBI = std::next(MBBI);
      bool eliminable;

      switch (MI->getOpcode()) {
      case X86::checkstore64m: {
        X86AddressMode AM = getAddressFromInstr(MI, 0);
        eliminable = RT.isInRange(AM, WriteRegion, Ranges);
      } break;
      case X86::checkload64m: {
        X86AddressMode AM = getAddressFromInstr(MI, 0);
        eliminable = RT.isInRange(AM, ReadRegion, Ranges);
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
      LoweringMI(MBB, MBBI, eliminable);
      MBBI = NMBBI;
    }
  }
  return true;
}

bool X86ConstraintCheck::LoweringMI(MachineBasicBlock &MBB,
                                    MachineBasicBlock::iterator MBBI,
                                    bool eliminable) {
  MachineInstr &IMI = *MBBI;
  MachineInstr *MI = &IMI;
  DebugLoc DL = MI->getDebugLoc();

  if (!enableX86Boundchecker) {
    switch (MI->getOpcode()) {
    case X86::checkstore64m:
    case X86::checkload64m: {
      MBBI->eraseFromParent();
      return true;
    }
    }
    return false;
  }

  switch (MI->getOpcode()) {
  case X86::checkstore64m:
  case X86::checkload64m: {
    if (eliminable) {
      LLVM_DEBUG(dbgs() << "eliminable guard " << MI->getOperand(0) << "\n");
      MBBI->eraseFromParent();
      return true;
    }
  }
  }

  switch (MI->getOpcode()) {
  case X86::checkstore64m: {
    MachineInstr *Upcheck =
        BuildMI(MBB, MBBI, DL, TII->get(X86::BNDCU64rm), X86::BND1);
    MachineInstr *Lowcheck =
        BuildMI(MBB, MBBI, DL, TII->get(X86::BNDCL64rm), X86::BND1);
    for (const MachineOperand &MO : MBBI->operands()) {
      Upcheck->addOperand(MO);
      Lowcheck->addOperand(MO);
    }
    MBBI->eraseFromParent();
    return true;
  }

  case X86::checkload64m: {
    MachineInstr *Upcheck =
        BuildMI(MBB, MBBI, DL, TII->get(X86::BNDCU64rm), X86::BND0);
    MachineInstr *Lowcheck =
        BuildMI(MBB, MBBI, DL, TII->get(X86::BNDCL64rm), X86::BND0);
    for (const MachineOperand &MO : MBBI->operands()) {
      Upcheck->addOperand(MO);
      Lowcheck->addOperand(MO);
    }
    MBBI->eraseFromParent();
    return true;
  }
  }
  return false;
}

#undef DEBUG_TYPE

FunctionPass *llvm::createX86ConstraintCheck() {
  return new X86ConstraintCheck();
}
