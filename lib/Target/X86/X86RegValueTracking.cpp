#include "X86.h"
#include "X86InstrBuilder.h"
#include "X86InstrInfo.h"
#include "X86RegValueTracking.h"
#include "X86Subtarget.h"

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/RegsRange.h"

using namespace llvm;
#define DEBUG_TYPE "VT"

void X86RegValueTracking::init(MachineFunction &Fn) {
  STI = &static_cast<const X86Subtarget &>(Fn.getSubtarget());
  TRI = STI->getRegisterInfo();
  Loops = nullptr;
  OutRanges.clear();
  InRanges.clear();

  for (MachineBasicBlock &MBB : Fn) {
    // insert
    if (OutRanges.find(&MBB) == OutRanges.end()) {
      OutRanges.emplace(&MBB, RegsRange(*TRI));
      LLVM_DEBUG(dbgs() << "scaned MBB " << &MBB << "\n");
    }
    if (InRanges.find(&MBB) == InRanges.end()) {
      InRanges.emplace(&MBB, RegsRange(*TRI));
    }
  }
}

#define WINDEN_GUARD 20
bool X86RegValueTracking::computeRange(MachineFunction &Fn) {

  unsigned widen = 0;
  bool Changed = true;

  while (Changed) {
    Changed = false;
    widen++;

    ReversePostOrderTraversal<MachineFunction *> RPOT(&Fn);

    for (MachineBasicBlock *MBB : RPOT) {

      RegsRange oldout = OutRanges.at(MBB);

      transfer(MBB);

      if (!oldout.equal(OutRanges.at(MBB))) {
        Changed = true;
        if (widen == WINDEN_GUARD) {
          OutRanges.at(MBB).setChangedUnknown(oldout);
        }
      }
    }
  }
}
#undef WINDEN_GUARD

void X86RegValueTracking::transfer(MachineBasicBlock *MBB) {
  // get InRanges copy
  RegsRange Ranges = InRanges.at(MBB);
  for (auto &I : MBB->instrs()) {
    MachineInstr *MI = &I;
    step(MI, Ranges);
    // if instruction defined eflags, find that is this eflags used by jcc
    // if so, propagate it's range
    MachineOperand *DefOp = MI->findRegisterDefOperand(X86::EFLAGS);
    /* if(MachineOperand *DefOp = MI->findRegsiterDefOperand(X86::EFLAGS,false,
     * false,nullptr)){ */
    if (DefOp != nullptr && !DefOp->isDead() && !MI->mayLoad()) {
      // TODO
      // guess the range based on jcc
    }
  }
  OutRanges.at(MBB) = Ranges;

  /* MachineBasicBlock *nextMBB = MBB->getFallThrough(); */
  /* if(nextMBB != nullptr) */
  /*   InRanges.at(nextMBB).merge(OutRanges.at(MBB)); */

  for (auto &it : MBB->successors()) {
    MachineBasicBlock *nextMBB = &*it;
    /* LLVM_DEBUG(dbgs() << "merge before, OutRange " << MBB
     * <<OutRanges.at(MBB)); */
    /* LLVM_DEBUG(dbgs() << "merge before, InRange " << nextMBB <<
     * InRanges.at(nextMBB)); */
    InRanges.at(nextMBB).merge(OutRanges.at(MBB));
    /* LLVM_DEBUG(dbgs() << "merge end, OutRange " << MBB << OutRanges.at(MBB));
     */
    /* LLVM_DEBUG(dbgs() << "merge end, InRange " << nextMBB<<
     * InRanges.at(nextMBB)); */
  }
}

void X86RegValueTracking::step(MachineInstr *MI, RegsRange &Ranges) {
  switch (MI->getOpcode()) {
  // check ;
  case X86::checkload64m:
  case X86::checkstore64m: {
    InferenceCheck(MI, Ranges);
  } break;
  // register to register
  // TODO  handle more instructions
  // xor
  case X86::XOR8rr:
  case X86::XOR16rr:
  case X86::XOR32rr:
  case X86::XOR64rr: {
    unsigned src = MI->getOperand(0).getReg();
    unsigned dst = MI->getOperand(1).getReg();
    if (src == dst) {
      RangeInfo r1(SmallNum, 0, 0);
      Ranges.setRegRange(dst, r1);
    } else{
      // we don't make complicated arithmetic now
      RangeInfo r1(Unknown);
      Ranges.setRegRange(dst, r1);
    }
  } break;
  // lea
  case X86::LEA16r:
  case X86::LEA32r:
  case X86::LEA64r:
  case X86::LEA64_32r:
    break;
  // add
  case X86::ADD64ri8:
    break;
  default: {
    if (MI->isMoveReg() && !MI->mayLoad() && !MI->mayStore()) {
      unsigned src = MI->getOperand(0).getReg();
      unsigned dst = MI->getOperand(1).getReg();
      RangeInfo r1 = Ranges.getRegRange(src);
      Ranges.setRegRange(dst, r1);
    }
  // memory to register
  // read from memory will earse register's range
  // if mayLoad, then find the def register and set it's range to unknown
    if (MI->mayLoad()) {
      for (auto &MO : MI->defs()) {
        if (MO.isReg()) {
          auto Reg = MO.getReg();
          if (X86::GR64RegClass.contains(Reg) ||
              X86::GR32RegClass.contains(Reg) ||
              X86::GR16RegClass.contains(Reg) ||
              X86::GR8RegClass.contains(Reg)) {
            RangeInfo r(Unknown);
            Ranges.setRegRange(Reg, r);
          }
        }
      }
    }
    //unhandled instructions
    if(!MI->mayLoad() && !MI->mayStore()){
      for (auto &MO : MI->defs()) {
        if (MO.isReg()) {
          auto Reg = MO.getReg();
          if (X86::GR64RegClass.contains(Reg) ||
              X86::GR32RegClass.contains(Reg) ||
              X86::GR16RegClass.contains(Reg) ||
              X86::GR8RegClass.contains(Reg)) {
            RangeInfo r(Unknown);
            Ranges.setRegRange(Reg, r);
          }
        }
      }
    }
  } break;
  }

  // call instruction
  // set all range to unknown;
  if (MI->isCall()) {
    Ranges.setAllUnknown();
  }
}

#define getCheckRegion(MI)                                                     \
  MI->getOpcode() == X86::checkload64m ? InRead : InWrite

// X64 address mode
// Base + [1,2,4,8] * IndexReg + Disp32
// Index:        0     |    1        2       3           4
// Meaning:   DestReg, | BaseReg,  Scale, IndexReg, Displacement
// OperandTy: VirtReg, | VirtReg, UnsImm, VirtReg,   SignExtImm
// Two common ways to construct an address we observered
// Firstly is the most common way, basereg + offset
// BaseReg, Scale , IndexReg, Displacement
//  base  ,   1   ,   NoReg   ,   Imm/Expr
//
// Secondly is for an array
// BaseReg, Scale, IndexReg, Displacement
//    0   ,   4  ,   index ,    baseaddr of array
// The scale is the sizeof(elementType)
// the index usually is rax or other gerenal register
// the displacement is the BaseAddr of the array
//
// Summary:
// The address mode of X84 is X + a*Y + b
// X is basereg, and Y is index reg, while a and b are imm value
// Consider X and Y,if only X exist, we can check X if b is less then 4K.
// if Y is not noreg, we need range analysis to know the range of Y
void X86RegValueTracking::InferenceCheck(MachineInstr *MI, RegsRange &Ranges) {
  X86AddressMode AM;
  AM = getAddressFromInstr(MI, 0);
  if (AM.BaseType == X86AddressMode::RegBase) {
    if (AM.IndexReg != 0 || AM.GV != nullptr) {
      return;
    }
    RangeInfo r1 = Ranges.getRegRange(AM.Base.Reg);
    RangeInfo r2(getCheckRegion(MI), -AM.Disp, -AM.Disp);

    // r2 should be new range, it's not about meet
    /* LLVM_DEBUG(dbgs() << "Before cutRange: " <<
     * TRI->getRegAsmName(AM.Base.Reg)<< " " << r1); */
    /* LLVM_DEBUG(dbgs() << "cutRange: " << r2); */
    RangeInfo result = RangeInfo::cutRange(r1, r2);
    /* LLVM_DEBUG(dbgs() << "After cutRange: " <<
     * TRI->getRegAsmName(AM.Base.Reg)<< " " << result); */
    // if meet is Unknown, set it's range to check result;
    Ranges.setRegRange(AM.Base.Reg, result);
  } else {
  }
}

bool X86RegValueTracking::isInRange(X86AddressMode &AM, RangeInfo R,
                                    RegsRange &Ranges) {
  if (AM.BaseType == X86AddressMode::RegBase) {
    if (AM.IndexReg != 0 || AM.GV != nullptr) {
      return false;
    }
    RangeInfo r1 = Ranges.getRegRange(AM.Base.Reg);
    r1.add(AM.Disp);
    LLVM_DEBUG(dbgs() << "isInRange: " << TRI->getRegAsmName(AM.Base.Reg) << " "
                      << r1 << "\n");
    // if R contains r1, then meet will return R;
    return r1.isInRange(R);
  } else {
    return false;
  }
}
#undef DEBUG_TYPE
