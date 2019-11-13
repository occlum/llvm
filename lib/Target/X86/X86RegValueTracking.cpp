#include "X86.h"
#include "X86InstrBuilder.h"
#include "X86InstrInfo.h"
#include "X86Subtarget.h"

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/RegsRange.h"

using namespace llvm;
#define DEBUG_TYPE "VT"
class X86RegValueTracking {
public:
  const X86Subtarget *STI;
  const TargetInstrInfo *TII;
  const TargetRegisterInfo *TRI;

  bool computeRange(MachineFunction &Fn);
  void step(MachineInstr *MI, RegsRange &R);
  void InferenceCheck(MachineInstr *MI, RegsRange &Ranges);
  void transfer(MachineBasicBlock *MBB);
  std::map<MachineBasicBlock *, RegsRange> OutRanges;
  std::map<MachineBasicBlock *, RegsRange> InRanges;
  bool isInRange(X86AddressMode &AM);
  bool isCFILabel(const MachineInstr *MI);
  bool isInRange(X86AddressMode &AM, RangeInfo R, RegsRange &Ranges);
  void init(MachineFunction &Fn);
};

bool X86RegValueTracking::isCFILabel(const MachineInstr * MI) {
  if( MI->getOpcode() != X86::NOOPL || MI->getNumOperands()!= 5){
    return false;
  }
  MachineOperand MO = MI->getOperand(0);
  if(!MO.isReg() || MO.getReg() != X86::RBX){
    return false;
  }
  MO = MI->getOperand(1);
  if(!MO.isImm() || MO.getImm() != 1){
    return false;
  }
  MO = MI->getOperand(2);
  if(!MO.isReg() || MO.getReg() != X86::RBX){
    return false;
  }
  MO = MI->getOperand(3);
  if(!MO.isImm() || MO.getImm() != 512){
    return false;
  }
  MO = MI->getOperand(4);
  if(!MO.isReg() || MO.getReg() != 0){
    return false;
  }
  return true;
}

void X86RegValueTracking::init(MachineFunction &Fn) {
  STI = &static_cast<const X86Subtarget &>(Fn.getSubtarget());
  TRI = STI->getRegisterInfo();
  OutRanges.clear();
  InRanges.clear();
  RangeInfo writeable(InWrite, 0, 0);

  // FIXME
  // maybe only Ranges of used MBB requires init
  // This can save memorys
  for (MachineBasicBlock &MBB : Fn) {
    // insert
    if (OutRanges.find(&MBB) == OutRanges.end()) {
      OutRanges.emplace(&MBB, RegsRange(*TRI));
      /* LLVM_DEBUG(dbgs() << "scaned MBB "); */
      /* LLVM_DEBUG(MBB.printAsOperand(dbgs(), false)); */
      /* LLVM_DEBUG(dbgs() << "\n"); */
    }
    if (InRanges.find(&MBB) == InRanges.end()) {
      InRanges.emplace(&MBB, RegsRange(*TRI));
      InRanges.at(&MBB).setRegRange(X86::RSP, writeable);
    }
  }
}

#define WIDEN_GUARD 10
bool X86RegValueTracking::computeRange(MachineFunction &Fn) {

  unsigned widen = 0;
  bool Changed = true;

  while (Changed) {
    Changed = false;
    widen++;

    ReversePostOrderTraversal<MachineFunction *> RPOT(&Fn);

    for (MachineBasicBlock *MBB : RPOT) {

      RegsRange oldout = OutRanges.at(MBB);

      for (auto &it : MBB->predecessors()) {
        MachineBasicBlock *beforeMBB = &*it;
        InRanges.at(MBB).merge(OutRanges.at(beforeMBB));
      }

      transfer(MBB);

      if (!oldout.equal(OutRanges.at(MBB))) {
        Changed = true;
        if (widen == WIDEN_GUARD) {
          OutRanges.at(MBB).setChangedUnknown(oldout);
        }
      }
    }
  }
  LLVM_DEBUG(dbgs() << "finished compute Range\n");
}
#undef WINDEN_GUARD

void X86RegValueTracking::transfer(MachineBasicBlock *MBB) {
  // get InRanges copy
  RegsRange Ranges = InRanges.at(MBB);
  for (auto &I : MBB->instrs()) {
    LLVM_DEBUG(dbgs() << "Before step: " << Ranges);
    MachineInstr *MI = &I;
    step(MI, Ranges);
    LLVM_DEBUG(dbgs() << "After step: " << Ranges);
  }
  OutRanges.at(MBB) = Ranges;
}

void X86RegValueTracking::step(MachineInstr *MI, RegsRange &Ranges) {
  LLVM_DEBUG(dbgs() << *MI << "\n");
  int i = 0;
  /* LLVM_DEBUG(dbgs() << "Machine Operands: "); */
  /* for (const MachineOperand &MO : MI->operands()) { */
  /*   LLVM_DEBUG(dbgs() << " " << i << " : " << MO); */
  /*   i++; */
  /* } */
  /* LLVM_DEBUG(dbgs() << "\n"); */
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
    unsigned dst = MI->getOperand(0).getReg();
    unsigned src = MI->getOperand(1).getReg();
    if (src == dst) {
      RangeInfo r1(SmallNum, 0, 0);
      Ranges.setRegRange(dst, r1);
    } else {
      // we don't make complicated arithmetic now
      RangeInfo r1(Unknown);
      Ranges.setRegRange(dst, r1);
    }
  } break;
  // lea
  case X86::LEA16r:
  case X86::LEA32r:
  case X86::LEA64r:
  case X86::LEA64_32r: {
    X86AddressMode AM;
    unsigned dst = MI->getOperand(0).getReg();
    if (!MI->getOperand(4).isImm())
      break;
    AM = getAddressFromInstr(MI, 1);

    if (AM.GV != nullptr) {
      RangeInfo r1(Unknown);
      Ranges.setRegRange(dst, r1);
      break;
    }
    if (AM.BaseType == X86AddressMode::RegBase) {
      if (AM.IndexReg == 0) {
        RangeInfo r1 = Ranges.getRegRange(AM.Base.Reg);
        bool cal = r1.add(AM.Disp);
        if (cal) {
          Ranges.setRegRange(dst, r1);
        } else {
          RangeInfo unknownr = RangeInfo(Unknown);
          Ranges.setRegRange(dst, unknownr);
        }
      }
    }
  } break;
  // add
  case X86::ADD64ri8:
  case X86::ADD32ri8:
  case X86::ADD16ri8:
  case X86::ADD8ri8: {
    unsigned dst = MI->getOperand(0).getReg();
    if (!MI->getOperand(2).isImm()) {
      break;
    }
    int imm = MI->getOperand(2).getImm();
    RangeInfo r1 = Ranges.getRegRange(dst);
    if (r1.add(imm))
      Ranges.setRegRange(dst, r1);
  } break;
  // movri
  // X86::MOV64ri movabs
  case X86::MOV64ri:
  case X86::MOV64ri32:
  case X86::MOV32ri:
  case X86::MOV16ri:
  case X86::MOV8ri: {
    unsigned dst = MI->getOperand(0).getReg();
    if (!MI->getOperand(1).isImm()) {
      break;
    }
    int imm = MI->getOperand(1).getImm();
    RangeInfo r1(SmallNum, imm, imm);
    Ranges.setRegRange(dst, r1);
  } break;
  default: {
    if (MI->isMoveReg() && !MI->mayLoad() && !MI->mayStore()) {
      unsigned dst = MI->getOperand(0).getReg();
      unsigned src = MI->getOperand(1).getReg();
      RangeInfo r1 = Ranges.getRegRange(src);
      LLVM_DEBUG(dbgs() << "isMoveReg get Range: " << r1 << "\n");
      Ranges.setRegRange(dst, r1);
    }
    // memory to register
    // read from memory will earse register's range
    // if mayLoad, then find the def register and set it's range to unknown
    else if (MI->mayLoad()) {
      for (auto &MO : MI->defs()) {
        if (MO.isReg()) {
          auto Reg = MO.getReg();
          if (X86::GR64RegClass.contains(Reg) ||
              X86::GR32RegClass.contains(Reg) ||
              X86::GR16RegClass.contains(Reg) ||
              X86::GR8RegClass.contains(Reg) || Reg == X86::RSP) {
            RangeInfo r(Unknown);
            Ranges.setRegRange(Reg, r);
          }
        }
      }
    }
    // unhandled instructions
    else if (!MI->mayLoad() && !MI->mayStore()) {
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
  // actual should be CFI_LABEL
  if (isCFILabel(MI)) {
    LLVM_DEBUG(dbgs() << "Meet CFI label: " << *MI << "\n");
    Ranges.setAllUnknown();
    RangeInfo writeable(InWrite, 0, 0);
    Ranges.setRegRange(X86::RSP, writeable);
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
  if (AM.GV != nullptr) {
    // do not handle GV case now
    return;
  }
  if (AM.BaseType == X86AddressMode::RegBase) {
    if (AM.IndexReg == 0) {
      // FIXME
      // This check function might be eliminable in the future, which means the
      // range we infered here is not reliable
      RangeInfo r1 = Ranges.getRegRange(AM.Base.Reg);
      RangeInfo r2(getCheckRegion(MI), -AM.Disp, -AM.Disp);

      // r2 should be new range, it's not about meet
      RangeInfo result = RangeInfo::cutRange(r1, r2);
      // if meet is Unknown, set it's range to check result;
      Ranges.setRegRange(AM.Base.Reg, result);
    } else {
      /* RangeInfo BaseRange = Ranges.getRegRange(AM.Base.Reg); */
      /* RangeInfo IndexRange = Ranges.getRegRange(AM.IndexReg); */
      /* // if BaseRange is in one region, then cal the index range as small num
       */
      /* if(BaseRange.RangeClass == InRead || BaseRange.RangeClass == InWrite){
       */
      /*   RangeInfo newrange(SmallNum, -(BaseRange.UpRange + AM.Disp)/AM.Scale,
       * -(BaseRange.LowRange + AM.Disp)/AM.Scale); */
      /*   RangeInfo result = RangeInfo::cutRange(IndexRange, newrange); */
      /* Ranges.setRegRange(AM.IndexReg, result); */
      /* return; */
      /* }else if (IndexRange.RangeClass == SmallNum){ */
      /*   RangeInfo r2 (getCheckRegion(MI), -IndexRange.UpRange*AM.Scale -
       * AM.Disp, - IndexRange.LowRange *AM.Scale -AM.Disp); */
      /*   RangeInfo result = RangeInfo::cutRange(BaseRange, r2); */
      /*   Ranges.setRegRange(AM.Base.Reg, result); */
      /*   return; */
      /* } */
      return;
    }
  } else {
    // TODO
    // for benign compiler, GV , which is a symbol, must located in the data
    // domain as a result, we can assume GV is in write region then inference
    // Index reg's range for example checkstore GV(,%rax,4) which means rax must
    // less then guardzone/4 nothing I can do now
  }
}

bool X86RegValueTracking::isInRange(X86AddressMode &AM, RangeInfo R,
                                    RegsRange &Ranges) {
  bool calculable;
  if (AM.BaseType == X86AddressMode::RegBase) {
    if (AM.Base.Reg == X86::RIP){
      LLVM_DEBUG(dbgs() << "RIP as BaseReg\n");
      return true;
    }
    if (AM.GV != nullptr) {
      return false;
    }
    RangeInfo r1 = Ranges.getRegRange(AM.Base.Reg);
    if (AM.IndexReg != 0) {
      // if Index Reg is not noreg, then
      // Index Reg should be a small number
      // AM.GV or BaseReg can only exist one

      calculable = r1.add(AM.Disp);
      RangeInfo indexrange = Ranges.getRegRange(AM.IndexReg);
      calculable = indexrange.multiple(AM.Scale);
      calculable = r1.add(indexrange);
      return calculable ? r1.isInRange(R) : false;
    }

    calculable = r1.add(AM.Disp);
    LLVM_DEBUG(dbgs() << "isInRange: " << TRI->getRegAsmName(AM.Base.Reg) << " "
                      << r1 << "\n");
    // if R contains r1, then meet will return R;
    return calculable ? r1.isInRange(R) : false;
  } else {
    // FIXME
    // currently we assum the GV is in the range
    // GV + indexreg * scale
    return false;
  }
}
#undef DEBUG_TYPE
