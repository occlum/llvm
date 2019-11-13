#ifndef LLVM_LIB_TARGET_X86_X86REGVALUETRACKING_H
#define LLVM_LIB_TARGET_X86_X86REGVALUETRACKING_H
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
  bool isInRange(X86AddressMode &AM, RangeInfo R, RegsRange &Ranges);
  void init(MachineFunction &Fn);
};

#endif
