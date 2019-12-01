

#include "X86.h"
#include "X86InstrBuilder.h"
#include "X86InstrInfo.h"
#include "X86Subtarget.h"

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/RegisterScavenging.h"

using namespace llvm;

namespace llvm {
void initializeX86MDSFIControlGuardPass(PassRegistry &);
}

#define DEBUG_TYPE "MDSFICG"

static cl::opt<bool> enableX86MDSFICG("enable-x86-mdsficg", cl::init(true),
                                      cl::Hidden,
                                      cl::desc("Enable X86 cfi instrument."));
static cl::opt<bool>
    enableX86FSRelocate("enable-x86-fs-relocate", cl::init(false), cl::Hidden,
                        cl::desc("Enable X86 relocate with FS."));

namespace {
class X86MDSFIControlGuard : public MachineFunctionPass {
public:
  static char ID;
  X86MDSFIControlGuard() : MachineFunctionPass(ID) {
    initializeX86MDSFIControlGuardPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &Fn) override;

  const X86Subtarget *STI;
  const TargetInstrInfo *TII;
  const TargetRegisterInfo *TRI;
  std::unique_ptr<RegScavenger> RS;

  /// Handle all Calls, invoke all indirect jumps
  bool HandleCalls(MachineBasicBlock &MBB, MachineInstr &MI);

  /// This function will replace one return instruction with a pop, CFI check
  /// and an indirect jump
  bool HandleRets(MachineBasicBlock &MBB, MachineInstr &MI);
  bool HandleRet(MachineBasicBlock &MBB, MachineInstr &MI);

  /// Handle all indirect jumps
  bool HandleIndirectBrs(MachineBasicBlock &MBB, MachineInstr &MI);

  bool HandleRegInst(MachineBasicBlock &MBB, MachineInstr &MI, unsigned LabelReg);
  bool HandleMemInst(MachineBasicBlock &MBB, MachineInstr &MI, unsigned Opcode, unsigned TargetReg, unsigned LabelReg);

  /// Insert CFI label before every function. If hasIndirectJump is true, then
  /// insert CFI labels before every basicblock either.
  bool InsertCFILabels(MachineFunction &Fn, bool hasIndirectJump);
  bool InsertOneCFILabel(MachineInstr &MI);
  bool InsertOneCFILabel(MachineInstr &MI, bool InsertAfter);
  bool isCFILabel(MachineInstr & MI);

  bool RelocatePIC(MachineFunction &Fn);
  bool ds2fs(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
             unsigned indexofRIP);

  bool CFIInstrument(MachineFunction &Fn);
};
} // namespace
char X86MDSFIControlGuard::ID = 0;

bool X86MDSFIControlGuard::HandleRets(MachineBasicBlock &MBB,
                                      MachineInstr &MI) {
  switch (MI.getOpcode()) {
  default:
    LLVM_DEBUG(dbgs() << "Unhandled return instruction\t" << MI);
    return false;
    break;
  case X86::RET:
  case X86::RETL:
  case X86::RETQ:
    HandleRet(MBB, MI);
    break;
  case X86::TAILJMPr64:
    HandleRegInst(MBB, MI, X86::R10);
    break;
  case X86::TAILJMPm64:
    HandleMemInst(MBB, MI, X86::TAILJMPr64,X86::R10, X86::R11);
    break;
    // jmp with imm target, no need to handle
  case X86::TAILJMPd64:
  case X86::TAILJMPd_CC:
    break;
  }
  return true;
}

bool X86MDSFIControlGuard::HandleRet(MachineBasicBlock &MBB, MachineInstr &MI) {
  const DebugLoc &DL = MI.getDebugLoc();
  // popq %r11
  BuildMI(MBB, MI, DL, TII->get(X86::POP64r), X86::R11);
  // movq (%r11), %r10
  BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm))
      .addReg(X86::R10)
      .addReg(X86::R11)
      .addImm(1)
      .addReg(0)
      .addImm(0)
      .addReg(0);
  // bndcu %r10, %bnd2
  // bndcl %r10, %bnd2
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rr), X86::BND2).addReg(X86::R10);
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rr), X86::BND2).addReg(X86::R10);

  // jmpq *%r11
  BuildMI(MBB, MI, DL, TII->get(X86::JMP64r)).addReg(X86::R11);
  // remove ret from code;
  MI.eraseFromParent();
  return true;
}
/// opcode is the replacement of the original instruction
bool X86MDSFIControlGuard::HandleMemInst(MachineBasicBlock &MBB,
                                         MachineInstr &MI, unsigned opcode, 
                                         unsigned TargetReg, unsigned LabelReg) {
  const DebugLoc &DL = MI.getDebugLoc();
  LLVM_DEBUG(dbgs() << "HandleMemInst\t" << MI);
  // We might need two register, one for target address, the other for CFI label.
  unsigned MemReg;

  // Does TargetReg need spill? Does LabelReg need spill?
  bool spillTarget = false, spillLabel = false;

  X86AddressMode AM;
  // Which physic register is used in this Memory address?
  AM = getAddressFromInstr(&MI, 0);
  MemReg = AM.BaseType == X86AddressMode::RegBase ? AM.Base.Reg : AM.IndexReg;

  // We didn't specific TargetReg
  if(TargetReg == 0) {

    // if MemReg is a general purpose register, we use MemReg as TargetReg 
    // Note that GR64 class include RIP and RSP, and we have to exclude it
    if(!X86::GR64RegClass.contains(MemReg) && MemReg!= X86::RSP && MemReg != X86::RIP){
      TargetReg = MemReg;
    } else {
      // otherwise if MemReg is RSP or RIP, we have to get another register
      RegScavenger *RegSc = RS.get();
      RegSc->enterBasicBlockEnd(MBB);
      TargetReg = RegSc->scavengeRegisterBackwards(
          X86::GR64RegClass, MachineBasicBlock::iterator(MI), false, 0, false);
      if (TargetReg == 0) {
        LLVM_DEBUG(dbgs() << "Can not find free register for indirect branch\t"
                          << MI);
        // Since MemReg is not a GR, we use R10 directly.
        TargetReg = X86::R10;
        LLVM_DEBUG(dbgs() << "spill one for TargetReg\t"<<TRI->getRegAsmName(TargetReg)<<"\n");
        spillTarget = true;
      }
    }
  }

  if(LabelReg == 0){
    RegScavenger *RegSc = RS.get();
    RegSc->enterBasicBlockEnd(MBB);
    LabelReg = RegSc->scavengeRegisterBackwards(
        X86::GR64RegClass, MachineBasicBlock::iterator(MI), false, 0, false);
    if (LabelReg == 0) {
      LLVM_DEBUG(dbgs() << "Can not find free register for indirect branch\t"
                        << MI);
      // If TargetReg already token R10, we use R11
      LabelReg = TargetReg == X86::R10 ? X86::R11 : X86::R10;
      LLVM_DEBUG(dbgs() << "spill one for LabelReg \t"<<TRI->getRegAsmName(LabelReg)<<"\n");
      spillLabel = true;
    }
  }

  LLVM_DEBUG(dbgs() << "TargetReg for memory based Instruction: "
                    << TRI->getRegAsmName(TargetReg) << "\n");
  LLVM_DEBUG(dbgs() << "LabelReg for memory based Instruction: "
                    << TRI->getRegAsmName(LabelReg) << "\n");

  if(spillTarget)
    BuildMI(MBB, MI, DL, TII->get(X86::PUSH64r), TargetReg);
  if(spillLabel)
    BuildMI(MBB, MI, DL, TII->get(X86::PUSH64r), LabelReg);

  // movq (x), x
  addFullAddress(BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), TargetReg), AM);

  // movq (%TargetReg) , %FreeReg; load cfi lable
  addDirectMem(BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), LabelReg),
               TargetReg);
  // bndck %FreeReg
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rr), X86::BND2).addReg(LabelReg);
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rr), X86::BND2).addReg(LabelReg);
  if(spillTarget)
    BuildMI(MBB, MI, DL, TII->get(X86::POP64r), TargetReg);
  if(spillLabel)
    BuildMI(MBB, MI, DL, TII->get(X86::POP64r), LabelReg);

  BuildMI(MBB, MI, DL, TII->get(opcode), TargetReg);
  MI.eraseFromParent();
  return true;
}

bool X86MDSFIControlGuard::HandleRegInst(MachineBasicBlock &MBB,
                                         MachineInstr &MI, unsigned LabelReg) {
  const DebugLoc &DL = MI.getDebugLoc();
  LLVM_DEBUG(dbgs() << "HandleRegInst\t" << MI);
  unsigned TargetReg;
  bool spillLabel = false;
  TargetReg = MI.getOperand(0).getReg();
  if(LabelReg == 0){
    RegScavenger *RegSc = RS.get();
    RegSc->enterBasicBlockEnd(MBB);
    LabelReg = RegSc->scavengeRegisterBackwards(
        X86::GR64RegClass, MachineBasicBlock::iterator(MI), false, 0, false);
    if (LabelReg == 0) {
      LLVM_DEBUG(dbgs() << "Can not find free register for indirect branch\t"
                        << MI);
      LabelReg = TargetReg == X86::R10 ? X86::R11 : X86::R10;
      LLVM_DEBUG(dbgs() << "spill one for LabelReg\t"<<TRI->getRegAsmName(LabelReg)<<"\n");
      spillLabel = true;
    }
  }

  LLVM_DEBUG(dbgs() << "LabelReg for register based instruction: "
                    << TRI->getRegAsmName(LabelReg) << "\n");
  if(spillLabel)
    BuildMI(MBB, MI, DL, TII->get(X86::PUSH64r), LabelReg);
  // movq (%TargetReg) , %FreeReg load cfi lable
  addDirectMem(BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), LabelReg),
               TargetReg);
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rr), X86::BND2).addReg(LabelReg);
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rr), X86::BND2).addReg(LabelReg);
  if(spillLabel)
    BuildMI(MBB, MI, DL, TII->get(X86::POP64r), LabelReg);
  return true;
}


bool X86MDSFIControlGuard::HandleIndirectBrs(MachineBasicBlock &MBB,
                                             MachineInstr &MI) {
  switch (MI.getOpcode()) {
  default:
    LLVM_DEBUG(dbgs() << "Unhandled indirect branch instruction\t" << MI
                      << "\n");
    return false;
    break;
  case X86::JMP16r:
  case X86::JMP32r:
  case X86::JMP64r:
    HandleRegInst(MBB, MI, 0);
    break;
  case X86::JMP16m:
  case X86::JMP32m:
  case X86::JMP64m:
    HandleMemInst(MBB, MI, X86::JMP64r, 0, 0);
    break;
  }
  return true;
}
bool X86MDSFIControlGuard::isCFILabel(MachineInstr & MI) {
  if( MI.getOpcode() != X86::NOOPL || MI.getNumOperands()!= 5){
    return false;
  }
  MachineOperand MO = MI.getOperand(0);
  if(!MO.isReg() || MO.getReg() != X86::RBX){
    return false;
  }
  MO = MI.getOperand(1);
  if(!MO.isImm() || MO.getImm() != 1){
    return false;
  }
  MO = MI.getOperand(2);
  if(!MO.isReg() || MO.getReg() != X86::RBX){
    return false;
  }
  MO = MI.getOperand(3);
  if(!MO.isImm() || MO.getImm() != 512){
    return false;
  }
  MO = MI.getOperand(4);
  if(!MO.isImm() || MO.getImm() != 0){
    return false;
  }
  return true;
}

//insert one CFI label before MI
bool X86MDSFIControlGuard::InsertOneCFILabel(MachineInstr &MI){
  const DebugLoc &DL = MI.getDebugLoc();
  MachineBasicBlock &MBB = *MI.getParent();

  if(isCFILabel(MI))
    return false;
  BuildMI(MBB, MI, DL, TII->get(X86::NOOPL))
      .addReg(X86::RBX)
      .addImm(1)
      .addReg(X86::RBX)
      .addImm(512)
      .addReg(0);
  return true;
}

bool X86MDSFIControlGuard::InsertOneCFILabel(MachineInstr &MI, bool InsertAfter){
  const DebugLoc &DL = MI.getDebugLoc();
  MachineBasicBlock &MBB = *MI.getParent();

  if(isCFILabel(MI))
    return false;
  MachineInstr *LabelMI = BuildMI(MBB, MI, DL, TII->get(X86::NOOPL))
      .addReg(X86::RBX)
      .addImm(1)
      .addReg(X86::RBX)
      .addImm(512)
      .addReg(0);
  if(InsertAfter){
    MachineBasicBlock::iterator pos = MI;
    MBB.insertAfter(pos, LabelMI->removeFromParent());
  }
  return true;
}
  

bool X86MDSFIControlGuard::InsertCFILabels(MachineFunction &Fn,
                                          bool hasIndirectJump) {
  MachineFunction::iterator FirstMBBI = Fn.begin();

  // sometimes the first basicblock of function might be empty
  // handle this by step to next basic block;
  // this is because some function do not need to save frame
  for (; FirstMBBI->empty();) {
    auto NMBBI = std::next(FirstMBBI);
    if (FirstMBBI == Fn.end()) {
      // can't find non-empty BB, exist function
      LLVM_DEBUG(dbgs() << "Error: no non-empty basic block at function "
                        << Fn.getName());
      return false;
    }
    FirstMBBI = NMBBI;
  }
  // now the firstMBB is the first non-empty BB at Fn
  // insert CFI_LABEL before first instruction at the function
  auto &FirstMBB = *FirstMBBI;
  auto &FirstMI = *FirstMBB.getFirstNonPHI();
  /* LLVM_DEBUG(dbgs() <<"First MI : " << FirstMI); */
  InsertOneCFILabel(FirstMI);

  // scan every instruction, insert CFI_LABEL after call
  for (auto &MBB : Fn) {
    if (MBB.empty())
      continue;

    MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
    while (MBBI != E) {
      MachineBasicBlock::iterator NMBBI = std::next(MBBI);
      MachineInstr &MI = *MBBI;

      if(MI.isCall() && !MI.isReturn()){
        InsertOneCFILabel(MI, true);
      }
      MBBI = NMBBI;
    }
  }
  // if there is no direct jump in the function, then do not insert cfi_label
  // before every bb just return;
  if (!hasIndirectJump)
    return true;
  // insert CFI_LABEL before every BB.
  // Because there are some indirect jump inside this funciton, it might
  // want to jump to any of BB at this function
  // We omit first basicblock because it's already have one CFI_LABEL
  for (MachineFunction::iterator MBBI = ++Fn.begin(); MBBI != Fn.end();
       MBBI++) {
    MachineBasicBlock &MBB = *MBBI;
    if (MBB.empty())
      continue;
    MachineBasicBlock::instr_iterator I = MBB.instr_begin();
    MachineInstr &MI = *I;
    InsertOneCFILabel(MI);
  }
  return true;
}

bool X86MDSFIControlGuard::runOnMachineFunction(MachineFunction &Fn) {
  STI = &static_cast<const X86Subtarget &>(Fn.getSubtarget());
  TRI = STI->getRegisterInfo();
  TII = STI->getInstrInfo();
  RS.reset(new RegScavenger());

  if (Fn.getName().startswith("_boundchecker_"))
    return false;
  if (!enableX86MDSFICG) {
    return false;
  }

  bool hasIndirectJump = false;
  hasIndirectJump = CFIInstrument(Fn);
  InsertCFILabels(Fn, hasIndirectJump);

  // disable if for SPEC
  if (enableX86FSRelocate)
    RelocatePIC(Fn);

  return true;
}

bool X86MDSFIControlGuard::ds2fs(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator MBBI,
                                 unsigned indexofRIP) {
  switch (MBBI->getOpcode()) {
  case X86::LEA64r:
  case X86::LEA64_32r:
  case X86::LEA32r:
  case X86::LEA16r: {

    const DebugLoc &DL = MBBI->getDebugLoc();
    auto NMBBI = std::next(MBBI);
    MachineOperand &dest = MBBI->getOperand(0);

    assert(dest.isReg() && "LEA dest is not register");

    unsigned destReg = dest.getReg();
    unsigned victimReg = destReg == X86::R10 ? X86::R11 : X86::R10;

    BuildMI(MBB, MBBI, DL, TII->get(X86::PUSH64r), victimReg);
    if (X86::GR64RegClass.contains(destReg))
      BuildMI(MBB, MBBI, DL, TII->get(X86::RDFSBASE64), victimReg);
    else if (X86::GR32RegClass.contains(destReg))
      BuildMI(MBB, MBBI, DL, TII->get(X86::RDFSBASE), victimReg);
    else
      assert(false && "RDFSBASE to too small register");

    addRegReg(BuildMI(MBB, NMBBI, DL, TII->get(X86::LEA64r), destReg),
              victimReg, false, destReg, true);
    BuildMI(MBB, NMBBI, DL, TII->get(X86::POP64r), victimReg);

    return true;
  } break;
  }
  unsigned segregidx = indexofRIP + 4;
  MachineOperand &MO = MBBI->getOperand(segregidx);
  assert(segregidx < MBBI->getNumOperands() && "Out of operands bounds");
  assert(MO.getReg() == 0 && "Segments of RIP address  is zero");
  MO.setReg(X86::FS);
  return true;
}

bool X86MDSFIControlGuard::RelocatePIC(MachineFunction &Fn) {
  for (auto &MBB : Fn) {
    if (MBB.empty())
      continue;

    MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
    while (MBBI != E) {
      MachineBasicBlock::iterator NMBBI = std::next(MBBI);

      for (unsigned i = 0; i < MBBI->getNumOperands(); i++) {
        MachineOperand &MO = MBBI->getOperand(i);
        if (MO.isReg() && MO.getReg() == X86::RIP) {
          ds2fs(MBB, MBBI, i);
          // jump out for loop to next instruction
          break;
        }
      }
      MBBI = NMBBI;
    }
  }
  return true;
}

bool X86MDSFIControlGuard::HandleCalls(MachineBasicBlock &MBB,
                                       MachineInstr &MI) {
  bool changed = false;
  switch (MI.getOpcode()) {
  default:
    /* LLVM_DEBUG(dbgs() <<"Unhandled call instruction\t"<<MI<<"\n"); */
    break;
  case X86::CALL16r:
  case X86::CALL32r:
  case X86::CALL64r:
    changed = HandleRegInst(MBB, MI, X86::R10);
    break;
  case X86::CALL16m:
  case X86::CALL32m:
  case X86::CALL64m:
    changed = HandleMemInst(MBB, MI, X86::CALL64r, X86::R10, X86::R11);
    break;
  }
  return changed;
}

bool X86MDSFIControlGuard::CFIInstrument(MachineFunction &Fn) {
  bool hasIndirectJump = false;
  for (auto &MBB : Fn) {
    if (MBB.empty())
      continue;

    MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
    while (MBBI != E) {
      MachineBasicBlock::iterator NMBBI = std::next(MBBI);

      MachineInstr &MI = *MBBI;
      MBBI = NMBBI;
      if (MI.isReturn()) {
        HandleRets(MBB, MI);
      } else if (MI.isIndirectBranch()) {
        hasIndirectJump = true;
        HandleIndirectBrs(MBB, MI);
      } else if (MI.isCall()) {
        // note some pseudo which is both return and call will not reach here
        HandleCalls(MBB, MI);
      }
    }
  }
  return hasIndirectJump;
}
#undef DEBUG_TYPE

INITIALIZE_PASS(X86MDSFIControlGuard, "mdsfi-cg-instrument", "CFI Instrument",
                false, false)

FunctionPass *llvm::createX86MDSFIControlGuard() {
  return new X86MDSFIControlGuard();
}
