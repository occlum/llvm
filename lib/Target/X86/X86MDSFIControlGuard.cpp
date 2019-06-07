

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
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/Passes.h"

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

  /// This function will replace return instruction with a pop, CFI check and an indirect jump
  bool HandleRet(MachineBasicBlock &MBB, MachineInstr &MI);

  bool HandleIndirectBr(MachineBasicBlock &MBB, MachineInstr &MI);

  /// Replace a memory-based indirect call with a memory load and register-based indirect call
  /// also insert a CFI check before register-based indirect call 
  bool HandleIndirectMemCall(MachineBasicBlock &MBB, MachineInstr &MI);

  /// Insert a CFI check before register-based indirect call
  bool HandleIndirectRegCall(MachineBasicBlock &MBB, MachineInstr &MI);

  /// Insert CFI label before every function. If hasIndirectJump is true, then insert CFI labels before every basicblock either.
  bool InsertCFILabel(MachineFunction &Fn, bool hasIndirectJump);

  bool RelocatePIC(MachineFunction &Fn);
  bool ds2fs(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
             unsigned indexofRIP);

  bool CFIInstrument(MachineFunction &Fn);
};
} // namespace
char X86MDSFIControlGuard::ID = 0;

bool X86MDSFIControlGuard::HandleRet(MachineBasicBlock &MBB, MachineInstr &MI) {
      switch (MI.getOpcode()) {
      default:
        LLVM_DEBUG(dbgs() <<"Unhandled return instruction\t"<<MI<<"\n");
        return false;
        break;
      case X86::RET:
      case X86::RETL:
      case X86::RETQ:
        break;
      }
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

bool X86MDSFIControlGuard::HandleIndirectBr(MachineBasicBlock &MBB,
                                            MachineInstr &MI) {
  const DebugLoc &DL = MI.getDebugLoc();
  LLVM_DEBUG(dbgs() << "HandleIndirectBr\n");
  // No matter under what situation(Memory based jump or register based jump), we need a free register;
  // Firstly we try to use RegisterScavenging
  RegScavenger *RegSc = RS.get();
  RegSc->enterBasicBlockEnd(MBB);
  unsigned ScavReg = RegSc->scavengeRegisterBackwards(X86::GR64RegClass, MachineBasicBlock::iterator(MI),false,0);
  if(ScavReg == 0){
    LLVM_DEBUG(dbgs() <<"Can not find free register for indirect branch\n");
    return false;
  }


  LLVM_DEBUG(dbgs() <<"ScavReg for indirectBranch: "<< TRI->getRegAsmName(ScavReg)<<"\n");

  unsigned TargetReg;
  X86AddressMode AM;
  MachineInstr *loadtarget;
  switch (MI.getOpcode()) {
  default:
    LLVM_DEBUG(dbgs() <<"Unhandled indirect branch instruction\t"<<MI<<"\n");
    return false;
    break;
  case X86::JMP16m:
  case X86::JMP32m:
  case X86::JMP64m:
    // which physic register is used in this jump?
    AM = getAddressFromInstr(&MI, 0);
    TargetReg = AM.BaseType == X86AddressMode::RegBase? AM.Base.Reg : AM.IndexReg;
    // movq (x), x
    loadtarget = BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), TargetReg);
    for (auto &MO : MI.operands()) {
      loadtarget->addOperand(MO);
      MI.RemoveOperand(MO.getIndex());
    }
    MI.setDesc(TII->get(X86::JMP64r));
    MachineInstrBuilder(*MBB.getParent(), &MI).addReg(TargetReg);
    break;
  case X86::JMP16r:
  case X86::JMP32r:
  case X86::JMP64r:
  TargetReg = MI.getOperand(0).getReg();
    break;
  }
  // movq (%TargetReg) , %FreeReg load cfi lable
  addDirectMem(BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), ScavReg),
               TargetReg);
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rr), X86::BND2).addReg(ScavReg);
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rr), X86::BND2).addReg(ScavReg);
  return true;
}

// replace call memory to call register
bool X86MDSFIControlGuard::HandleIndirectMemCall(MachineBasicBlock &MBB,
                                                 MachineInstr &MI) {
  const DebugLoc &DL = MI.getDebugLoc();
  LLVM_DEBUG(dbgs() << "HandleIndirectMemCall " << MI << "\n");
  // movq (x),%r11
  MachineInstr *loadtarget =
      BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), X86::R11);
  for (auto &MO : MI.operands()) {
    loadtarget->addOperand(MO);
  }
  LLVM_DEBUG(dbgs() << "loadtarget " << *loadtarget << "\n");
  // movq (%r11) , %r10
  addDirectMem(BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), X86::R10),
               X86::R11);
  // bndcl (%r10), %bnd2
  // directly check because code section must be readonly
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rr), X86::BND2).addReg(X86::R10);
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rr), X86::BND2).addReg(X86::R10);

  // call *r10
  BuildMI(MBB, MI, DL, TII->get(X86::CALL64r), X86::R11);
  MI.eraseFromParent();
  return true;
}

bool X86MDSFIControlGuard::HandleIndirectRegCall(MachineBasicBlock &MBB,
                                                 MachineInstr &MI) {
  const DebugLoc &DL = MI.getDebugLoc();
  MachineOperand &MO = MI.getOperand(0);
  LLVM_DEBUG(dbgs() << "HandleIndirectRegCall " << MI << "\n");
  unsigned labelReg = MO.getReg() == X86::R10 ? X86::R11 : X86::R10;
  // mov (MO), %r10
  addDirectMem(BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), labelReg),
               MO.getReg());
  // bndcu (%r10), bnd2
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rr), X86::BND2).addReg(labelReg);
  // bndcl (%r10), bnd2
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rr), X86::BND2).addReg(labelReg);
  return true;
}

bool X86MDSFIControlGuard::InsertCFILabel(MachineFunction &Fn,
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
  const DebugLoc &FirstDL = FirstMI.getDebugLoc();
  BuildMI(FirstMBB, FirstMI, FirstDL, TII->get(X86::NOOPL))
      .addReg(X86::RBX)
      .addImm(1)
      .addReg(X86::RBX)
      .addImm(512)
      .addReg(0);

  // scan every instruction, insert CFI_LABEL after call
  for (auto &MBB : Fn) {
    if (MBB.empty())
      continue;

    MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
    while (MBBI != E) {
      MachineBasicBlock::iterator NMBBI = std::next(MBBI);
      MachineInstr &MI = *MBBI;
      const DebugLoc &DL = MI.getDebugLoc();
      switch (MI.getOpcode()) {
      default:
        break;
      /* case X86::CALL16m: */
      /* case X86::CALL32m: */
      /* case X86::CALL64m: */
      case X86::CALL16r:
      case X86::CALL32r:
      case X86::CALL64r:
      case X86::CALL64pcrel32:
      case X86::CALLpcrel32:
      case X86::CALLpcrel16:
        BuildMI(MBB, NMBBI, DL, TII->get(X86::NOOPL))
            .addReg(X86::RBX)
            .addImm(1)
            .addReg(X86::RBX)
            .addImm(512)
            .addReg(0);
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
  for (auto &MBB : Fn) {
    if (MBB.empty())
      continue;
    MachineBasicBlock::instr_iterator I = MBB.instr_begin();
    MachineInstr &MI = *I;
    const DebugLoc &DL = MI.getDebugLoc();
    BuildMI(MBB, MI, DL, TII->get(X86::NOOPL))
        .addReg(X86::RBX)
        .addImm(1)
        .addReg(X86::RBX)
        .addImm(512)
        .addReg(0);
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
  /* CFIInstrument(Fn); */
  InsertCFILabel(Fn, hasIndirectJump);

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
      if(MI.isReturn()){
        HandleRet(MBB,MI);
      }else if(MI.isIndirectBranch()){
        hasIndirectJump = true;
        HandleIndirectBr(MBB,MI);
      } else if(MI.isCall()){
      switch (MI.getOpcode()) {
      default:
        /* LLVM_DEBUG(dbgs() <<"Unhandled call instruction\t"<<MI<<"\n"); */
        break;
      case X86::CALL16m:
      case X86::CALL32m:
      case X86::CALL64m:
        HandleIndirectMemCall(MBB, MI);
        break;
      case X86::CALL16r:
      case X86::CALL32r:
      case X86::CALL64r:
        HandleIndirectRegCall(MBB, MI);
        break;
      }
      }
    }
  }
  return hasIndirectJump;
}
#undef DEBUG_TYPE

INITIALIZE_PASS(X86MDSFIControlGuard, "mdsfi-cg-instrument", "CFI Instrument", false,
                false)

FunctionPass *llvm::createX86MDSFIControlGuard() {
  return new X86MDSFIControlGuard();
}
