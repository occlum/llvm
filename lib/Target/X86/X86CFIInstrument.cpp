

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

using namespace llvm;

namespace llvm{
  void initializeX86CFIInstrumentPass(PassRegistry&);
}

#define DEBUG_TYPE "CFIInstrument"

static cl::opt<bool>
    enableX86CFIInstr("enable-x86-cfiinstr", cl::init(true), cl::Hidden,
                          cl::desc("Enable X86 cfi instrument."));

namespace {
class X86CFIInstrument : public MachineFunctionPass {
public:
  static char ID;
  X86CFIInstrument() : MachineFunctionPass(ID) {
      initializeX86CFIInstrumentPass(*PassRegistry::getPassRegistry());
  }

  bool runOnMachineFunction(MachineFunction &Fn) override;

  const X86Subtarget *STI;
  const TargetInstrInfo *TII;

  bool HandleRet(MachineBasicBlock &MBB, MachineInstr &MI);
  bool HandleIndirectBr(MachineBasicBlock &MBB, MachineInstr &MI);
  bool HandleIndirectMemCall(MachineBasicBlock &MBB, MachineInstr &MI);
  bool HandleIndirectRegCall(MachineBasicBlock &MBB, MachineInstr &MI);
  bool InsertCFILabel(MachineFunction &Fn, bool hasIndirectJump);

  bool RelocatePIC(MachineFunction &Fn);
  bool ds2fs(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
             unsigned indexofRIP);

  bool CFIInstrument(MachineFunction &Fn);
};
}
char X86CFIInstrument::ID = 0;

bool X86CFIInstrument::HandleRet(MachineBasicBlock &MBB, MachineInstr &MI) {
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

	//jmpq *%r11
  BuildMI(MBB, MI, DL, TII->get(X86::JMP64r)).addReg(X86::R11);
  // remove ret from code;
  MI.eraseFromParent();
  return true;
}

bool X86CFIInstrument::HandleIndirectBr(MachineBasicBlock &MBB,
                                        MachineInstr &MI) {
  /* const DebugLoc &DL = MI.getDebugLoc(); */
  /* LLVM_DEBUG(dbgs() << "HandleIndirectBr\n"); */
  /* MachineInstr *LEA = BuildMI(MBB, MI, DL, TII->get(X86::LEA64r), X86::R10); */
   
  /* for (auto &MO : MI->operands()) { */
  /* LEA->addOperand(MO); */
  /* } */
  /* BuildMI(MBB,MI,DL,TII->get(X86::JMP64r), X86::R10); */
  /* MI->eraseFromParent(); */
  return false;
}

// replace call memory to call register
bool X86CFIInstrument::HandleIndirectMemCall(MachineBasicBlock &MBB,
                                             MachineInstr &MI) {
  const DebugLoc &DL = MI.getDebugLoc();
  LLVM_DEBUG(dbgs() << "HandleIndirectMemCall " << MI << "\n");
  // movq (x),%r11
  MachineInstr *loadtarget = BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), X86::R11);
  for (auto &MO : MI.operands()) {
    loadtarget->addOperand(MO);
  }
  LLVM_DEBUG(dbgs() << "loadtarget " << *loadtarget<< "\n");
  // movq (%r11) , %r10
  addDirectMem(BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), X86::R10),X86::R11);
  // bndcl (%r10), %bnd2
  // directly check because code section must be readonly
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rr), X86::BND2)
      .addReg(X86::R10);
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rr), X86::BND2)
      .addReg(X86::R10);

  // call *r10
  BuildMI(MBB, MI, DL, TII->get(X86::CALL64r), X86::R11);
  MI.eraseFromParent();
  return true;
}

bool X86CFIInstrument::HandleIndirectRegCall(MachineBasicBlock &MBB,
                                             MachineInstr &MI) {
  const DebugLoc &DL = MI.getDebugLoc();
  MachineOperand &MO = MI.getOperand(0);
  LLVM_DEBUG(dbgs() << "HandleIndirectRegCall " << MI << "\n");
  unsigned labelReg = MO.getReg() == X86::R10 ? X86::R11: X86::R10;
  // mov (MO), %r10
  addDirectMem(BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), labelReg),MO.getReg());
  // bndcu (%r10), bnd2
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rr), X86::BND2)
      .addReg(labelReg);
	//bndcl (%r10), bnd2
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rr),	X86::BND2)
      .addReg(labelReg);
  return true;
}

bool X86CFIInstrument::InsertCFILabel(MachineFunction &Fn,
                                      bool hasIndirectJump) {
	MachineFunction::iterator FirstMBBI = Fn.begin();

	//sometimes the first basicblock of function might be empty
	//handle this by step to next basic block;
	// this is because some function do not need to save frame
	for(;FirstMBBI->empty();){
    auto NMBBI = std::next(FirstMBBI);
		if(FirstMBBI== Fn.end()){
			//can't find non-empty BB, exist function
			LLVM_DEBUG(dbgs() <<"Error: no non-empty basic block at function " << Fn.getName());
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
        .addReg(X86::RAX)
        .addImm(1)
        .addReg(X86::RAX)
        .addImm(512)
        .addReg(0);

	//scan every instruction, insert CFI_LABEL after call
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
        .addReg(X86::RAX)
        .addImm(1)
        .addReg(X86::RAX)
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
        .addReg(X86::RAX)
        .addImm(1)
        .addReg(X86::RAX)
        .addImm(512)
        .addReg(0);
  }
  return true;
}

bool X86CFIInstrument::runOnMachineFunction(MachineFunction &Fn) {
  STI = &static_cast<const X86Subtarget &>(Fn.getSubtarget());
  TII = STI->getInstrInfo();

  if (Fn.getName().startswith("_boundchecker_"))
    return false;
  if(!enableX86CFIInstr) {
    return false;
  }

  bool hasIndirectJump = false;
  hasIndirectJump = CFIInstrument(Fn);
  /* CFIInstrument(Fn); */
  InsertCFILabel(Fn, hasIndirectJump);

  // disable if for SPEC
  /* RelocatePIC(Fn); */

  /* for (auto &MBB : Fn) { */
  /*   if (MBB.empty()) */
  /*     continue; */

  /*   for (MachineBasicBlock::instr_iterator I = MBB.instr_begin(); */
  /*        I != MBB.instr_end(); I++) { */
  /*     MachineInstr &MI = *I; */
			/* LLVM_DEBUG(dbgs() << "Machine Instr at end: << " << MI << "\n"); */
		/* } */
	/* } */
  return true;
}

bool X86CFIInstrument::ds2fs(MachineBasicBlock &MBB,
                             MachineBasicBlock::iterator MBBI,
                             unsigned indexofRIP) {
  unsigned segregidx = indexofRIP + 4;
  MachineOperand &MO = MBBI->getOperand(segregidx);
  assert(segregidx < MBBI->getNumOperands() && "Out of operands bounds");
  assert(MO.getReg() == 0 && "Segments of RIP address  is zero");
  MO.setReg(X86::FS);
  return true;
}

bool X86CFIInstrument::RelocatePIC(MachineFunction &Fn) {
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

bool X86CFIInstrument::CFIInstrument(MachineFunction &Fn) {
  bool hasIndirectJump = false;
  for (auto &MBB : Fn) {
    if (MBB.empty())
      continue;

    MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
    while (MBBI != E) {
      MachineBasicBlock::iterator NMBBI = std::next(MBBI);

      MachineInstr &MI = *MBBI;
      MBBI = NMBBI;
      switch (MI.getOpcode()) {
      default:
        break;
      // FIXME ret might have a imm to pop
      case X86::RET:
      case X86::RETL:
      case X86::RETQ:
				/* if(Fn.getName() == "main") */ 
				/* 	continue; */
        HandleRet(MBB, MI);
        break;
      case X86::JMP16m:
      case X86::JMP32m:
      case X86::JMP64m:
      case X86::JMP16r:
      case X86::JMP32r:
      case X86::JMP64r:
        hasIndirectJump = true;
        HandleIndirectBr(MBB, MI);
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
  return hasIndirectJump;
}
#undef DEBUG_TYPE

INITIALIZE_PASS(X86CFIInstrument, "cfi-instrument", "CFI Instrument",
    false, false)

FunctionPass *llvm::createX86CFIInstrument() { return new X86CFIInstrument(); }

