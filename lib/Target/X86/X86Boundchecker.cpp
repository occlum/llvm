#include <bitset>

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

namespace {
using RegsBitSet = std::bitset<X86::NUM_TARGET_REGS>;

class CheckedRegs {
public:
  CheckedRegs(const TargetRegisterInfo *TRI) {
    this->TRI = TRI;
    assert(TRI && "Init CheckedReg from pointer TRI not inited ");
    Regs.reset();
  }

  CheckedRegs(const CheckedRegs &CR) {
    this->TRI = CR.TRI;
    assert(TRI && "Init CheckedReg from pointer TRI not inited ");
    this->Regs = CR.Regs;
  }

  CheckedRegs(const CheckedRegs *CR) {
    this->TRI = CR->TRI;
    assert(TRI && "Init CheckedReg from pointer CR which TRI is not inited ");
    this->Regs = CR->Regs;
  }

  CheckedRegs &operator=(const CheckedRegs &src) {
    this->TRI = src.TRI;
    this->Regs = src.Regs;
    return *this;
  }

  CheckedRegs() = delete;

  void clear() { Regs.reset(); }
  void reset() { Regs.reset(); }
  bool empty() { return Regs.none(); }
  bool none() { return Regs.none(); }

  // remember that handle alias of register(rax,eax,ex they are the same)
  // carefully
  void addReg(unsigned Reg) {
    assert(TRI && "Checked Regs is not initialized. ");
    assert(Reg <= TRI->getNumRegs() && "Expected a physical register");
    for (MCSubRegIterator SubRegs(Reg, TRI, true); SubRegs.isValid(); ++SubRegs)
      Regs.set(*SubRegs);
  }

  // Add a set of regs
  // All those reg has same range
  void addSet(unsigned Reg) {}

  void removeReg(unsigned Reg) {
    assert(TRI && "Checked Regs is not initialized. ");
    assert(Reg <= TRI->getNumRegs() && "Expected a physical register");
    for (MCSubRegIterator SubRegs(Reg, TRI, true); SubRegs.isValid(); ++SubRegs)
      Regs.reset(*SubRegs);
  }

  bool contains(unsigned Reg) const { return Regs.test(Reg); }

private:
  const TargetRegisterInfo *TRI = nullptr;
  RegsBitSet Regs;
};

// ProgramStates records the states when we scan a function by BB.
// MBB means the Machine BasicBlock to scan
// CheckedRegs means the input states of This MBB, usually is the result from
// last MBB
class ProgramStates {
public:
  // Everytime, new a Program States will create a new copy of Checked Regs
  ProgramStates(MachineBasicBlock *cMBB, CheckedRegs *Loads,
                CheckedRegs *Stores)
      : CheckedLoadRegs(Loads), CheckedStoreRegs(Stores) {
    MBB = cMBB;
    assert(Loads != nullptr && "LoadRegs is nullptr");
    assert(Stores != nullptr && "StoreRegs is nullptr");
  }

  MachineBasicBlock *getMBB() { return MBB; }
  CheckedRegs *getCheckedLoadRegs() { return &CheckedLoadRegs; }
  CheckedRegs *getCheckedStoreRegs() { return &CheckedStoreRegs; }

private:
  MachineBasicBlock *MBB;
  // we store all register checked in Checked[Load|Store]Regs.
  // If the register is killed, we remove it from the set.
  CheckedRegs CheckedLoadRegs;
  CheckedRegs CheckedStoreRegs;
};

class X86CFIInstrument : public MachineFunctionPass {
public:
  static char ID;
  X86CFIInstrument() : MachineFunctionPass(ID) {}

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
char X86CFIInstrument::ID = 0;

class X86ConstraintCheck : public MachineFunctionPass {
public:
  static char ID;
  const X86Subtarget *STI;
  const TargetInstrInfo *TII;
  const TargetRegisterInfo *TRI;

  X86ConstraintCheck() : MachineFunctionPass(ID) {}

  bool runOnMachineFunction(MachineFunction &Fn) override;
  bool ScanMF(MachineFunction &Fn);
  bool LoweringCheck(MachineFunction &Fn);
  bool LoweringMI(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI);
  bool ScanMBB(MachineBasicBlock *MBB);
  bool OptMI(MachineInstr *MI, bool isload);

  bool hasIndirectCall(MachineFunction &Fn);

private:
  // GuardZone size 4K
  int GuardZoneSize = 4 * 1024;
  // We store Check Functions in CheckFuncList
  // The value bool means eliminable. False means it's not eliminable
  std::map<MachineInstr *, bool> CheckFuncList;
  // point to currently used checkedregs set
  CheckedRegs *CheckedLoadRegs = nullptr;
  CheckedRegs *CheckedStoreRegs = nullptr;
};
char X86ConstraintCheck::ID = 0;
} // namespace

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
  /* outs() << "HandleIndirectBr\n"; */
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
  outs() << "HandleIndirectMemCall " << MI << "\n";
  // movq (x),%r10
  MachineInstr *loadtarget = BuildMI(MBB, MI, DL, TII->get(X86::MOV64rm), X86::R10);
  for (auto &MO : MI.operands()) {
    loadtarget->addOperand(MO);
  }
  // bndcl (r10), bnd2
  // directly check because code section must be readonly
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rm), X86::BND2)
      .addReg(X86::R10)
      .addImm(1)
      .addReg(0)
      .addImm(0)
      .addReg(0);
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rm), X86::BND2)
      .addReg(X86::R10)
      .addImm(1)
      .addReg(0)
      .addImm(0)
      .addReg(0);

  // call *r10
  BuildMI(MBB, MI, DL, TII->get(X86::CALL64r), X86::R10);
  MI.eraseFromParent();
  return true;
}

bool X86CFIInstrument::HandleIndirectRegCall(MachineBasicBlock &MBB,
                                             MachineInstr &MI) {
  const DebugLoc &DL = MI.getDebugLoc();
  MachineOperand &MO = MI.getOperand(0);
  // bndcu (MO), bnd2
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCU64rm), X86::BND2)
      .add(MO)
      .addImm(1)
      .addReg(0)
      .addImm(0)
      .addReg(0);
	//bndcl (MO), bnd2
  BuildMI(MBB, MI, DL, TII->get(X86::BNDCL64rm),	X86::BND2)
      .add(MO)
      .addImm(1)
      .addReg(0)
      .addImm(0)
      .addReg(0);
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
			outs() <<"Error: no non-empty basic block at function " << Fn.getName();
			return false;
		}
		FirstMBBI = NMBBI;
	}
	// now the firstMBB is the first non-empty BB at Fn
  // insert CFI_LABEL before first instruction at the function
	auto &FirstMBB = *FirstMBBI;
	auto &FirstMI = *FirstMBB.getFirstNonPHI();
	/* outs() <<"First MI : " << FirstMI; */
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

  bool hasIndirectJump = false;
  hasIndirectJump = CFIInstrument(Fn);
  InsertCFILabel(Fn, hasIndirectJump);

  // disable if for SPEC
  /* RelocatePIC(Fn); */

  /* for (auto &MBB : Fn) { */
  /*   if (MBB.empty()) */
  /*     continue; */

  /*   for (MachineBasicBlock::instr_iterator I = MBB.instr_begin(); */
  /*        I != MBB.instr_end(); I++) { */
  /*     MachineInstr &MI = *I; */
			/* outs()<< "Machine Instr at end: << " << MI << "\n"; */
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
      switch (MI.getOpcode()) {
      default:
        break;
      // FIXME ret might have a imm to pop
      case X86::RET:
      case X86::RETL:
      case X86::RETQ:
				if(Fn.getName() == "main") 
					continue;
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
      MBBI = NMBBI;
    }
  }
  return hasIndirectJump;
}

/* //read metadata and extract constraint. */
/* bool X86ConstraintCheck::ExtracatConstraint(){ */
/*   return false; */
/* } */

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
  assert(TRI && "TRI is not initialized. ");

  if (Fn.getName().startswith("_boundchecker_"))
    return false;

  // if there are any indirectCall, then we can not optimization this function
  // because it can jump before basicblock
  /* bool canOptimizate = !hasIndirectCall(Fn); */
  /* if(canOptimizate == true) */
  /*   ScanMF(Fn); */
  LoweringCheck(Fn);

  CheckFuncList.clear();
  /* CheckedLoadRegs->reset(); */
  /* CheckedStoreRegs->reset(); */

  return true;
}

bool X86ConstraintCheck::OptMI(MachineInstr *MI, bool isload) {
  // X64 address mode
  // Base + [1,2,4,8] * IndexReg + Disp32
  // Index:        0     |    1        2       3           4
  // Meaning:   DestReg, | BaseReg,  Scale, IndexReg, Displacement
  // OperandTy: VirtReg, | VirtReg, UnsImm, VirtReg,   SignExtImm
  // Two common ways to construct an address we observered
  // Firstly is the most common way, basereg + offset
  // BaseReg, Scale , IndexReg, Displacement
  //  base  ,   1   ,   NoReg   ,   Imm/Expr
  // To optimize:
  // a. the base should inside the zone
  // b. the scale + IndexReg + Disp32 can not large then 4K(a variable).
  // Currently this requires the IndexReg is $noreg.
  //
  // TODO add range analysis if the indexreg exist we can detemine the range of
  // indexreg
  // This should be useful in a loop
  //
  // Secondly is for an array
  // BaseReg, Scale, IndexReg, Displacement
  //    0   ,   4  ,   index ,    baseaddr of array
  // The scale is the sizeof(element)
  // the index usually is rax or other gerenal register
  // the displacement is the BaseAddr of the array
  //
  // Summary:
  // The address mode of X84 is X + a*Y + b
  // X is basereg, and Y is index reg, while a and b is imm
  // Consider X and Y,if only X exist, we can check X if b is less then 4K.
  // if Y is not noreg, we need range analysis to know the range of Y
  MachineOperand &BaseReg = MI->getOperand(0);
  int64_t ScaleVal = MI->getOperand(1).getImm();
  MachineOperand &IndexReg = MI->getOperand(2);
  MachineOperand &Disp = MI->getOperand(3);
  CheckedRegs *Regs = isload ? CheckedLoadRegs : CheckedStoreRegs;
  // BaseReg exist and Index Reg not exist
  if (BaseReg.getReg() != 0 && IndexReg.getReg() == 0) {
    // this BaseReg is in the region
    if (Regs->contains(BaseReg.getReg())) {
      if (Disp.isImm()) {
        int64_t DispVal = Disp.getImm();
        // if the dispval large to GuardZone
        // keep this checkfunc
        if (abs(DispVal) > GuardZoneSize) {
          CheckFuncList[MI] = false;
        }
        /* outs() << "This time eliminable\n\n"; */
      } else if (Disp.isGlobal()) {
        outs() << "Global Disp with BaseReg: " << *MI;
      }
    } else {
      CheckFuncList[MI] = false;
    }
    // no matter what, it means this register is in the region after this
    // instruction
    Regs->addReg(BaseReg.getReg());
    // no matter waht, it must in readable region
    CheckedLoadRegs->addReg(BaseReg.getReg());
  } else {
    outs() << "BaseReg is noreg or IndexReg is no noreg\n";
    CheckFuncList[MI] = false;
  }
  return true;
}

/* This Function will scan the block instruction by instruction. */
/* If the instruction is a check function, then we find the register it checked
 * and put the register in the checked set. */
/* otherwise the instruction is other instruction, we will check if a register
 * in checked set is modified by this instruction. */
/* If so, we shall remove corresponding register from check set */
/* Also if the instruction is a call, we shall reset all CheckedRegs Since all
 */
/* register might be spilled in other call, and recovered as ABI defined. */
/* TODO add range analysis for the register already checked */
/* FIXME Bug might happen. If the compiler pass an intermediate register to
 * check function, then we track the wrong register. */
/* This will happen when using O0, and might happened at O1-O3. */
bool X86ConstraintCheck::ScanMBB(MachineBasicBlock *MBB) {
  for (MachineBasicBlock::iterator I : *MBB) {
    MachineInstr *MI = &*I;
    /* outs() << "Scan instruction: " << *I; */
    bool isload = false;
    switch (MI->getOpcode()) {
    case X86::checkload64m:
      isload = true;
    case X86::checkstore64m: {
      // first time visit, write it clearly
      if (CheckFuncList.count(MI) == 0)
        CheckFuncList[MI] = true;
      if (CheckFuncList[MI] == false)
        continue;
      OptMI(MI, isload);
    } break;

    case X86::MOV64rr:
    case X86::MOV32rr: {
      MachineOperand &src = MI->getOperand(0);
      MachineOperand &dst = MI->getOperand(0);
      // make a link between src and dst
      if (CheckedLoadRegs->contains(src.getReg())) {
        CheckedLoadRegs->addReg(dst.getReg());
      }
      if (CheckedStoreRegs->contains(src.getReg())) {
        CheckedStoreRegs->addReg(dst.getReg());
      }
    } break;
    case X86::LEA64r:
    case X86::LEA64_32r: {
      MachineOperand &dst = MI->getOperand(0);
      MachineOperand &BaseReg = MI->getOperand(1);
      MachineOperand &src = BaseReg;
      MachineOperand &IndexReg = MI->getOperand(3);
      MachineOperand &Disp = MI->getOperand(4);
      // BaseReg exist and Index Reg not exist
      if (BaseReg.getReg() != 0 && IndexReg.getReg() == 0) {
        // this BaseReg is in the region
        if (Disp.isImm()) {
          int64_t DispVal = Disp.getImm();
          // if the dispval large to GuardZone
          // keep this checkfunc
          if (abs(DispVal) > GuardZoneSize) {
            if (CheckedLoadRegs->contains(src.getReg())) {
              CheckedLoadRegs->addReg(dst.getReg());
            }
            if (CheckedStoreRegs->contains(src.getReg())) {
              CheckedStoreRegs->addReg(dst.getReg());
            }
          }
        }
      }
    } break;

    default:
      if (MI->isCall()) {
        CheckedLoadRegs->reset();
        CheckedStoreRegs->reset();
      }

      break;
    }
    // if MO is reg and kill, then remove it out of set
    // carefully tackle alias of register(rax,eax,cx)
    for (auto &MO : MI->operands()) {
      if (MO.isReg() && MO.isKill()) {
        auto Reg = MO.getReg();
        if (X86::GR64RegClass.contains(Reg) ||
            X86::GR32RegClass.contains(Reg) ||
            X86::GR16RegClass.contains(Reg) || X86::GR8RegClass.contains(Reg)) {
          outs() << "Kill reg: " << MO << "\n";
          CheckedLoadRegs->removeReg(MO.getReg());
          CheckedStoreRegs->removeReg(MO.getReg());
        }
      }
    }
  };
  return true;
}

// Scan a function basicblock by basicblock.
// Each step has a unique status. Before move on next step, push the status too.
// While step back, pop the steps at the same time;
bool X86ConstraintCheck::ScanMF(MachineFunction &Fn) {
  MachineBasicBlock *startMBB = &Fn.front();
  SmallVector<ProgramStates *, 32> WorkList;
  SmallPtrSet<MachineBasicBlock *, 32> VisitedSet;

  CheckedRegs initRegs = CheckedRegs(TRI);
  WorkList.push_back(new ProgramStates(startMBB, &initRegs, &initRegs));

  bool fresh;

  while (!WorkList.empty()) {
    ProgramStates *curPS = WorkList.pop_back_val();
    MachineBasicBlock *curMBB = curPS->getMBB();
    CheckedLoadRegs = curPS->getCheckedLoadRegs();
    CheckedStoreRegs = curPS->getCheckedStoreRegs();

    outs() << "Scan block: " << curMBB->getFullName() << "\n";
    // If this Basic block is not visited, then put it's successors to worklist
    fresh = VisitedSet.insert(curMBB).second;

    ScanMBB(curMBB);

    if (fresh) {
      for (MachineBasicBlock *SucMBB : curMBB->successors()) {
        // if first scan of current Basic Block
        // put all successors in worklist
        // After scan, push current State with it's successors
        WorkList.push_back(
            new ProgramStates(SucMBB, CheckedLoadRegs, CheckedStoreRegs));
        outs() << "put to worklist: " << SucMBB->getFullName() << "\n";
      }
    }
    delete curPS;
  }
  return true;
}

bool X86ConstraintCheck::LoweringMI(MachineBasicBlock &MBB,
                                    MachineBasicBlock::iterator MBBI) {
  /* outs() << "Lowering instruction: " << *MBBI; */
  MachineInstr &IMI = *MBBI;
  MachineInstr *MI = &IMI;
  DebugLoc DL = MI->getDebugLoc();
#define DISABLE_DATA_SANDBOX
#ifdef DISABLE_DATA_SANDBOX
  switch (MI->getOpcode()) {
  case X86::checkstore64m:
  case X86::checkload64m: {
    MBBI->eraseFromParent();
    return true;
  }
  }
  return false;
#endif

  switch (MI->getOpcode()) {
  case X86::checkstore64m: {
    if (CheckFuncList[MI] == true) {
      /* outs() << "Eliminate a CheckStore\n"; */
      MBBI->eraseFromParent();
      return true;
    }
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
    // If it is eliminable, erase it and goto next instruction
    if (CheckFuncList[MI] == true) {
      /* outs() << "Eliminate a CheckLoad\n"; */
      MBBI->eraseFromParent();
      return true;
    }
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

bool X86ConstraintCheck::LoweringCheck(MachineFunction &Fn) {
  for (MachineBasicBlock &MBB : Fn) {
    if (MBB.empty())
      continue;
    MachineBasicBlock::iterator MBBI = MBB.begin(), E = MBB.end();
    while (MBBI != E) {
      MachineBasicBlock::iterator NMBBI = std::next(MBBI);
      LoweringMI(MBB, MBBI);
      MBBI = NMBBI;
    }
  }
  return true;
}

FunctionPass *llvm::createX86CFIInstrument() { return new X86CFIInstrument(); }
FunctionPass *llvm::createX86ConstraintCheck() {
  return new X86ConstraintCheck();
}
