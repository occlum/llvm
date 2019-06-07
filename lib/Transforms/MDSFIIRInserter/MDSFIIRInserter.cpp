#include <cassert>
#include <set>
#include <string>

#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>
#include <llvm/PassAnalysisSupport.h>
#include <llvm/PassRegistry.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>

using namespace llvm;

static cl::opt<bool> CheckStoreOnly("check-store-only", cl::init(true),
                                    cl::desc("only check store operation"),
                                    cl::Hidden);
static cl::opt<bool>
    NoSFI("disable-SFI", cl::desc("do not check memory operation"), cl::Hidden);

struct MDSFIIRInserterPass : public ModulePass {
public:
  static char ID;
  MDSFIIRInserterPass() : ModulePass(ID) {}
  virtual bool runOnModule(Module &M);

private:
  Module *curModule;
  void insertCheck(Value *ptr, Instruction *I, bool isload);
  void handleInst(Instruction *I);
  bool ScanStorePtr(Value *u);
  std::set<Value *> StorePtrSet;
};

void MDSFIIRInserterPass::insertCheck(Value *ptr, Instruction *I, bool isload) {
  if (NoSFI) {
    return;
  }
  // static check can be done here;
  if (isa<Constant>(ptr)) {
    return;
  }
  if (CheckStoreOnly) {
    if (isload) {
      return;
    }
  }
  Function *checkFun = nullptr;

  IRBuilder<> B(I);
  if (isload) {
    checkFun = Intrinsic::getDeclaration(curModule, Intrinsic::x86_checkload,
                                         {ptr->getType()});
  } else {
    checkFun = Intrinsic::getDeclaration(curModule, Intrinsic::x86_checkstore,
                                         {ptr->getType()});
  }
  if (checkFun == nullptr) {
    return;
  }
  B.CreateCall(checkFun, {ptr});
  return;
}

void MDSFIIRInserterPass::handleInst(Instruction *I) {
  if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
    insertCheck(LI->getOperand(0), LI, true);
  } else if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
    insertCheck(SI->getOperand(1), SI, false);
  } else if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(I)) {
    MemTransferInst *MTI = dyn_cast<MemTransferInst>(MI);
    if (MTI) {
      insertCheck(MTI->getRawSource(), MTI, true);
    }
    insertCheck(MI->getRawDest(), MI, false);
  } 
}

// This function scan a pointer to see if it's in writable region
bool MDSFIIRInserterPass::ScanStorePtr(Value *V) {
  for (User *u : V->users()) {
    if (dyn_cast<StoreInst>(u)) {
      StorePtrSet.insert(V);
      return true;
    } else if (MemIntrinsic *MI = dyn_cast<MemIntrinsic>(u)) {
      if (u == MI->getRawDest()) {
        StorePtrSet.insert(V);
        return true;
      }
    }
  }
  return false;
}

bool MDSFIIRInserterPass::runOnModule(Module &M) {
  errs() << "MDSFIIRInserter running...\n";

  curModule = &M;

  // Fix up tracking variables so static lib knows compilation params
  for (Function &F : M) {
    if (F.isDeclaration())
      continue;

    // Scan Function parameters
    for (auto arg = F.arg_begin(); arg != F.arg_end(); arg++) {
      if (NoSFI)
        break;
      if (arg->getType()->isPointerTy()) {
        ScanStorePtr(arg);
        Instruction *I = &*inst_begin(&F);
        IRBuilder<> B(I);
        B.SetInsertPoint(I);
        Function *checkstore = Intrinsic::getDeclaration(
            curModule, Intrinsic::x86_checkstore, {arg->getType()});
        B.CreateCall(checkstore, {arg});
      }
    }

    // insert check before every memory operation
    for (inst_iterator II = inst_begin(&F), E = inst_end(&F); II != E; ++II) {
      Instruction *I = &*II;
      handleInst(I);
    }

    StorePtrSet.clear();
  }
  errs() << "MDSFIIRInserter finished...\n";
  return true;
}

char MDSFIIRInserterPass::ID = 0;
static RegisterPass<MDSFIIRInserterPass>
    X("MDSFIIRInserter", "MDSFIIRInserter pass", false, false);

// This function is of type PassManagerBuilder::ExtensionFn
static void loadPass(const PassManagerBuilder &Builder,
                     legacy::PassManagerBase &PM) {
  PM.add(new MDSFIIRInserterPass());
}
// These constructors add our pass to a list of global extensions.
// static RegisterStandardPasses
// MDSFIIRInserterLoader_Ox(PassManagerBuilder::EP_EarlyAsPossible, loadPass);
/* static RegisterStandardPasses
 * MDSFIIRInserterLoader_Ox(PassManagerBuilder::EP_ModuleOptimizerEarly,
 * loadPass);
 */
static RegisterStandardPasses
    MDSFIIRInserterLoader_Ox(PassManagerBuilder::EP_OptimizerLast, loadPass);
static RegisterStandardPasses
    MDSFIIRInserterLoader_O0(PassManagerBuilder::EP_EnabledOnOptLevel0,
                             loadPass);
