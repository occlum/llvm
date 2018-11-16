This is toolchain for occlum. build your project with this toolchain.  
## How to build
This toolchain also require clang and lld, so clone occlum lld into lld tools
```
git clone -b develop https://github.com/occlum/lld tools/
```
Then clone clang to tools and checkout to our developing commit
```
git clone https://github.com/llvm-mirror/clang tools/
pushd tools/clang
git checkout 2ed6f58935a3e4d7ee4ede934d9bbda7db9a5cee
```

To build llvm, make a build dir aloneside with llvm
```
pushd ../
mkdir build-llvm
pushd build-llvm
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=True -DLLVM_TARGETS_TO_BUILD="X86" CMAKE_BUILD_TYPE=Debug ../llvm/
make
popd
```

## How to use
To use this tool chain, When compile your project, use the clang, llvm and lld in this project.
Here is a Compile command example
```
BC_FLAGS=-Xclang -load -Xclang $(LLVMPATH)/lib/LLVMBoundchecker.so -mllvm -check-store-only=false
CFLAGS := -Wall -g -O0 -I../include -fPIC
CFLAGS += $(BC_FLAGS)
LDFLAGS := -g -O0 -pie -fuse-ld=lld 
```

For more details about usage, please refer to toolchain-benchmark: [toolchain-benchmark](https://github.com/occlum/toolchain-benchmark)

