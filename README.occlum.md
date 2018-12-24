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
git checkout 0513b409d5e34b2d2a28ae21b6d620cc52de0e57 
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
Here is a Compile command example:   
```
BC_FLAGS=-Xclang -load -Xclang $(LLVMPATH)/lib/LLVMBoundchecker.so -mllvm -check-store-only=false
CFLAGS := -Wall -g -O0 -I../include -fPIC
CFLAGS += $(BC_FLAGS)
LDFLAGS := -g -O0 -pie -fuse-ld=lld 
```

### Optionals  
These optionals are used for control the behavior of toolchain. Here is a list and its default value.  

option | default value | description  
----------------| ------------|---------------
check-store-only |  false  | set this to true will insert guard before memory writes only  
disable-SFI   |   false  |   set this to true will not insert any memory guards  
enable-x86-boundchecker| true | set this to false will not translate memory guards instruction. I.e. all memory guard is eliminated  
enable-x86-boundchecker-opt | true| set this to false will disable all optimizations on memory guards except GOT optimization  
enable-x86-boundchecker-loop-opt | true | set this to false will disable loop optimizations on memory guards  
enable-x86-cfiinstr | true | set this to false will disable all CFI instrument in binary  
enable-x86-fs-relocate | false | set this to true will insert FS relocate code.  
 
For more details about usage, please refer to toolchain-benchmark: [toolchain-benchmark](https://github.com/occlum/toolchain-benchmark)

