This is toolchain for occlum. build your project with this toolchain.  
## How to build
This toolchain also require clang and lld, so clone occlum lld into lld tools
```
cd tools
git clone -b for_occlum https://github.com/occlum/lld
```
Then clone clang to tools and checkout our commit.
```
cd tools
git clone https://github.com/llvm-mirror/clang 
git checkout 0513b409d5e
```

To build llvm, make a build dir aloneside with llvm  
```
cd ../
mkdir build-llvm
cd build-llvm
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=True -DLLVM_TARGETS_TO_BUILD="X86" ../llvm/
make
```

## How to use   
To use this tool chain, When compile your project, use the clang, llvm and lld in this project.  
Here is a compiler command example:   
```
BC_FLAGS=-Xclang -load -Xclang $(LLVMPATH)/lib/LLVMMDSFIIRInserter.so -mllvm -check-store-only=false
CFLAGS := -Wall -g -O0 -I../include -fPIC
CFLAGS += $(BC_FLAGS)
LDFLAGS := -g -O0 -pie -fuse-ld=lld 
```

### Options  
These options are used for control the behavior of toolchain. Here is a list and its default value.  

option | default value | description  
----------------| ------------|---------------
check-store-only |  true| set this to true will insert guard before memory writes only  
disable-SFI   |   false  |   set this to true will not insert any data guards  
enable-x86-mdsfidg | true | set this to false will not translate memory guards instruction. I.e. all memory guard is eliminated  
enable-x86-mdsfidg-opt | true| set this to false will disable all optimizations on memory guards except GOT optimization  
enable-x86-mdsfidg-loop-opt | true | set this to false will disable loop optimizations on memory guards  
enable-x86-mdsficg | true | set this to false will disable all CFI instrument in binary  
enable-x86-fs-relocate | false| set this to true will insert FS relocate code.  
 
For more details about usage, please refer to examples in LibOS: [LibOS](https://github.com/occlum/libos)

