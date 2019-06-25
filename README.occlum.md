# Occlum LLVM Toolchain

Occlum's LLVM toolchain compiles C/C++ programs and generates ELF binaries that can be loaded and run by Occlum LibOS inside Intel SGX enclaves.

## How to build

Occlum toolchain requires [Clang](https://github.com/llvm-mirror/clang), the LLVM frontend for C family languages, and [LLD](https://github.com/occlum/lld), the LLVM linker. As the code of Clang and LLD are in separate repositories, we have to download them first into the `tools` directory of LLVM.

    cd /path/to/occlum/llvm/tools

    git clone https://github.com/llvm-mirror/clang
    cd clang
    git checkout 0513b409d5e

    cd ../
    git clone https://github.com/occlum/lld -b for_occlum

Then, we can build LLVM, Clang, and LLD with the following commands:


    cd /path/to/occlum/llvm/../
    mkdir build-llvm
    cd build-llvm
    cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=True -DLLVM_TARGETS_TO_BUILD="X86" -DCMAKE_INSTALL_PREFIX=/usr/local/occlum/ ../llvm/
    make
    sudo make install

After LLVM is built, make sure LLVM binaries can be found by adding the following line to the config file for your shell (e.g., `~/.bashrc`)

    export PATH="/usr/local/occlum/bin:$PATH"


## How to use

Occlum LLVM toolchain can be used to compile C/C++ programs just like GCC, but with some extra flags. Here is a sample code for Makefile

    CC := clang
    CLANG_BIN_PATH := $(shell clang -print-prog-name=clang)
    LLVM_PATH := $(abspath $(dir $(CLANG_BIN_PATH))../)

    CFLAGS := -fPIC
    LDFLAGS := -fuse-ld=ld -pie

### Extra Compiler Options

Occlum LLVM toolchain has extra compiler arguments are used for controlling the instrumentation of Multi-Domain Software Fault Isolation (MDSFI). Here is the list of the extra arguments and its default value.

Options | Default Values | Descriptions  
----------------| ------------|---------------
check-store-only |  true| set this to true will insert guard before memory writes only  
disable-SFI   |   false  |   set this to true will not insert any data guards  
enable-x86-mdsfidg | true | set this to false will not translate memory guards instruction. I.e. all memory guard is eliminated  
enable-x86-mdsfidg-opt | true| set this to false will disable all optimizations on memory guards except GOT optimization  
enable-x86-mdsfidg-loop-opt | true | set this to false will disable loop optimizations on memory guards  
enable-x86-mdsficg | true | set this to false will disable all CFI instrument in binary  
enable-x86-fs-relocate | false| set this to true will insert FS relocate code.  

## Note

Occlum LLVM toolchain is not meant to compile C programs for native platforms (e.g., Linux). For more concrete examples of how to use this toolchain with Occlum LibOS, see [the Makefile](https://github.com/occlum/libos/blob/master/test/test_common.mk) for compiling the C test programs in Occlum LibOS.
