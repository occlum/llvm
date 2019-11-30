#!/bin/sh 
#Usage If you want to install toolchain at a high privilege level location: sudo ./buildtoolchain.sh

SRCROOT=`pwd`/toolchain
mkdir -p ${SRCROOT}
cd ${SRCROOT}
PREFIX=/usr/local/occlum

#don't know why but clang will try to find compiler-rt at ${PREFIX}${COMPILER_RT_PREFIX} rather than ${PREFIX} 
# so we install compiler rt at there.
COMPILER_RT_PREFIX=/lib/clang/9.0.0/

#clean all previous tools and libraries
rm ${PREFIX}/* -rf

#checkout repo
git clone -b release_90 https://github.com/occlum/llvm
git clone -b master https://github.com/occlum/musl
git clone -b release_90 https://github.com/llvm-mirror/clang
git clone -b release_90 https://github.com/llvm-mirror/libcxx
git clone -b release_90 https://github.com/llvm-mirror/libcxxabi
git clone -b release_90 https://github.com/llvm-mirror/libunwind
git clone -b release_90 https://github.com/llvm-mirror/compiler-rt

# first stage
mkdir stage_1
pushd stage_1
printf "occlum build llvm\n"
cmake -DLLVM_TARGETS_TO_BUILD="X86"  -DLLVM_ENABLE_PROJECTS="clang" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${PREFIX} ../llvm
make install \
  || { printf "occlum build llvm failed\n"; exit 1; }
popd

# softlink linux header
printf "softlink linux header\n"
ln -s /usr/include/linux ${PREFIX}/include/linux
ln -s /usr/include/asm ${PREFIX}/include/asm
ln -s /usr/include/asm-generic ${PREFIX}/include/asm-generic

#second stage
printf "start second stage\n"
export PATH=/usr/local/occlum/bin:$PATH

#build musl
printf "build musl"
pushd musl
make distclean
CC=clang ./configure --prefix=${PREFIX} --enable-wrapper=clang
make install -j\
  || { printf "build musl failed\n"; exit 1; }
popd

mkdir stage_2
pushd stage_2
#build compiler-rt builtin 
printf "build compiler-rt builtin\n"
mkdir compiler-rt1
pushd compiler-rt1
rm * -rf
#It's ok to use clang++ here because there is no c++ objects here actually
cmake ../../compiler-rt -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${PREFIX}${COMPILER_RT_PREFIX} -DCMAKE_C_COMPILER=musl-clang -DCMAKE_CXX_COMPILER=clang -DCOMPILER_RT_BUILD_BUILTINS=ON -DCOMPILER_RT_BUILD_SANITIZERS=OFF -DCOMPILER_RT_BUILD_XRAY=OFF -DCOMPILER_RT_BUILD_LIBFUZZER=OFF -DCOMPILER_RT_BUILD_PROFILE=OFF -DCOMPILER_RT_BUILD_CRT=ON -DCMAKE_C_FLAGS="-fPIC " -DCMAKE_CXX_FLAGS="-fPIC "
make install -j \
  || { printf "build compiler-rt builtin failed\n" ;exit 1; }
popd
popd

# rebuild musl use compiler-rt this time
printf "build musl round 2"
pushd musl
make distclean
LIBCC="$(musl-clang -print-libgcc-file-name --rtlib=compiler-rt)" CC=musl-clang ./configure --prefix=${PREFIX} --enable-wrapper=clang
make install -j\
  || { printf "build musl failed\n"; exit 1; }
popd
pushd stage_2

#build libunwind
printf "build libunwind\n"
mkdir libunwind
pushd libunwind
rm * -rf
cmake ../../libunwind -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${PREFIX} -DCMAKE_C_COMPILER=musl-clang -DCMAKE_CXX_COMPILER=musl-clang -DLIBUNWIND_ENABLE_SHARED=OFF -DLLVM_ENABLE_LIBCXX=ON -DLIBUNWIND_USE_COMPILER_RT=ON -DCMAKE_C_FLAGS="-fPIC " -DCMAKE_CXX_FLAGS="-fPIC "
make install -j \
  || { printf "build libunwind failed\n"; exit 1; }
popd

#build libcxx first round
printf "build libcxx round 1\n"
mkdir libcxx1
pushd libcxx1
rm * -rf
cmake ../../libcxx -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${SRCROOT}/stage_2/cxxwithoutabi -DCMAKE_C_COMPILER=musl-clang -DCMAKE_CXX_COMPILER=musl-clang -DCMAKE_C_FLAGS="-fPIC " -DCMAKE_CXX_FLAGS="-fPIC " -DLIBCXX_HAS_MUSL_LIBC=ON -DLIBCXX_ENABLE_SHARED=0 -DLIBCXX_USE_COMPILER_RT=ON
make install -j \
  || { printf "build libcxx round 1 failed\n"; exit 1; }
popd

#build libcxxabi with libcxx
printf "build libcxxabi\n"
mkdir libcxxabi
pushd libcxxabi
rm * -rf
cmake ../../libcxxabi -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${PREFIX} -DCMAKE_C_COMPILER=musl-clang -DCMAKE_CXX_COMPILER=musl-clang -DLIBCXXABI_ENABLE_STATIC_UNWINDER=ON -DLIBCXXABI_ENABLE_SHARED=OFF -DLIBCXXABI_USE_LLVM_UNWINDER=ON -DCMAKE_C_FLAGS="-fPIC " -DCMAKE_CXX_FLAGS="-fPIC " -DLIBCXXABI_LIBCXX_PATH=../../libcxx -DLLVM_ENABLE_LIBCXX=ON -DLIBCXXABI_USE_COMPILER_RT=ON
make install -j \
  || { printf "build libcxxabi failed\n"; exit 1; }
popd

#build libcxx second round, with libcxxabi
printf "build libcxx round 2"
mkdir libcxx2
pushd libcxx2
rm * -rf
cmake ../../libcxx -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${PREFIX} -DCMAKE_C_COMPILER=musl-clang -DCMAKE_CXX_COMPILER=musl-clang -DLIBCXX_HAS_MUSL_LIBC=ON -DLIBCXX_CXX_ABI=libcxxabi -DLIBCXX_ENABLE_SHARED=0 -DLIBCXXABI_USE_LLVM_UNWINDER=ON -DLIBCXX_CXX_ABI_LIBRARY_PATH=${PREFIX}/lib -DLIBCXX_CXX_ABI_INCLUDE_PATHS=../../libcxxabi/include -DCMAKE_C_FLAGS="-fPIC " -DCMAKE_CXX_FLAGS="-fPIC " -DLIBCXX_USE_COMPILER_RT=ON
make install -j \
  || { printf "build libcxx round 2 failed\n"; exit 1; }
popd

# after this time, cxx is ready

#currently musl does not support fstab.h file so we ommit compiler-rt‘s other part now
# printf "build compiler-rt c++ part"
# mkdir compiler-rt2
# pushd compiler-rt2
# rm * -rf
# cmake ../../compiler-rt -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=${PREFIX}${COMPILER_RT_PREFIX} -DCMAKE_C_COMPILER=musl-clang -DCMAKE_CXX_COMPILER=musl-clang++ -DCOMPILER_RT_USE_LIBCXX=ON -DCMAKE_C_FLAGS="-fPIC " -DCMAKE_CXX_FLAGS="-fPIC "
# make install -j \
#   || { printf "build compiler-rt c++ part failed\n"; exit 1; }
# popd

#copy new musl-clang wrapper which will use llvm toolchain as default.
mv ${PREFIX}/bin/musl-clang.fin ${PREFIX}/bin/musl-clang
mv ${PREFIX}/bin/ld.musl-clang.fin ${PREFIX}/bin/ld.musl-clang
mv ${PREFIX}/bin/musl-clang++.fin ${PREFIX}/bin/musl-clang++
mv ${PREFIX}/bin/ld.musl-clang++.fin ${PREFIX}/bin/ld.musl-clang++
popd

