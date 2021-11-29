rm -f binaries.txt
find ${HOME}/workspace/x86_64-binaries/elf/binutils -type f -print >> binaries.txt
find ${HOME}/workspace/x86_64-binaries/elf/findutils -type f -print >> binaries.txt
find ${HOME}/workspace/x86-binaries/elf/binutils -type f -print >> binaries.txt
find ${HOME}/workspace/x86-binaries/elf/findutils -type f -print  >> binaries.txt
find ${HOME}/workspace/x86-binaries/elf/coreutils -type f -print  >> binaries.txt
find ${HOME}/workspace/x86_64-binaries/elf/coreutils -type f -print  >> binaries.txt
find ${HOME}/workspace/arm-binaries/coreutils -type f -print  >> binaries.txt
