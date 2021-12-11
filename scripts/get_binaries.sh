rm -f binaries.txt
find /Volumes/corpus/x86_64/x86_64-binaries/elf/binutils -type f -print >> binaries.txt
find /Volumes/corpus/x86_64-binaries/elf/findutils -type f -print >> binaries.txt
find /Volumes/corpus/x86-binaries/elf/binutils -type f -print >> binaries.txt
find /Volumes/corpus/x86-binaries/elf/findutils -type f -print  >> binaries.txt
find /Volumes/corpus/x86-binaries/elf/coreutils -type f -print  >> binaries.txt
find /Volumes/corpus/x86_64-binaries/elf/coreutils -type f -print  >> binaries.txt
find /Volumes/corpus/arm-binaries/coreutils -type f -print  >> binaries.txt
