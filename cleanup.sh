bindir=${HOME}/workspace/
clean_dir() {
  subdir=${1}
  find ${bindir}/${subdir} -name "*.graph" -type f -exec rm {} \;
  find ${bindir}/${subdir} -name "*.meta" -type f -exec rm {} \;
  find ${bindir}/${subdir} -name "*.map" -type f -exec rm {} \;
  find ${bindir}/${subdir} -name "*.lserr" -type f -exec rm {} \;
}

clean_dir x86_64-binaries/elf/binutils
clean_dir x86_64-binaries/elf/coreutils
clean_dir x86_64-binaries/elf/findutils
clean_dir x86-binaries/elf/binutils
clean_dir x86-binaries/elf/coreutils
clean_dir x86-binaries/elf/findutils
