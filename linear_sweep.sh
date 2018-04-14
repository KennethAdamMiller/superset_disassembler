#!/bin/bash
bindir=${HOME}/workspace/

dump_errors() {
  subdir=${1}
  rm files.txt
  find ${bindir}/${subdir} -type f -print > /tmp/files.txt 
  cat /tmp/files.txt | grep -v ".graph" | grep -v ".map" | grep -v ".meta" | grep -v ".lserr" > files.txt #grep "so$" > files.txt 
  #rm /tmp/files.txt
  for f in `cat files.txt` ; do
    export s=${f/'elf'/'elf_stripped'}
    objdump -no-show-raw-insn -no-symbolic-operands -no-leading-addr -no-leading-headers -d ${s}  | grep ":" | cut -d ":" -f 1 | grep -e "[0-9a-f]" > ${f}.lserr
    echo "$(tail -n +2 ${f}.lserr)" > ${f}.lserr
    echo "Processed ${f}"
  done
}

#dump_errors "../Desktop/linux_binaries/stripped_lib"

dump_errors "../Desktop/linux_binaries/stripped_bin"

#dump_errors "hmmer-3.1b2-cygwin32/binaries/"
#dump_errors "hmmer-3.1b2-cygwin32/binaries/"

#dump_errors "x86_64-binaries/elf/coreutils"
#dump_errors "x86_64-binaries/elf/findutils"
#dump_errors "x86_64-binaries/elf/binutils"

#dump_errors "x86-binaries/elf/coreutils"
#dump_errors "x86-binaries/elf/findutils"
#dump_errors "x86-binaries/elf/binutils"

