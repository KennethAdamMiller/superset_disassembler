#!/bin/bash

collect_fn() {
	for f in `cat files.txt` ; do 
		name="${1}_results/results.txt"
		rm -f "${name}"
		echo "${f}" >> "${name}" ; 
		python3 ~/workspace/binary_pgm/calc_fn.py "${f}".truth "${f}".lserr "${1}/${f}"_addrs.txt >> "${name}" ; 
	done
}
