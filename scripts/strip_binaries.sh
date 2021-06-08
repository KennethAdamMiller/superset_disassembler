unstripped=${HOME}/workspace/unstripped
mkdir -p ${unstripped}
cat binaries.txt | while read f ; do
	cp ${f} ${unstripped}
	strip ${f}
done
