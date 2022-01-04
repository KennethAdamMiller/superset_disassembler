cat binaries.txt | while read f; do
	echo "${f} $(bap superset_cache --is_present ${f})"
done
