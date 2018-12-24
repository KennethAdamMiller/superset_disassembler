rm -f remaining_files_sizes.txt ; cat remaining_files.txt | while read f ; do
	ls -l "${f}" >> remaining_files_sizes.txt
done
sort -k 5 remaining_files_sizes.txt > remaining_sizes.txt
mv remaining_sizes.txt remaining_files_sizes.txt

min_size=5000
max_size=$(( 1024 * 1024 * 5 ))
awk -v threshold="$min_size" '$5 >= threshold' remaining_file_sizes.txt > remaining_sizes.txt
mv remaining_sizes.txt remaining_files_sizes.txt
awk -v threshold="$max_size" '$5 <= threshold' remaining_files_sizes.txt > remaining_sizes.txt
mv remaining_sizes.txt remaining_files_sizes.txt
cut -d "/" -f 2 remaining_files_sizes.txt > remaining_sizes.txt
mv remaining_sizes.txt remaining_files_sizes.txt

