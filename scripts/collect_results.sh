#cat results/*.txt > metrics.txt
cat *.metrics > metrics.txt
cat metrics.txt| grep "vertices after trimming" | cut -d " " -f6 > total_removed.txt
cat metrics.txt| grep "Total instructions" | cut -d " " -f4 >  final_total.txt
cat metrics.txt| grep "disasm binary execution time" | cut -d ":" -f2 > times.txt
cat metrics.txt| grep "superset_isg_of_mem" | grep length | cut -d " " -f3 > mem_size.txt
cat metrics.txt| grep "superset_map" | grep length | cut -d " " -f6 > original_superset.txt
cat metrics.txt| grep "Actual function entrances" | cut -d " " -f4 > total_functions.txt
cat metrics.txt| grep "Occlusion" | cut -d " " -f2 > occlusion.txt
cat metrics.txt| grep "Reduced occlusion" | cut -d " " -f3 > reduced_occlusion.txt
cat metrics.txt| grep "False negatives" | cut -d " " -f3 > false_negatives.txt
cat metrics.txt| grep "True positives" | cut -d " " -f3 > true_positives.txt
cat metrics.txt| grep "Number of possible reduced" | cut -d " " -f7 > tp_byte_space.txt
#grep -L "False negatives: 0" results/*.txt > fn_binaries.txt

#cat binaries.txt | grep -v ".map" | grep -v ".graph" | grep -v ".meta"  > bins_only.txt
#mv bins_only.txt binaries.txt
#cat -n binaries.txt > numbered_binaries.txt
#mv numbered_binaries.txt binaries.txt
#cat binaries.txt | grep x86 | grep -v _64 | sed '/^$/d' | cut -d " " -f3 | cut -f1 > x86_binaries.txt
#cat binaries.txt | grep x86_64 | sed '/^$/d' | cut -f1 > x86_64binaries.txt
#cat binaries.txt | grep gcc | sed '/^$/d' | cut -f1 > gcc_binaries.txt
#cat binaries.txt | grep icc | sed '/^$/d' | cut -f1 > icc_binaries.txt
#cat binaries.txt | grep "Number of possible reduced false" | cut -d " " -f7 > possible_fp.txt
