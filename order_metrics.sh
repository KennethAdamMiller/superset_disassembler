rm ordered_tp_byte_space.txt
rm ordered_reduced_occlusion.txt
rm ordered_objdump_false_negatives.txt

cat ../fns_top_1000.txt | while read f ; do  
    cat "$(basename "${f}").metrics" | grep "Number of possible reduced false positives" | cut -d " " -f7 >> ordered_tp_byte_space.txt; 
    cat "$(basename "${f}").metrics" | grep "Reduced occlusion" | cut -d " " -f3 >> ordered_reduced_occlusion.txt;
done

cat ../fns_top_1000.txt | while read f ; do
    echo "$(basename "${f}")" >> ordered_objdump_false_negatives.txt
    python3 ~/workspace/superset_disassembler/calc_fn.py ../"$(basename "${f}")".truth ../"$(basename "${f}")".lserr "" >> ordered_objdump_false_negatives.txt
done

cat ordered_objdump_false_negatives.txt | grep "Objdump false negatives" | cut -d ":" -f 2 > objfns.txt
mv objfns.txt ordered_objdump_false_negatives.txt
python calc_intervals.sh
