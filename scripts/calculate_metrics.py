from array import array

def file_to_list(fname):
    try:
        with open(fname) as f:
            return [x for x in f.read().splitlines()]
    except Exception: return []

def read_file(fname):
    try:
        with open(fname) as f:
            return [float(x) for x in f.read().splitlines()]
    except Exception: return []

def filter_list(indexes, tofilter):
    toRet=[]
    for idx in indexes:
        toRet.append(tofilter[int(idx)-1])
    return toRet

raw_superset = read_file("original_superset.txt")
total_removed = read_file("total_removed.txt")
#loop_reduction = read_file("loop_removal.txt")
total_functions = read_file("total_functions.txt")
mem_size = read_file("mem_size.txt")
occlusion = read_file("occlusion.txt")
reduced_occlusion = read_file("reduced_occlusion.txt")
final_total_instructions = read_file("final_total.txt")
false_negatives = read_file("false_negatives.txt")
true_positives = read_file("true_positives.txt")
binaries = file_to_list("binaries.txt")
x86_64_binaries = read_file("x86_64binaries.txt")
x86_binaries = read_file("x86_binaries.txt")
gcc_binaries = read_file("gcc_binaries.txt")
icc_binaries = read_file("icc_binaries.txt")
possible_fp  = read_file("tp_byte_space.txt")

def avg(l):
    s=sum(l)
    n=len(l)
    if n == 0:
        return 0
    else:
        return s / n

raw_superset = [superset + removed for superset, removed in zip(raw_superset, total_removed)]
assert len(raw_superset)==len(possible_fp), ("expected %d, got %d" % (len(raw_superset), len(possible_fp)))
#assert len(raw_superset)==len(loop_reduction)
print("Total binaries: ", len(mem_size))
print("Total false negatives: ", sum(false_negatives))
print("Total functions: ", sum(total_functions))
print("% error: ", sum(false_negatives) / sum(total_functions))
percent_mem=[raw / mem for raw, mem in zip(raw_superset, mem_size)]
average_percent_mem=avg(percent_mem) 
#print("Average percent of memory {:.0%}".format(average_percent_mem), "max percent of memory", max(percent_mem), "min percent", min(percent_mem))
percent_removed = [removed / raw_size for removed, raw_size in zip(total_removed, raw_superset)]
print("Average total removed (% of superset): ", "{:.2%}".format(avg(percent_removed)))
#percent_loop_removed_of_total = [ loop_body / raw_size for loop_body, raw_size in zip(loop_reduction, raw_superset)]
#print("Average loop removed of (% of superset): ", sum(percent_loop_removed_of_total) / len(percent_loop_removed_of_total))
#print("Max/Min total removed (% of superset): ", max(percent_removed), min(percent_removed))
#print("Max/Min loop removed (% of superset): ", max(percent_loop_removed_of_total), min(percent_loop_removed_of_total))
#phase1_output = [ raw - total + loop for raw, total, loop in zip(raw_superset, total_removed, loop_reduction)]
#additional_savings = [ loop / phase1 for loop, phase1 in zip(loop_reduction, phase1_output)]
#print("Average additional reduction (% of phase 1 output): ", sum(additional_savings) / len(additional_savings))
occlusion_percent = [rate / final_total for rate, final_total in zip(occlusion, final_total_instructions)]
average_occlusion = avg(occlusion_percent) 
#print("Average *original* occlusion (as rate of total instructions, including the true positive disrupted)", average_occlusion)
min_false_positive_estimate=average_occlusion / 2
max_false_positive_estimate=average_occlusion * (15.0/16.0)
#print("Overall estimated false positive rate within occlusion: ", max_false_positive_estimate, min_false_positive_estimate)
reduced_occlusion_percent = [rate / final_total for rate, final_total in zip(reduced_occlusion, final_total_instructions)]
average_reduced_occlusion = avg(reduced_occlusion_percent) 
#print("Overall average *reduced* occlusion (as rate of total instructions to reduced occlusion)", average_reduced_occlusion)
min_reduced_false_positive_estimate=average_reduced_occlusion / 2
max_reduced_false_positive_estimate = average_reduced_occlusion * (15.0 / 16.0)
#print("Overall estimated false positive rate within *reduced* occlusion: ", max_reduced_false_positive_estimate, min_reduced_false_positive_estimate)
percent_of_memory=percent_mem
orig_percent_of_memory=avg(percent_of_memory) 
#print("Overall average original number of instructions within memory percent memory: ", orig_percent_of_memory)

improvement_of_memory = [final / mem for final, mem in zip(final_total_instructions, mem_size)]
improvement_percent_of_memory=avg(improvement_of_memory) 
#print("Overall Average improvement as percent memory: ", improvement_percent_of_memory)

percent_true_positives=[tp / superset for tp, superset in zip(true_positives, raw_superset)]
average_true_positives=avg(percent_true_positives) 
#print("Overall Average original percent true positives of superset: ", average_true_positives)
percent_improvement=[tp / final for tp, final in zip(true_positives, final_total_instructions)]
average_improvement=avg(percent_improvement)
#print("Overall Average improvement as percent true positives of superset: ", average_improvement)

percent_tp_of_occlusion=[tp / (tp + reduced) for tp, reduced in zip(true_positives, reduced_occlusion)]
avg_tp_of_occlusion=avg(percent_tp_of_occlusion)
#print("Overall average true positives of reduced occlusion: ", avg_tp_of_occlusion)

percent_tp_of_original=[tp / original for tp, original in zip(true_positives, raw_superset)]
avg_tp_of_original=avg(percent_tp_of_original)
#print("Average true positives of original: ", avg_tp_of_original)

#print("")
percent_tp_of_total_bytes = [ tp / (tp + tp_bytes) for tp, tp_bytes in zip(true_positives, possible_fp)]
avg_percent_tp_of_total_bytes = avg(percent_tp_of_total_bytes)
print("avg percent true positives of total true positive byte space: ", avg_percent_tp_of_total_bytes)
print("min % tp of tp byte space: ", min(percent_tp_of_total_bytes))
print("max % tp of tp byte space: ", max(percent_tp_of_total_bytes))

print("")
percent_reduced_of_tp_bytes = [ (reduced / (tp_bytes)) for reduced, tp_bytes in zip(reduced_occlusion, possible_fp)]
print("avg percent reduced occlusion of total true positive byte space: ",avg(percent_reduced_of_tp_bytes))
print("min % reduced occlusion of total true positive byte space: ", min(percent_reduced_of_tp_bytes)) 
print("max % reduced occlusion of total true positive byte space: ", max(percent_reduced_of_tp_bytes)) 

return

#print("avg percent reduced occlusion of tp byte space (without worst): ", avg(sorted(percent_reduced_of_tp_bytes)[:-1500]))
worst_reduced_of_tp_bytes = [ "{:.2%}".format(x) for x in sorted(percent_reduced_of_tp_bytes)[-15:] ]
print("worst reduced occlusion of tp byte space: ", worst_reduced_of_tp_bytes)

percent_icc_of_mem=[superset / mem for superset, mem in zip(filter_list(icc_binaries, raw_superset), filter_list(icc_binaries, mem_size))]
avg_icc_of_mem=avg(percent_icc_of_mem)
percent_icc_improvement=[final / mem for final, mem in zip(filter_list(icc_binaries, final_total_instructions), filter_list(icc_binaries, mem_size))]
avg_icc_improvement=avg(percent_icc_of_mem) 
print("Average icc original of mem", avg_icc_of_mem)
print("Average icc improvement of mem", avg_icc_improvement)
percent_icc_tp_of_superset=[ tp / superset for tp, superset in zip(filter_list(icc_binaries, true_positives), filter_list(icc_binaries, raw_superset))]
avg_tp_of_superset=avg(percent_icc_tp_of_superset) 
print("Average icc true positives of original superset", avg_tp_of_superset)
percent_icc_tp_of_reduced=[ tp / (tp + reduced) for tp, reduced in zip(filter_list(icc_binaries, true_positives), filter_list(icc_binaries, reduced_occlusion))]
avg_tp_of_reduced=avg(percent_icc_tp_of_reduced)
print("Average icc true positive of reduced occlusion", avg_tp_of_reduced)

percent_gcc_of_mem=[superset / mem for superset, mem in zip(filter_list(gcc_binaries, raw_superset), filter_list(gcc_binaries, mem_size))]
avg_gcc_of_mem=avg(percent_gcc_of_mem)
print("Average gcc original of mem", avg_gcc_of_mem)
percent_gcc_improvement=[final / mem for final, mem in zip(filter_list(gcc_binaries, final_total_instructions), filter_list(gcc_binaries, mem_size))]
avg_percent_gcc_improvement=avg(percent_gcc_improvement)
print("Average gcc improvement of mem", avg_percent_gcc_improvement)
percent_gcc_tp_of_superset=[tp / superset for tp, superset in zip(filter_list(gcc_binaries, true_positives), filter_list(gcc_binaries, raw_superset))]
avg_gcc_tp_of_superset=avg(percent_gcc_tp_of_superset)
print("Average gcc true positives of original", avg_tp_of_superset)
percent_gcc_tp_of_reduced=[tp / (tp + reduced) for tp, reduced in zip(filter_list(gcc_binaries, true_positives), filter_list(gcc_binaries, reduced_occlusion))]
avg_gcc_tp_of_reduced=avg(percent_gcc_tp_of_reduced)
print("Average gcc true positive of reduced occlusion", avg_gcc_tp_of_reduced)

percent_x86_original_of_mem=[superset / mem for superset, mem in zip(filter_list(x86_binaries, raw_superset), filter_list(x86_binaries, mem_size))]
avg_x86_original_of_mem=avg(percent_x86_original_of_mem)
print("Average (x86 subset) original of mem", avg_x86_original_of_mem)
percent_x86_improvement=[final / mem for final, mem in zip(filter_list(x86_binaries, final_total_instructions), filter_list(x86_binaries, mem_size))]
avg_x86_improvement=avg(percent_x86_improvement)
print("Average (x86 subset) improvement of mem", avg_x86_improvement)
percent_x86_of_superset=[tp / superset for tp, superset in zip(filter_list(x86_binaries, true_positives), filter_list(x86_binaries, raw_superset))]
avg_x86_of_superset=avg(percent_x86_of_superset)
print("Average (x86 subset) true positives of original: ", avg_x86_of_superset)
percent_x86_of_reduced=[tp / (tp + reduced) for tp, reduced in zip(filter_list(x86_binaries, true_positives), filter_list(x86_binaries, reduced_occlusion))]
avg_x86_of_reduced=avg(percent_x86_of_reduced)
print("Average (x86 subset) true positives of reduced occlusion: ", avg_x86_of_reduced)
percent_x86_64_original_of_mem=[superset / mem for superset, mem in zip(filter_list(x86_64_binaries, raw_superset), filter_list(x86_64_binaries, mem_size))]
avg_x86_64_of_mem=avg(percent_x86_64_original_of_mem)
print("Average (x86_64 subset) original of mem", avg_x86_64_of_mem)
percent_x86_64_improvement=[final / mem for final, mem in zip(filter_list(x86_64_binaries, final_total_instructions), filter_list(x86_64_binaries, mem_size))]
avg_x86_64_of_reduced=avg(percent_x86_64_improvement)
print("Average (x86_64 subset) improvement of mem", avg_x86_64_of_reduced)
percent_x86_64_of_superset=[tp / superset for tp, superset in zip(filter_list(x86_64_binaries, true_positives), filter_list(x86_64_binaries, raw_superset))]
avg_x86_64_of_superset=avg(percent_x86_64_of_superset)
print("Average (x86_64 subset) true positives of original: ", avg_x86_64_of_superset)
percent_x86_64_of_reduced=[tp / (tp + reduced) for tp, reduced in zip(filter_list(x86_64_binaries, true_positives), filter_list(x86_64_binaries, reduced_occlusion))]
avg_x86_64_tp_of_reduced=avg(percent_x86_64_of_reduced)
print("Average (x86_64 subset) true positives of reduced occlusion: ", avg_x86_64_tp_of_reduced)


print("")
improvement_per_memory=[ (original - final) / mem for original, final, mem in zip(raw_superset, final_total_instructions, mem_size)]
highest_improvement_per_memory=max(improvement_per_memory)
lowest_improvement_per_memory = min(improvement_per_memory)
print("Highest improvement: ", lowest_improvement_per_memory, "-", highest_improvement_per_memory)
best_five=sorted(improvement_per_memory)[-10:]
print("Best five: ", best_five)
#binaries={ idx : binary for idx, binary in zip(range(len(binaries)), binaries)}
def indexes_of(items, to_find):
    to_find=set(to_find)
    found=set()
    idx=0
    for item in items:
        if item in to_find and idx not in found:
            found.add(idx)
        idx+=1
    return sorted(found)
            
best_five_indexes = indexes_of(improvement_per_memory, best_five)
best_total_removed = [total_removed[idx] for idx in best_five_indexes]
best_occlusion = [occlusion[idx] for idx in best_five_indexes]
best_reduced_occlusion = [reduced_occlusion[idx] for idx in best_five_indexes]
best_false_negative = [false_negatives[idx] for idx in best_five_indexes]
best_true_positives = [true_positives[idx] for idx in best_five_indexes]
best_five_names = [binaries[x].split("\t")[1].split("/")[-1] for x in best_five_indexes]
best_improvement_percent = [improvement_per_memory[idx] for idx in best_five_indexes]
print("Best five indexes: ", best_five_indexes, best_five_names)
print("Binary name & Removed & Occlusion & Reduced Occlusion & Improvement \\\\")
for name, removed, occ, reduced, percent in zip(best_five_names, best_total_removed, best_occlusion, best_reduced_occlusion, best_improvement_percent):
    print("%s & %s & %s & %s & %s \\\\" % (name.replace("_", "\_"), removed, occ, reduced, "{0:.0f}\\%".format(percent*100)))

print("")
worst_five=sorted(improvement_per_memory)[0:10]
worst_five_indexes=indexes_of(improvement_per_memory, worst_five)
print(worst_five_indexes)
worst_five_names=[binaries[x-1].split("\t")[1].split("/")[-1] for x in worst_five_indexes]
worst_total_removed = [total_removed[idx] for idx in worst_five_indexes]
worst_occlusion=[occlusion[idx] for idx in worst_five_indexes]
worst_reduced_occlusion = [reduced_occlusion[idx] for idx in worst_five_indexes]
worst_improvement_percent = [improvement_per_memory[idx] for idx in worst_five_indexes]
print("Binary name & Removed & Occlusion & Reduced Occlusion & Improvement \\\\")
for name, removed, occlusion, reduced, percent in zip(worst_five_names, worst_total_removed, worst_occlusion, worst_reduced_occlusion, worst_improvement_percent):
    print("%s & %s & %s & %s & %s \\\\" % (name.replace("_", "\_"), removed, occlusion, reduced, "{0:.0f}\\%".format(percent*100)))

print("")
best_tp_of_reduced=max(percent_tp_of_occlusion)
print("Best & worst progress overall: ", best_tp_of_reduced, " to: ", min(percent_tp_of_occlusion))
best_icc_progress=max(percent_icc_tp_of_reduced)
print("Best icc progress: ", best_icc_progress)
best_gcc_progress=max(percent_gcc_tp_of_reduced)
print("Best gcc progress", best_gcc_progress)
best_x86_progress=max(percent_x86_of_reduced)
print("Best x86 progress", best_x86_progress)
best_x86_64_progress=max(percent_x86_64_of_reduced)
print("Best x86_64 progress", best_x86_64_progress)


worst_icc_progress=min(percent_icc_tp_of_reduced)
print("Worst icc progress", worst_icc_progress)
worst_gcc_progress=min(percent_gcc_tp_of_reduced)
print("Worst gcc progress", worst_gcc_progress)
worst_x86_progress=min(percent_x86_of_reduced)
print("Worst x86 progress", worst_x86_progress)
worst_x86_64_progress=min(percent_x86_64_of_reduced)
print("Worst x86_64 progress", worst_x86_64_progress)


print("")


