def read_file(fname):
    with open(fname) as f:
        return [float(x) for x in f.read().splitlines()]
    
raw_superset = read_file("original_superset.txt")
total_removed = read_file("total_removed.txt")
loop_reduction = read_file("loop_removal.txt")
assert len(raw_superset)==len(total_removed)
assert len(raw_superset)==len(loop_reduction)
percent_removed = [removed / raw_size for removed, raw_size in zip(total_removed, raw_superset)]
print("Average total removed (% of superset): ", sum(percent_removed) / len(percent_removed))
percent_loop_removed_of_total = [ loop_body / raw_size for loop_body, raw_size in zip(loop_reduction, raw_superset)]
print("Average loop removed of (% of superset): ", sum(percent_loop_removed_of_total) / len(percent_loop_removed_of_total))
print("Max/Min total removed (% of superset): ", max(percent_removed), min(percent_removed))
print("Max/Min loop removed (% of superset): ", max(percent_loop_removed_of_total), min(percent_loop_removed_of_total))
phase1_output = [ raw - total + loop for raw, total, loop in zip(raw_superset, total_removed, loop_reduction)]
additional_savings = [ loop / phase1 for loop, phase1 in zip(loop_reduction, phase1_output)]
print("Average additional reduction (% of phase 1 output): ", sum(additional_savings) / len(additional_savings))
occlusion = read_file("occlusion.txt")
final_total_instructions = read_file("final_total.txt")
occlusion_percent = [rate / final_total for rate, final_total in zip(occlusion, final_total_instructions)]
average_occlusion = sum(occlusion_percent) / len(occlusion_percent)
print("Average occlusion (as rate of total instructions, including the true positive disrupted)", average_occlusion)
min_false_positive_estimate=average_occlusion / 2
max_false_positive_estimate=average_occlusion * (15.0/16.0)
print("Estimated false positive rate within occlusion: ", max_false_positive_estimate, min_false_positive_estimate)
