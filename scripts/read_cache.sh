read_cache() {
    echo bap superset_distribution --metrics "ro: %1 of %2 - %3 tps %4 fns","superset-disasm-metrics:reduced_occlusion","superset-disasm-metrics:occlusive_space","superset-disasm-metrics:true_positives","superset-disasm-metrics:false_negatives" $@
    bap superset_distribution --metrics "ro: %1 of %2 - %3 tps %4 fns","superset-disasm-metrics:reduced_occlusion","superset-disasm-metrics:occlusive_space","superset-disasm-metrics:true_positives","superset-disasm-metrics:false_negatives" $@
}
