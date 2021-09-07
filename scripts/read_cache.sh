read_cache() {
    echo "bap superset_distribution --metrics \"ro: %1\",\"superset-disasm-metrics:reduced_occlusion\" $@"
    bap superset_distribution --metrics "ro: %1","superset-disasm-metrics:reduced_occlusion" $@
}
