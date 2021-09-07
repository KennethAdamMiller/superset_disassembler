read_cache() {
    echo "bap superset_distribution --metrics \"ro: %1\",\"superset-disasm-metrics:reduced_occlusion\",\"superset_disasm-metrics:occlusives_space\" $@"
    bap superset_distribution --metrics "ro: %1 of %2","superset-disasm-metrics:reduced_occlusion","superset-disasm-metrics:occlusive_space" $@
}
