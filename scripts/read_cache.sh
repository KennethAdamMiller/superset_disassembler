read_cache() {
    echo "bap superset_distribution --metrics \"ro: %1\",\"reduced_occlusion\" $@"
    bap superset_distribution --metrics "ro: %1","reduced_occlusion" $@
}
