echo "Evaluating individual features with target: ${1}"

target=${1}

process() {
    name=$1
    phases=$2
    options=$3
    echo ${name}
    #time ~/workspace/superset_disassembler/superset_disasm.native --target ${target} --trimmer=DeadBlockResistant --phases=${phases}
    time ~/workspace/superset_disassembler/superset_disasm.native --target ${target} --trimmer=DeadBlockResistant --phases=${phases} --cut=DFS,random,500
    dot -Tpng ${target}.dot -o ${target}.png -Grotate=180
    mkdir "${name}_results"
    mv *dot "${name}_results/"
    mv *addrs.txt "${name}_results/"
    mv *png "${name}_results/"
    mv *pdf "${name}_results/"
}

process "No features" "" ""

process "Target not in memory" "Target_out_of_bounds" ""

process "Bad target" "Target_is_bad" ""

process "Invalid memory access" "Invalid memory accesses" ""

process "Target_within_body" "Target_within_body" ""

process "Non instruction opcode" "Non instruction opcode" ""

process "Strongly Connected Component Data" "Strongly Connected Component Data" ""

process "Cross Layer Invalidation" "Cross Layer Invalidation" ""

process "Grammar convergent" "Grammar convergent" ""

