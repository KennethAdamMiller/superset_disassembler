echo "Evaluating individual features with target: ${1}"

target=${1}

process() {
    name=$1
    invariants=$2
    options=$3
    echo ${name}
    time ~/workspace/superset_disassembler/superset_disasm.native --target "${target}" --trimmer=DeadBlockResistant --invariants="${invariants}" --cut=DFS,random,500 --ground_truth_file=./instr-addresses-actual.txt
    dot -Tpdf "${target}.dot" -o "${target}.pdf" -Grotate=180
    python ~/workspace/binary_pgm/calc_fn.py ./instr-addresses-actual.txt "${target}.lserr" "${target}_addrs.txt"
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

