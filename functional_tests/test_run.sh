echo "====test_run.sh===="
echo "int main(void){return 0;}" > "empty.c"
gcc empty.c -o empty
cp ./empty ./empty_stripped
strip ./empty_stripped
../superset_disasm.native --target="empty_stripped" --ground_truth_bin="./empty" --save_addrs --enable_feature="${1}" --rounds=2 --tp_threshold="0.99" > empty_disasm.txt;
