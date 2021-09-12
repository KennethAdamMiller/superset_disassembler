#this should run docker or docker should call this?
make clean && make && ./test_superset_disasm.native
./functional_tests/test_cache.sh > cache_tests.txt
cat cache_tests.txt

./functional_tests/test_cmdline.sh > cmdline_tests.txt
cat cmdline_tests.txt

./functional_tests/test_fns.sh > fns_tests.txt
cat fns_tests.txt

./functional_tests/test_reports.sh > report_tests.txt
cat report_tests.txt

./functional_tests/test_run.sh > tests.txt
cat tests.txt
