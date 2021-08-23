#this should run docker or docker should call this?
./functional_tests/test_cache.sh > cache_tests.txt
cat cache_tests.txt
./functional_tests/test_cmdline.sh > cmdline_tests.txt
cat cmdline_tests.txt 
./functional_tests/test_run.sh > tests.txt
cat tests.txt
