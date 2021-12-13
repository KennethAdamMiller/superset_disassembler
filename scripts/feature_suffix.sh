#for each variable, add the variable name and value to a hash

export FSUFFIX=$(env -i bash -c 'excluded=$(env | LC_ALL=C sed -n "s/^\([A-Z_a-z][0-9A-Z_a-z]*\)=.*/\1/p" | tr "\n" " ") ; unset ${excluded} ; source ./vars.sh; echo $(env) | md5sum | cut -c1-6')
