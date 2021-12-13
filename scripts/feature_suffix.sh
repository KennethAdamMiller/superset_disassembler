#for each variable, add the variable name and value to a hash
export FSUFFIX=$(bash -c "source ./vars.sh; echo $(env | md5sum | cut -c1-6)")
