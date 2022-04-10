#!/bin/bash

echo "--------------------------------------------------------------"
echo "DOING TESTS FOR THE ERRORS"

make -s -C ../

directory="./errors/"

script_files_erros=$(find $directory -type f -name "*.txt")
script_files_erros=($script_files_erros)

succ_tests_errors=0
fail_tests_errors=0

for script in "${script_files_erros[@]}"
do
    echo "--------------------------------------------------------------"
    script_name=$(basename $script)
    name_without_extension=${script_name%.*}
    echo "Doing test: $name_without_extension..."
    ../flp21-fun -i $script 2> /dev/null > /dev/null
    
    if [ $? != 1 ] 
    then
        ((fail_tests_errors=fail_tests_errors+1))
        echo "Test failed"
    else
        ((succ_tests_errors=succ_tests_errors+1))
        echo "Test successful"
    fi
done
echo "=============================================================="
echo "Successful tests: ${succ_tests_errors}"
echo "Failed tests: ${fail_tests_errors}"
echo "=============================================================="
