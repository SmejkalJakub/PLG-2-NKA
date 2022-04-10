#!/bin/bash

echo "--------------------------------------------------------------"
echo "DOING TESTS FOR THE -i PARAMETER"

make -s -C ../

directory="./printing_grammar/"

script_files=$(find $directory -type f -name "*.txt")
script_files=($script_files)

output_files=$(find $directory -type f -name "*_output.txt")
output_files=($output_files)

declare -A tests_dict_normal

succ_tests_normal=0
fail_tests_normal=0

for script in "${script_files[@]}"
do
    script_name=$(basename $script)
    name_without_extension=${script_name%.*}
    name_without_extension+="_output"
    for output in "${output_files[@]}"
    do
        output_name=$(basename $output)
        output_name_without_extension=${output_name%.*}
        if [[ "${output_name_without_extension}" == "${name_without_extension}" ]]
        then
            tests_dict_normal[$script]=$output
            break
        fi
    done
done

for key in "${!tests_dict_normal[@]}"; do
    echo "--------------------------------------------------------------"
    test_name=$(basename $key)
    test_name_without_extension=${test_name%.*} 
    echo "Doing test: $test_name_without_extension..."
    #diff -u --strip-trailing-cr -w -b ${tests_dict_normal[$key]} <(./flp21-fun -i $key)
    DIFF=$(diff -u --strip-trailing-cr -w -b ${tests_dict_normal[$key]} <(../flp21-fun -i $key))

    if [ "$DIFF" ] && [ $? == 0 ] 
    then
        ((fail_tests_normal=fail_tests_normal+1))
        echo "Test failed"
    else
        ((succ_tests_normal=succ_tests_normal+1))
        echo "Test successful"
    fi
done
echo "=============================================================="
echo "Successful tests: ${succ_tests_normal}"
echo "Failed tests: ${fail_tests_normal}"
echo "=============================================================="
