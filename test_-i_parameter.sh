#!/bin/bash

echo "--------------------------------------------------------------"
echo "DOING TESTS FOR THE -i PARAMETER"

make -s

directory="./tests/printing_grammar/"

script_files=$(find $directory -type f -name "*.txt")
script_files=($script_files)

output_files=$(find $directory -type f -name "*_output.txt")
output_files=($output_files)

declare -A tests_dict

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
            tests_dict[$script]=$output
            break
        fi
    done
done

for key in "${!tests_dict[@]}"; do
    echo "--------------------------------------------------------------"
    test_name=$(basename $key)
    test_name_without_extension=${test_name%.*} 
    echo "Doing test: $test_name_without_extension..."
    diff -u --strip-trailing-cr -w -b ${tests_dict[$key]} <(./flp21-fun -i $key)
    DIFF=$(diff -u --strip-trailing-cr -w -b ${tests_dict[$key]} <(./flp21-fun -i $key))

    if [ "$DIFF" ] && [ $? == 0 ] 
    then
        echo "Test failed"
    else
        echo "Test successful"
    fi
done
echo "--------------------------------------------------------------"
