#!/bin/bash

# This script runs 4 test cases for Epic 3 and stores their outputs as "out_[test_case_number]"

# Compile InCollege
mkdir -p bin && cobc -x -free -o bin/InCollege /workspace/src/InCollege.cob

# Reset the Users file
rm -f InCollege-Users.txt

for i in 1 2 3 4
do
    echo "**********TESTING CASE $i...**********"

    # Copy test case as input
    cp tests/tests_epic_4/search_test_${i} InCollege-Input.txt

    # Run
    ./bin/InCollege

    # Move and rename output file
    mv InCollege-Output.txt tests/tests_epic_4/output_each/out_${i}
done