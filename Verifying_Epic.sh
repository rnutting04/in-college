#!/bin/bash

# This script runs test cases from the chosen Epic and stores their outputs
# as "out_[TestCase#]".

# To run test cases, execute this command: ./Verifying_Epic.sh X Y Z
# Where X is the Epic you want to run, Y is the first test case you want
# to run, and Z is the last test case. For example:
# ./Verifying_Epic.sh 4 2 5
# This command will run test cases 2 through 5 from Epic 4.

# By default, the Users, Profiles, and Connections files will not be reset. To
# explicitly reset them, add "r" to the end of the command:
# ./Verifying_Epic.sh 4 2 5 r

# IMPORTANT: TEST CASES MUST BE NAMED "test_#" OR ELSE THE SCRIPT WON'T WORK.
# FOLDER UNDER "tests" MUST BE NAMED "tests_epic_#".

# Compile InCollege
mkdir -p bin && cobc -x -free -o bin/InCollege /workspace/src/InCollege.cob

# Reset the Users, Profiles, and Connections file if the user specified it with "r"
if [ "$4" = "r" ]; then
    rm -f InCollege-Users.txt
    rm -f InCollege-Profiles.txt
    touch InCollege-Profiles.txt
    rm -f InCollege-Connections.txt
    touch InCollege-Connections.txt
fi

# Create a folder for the output files if it doesn't exist.
mkdir -p tests/tests_epic_${1}/output_each

a=$2
b=$3

for i in $(seq $a $b);
do
    echo "**********TESTING CASE $i...**********"

    # Copy test case as input
    cp tests/tests_epic_${1}/test_${i} InCollege-Input.txt

    # Run
    ./bin/InCollege

    # Move and rename output file
    mv InCollege-Output.txt tests/tests_epic_${1}/output_each/out_${i}
done

# Keep the output of the last test file in InCollege-Output.txt
cp tests/tests_epic_${1}/output_each/out_${i} InCollege-Output.txt