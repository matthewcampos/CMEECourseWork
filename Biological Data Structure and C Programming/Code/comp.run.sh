#!/bin/bash

gcc -Wall $1
error=$?

if [ $error != 0 ]
then
    echo "Fix errors to obtain output"

else
    ./a.out

fi
