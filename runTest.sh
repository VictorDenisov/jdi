#!/bin/bash

cd sample
javac -g Main.java
cd ..

cd sample
./run.sh&
cd ../

sleep 10
./dist/build/Test/Test
if (( $? == 0 )); then
	echo -e '\e[0;32m'Passed.'\e[0m'
else
	echo -e '\e[0;31m'Failed.'\e[0m'
fi
