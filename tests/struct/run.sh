#!/bin/sh

./onyxc -run $1/struct.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(struct.nyx $code)"
	exit 1
fi

./onyxc -run $1/init.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(init.nyx $code)"
	exit 1
fi

exit 0
