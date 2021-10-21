#!/bin/sh

./onyxc -run $1/if.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(if.nyx $code)"
	exit 1
fi

./onyxc -run $1/while.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(while.nyx $code)"
	exit 1
fi

./onyxc -run $1/for.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(for.nyx $code)"
	exit 1
fi

exit 0

