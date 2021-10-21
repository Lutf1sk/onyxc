#!/bin/sh

./onyxc -run $1/and.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(and.nyx $code)"
	exit 1
fi

./onyxc -run $1/xor.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(xor.nyx $code)"
	exit 1
fi

./onyxc -run $1/or.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(or.nyx $code)"
	exit 1
fi

./onyxc -run $1/shl.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(shl.nyx $code)"
	exit 1
fi

./onyxc -run $1/shr.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(shr.nyx $code)"
	exit 1
fi

./onyxc -run $1/not.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(not.nyx $code)"
	exit 1
fi

exit 0
