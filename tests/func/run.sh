#!/bin/sh

./onyxc -run $1/reg.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(reg.nyx $code)"
	exit 1
fi

./onyxc -run $1/struct.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(struct.nyx $code)"
	exit 1
fi

./onyxc -run $1/ptr.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(ptr.nyx $code)"
	exit 1
fi

