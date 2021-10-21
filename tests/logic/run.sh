#!/bin/sh

./onyxc -run $1/and.nyx
code=$?
if [ "$code" != "2" ]; then
	printf "(and.nyx $code)"
	exit 1
fi

./onyxc -run $1/or.nyx
code=$?
if [ "$code" != "2" ]; then
	printf "(or.nyx $code)"
	exit 1
fi

./onyxc -run $1/not.nyx
code=$?
if [ "$code" != "2" ]; then
	printf "(not.nyx $code)"
	exit 1
fi

./onyxc -run $1/lesser.nyx
code=$?
if [ "$code" != "2" ]; then
	printf "(lesser.nyx $code)"
	exit 1
fi

./onyxc -run $1/greater.nyx
code=$?
if [ "$code" != "2" ]; then
	printf "(greater.nyx $code)"
	exit 1
fi

./onyxc -run $1/lesser_or_equal.nyx
code=$?
if [ "$code" != "2" ]; then
	printf "(lesser_or_equal.nyx $code)"
	exit 1
fi

./onyxc -run $1/greater_or_equal.nyx
code=$?
if [ "$code" != "2" ]; then
	printf "(greater_or_equal.nyx $code)"
	exit 1
fi

exit 0
