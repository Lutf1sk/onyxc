#!/bin/sh

./onyxc -run $1/ii.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(ii.nyx $code)"
	exit 1
fi

./onyxc -run $1/ui.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(ui.nyx $code)"
	exit 1
fi

./onyxc -run $1/iu.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(iu.nyx $code)"
	exit 1
fi

./onyxc -run $1/uu.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(uu.nyx $code)"
	exit 1
fi

exit 0
