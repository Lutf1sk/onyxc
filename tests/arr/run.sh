#!/bin/sh

./onyxc -run $1/static.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(static.nyx $code)";
	exit 1;
fi

./onyxc -run $1/view.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(view.nyx $code)";
	exit 1;
fi

./onyxc -run $1/count.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(count.nyx $code)";
	exit 1;
fi

./onyxc -run $1/data.nyx
code=$?
if [ "$code" != "69" ]; then
	printf "(data.nyx $code)";
	exit 1;
fi

