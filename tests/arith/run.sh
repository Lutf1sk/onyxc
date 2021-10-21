#!/bin/sh

./onyxc -run $1/add.nyx
if [ "$?" != 69 ]; then
	printf "(add.nyx $?)"
	exit 1
fi

./onyxc -run $1/sub.nyx
if [ "$?" != 69 ]; then
	printf "(sub.nyx $?)"
	exit 1
fi

./onyxc -run $1/mul.nyx
if [ "$?" != 69 ]; then
	printf "(mul.nyx $?)"
	exit 1
fi

./onyxc -run $1/div.nyx
if [ "$?" != 69 ]; then
	printf "(div.nyx $?)"
	exit 1
fi

./onyxc -run $1/mod.nyx
if [ "$?" != 69 ]; then
	printf "(mod.nyx $?)"
	exit 1
fi

exit 0
