#!/bin/sh

for D in tests/*; do
    if [ -d "${D}" ]; then
        sh $D/run.sh $D
		if [ $? == 0 ] ; then
			echo -e "$(cat $D/info.txt) \x1b[32mSucceeded\x1b[0m"
		else
			echo -e "$(cat $D/info.txt) \x1b[31mFailed\x1b[0m"
		fi
		
    fi
done

