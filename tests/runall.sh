#!/bin/sh

failed=0
succeeded=0

for D in tests/*; do
    if [ -d "${D}" ]; then
		info=$(cat $D/info.txt)
        output=$(sh $D/run.sh $D)
		if [ $? == 0 ] ; then
			printf "%-24s \x1b[32m%-12s\x1b[0m $output\n" "$info" "Succeeded"
			succeeded=$((succeeded+1))
		else
			printf "%-24s \x1b[31m%-12s\x1b[0m $output\n" "$info" "Failed"
			failed=$((failed+1))
		fi
		
    fi
done

total=$((succeeded+failed))
printf "($succeeded/$total) Tests passed\n"

