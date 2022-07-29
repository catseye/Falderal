#!/bin/sh

while IFS= read -r line; do
    if [ "$line" = "print x" ]; then
        printf %s\\n "$X"
    elif [ "$line" = "print y" ]; then
        printf %s\\n "$Y"
    elif [ "$line" = "read x" ]; then
        X=`read -r line`
    elif [ "$line" = "read y" ]; then
        Y=`read -r line`
    fi
done < "$1"
