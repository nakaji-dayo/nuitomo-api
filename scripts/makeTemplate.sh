#!/bin/sh

set -eu

template="nuitomo-api.hsfiles-tmp"

rm -f "$template"

find . \( -path './.stack-work' \
     -o -path './dev-data' \
     -o -path './.git' \
     \) -prune -o -type f -print \
    | LC_ALL=C sort \
    | while read input_file; do
    if ! git check-ignore -q "$input_file" ; then
        dist_file=`echo "$input_file" | sed "s/^\.\///" | sed "s/nuitomo-api/nuitomo-api/g"`
        echo "{-# START_FILE $dist_file #-}" >> "$template"
        cat "$input_file" \
            | sed "s/nuitomo-api/nuitomo-api/g" \
                  >> "$template"
    else
        echo "ignore $input_file"
    fi
done