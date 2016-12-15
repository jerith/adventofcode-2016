#!/bin/sh

day=$1; shift
txt=$(printf 'day%02d.txt' $day)

./aoc.byte $day inputs/$txt -a answers/$txt 1 "$@"
./aoc.byte $day inputs/$txt -a answers/$txt 2 "$@"
