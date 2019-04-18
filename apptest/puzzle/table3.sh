#!/bin/bash

echo > binary-diff.table
echo > binary-ex.table

for i in 56 61 73 91 109
do
    paste <(./table2.sh "e-$i-*-binary*.json") <(./table2.sh "e-$i-*-diff*.json") >> binary-diff.table
    paste <(./table2.sh "e-$i-*-binary*.json") <(./table2.sh "e-$i-*-extrinsic*.json") >> binary-ex.table
    paste <(./table2.sh "e-$i-*-binary*.json") <(./table2.sh "e-$i-*-diff*.json") > binary-diff-$i.table
    paste <(./table2.sh "e-$i-*-binary*.json") <(./table2.sh "e-$i-*-extrinsic*.json") > binary-ex-$i.table
    ./plot.ros binary diff $i
    ./plot.ros binary ex   $i
done

./plot.ros binary diff
./plot.ros binary ex

