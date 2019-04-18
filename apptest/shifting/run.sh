#!/bin/bash

parallel -j 4 \
         'dsama train-model test.tsv "(:precondition {1} {2} :verbose t :n-tree 10)" p-{1}-{2}.model' \
         ::: $(seq 0 3) \
         ::: binary-random-forest-classifier pu-random-forest-classifier

parallel -j 4 \
         'dsama train-model test.tsv "(:effect {1} {2} {3} :verbose t :n-tree 10)" e-{1}-{2}-{3}.model' \
         ::: $(seq 0 3) \
         ::: $(seq 0 7) \
         ::: binary-random-forest-classifier diff-random-forest-classifier
