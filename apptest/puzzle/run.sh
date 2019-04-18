#!/bin/bash

# scp ccc016:repos/latplan-zsae-icaps/latplan/samples/puzzle_CosineTransitionAE_mandrill_3_3_144_20000_0.7_1.0_tanh/action+ids.csv .
# scp ccc016:repos/latplan-zsae-icaps/latplan/samples/puzzle_CosineTransitionAE_mandrill_3_3_144_20000_0.7_1.0_tanh/aux.json .
# scp ccc016:repos/latplan-zsae-icaps/latplan/samples/puzzle_CosineTransitionAE_mandrill_3_3_144_20000_0.7_1.0_tanh/_aae/aux.json aae_aux.json

parallel -j 4 \
         'dsama train-model action+ids.csv "(:precondition {1} {2} :verbose t :n-tree 10)" p-{1}-{2}.model' \
         ::: $(seq 0 120) \
         ::: binary-random-forest-classifier pu-random-forest-classifier

parallel -j 4 \
         'dsama train-model action+ids.csv "(:effect {1} {2} {3} :verbose t :n-tree 10)" e-{1}-{2}-{3}.model' \
         ::: $(seq 0 120) \
         ::: $(seq 0 144) \
         ::: \
         binary-random-forest-classifier \
         diff-random-forest-classifier \
         binary-cnf-classifier \
         diff-cnf-classifier
         binary-cnf+-classifier \
         diff-cnf+-classifier
         binary-eqv-classifier \
         diff-eqv-classifier
         binary-eqv+-classifier \
         diff-eqv+-classifier

# extrinsic-random-forest-classifier was bad

