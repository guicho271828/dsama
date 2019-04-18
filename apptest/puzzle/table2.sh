#!/bin/bash

query=$1

paste <(cat $query | jq .train.accuracy) \
      <(cat $query | jq .train.samples) \
      <(cat $query | jq .val.accuracy) \
      <(cat $query | jq .val.samples)


