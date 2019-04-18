#!/bin/bash

action=${1:-1}

paste <(ls -1 e-$action-*.json) \
      <(cat e-$action-*.json | jq .train.accuracy) \
      <(cat e-$action-*.json | jq .train.samples) \
      <(cat e-$action-*.json | jq .val.accuracy) \
      <(cat e-$action-*.json | jq .val.samples) \
    | column -t | sort -h 

