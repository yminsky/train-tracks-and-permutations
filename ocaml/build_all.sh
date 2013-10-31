#!/usr/bin/env bash

corebuild \
    -cflag -g -j 4 \
    -pkg core_extended,async \
    permutation.cmo experiments.native
