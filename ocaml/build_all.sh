#!/usr/bin/env bash

corebuild \
    -cflag -g -j 4 \
    -pkg core_extended,async \
    -pkg pa_ounit \
    permutation.cmo experiments.native
