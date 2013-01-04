#!/bin/sh
export CSC_OPTIONS="-static-libs -inline -strip -lxcb"
csc -o hashtwm3 hashtwm3.scm
