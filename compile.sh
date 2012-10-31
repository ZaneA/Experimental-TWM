#!/bin/sh
export CSC_OPTIONS="-static -inline -gui -strip"
csc -o hashtwm3 main.scm
