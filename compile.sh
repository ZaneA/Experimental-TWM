#!/bin/sh
export CSC_OPTIONS="-static -inline -strip"
csc -o hashtwm3 main.scm
