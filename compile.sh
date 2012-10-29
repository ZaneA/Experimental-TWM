#!/bin/sh
export CSC_OPTIONS="-static -inline -gui -strip -cc i486-mingw32-gcc"
csc -o hashtwm3 main.scm
