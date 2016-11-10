#!/bin/sh -f
LD_LIBRARY_PATH=./lib
export LD_LIBRARY_PATH
# need at least -d 512 (for GC runtime)
# and -v 7000 or so (for shared libraries) -v 8192 gives 1MB < maxheap < 1.25MB
ulimit -t 30 -d 1024 -v 7168 # about 530 KB heap
exec ./a.out
