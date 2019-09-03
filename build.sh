clang-format -i mrttest.c libmrt.c libmrt.h mrtribdumptest.c libmrtribdump.c libmrtribdump.h
gcc -g -O2 -o mrttest  mrttest.c libmrt.c
gcc -g -O2 -o mrtribdumptest mrtribdumptest.c libmrtribdump.c libmrt.c
