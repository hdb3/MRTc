clang-format -i mrttest.c libmrt.c libmrt.h mrttabledumptest.c libmrttabledump.c
gcc -g -O2 -o mrttest  mrttest.c libmrt.c
gcc -g -O2 -o mrttabledumptest mrttabledumptest.c libmrttabledump.c libmrt.c
