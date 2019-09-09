clang-format -i mrtupdatetest.c libmrt.c libmrt.h mrttabledumptest.c libmrttabledump.c
gcc -g -O2 -o mrtupdatetest  mrtupdatetest.c libmrt.c libmrttabledump.c
gcc -g -O2 -o mrttabledumptest mrttabledumptest.c libmrttabledump.c libmrt.c
gcc -g -O2 -o mrttest2 mrttest2.c libmrttabledump.c libmrt.c
