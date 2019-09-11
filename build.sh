clang-format -i libmrtcommon.c libmrtextra.c libmrttabledump.c libmrtupdates.c mrttest2.c mrtupdatetest.c mrttabledumptest.c
clang-format -i libmrtcommon.h libmrtextra.h libmrt.h libmrttabledump.h libmrtupdates.h
gcc -g -O2 -c libmrtcommon.c libmrtextra.c libmrttabledump.c libmrtupdates.c
ar -cr libmrt.a libmrtcommon.o libmrtextra.o libmrttabledump.o libmrtupdates.o
gcc -g -O2 -o mrttest2 mrttest2.c libmrt.a
gcc -g -O2 -o mrtupdatetest  mrtupdatetest.c libmrt.a
gcc -g -O2 -o mrttabledumptest mrttabledumptest.c libmrt.a
