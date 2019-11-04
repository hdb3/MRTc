clang-format -i libmrtcommon.c libmrtextra.c libmrttabledump.c libmrtupdates.c mrttest2.c mrtupdatetest.c mrttabledumptest.c libmrttabledumpextra.c
clang-format -i libmrtcommon.h libmrtextra.h libmrt.h libmrttabledump.h libmrtupdates.h
gcc -g -O2 -c libmrtcommon.c libmrtextra.c libmrttabledump.c libmrtupdates.c libmrttabledumpextra.c timespec.c
rm -f libmrt.a
ar -cr libmrt.a libmrtcommon.o libmrtextra.o libmrttabledump.o libmrtupdates.o libmrttabledumpextra.o timespec.o
rm libmrtcommon.o libmrtextra.o libmrttabledump.o libmrtupdates.o libmrttabledumpextra.o
gcc -g -O2 -o mrttest2 mrttest2.c libmrt.a
gcc -g -O2 -o mrtupdatetest  mrtupdatetest.c libmrt.a
gcc -DBUILD_UPDATE_LIST=1 -g -O2 -o mrtupdatetest.BUILDLIST  mrtupdatetest.c libmrt.a
gcc -DNOUPDATE -g -O2 -o mrtupdatetest.NOUPDATE  mrtupdatetest.c libmrt.a
gcc -DNOPPA -g -O2 -o mrtupdatetest.NOPPA  mrtupdatetest.c libmrt.a
gcc -DNOSTATS -g -O2 -o mrtupdatetest.NOSTATS  mrtupdatetest.c libmrt.a
gcc -DNOSTATS -DNOPPA -g -O2 -o mrtupdatetest.NOSTATSORPPA  mrtupdatetest.c libmrt.a
gcc -g -O2 -o mrttabledumptest mrttabledumptest.c libmrt.a
gcc -g -O2 -o main main.c libmrt.a
gcc -g -O2  -o bgpparse bgpparse.c timespec.c
