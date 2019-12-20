LIBS="libmrtcommon.c libmrtextra.c libmrttabledump.c libmrtupdates.c libmrttabledumpextra.c"
HEADERS="libmrtcommon.h libmrtextra.h libmrt.h libmrttabledump.h libmrtupdates.h"
APPS="mrttest2.c mrtupdatetest.c mrttabledumptest.c main.c bgpparse.c"
clang-format -i $LIBS $HEADERS
clang-format -i libmrtcommon.h libmrtextra.h libmrt.h libmrttabledump.h libmrtupdates.h
LIBS+=" timespec.c"
# gcc -g -O3 -c libmrtcommon.c libmrtextra.c libmrttabledump.c libmrtupdates.c libmrttabledumpextra.c timespec.c
# rm -f libmrt.a
# ar -cr libmrt.a libmrtcommon.o libmrtextra.o libmrttabledump.o libmrtupdates.o libmrttabledumpextra.o timespec.o
# rm libmrtcommon.o libmrtextra.o libmrttabledump.o libmrtupdates.o libmrttabledumpextra.o
gcc -g -O3 -o mrttest2 mrttest2.c $LIBS
gcc -g -O3 -o mrtupdatetest mrtupdatetest.c $LIBS
gcc -DBUILD_UPDATE_LIST=1 -g -O3 -o mrtupdatetest.BUILDLIST mrtupdatetest.c $LIBS
gcc -DNOUPDATE -g -O3 -o mrtupdatetest.NOUPDATE mrtupdatetest.c $LIBS
gcc -DNOPPA -g -O3 -o mrtupdatetest.NOPPA mrtupdatetest.c $LIBS
gcc -DNOSTATS -g -O3 -o mrtupdatetest.NOSTATS mrtupdatetest.c $LIBS
gcc -DNOSTATS -DNOPPA -g -O3 -o mrtupdatetest.NOSTATSORPPA mrtupdatetest.c $LIBS
gcc -g -O3 -o mrttabledumptest mrttabledumptest.c $LIBS
gcc -g -O3 -o main main.c $LIBS
