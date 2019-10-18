produce CDF graphs like this:
$ gnuplot cdf.gpl -p -e "set title 'Update size in bytes' ; plot 'col0.txt' smooth csplines"
or
$ gnuplot cdf.gpl -p -e "set title 'Update size in bytes' ; set key title 'Log Cumulative' ; plot 'col0.txt' smooth csplines"
