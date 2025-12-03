set boxwidth 0.9 absolute
set style fill solid 1.00 border lt -1
set style histogram clustered gap 5 title textcolor lt -1
set style data histograms
set key outside top center horizontal
set xtics rotate by -45
set grid
set xlabel "DFA state count"
set ylabel "Percentage of DFAs per state count"
set title "DFA State distribution for Rte " font ',10'


$MyData << EOD
"DFA state count" "tree-split-linear samples=412" "tree-split-gauss samples=412" "tree-split-inv-gauss samples=411" "flajolet samples=410" "tbnl samples=200" "comb samples=405"
"2" 30.097 37.864 28.954 44.878 4.500 25.432
"3" 21.845 14.320 26.764 11.707 2.500 22.716
"4" 9.951 6.553 9.002 5.122 4.500 15.309
"5" 5.583 7.282 7.786 5.122 1.000 13.827
"6" 5.340 7.282 3.893 3.659 1.500 6.420
"7" 5.097 4.126 2.676 4.146 2.000 5.679
"8" 3.641 3.398 4.380 3.659 1.500 2.963
"9" 2.184 3.155 2.190 1.463 0.500 1.975
">= 10" 16.262 16.019 14.355 20.244 82.000 5.679
EOD

plot $MyData using 2:xtic(1) ti col, \
   $MyData using 3 ti col, \
   $MyData using 4 ti col, \
   $MyData using 5 ti col, \
   $MyData using 6 ti col, \
   $MyData using 7 ti col,
