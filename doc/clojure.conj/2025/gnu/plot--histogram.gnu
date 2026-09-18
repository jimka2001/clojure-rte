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
"DFA state count" "tree-split-linear samples=410" "tree-split-gauss samples=410" "tree-split-inv-gauss samples=410" "flajolet samples=410" "root-averse samples=413" "comb samples=405"
"-1" 0.242
"2" 30.000 36.585 29.268 44.878 3.874 25.432
"3" 21.463 14.634 26.585 11.220 2.663 23.210
"4" 10.000 7.073 9.268 5.366 3.390 14.321
"5" 5.854 7.317 7.561 5.122 1.695 14.074
"6" 5.366 7.805 4.146 3.659 1.211 6.420
"7" 5.122 4.146 2.683 4.390 1.453 5.679
"8" 3.902 3.171 4.390 4.146 1.211 3.210
"9" 2.195 2.927 2.195 1.463 1.453 1.975
">= 10" 16.098 16.341 13.902 19.756 82.809 5.679
EOD

plot $MyData using 2:xtic(1) ti col, \
   $MyData using 3 ti col, \
   $MyData using 4 ti col, \
   $MyData using 5 ti col, \
   $MyData using 6 ti col, \
   $MyData using 7 ti col,
