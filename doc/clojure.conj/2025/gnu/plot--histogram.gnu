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
"DFA state count" "tree-split-linear samples=404" "tree-split-gauss samples=404" "tree-split-inv-gauss samples=404" "flajolet samples=404" "tbnl samples=194" "comb samples=399"
"2" 29.703 37.129 28.960 45.297 4.639 25.564
"3" 21.782 14.604 26.733 11.386 2.577 23.308
"4" 9.901 6.683 9.406 5.198 4.639 14.286
"5" 5.693 7.426 7.673 4.950 1.031 13.784
"6" 5.446 7.426 3.960 3.713 1.546 6.516
"7" 5.198 4.208 2.723 4.208 2.062 5.764
"8" 3.713 3.218 4.208 3.713 1.546 3.008
"9" 2.228 2.970 2.228 1.485 0.515 2.005
">= 10" 16.337 16.337 14.109 20.050 81.443 5.764
EOD

plot $MyData using 2:xtic(1) ti col, \
   $MyData using 3 ti col, \
   $MyData using 4 ti col, \
   $MyData using 5 ti col, \
   $MyData using 6 ti col, \
   $MyData using 7 ti col,
