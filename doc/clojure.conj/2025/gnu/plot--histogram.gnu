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
"DFA state count" "tree-split-linear samples=406" "tree-split-gauss samples=406" "tree-split-inv-gauss samples=406" "flajolet samples=406" "root-averse samples=196" "comb samples=401"
"2" 29.803 36.946 29.064 45.074 4.592 25.436
"3" 21.675 14.532 26.601 11.330 2.551 23.441
"4" 10.099 7.143 9.360 5.419 4.592 14.214
"5" 5.665 7.389 7.635 4.926 1.020 13.716
"6" 5.419 7.389 4.187 3.695 1.531 6.484
"7" 5.172 4.187 2.709 4.433 2.041 5.736
"8" 3.695 3.202 4.187 3.695 1.531 3.242
"9" 2.217 2.956 2.217 1.478 0.510 1.995
">= 10" 16.256 16.256 14.039 19.951 81.633 5.736
EOD

plot $MyData using 2:xtic(1) ti col, \
   $MyData using 3 ti col, \
   $MyData using 4 ti col, \
   $MyData using 5 ti col, \
   $MyData using 6 ti col, \
   $MyData using 7 ti col,
