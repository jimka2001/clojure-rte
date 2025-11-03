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
"DFA state count" "tree-split-linear samples=402" "tree-split-gauss samples=402" "tree-split-inv-gauss samples=402" "flajolet samples=402" "comb samples=397"
"2" 29.851 37.065 29.104 45.025 25.693
"3" 21.393 14.677 26.866 11.443 22.922
"4" 9.950 6.716 9.204 5.224 14.358
"5" 5.721 7.463 7.711 4.975 13.854
"6" 5.473 7.463 3.980 3.731 6.549
"7" 5.224 4.229 2.488 4.229 5.793
"8" 3.731 3.234 4.229 3.731 3.023
"9" 2.239 2.985 2.239 1.493 2.015
">= 10" 16.418 16.169 14.179 20.149 5.793
EOD

plot $MyData using 2:xtic(1) ti col, \
   $MyData using 3 ti col, \
   $MyData using 4 ti col, \
   $MyData using 5 ti col, \
   $MyData using 6 ti col,
