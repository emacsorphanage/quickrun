set samples 20
set isosamples 21
set xlabel "X axis"
set ylabel "Y axis"
set zlabel "Z " offset 1, 0
set view 60, 30, 0.85, 1.1
set key at screen 1.0, 0.9
set style textbox opaque noborder margins 0.5, 0.5

set title "contour plot"
set contour
splot x*y
