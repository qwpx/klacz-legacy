#!/usr/bin/env bash 

width=$(identify -format "%w" $1)
convert -background none -gravity North -geometry +20+20 -font /usr/local/lib/X11/fonts/local/impact2.ttf -stroke black -strokewidth 2 -fill white -pointsize 52 -size ${width} caption:"${2^^}" top.png
convert -background none -gravity South -geometry +20+20 -font /usr/local/lib/X11/fonts/local/impact2.ttf -stroke black -strokewidth 2 -fill white -pointsize 52 -size ${width} caption:"${3^^}" bottom.png
composite -gravity North top.png $1 tmp.png
composite -gravity South bottom.png tmp.png $4
