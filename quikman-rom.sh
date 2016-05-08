#!/bin/bash

set -o xtrace
bin/ca65.exe --cpu 6502 --listing quikman-rom.s
bin/ld65.exe -C doc/vic20-cartA.cfg -o quikman-rom.a0 quikman-rom.o
set +o xtrace

echo -n "Launch (Y/N)? "
read choice
[ "$choice" = "y" -o "$choice" = "Y" ] && xvic -ntsc -sound -joydev1 2 -cartA quikman-rom.a0

