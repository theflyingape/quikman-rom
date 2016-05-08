@echo on

set SRC=%CD%

pushd %~dp0
cd ..\..\cc65

bin\ca65 --cpu 6502 --listing %SRC%\quikman-rom.s
bin\ld65 -C doc\vic20-cartA.cfg -o %SRC%\quikman-rom.a0 %SRC%\quikman-rom.o

pause

cd ..
xvic -memory none -ntsc -sound -joydev1 2 -cartA VIC20\quikman-rom\quikman-rom.a0

popd
pause
exit
