@echo off
setlocal

: set ACME="%USERPROFILE%\Acme\acme.exe"
set ACME="..\..\build\src\Release\acme.exe"
set C1541=c1541

%ACME% -I include\acme -I src -f cbm -o build\forth.prg -r build\forth.txt src\forth.asm

: # TODO generate d81 image with everything
: ${C1541} -format 'MEGA65 FORTH,1' d81 build/mega65-forth.d81
: ${C1541} build/mega65-forth.d81 -write build/forth.prg autoboot.c65
: ${C1541} build/mega65-forth.d81 -write src/bootstrap.f # TODO not a prg - usr? seq?
: ${C1541} build/mega65-forth.d81 -dir > build/mega65-forth.lst
