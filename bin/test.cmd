@echo off
setlocal

set XMEGA65="C:\Program Files\xemu\xmega65"

: %XMEGA65% -besure -forcerom -loadrom ..\..\rom\920235.bin -prg ..\..\build\forth.prg
%XMEGA65% -prg build/forth.prg

