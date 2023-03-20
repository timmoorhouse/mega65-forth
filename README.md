
# MEGA65 Forth (MEGA654<sup>th</sup>?)

An attempt at [Forth](https://forth-standard.org/standard/words) for the [MEGA65](https://mega65.org/).  This is heavily influenced by [FIG FORTH release 1.1](https://github.com/ptorric/figforth)

```
This publication has been made available by the 
Forth Interest Group, P. O. box 1105, San Carlos, CA 94070
```

![screenshot](doc/screenshot.png)

Note that we have actually switched to font A (the ASCII font).  The `\` word shows up correctly on screen but the screenshot from m65 assumes the default font.

This is still in the early stages of developement - check the status table below for details on what is and is not supported so far.  The most important missing pieces right now are likely support for `MARKER` and/or `FORGET`, an editor and an assembler word set.  Programs can be entered manually or read from pre-existing sequential files, however, and much of the core dictionary is implemented and fairly stable.

The next priorities are:
- `MARKER` has been implemented, but it might be worth trying to get a working `FORGET` too.
- We can now run the complete [preliminary](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/prelimtest.fth), [core](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/core.fr), [core plus](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/coreplustest.fth) and [core extension](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/coreexttest.fth) tests unmodified, but there are a number of failures that need to be addressed.
- Move more of the implementation from assembler to Forth.  As this happens, getting a functional `SEE` is becoming more and more important (even if it's just a hex dump at first).

My apologies if this isn't the greatest Forth implementation.  I don't really have much experience with Forth.  I'm doing this because I've always been intrigued by Forth, and with a new MEGA65 sitting on the dining room table, it seemed the perfect project to learn about Forth, shake decades of dust off my 6502 programming skills, and learn about some of the MEGA65-specific features.

An attempt will be made to:

- Be more [Forth 2012](http://www.forth200x.org/documents/forth-2012.pdf) compliant, or at least ANS Forth (94).  Things that have been removed in 2012 (such as `TIB`) almost certainly won't be implemented.  Things that are obsolescent in 2012 (such as `[COMPILE]`) may not be implemented either.
- Provide access to MEGA65 features.  I'll likely look at least at SUPER FORTH 64 (which was based on MVP-FORTH) for API ideas here, and possibly others, but I may not retain compatability.

I haven't looked at geoForth yet, but intend to at some point.  Making some sort of equivalent for the MEGA65 GEOS might be interesting, and give me an excuse to dig into GEOS a bit.  No promises here though.

Eventually, I want to sort out how to take advantage of more than 64K without breaking compliance with the Forth 2012 specification.  It would be nice if we could move much of the standard dictionary to bank 1, for example.

# BUILDING

## Requirements

- [ACME](https://sourceforge.net/projects/acme-crossass):  I was using the one from [MEGA65 github](https://github.com/MEGA65/acme) but switched to the svn version to get access to some of the more recent features, but I'm not certain I ended up using them.  It could be that the MEGA65 one will still work.

There is a build script in `bin/build.sh` that can be used.  Right now it doesn't do a huge amount, but eventually I expect
that the build process will get more involved:

- Compiling the builtins
- Use a MEGA65 or xemu to compile the rest of the standard dictionary, possibly for multiple configurations (a minimal set, a complete set, etc)
- Run a test suite
- Run benchmarks
- Building tools (an assembler word set? an editor?)
- Assembling a disk image with various build configuration, source code, examples, tools

## Recommendations

- [VICE](https://vice-emu.sourceforge.io/) for c1541 and petcat to manipulate d81 images
- m65, mega65_ftp from [MEGA65 Tools](https://github.com/MEGA65/mega65-tools)
- [m65dbg](https://github.com/MEGA65/m65dbg)
- [Xemu](https://github.com/lgblgblgb/xemu)

# RANDOM TODOs

These should get us to the point of bootstrapping with a dictionary written in forth and running unit tests:
- CORE
  - [ ] Need to reimplement the multiplication/division operations using the math unit.
- EXCEPTION
  - [ ] Need to be able to throw exceptions from assembler.  This likely means changing the `THROW` implementation to be native code instead of Forth bytecode.
  - [ ] Many bits of code need to be updated to throw an exception for error conditions.
- FILE
  - [ ] Handle file access modes.
  - [ ] Fix I/O status handling (when is status from READSS reset?).
  - [ ] Fix handling of I/O error cases.
- Error checking (and throwing the appropriate exception)
  - [ ] Unaligned address.
  - [ ] Divide by zero.
  - [ ] Pictured numeric output string overflow.
  - [ ] Parsed string overflow?
  - [ ] Mismatched control structures.
  - [ ] Invalid name.
  - [ ] File I/O error.
  - [ ] Unexpected EOF.
- Moving more of the implementation from assembler to Forth.
  - [ ] `READ-LINE`
  - [ ] `ACCEPT`
  - [ ] `REFILL`
  - [ ] `EVALUATE` possibly? (would need to tweak the bootstrap code)
    - [ ] `SAVE-INPUT`
    - [ ] `RESTORE-INPUT`
    - [ ] `SOURCE`
  - [ ] `UNUSED` possibly? would need access to DAREA
- Tests
  - [ ] [Test suite](https://github.com/gerryjackson/forth2012-test-suite)
  - [ ] Some reasonable way of capturing test results. 
    - It might make sense to have a way to redirect `EMIT` to a file.  gforth makes `EMIT` a deferred, which might be the cleanest option.
- Benchmarks
  - [ ] Compiling the dictionary?
  - [ ] Something without I/O?

There'll be lots more to do after that.

# STATUS

Note that some of these are implemented in Forth and the bootstrap process is not yet automated.

Word Set | Implemented | Not (Yet?) Implemented
-- | -- | --
BLOCK | | [`BLK`](https://forth-standard.org/standard/block/BLK) [`BLOCK`](https://forth-standard.org/standard/block/BLOCK) [`BUFFER`](https://forth-standard.org/standard/block/BUFFER) [`FLUSH`](https://forth-standard.org/standard/block/FLUSH) [`LOAD`](https://forth-standard.org/standard/block/LOAD) [`SAVE-BUFFERS`](https://forth-standard.org/standard/block/SAVE-BUFFERS) [`UPDATE`](https://forth-standard.org/standard/block/UPDATE)
BLOCK-EXT | | [`EMPTY-BUFFERS`](https://forth-standard.org/standard/block/EMPTY-BUFFERS) [`LIST`](https://forth-standard.org/standard/block/LIST) [`SCR`](https://forth-standard.org/standard/block/SCR) [`THRU`](https://forth-standard.org/standard/block/THRU)
CORE | [`!`](https://forth-standard.org/standard/core/Store) [`#`](https://forth-standard.org/standard/core/num) [`#>`](https://forth-standard.org/standard/core/num-end) [`#S`](https://forth-standard.org/standard/core/numS) [`'`](https://forth-standard.org/standard/core/Tick) [`(`](https://forth-standard.org/standard/core/p) [`*`](https://forth-standard.org/standard/core/Times) [`*/`](https://forth-standard.org/standard/core/TimesDiv) [`*/MOD`](https://forth-standard.org/standard/core/TimesDivMOD) [`/MOD`](https://forth-standard.org/standard/core/DivMOD) [`+`](https://forth-standard.org/standard/core/Plus) [`+!`](https://forth-standard.org/standard/core/PlusStore) [`+LOOP`](https://forth-standard.org/standard/core/PlusLOOP) [`,`](https://forth-standard.org/standard/core/Comma) [`-`](https://forth-standard.org/standard/core/Minus) [`.`](https://forth-standard.org/standard/core/d) [`."`](https://forth-standard.org/standard/core/Dotq) [`/`](https://forth-standard.org/standard/core/Div) [`0<`](https://forth-standard.org/standard/core/Zeroless) [`0=`](https://forth-standard.org/standard/core/ZeroEqual) [`1+`](https://forth-standard.org/standard/core/OnePlus) [`1-`](https://forth-standard.org/standard/core/OneMinus) [`2!`](https://forth-standard.org/standard/core/TwoStore) [`2*`](https://forth-standard.org/standard/core/TwoTimes) [`2/`](https://forth-standard.org/standard/core/TwoDiv) [`2@`](https://forth-standard.org/standard/core/TwoFetch) [`2DROP`](https://forth-standard.org/standard/core/TwoDROP) [`2DUP`](https://forth-standard.org/standard/core/TwoDUP) [`2OVER`](https://forth-standard.org/standard/core/TwoOVER) [`2SWAP`](https://forth-standard.org/standard/core/TwoSWAP) [`:`](https://forth-standard.org/standard/core/Colon) [`;`](https://forth-standard.org/standard/core/Semi) [`<`](https://forth-standard.org/standard/core/less) [`<#`](https://forth-standard.org/standard/core/num-start) [`=`](https://forth-standard.org/standard/core/Equal) [`>`](https://forth-standard.org/standard/core/more) [`>BODY`](https://forth-standard.org/standard/core/toBODY) [`>IN`](https://forth-standard.org/standard/core/toIN) [`>NUMBER`](https://forth-standard.org/standard/core/toNUMBER) [`>R`](https://forth-standard.org/standard/core/toR) [`?DUP`](https://forth-standard.org/standard/core/qDUP) [`@`](https://forth-standard.org/standard/core/Fetch) [`ABORT`](https://forth-standard.org/standard/core/ABORT) [`ABORT"`](https://forth-standard.org/standard/core/ABORTq) [`ABS`](https://forth-standard.org/standard/core/ABS) [`ACCEPT`](https://forth-standard.org/standard/core/ACCEPT) [`ALIGN`](https://forth-standard.org/standard/core/ALIGN) [`ALIGNED`](https://forth-standard.org/standard/core/ALIGNED) [`ALLOT`](https://forth-standard.org/standard/core/ALLOT) [`AND`](https://forth-standard.org/standard/core/AND) [`BASE`](https://forth-standard.org/standard/core/BASE) [`BEGIN`](https://forth-standard.org/standard/core/BEGIN) [`BL`](https://forth-standard.org/standard/core/BL) [`C!`](https://forth-standard.org/standard/core/CStore) [`C,`](https://forth-standard.org/standard/core/CComma) [`C@`](https://forth-standard.org/standard/core/CFetch) [`CELL+`](https://forth-standard.org/standard/core/CELLPlus) [`CELLS`](https://forth-standard.org/standard/core/CELLS) [`CHAR`](https://forth-standard.org/standard/core/CHAR) [`CHAR+`](https://forth-standard.org/standard/core/CHARPlus) [`CHARS`](https://forth-standard.org/standard/core/CHARS) [`CONSTANT`](https://forth-standard.org/standard/core/CONSTANT) [`COUNT`](https://forth-standard.org/standard/core/COUNT) [`CR`](https://forth-standard.org/standard/core/CR) [`CREATE`](https://forth-standard.org/standard/core/CREATE) [`DECIMAL`](https://forth-standard.org/standard/core/DECIMAL) [`DEPTH`](https://forth-standard.org/standard/core/DEPTH) [`DO`](https://forth-standard.org/standard/core/DO) [`DOES>`](https://forth-standard.org/standard/core/DOES) [`DROP`](https://forth-standard.org/standard/core/DROP) [`DUP`](https://forth-standard.org/standard/core/DUP) [`ELSE`](https://forth-standard.org/standard/core/ELSE) [`EMIT`](https://forth-standard.org/standard/core/EMIT) [`ENVIRONMENT?`](https://forth-standard.org/standard/core/ENVIRONMENTq) [`EVALUATE`](https://forth-standard.org/standard/core/EVALUATE) [`EXECUTE`](https://forth-standard.org/standard/core/EXECUTE) [`EXIT`](https://forth-standard.org/standard/core/EXIT) [`FILL`](https://forth-standard.org/standard/core/FILL) [`FIND`](https://forth-standard.org/standard/core/FIND) [`FM/MOD`](https://forth-standard.org/standard/core/FMDivMOD) [`HERE`](https://forth-standard.org/standard/core/HERE) [`HOLD`](https://forth-standard.org/standard/core/HOLD) [`I`](https://forth-standard.org/standard/core/I) [`IF`](https://forth-standard.org/standard/core/IF) [`IMMEDIATE`](https://forth-standard.org/standard/core/IMMEDIATE) [`INVERT`](https://forth-standard.org/standard/core/INVERT) [`J`](https://forth-standard.org/standard/core/J) [`KEY`](https://forth-standard.org/standard/core/KEY) [`LEAVE`](https://forth-standard.org/standard/core/LEAVE) [`LITERAL`](https://forth-standard.org/standard/core/LITERAL) [`LOOP`](https://forth-standard.org/standard/core/LOOP) [`LSHIFT`](https://forth-standard.org/standard/core/LSHIFT) [`M*`](https://forth-standard.org/standard/core/MTimes) [`MAX`](https://forth-standard.org/standard/core/MAX) [`MIN`](https://forth-standard.org/standard/core/MIN) [`MOD`](https://forth-standard.org/standard/core/MOD) [`MOVE`](https://forth-standard.org/standard/core/MOVE) [`NEGATE`](https://forth-standard.org/standard/core/NEGATE) [`OR`](https://forth-standard.org/standard/core/OR) [`OVER`](https://forth-standard.org/standard/core/OVER) [`POSTPONE`](https://forth-standard.org/standard/core/POSTPONE) [`QUIT`](https://forth-standard.org/standard/core/QUIT) [`R>`](https://forth-standard.org/standard/core/Rfrom) [`R@`](https://forth-standard.org/standard/core/RFetch) [`RECURSE`](https://forth-standard.org/standard/core/RECURSE) [`REPEAT`](https://forth-standard.org/standard/core/REPEAT) [`ROT`](https://forth-standard.org/standard/core/ROT) [`RSHIFT`](https://forth-standard.org/standard/core/RSHIFT) [`S"`](https://forth-standard.org/standard/core/Sq) [`S>D`](https://forth-standard.org/standard/core/StoD) [`SIGN`](https://forth-standard.org/standard/core/SIGN) [`SM/REM`](https://forth-standard.org/standard/core/SMDivREM) [`SOURCE`](https://forth-standard.org/standard/core/SOURCE) [`SPACE`](https://forth-standard.org/standard/core/SPACE) [`SPACES`](https://forth-standard.org/standard/core/SPACES) [`STATE`](https://forth-standard.org/standard/core/STATE) [`SWAP`](https://forth-standard.org/standard/core/SWAP) [`THEN`](https://forth-standard.org/standard/core/THEN) [`TYPE`](https://forth-standard.org/standard/core/TYPE) [`U.`](https://forth-standard.org/standard/core/Ud) [`U<`](https://forth-standard.org/standard/core/Uless) [`UM*`](https://forth-standard.org/standard/core/UMTimes) [`UM/MOD`](https://forth-standard.org/standard/core/UMDivMOD) [`UNLOOP`](https://forth-standard.org/standard/core/UNLOOP) [`UNTIL`](https://forth-standard.org/standard/core/UNTIL) [`VARIABLE`](https://forth-standard.org/standard/core/VARIABLE) [`WHILE`](https://forth-standard.org/standard/core/WHILE) [`WORD`](https://forth-standard.org/standard/core/WORD) [`XOR`](https://forth-standard.org/standard/core/XOR) [`[`](https://forth-standard.org/standard/core/Bracket) [`[']`](https://forth-standard.org/standard/core/BracketTick) [`[CHAR]`](https://forth-standard.org/standard/core/BracketCHAR) [`]`](https://forth-standard.org/standard/core/right-bracket) |
CORE-EXT | [`.(`](https://forth-standard.org/standard/core/Dotp) [`.R`](https://forth-standard.org/standard/core/DotR) [`0<>`](https://forth-standard.org/standard/core/Zerone) [`0>`](https://forth-standard.org/standard/core/Zeromore) [`2>R`](https://forth-standard.org/standard/core/TwotoR) [`2R>`](https://forth-standard.org/standard/core/TwoRfrom) [`2R@`](https://forth-standard.org/standard/core/TwoRFetch) [`:NONAME`](https://forth-standard.org/standard/core/ColonNONAME) [`<>`](https://forth-standard.org/standard/core/ne) [`?DO`](https://forth-standard.org/standard/core/qDO) [`ACTION-OF`](https://forth-standard.org/standard/core/ACTION-OF) [`AGAIN`](https://forth-standard.org/standard/core/AGAIN) [`BUFFER:`](https://forth-standard.org/standard/core/BUFFERColon) [`C"`](https://forth-standard.org/standard/core/Cq) [`CASE`](https://forth-standard.org/standard/core/CASE) [`COMPILE,`](https://forth-standard.org/standard/core/COMPILEComma) [`DEFER`](https://forth-standard.org/standard/core/DEFER) [`DEFER!`](https://forth-standard.org/standard/core/DEFERStore) [`DEFER@`](https://forth-standard.org/standard/core/DEFERFetch) [`ENDCASE`](https://forth-standard.org/standard/core/ENDCASE) [`ENDOF`](https://forth-standard.org/standard/core/ENDOF) [`ERASE`](https://forth-standard.org/standard/core/ERASE) [`FALSE`](https://forth-standard.org/standard/core/FALSE) [`HEX`](https://forth-standard.org/standard/core/HEX) [`HOLDS`](https://forth-standard.org/standard/core/HOLDS) [`IS`](https://forth-standard.org/standard/core/IS) [`MARKER`](https://forth-standard.org/standard/core/MARKER) [`NIP`](https://forth-standard.org/standard/core/NIP) [`OF`](https://forth-standard.org/standard/core/OF) [`PAD`](https://forth-standard.org/standard/core/PAD) [`PARSE`](https://forth-standard.org/standard/core/PARSE) [`PARSE-NAME`](https://forth-standard.org/standard/core/PARSE-NAME) [`PICK`](https://forth-standard.org/standard/core/PICK) [`REFILL`](https://forth-standard.org/standard/core/REFILL) [`RESTORE-INPUT`](https://forth-standard.org/standard/core/RESTORE-INPUT) [`ROLL`](https://forth-standard.org/standard/core/ROLL) [`SAVE-INPUT`](https://forth-standard.org/standard/core/SAVE-INPUT) [`SOURCE-ID`](https://forth-standard.org/standard/core/SOURCE-ID) [`S\"`](https://forth-standard.org/standard/core/Seq) [`TO`](https://forth-standard.org/standard/core/TO) [`TRUE`](https://forth-standard.org/standard/core/TRUE) [`TUCK`](https://forth-standard.org/standard/core/TUCK) [`U.R`](https://forth-standard.org/standard/core/UDotR) [`U>`](https://forth-standard.org/standard/core/Umore) [`UNUSED`](https://forth-standard.org/standard/core/UNUSED) [`VALUE`](https://forth-standard.org/standard/core/VALUE) [`WITHIN`](https://forth-standard.org/standard/core/WITHIN) [`\`](https://forth-standard.org/standard/core/bs) |
CORE-EXT obsolescent | | [`[COMPILE]`](https://forth-standard.org/standard/core/BracketCOMPILE)
DOUBLE | [`2CONSTANT`](https://forth-standard.org/standard/double/TwoCONSTANT) [`2LITERAL`](https://forth-standard.org/standard/double/TwoLITERAL) [`D+`](https://forth-standard.org/standard/double/DPlus) [`D-`](https://forth-standard.org/standard/double/DMinus) [`D.`](https://forth-standard.org/standard/double/Dd) [`D.R`](https://forth-standard.org/standard/double/DDotR) [`D>S`](https://forth-standard.org/standard/double/DtoS) [`DABS`](https://forth-standard.org/standard/double/DABS) [`DNEGATE`](https://forth-standard.org/standard/double/DNEGATE) | [`2VARIABLE`](https://forth-standard.org/standard/double/TwoVARIABLE) [`D0<`](https://forth-standard.org/standard/double/DZeroless) [`D0=`](https://forth-standard.org/standard/double/DZeroEqual) [`D2*`](https://forth-standard.org/standard/double/DTwoTimes) [`D2/`](https://forth-standard.org/standard/double/DTwoDiv) [`D<`](https://forth-standard.org/standard/double/Dless) [`D=`](https://forth-standard.org/standard/double/DEqual) [`DMAX`](https://forth-standard.org/standard/double/DMAX) [`DMIN`](https://forth-standard.org/standard/double/DMIN) [`M*/`](https://forth-standard.org/standard/double/MTimesDiv) [`M+`](https://forth-standard.org/standard/double/MPlus)
DOUBLE-EXT | | [`2ROT`](https://forth-standard.org/standard/double/TwoROT) [`2VALUE`](https://forth-standard.org/standard/double/TwoVALUE) [`DU<`](https://forth-standard.org/standard/double/DUless)
EXCEPTION | [`CATCH`](https://forth-standard.org/standard/exception/CATCH) [`THROW`](https://forth-standard.org/standard/exception/THROW) |
FACILITY | [`PAGE`](https://forth-standard.org/standard/facility/PAGE) | [`AT-XY`](https://forth-standard.org/standard/facility/AT-XY) [`KEY?`](https://forth-standard.org/standard/facility/KEYq)
FACILITY-EXT | [`K-DELETE`](https://forth-standard.org/standard/facility/K-DELETE) [`K-DOWN`](https://forth-standard.org/standard/facility/K-DOWN) [`K-F1`](https://forth-standard.org/standard/facility/K-F1) [`K-F10`](https://forth-standard.org/standard/facility/K-F10) [`K-F11`](https://forth-standard.org/standard/facility/K-F11) [`K-F12`](https://forth-standard.org/standard/facility/K-F12) [`K-F2`](https://forth-standard.org/standard/facility/K-F2) [`K-F3`](https://forth-standard.org/standard/facility/K-F3) [`K-F4`](https://forth-standard.org/standard/facility/K-F4) [`K-F5`](https://forth-standard.org/standard/facility/K-F5) [`K-F6`](https://forth-standard.org/standard/facility/K-F6) [`K-F7`](https://forth-standard.org/standard/facility/K-F7) [`K-F8`](https://forth-standard.org/standard/facility/K-F8) [`K-F9`](https://forth-standard.org/standard/facility/K-F9) [`K-HOME`](https://forth-standard.org/standard/facility/K-HOME) [`K-INSERT`](https://forth-standard.org/standard/facility/K-INSERT) [`K-LEFT`](https://forth-standard.org/standard/facility/K-LEFT) [`K-RIGHT`](https://forth-standard.org/standard/facility/K-RIGHT) [`K-UP`](https://forth-standard.org/standard/facility/K-UP) | [`+FIELD`](https://forth-standard.org/standard/facility/PlusFIELD) [`BEGIN-STRUCTURE`](https://forth-standard.org/standard/facility/BEGIN-STRUCTURE) [`CFIELD:`](https://forth-standard.org/standard/facility/CFIELDColon) [`EKEY`](https://forth-standard.org/standard/facility/EKEY) [`EKEY>CHAR`](https://forth-standard.org/standard/facility/EKEYtoCHAR) [`EKEY>FKEY`](https://forth-standard.org/standard/facility/EKEYtoFKEY) [`EKEY?`](https://forth-standard.org/standard/facility/EKEYq) [`EMIT?`](https://forth-standard.org/standard/facility/EMITq) [`END-STRUCTURE`](https://forth-standard.org/standard/facility/END-STRUCTURE) [`FIELD:`](https://forth-standard.org/standard/facility/FIELDColon) [`K-ALT-MASK`](https://forth-standard.org/standard/facility/K-ALT-MASK) [`K-CTRL-MASK`](https://forth-standard.org/standard/facility/K-CTRL-MASK) [`K-END`](https://forth-standard.org/standard/facility/K-END) [`K-NEXT`](https://forth-standard.org/standard/facility/K-NEXT) [`K-PRIOR`](https://forth-standard.org/standard/facility/K-PRIOR) [`K-SHIFT-MASK`](https://forth-standard.org/standard/facility/K-SHIFT-MASK) [`MS`](https://forth-standard.org/standard/facility/MS) [`TIME&DATE`](https://forth-standard.org/standard/facility/TIMEandDATE)
FILE | [`BIN`](https://forth-standard.org/standard/file/BIN) [`CLOSE-FILE`](https://forth-standard.org/standard/file/CLOSE-FILE) [`INCLUDE-FILE`](https://forth-standard.org/standard/file/INCLUDE-FILE) [`INCLUDED`](https://forth-standard.org/standard/file/INCLUDED) [`OPEN-FILE`](https://forth-standard.org/standard/file/OPEN-FILE)[^partial] [`R/O`](https://forth-standard.org/standard/file/RDivO) [`R/W`](https://forth-standard.org/standard/file/RDivW) [`READ-LINE`](https://forth-standard.org/standard/file/READ-LINE) [`W/O`](https://forth-standard.org/standard/file/WDivO) [`WRITE-FILE`](https://forth-standard.org/standard/file/WRITE-FILE) [`WRITE-LINE`](https://forth-standard.org/standard/file/WRITE-LINE) | [`CREATE-FILE`](https://forth-standard.org/standard/file/CREATE-FILE) [`DELETE-FILE`](https://forth-standard.org/standard/file/DELETE-FILE) [`FILE-POSITION`](https://forth-standard.org/standard/file/FILE-POSITION) [`FILE-SIZE`](https://forth-standard.org/standard/file/FILE-SIZE) [`READ-FILE`](https://forth-standard.org/standard/file/READ-FILE) [`REPOSITION-FILE`](https://forth-standard.org/standard/file/REPOSITION-FILE) [`RESIZE-FILE`](https://forth-standard.org/standard/file/RESIZE-FILE)
FILE-EXT | [`INCLUDE`](https://forth-standard.org/standard/file/INCLUDE) | [`FILE-STATUS`](https://forth-standard.org/standard/file/FILE-STATUS) [`FLUSH-FILE`](https://forth-standard.org/standard/file/FLUSH-FILE) [`RENAME-FILE`](https://forth-standard.org/standard/file/RENAME-FILE) [`REQUIRE`](https://forth-standard.org/standard/file/REQUIRE) [`REQUIRED`](https://forth-standard.org/standard/file/REQUIRED)
FLOATING | | [`>FLOAT`](https://forth-standard.org/standard/float/toFLOAT) [`D>F`](https://forth-standard.org/standard/float/DtoF) [`F!`](https://forth-standard.org/standard/float/FStore) [`F*`](https://forth-standard.org/standard/float/FTimes) [`F+`](https://forth-standard.org/standard/float/FPlus) [`F-`](https://forth-standard.org/standard/float/FMinus) [`F/`](https://forth-standard.org/standard/float/FDiv) [`F0<`](https://forth-standard.org/standard/float/FZeroless) [`F0=`](https://forth-standard.org/standard/float/FZeroEqual) [`F<`](https://forth-standard.org/standard/float/Fless) [`F>D`](https://forth-standard.org/standard/float/FtoD) [`F@`](https://forth-standard.org/standard/float/FFetch) [`FALIGN`](https://forth-standard.org/standard/float/FALIGN) [`FALIGNED`](https://forth-standard.org/standard/float/FALIGNED) [`FCONSTANT`](https://forth-standard.org/standard/float/FCONSTANT) [`FDEPTH`](https://forth-standard.org/standard/float/FDEPTH) [`FDROP`](https://forth-standard.org/standard/float/FDROP) [`FDUP`](https://forth-standard.org/standard/float/FDUP) [`FLITERAL`](https://forth-standard.org/standard/float/FLITERAL) [`FLOAT+`](https://forth-standard.org/standard/float/FLOATPlus) [`FLOATS`](https://forth-standard.org/standard/float/FLOATS) [`FLOOR`](https://forth-standard.org/standard/float/FLOOR) [`FMAX`](https://forth-standard.org/standard/float/FMAX) [`FMIN`](https://forth-standard.org/standard/float/FMIN) [`FNEGATE`](https://forth-standard.org/standard/float/FNEGATE) [`FOVER`](https://forth-standard.org/standard/float/FOVER) [`FROT`](https://forth-standard.org/standard/float/FROT) [`FROUND`](https://forth-standard.org/standard/float/FROUND) [`FSWAP`](https://forth-standard.org/standard/float/FSWAP) [`FVARIABLE`](https://forth-standard.org/standard/float/FVARIABLE) [`REPRESENT`](https://forth-standard.org/standard/float/REPRESENT)
FLOATING-EXT | | [`DF!`](https://forth-standard.org/standard/float/DFStore) [`DF@`](https://forth-standard.org/standard/float/DFFetch) [`DFALIGN`](https://forth-standard.org/standard/float/DFALIGN) [`DFALIGNED`](https://forth-standard.org/standard/float/DFALIGNED) [`DFFIELD`](https://forth-standard.org/standard/float/DFFIELD) [`DFLOAT+`](https://forth-standard.org/standard/float/DFLOATPlus) [`DFLOATS`](https://forth-standard.org/standard/float/DFLOATS) [`F**`](https://forth-standard.org/standard/float/FTimesTimes) [`F.`](https://forth-standard.org/standard/float/Fd) [`F>S`](https://forth-standard.org/standard/float/FtoS) [`FABS`](https://forth-standard.org/standard/float/FABS) [`FACOS`](https://forth-standard.org/standard/float/FACOS) [`FACOSH`](https://forth-standard.org/standard/float/FACOSH) [`FALOG`](https://forth-standard.org/standard/float/FALOG) [`FASIN`](https://forth-standard.org/standard/float/FASIN) [`FASINH`](https://forth-standard.org/standard/float/FASINH) [`FATAN`](https://forth-standard.org/standard/float/FATAN) [`FATAN2`](https://forth-standard.org/standard/float/FATAN2) [`FATANH`](https://forth-standard.org/standard/float/FATANH) [`FCOS`](https://forth-standard.org/standard/float/FCOS) [`FCOSH`](https://forth-standard.org/standard/float/FCOSH) [`FE.`](https://forth-standard.org/standard/float/FEd) [`FEXP`](https://forth-standard.org/standard/float/FEXP) [`FEXPM1`](https://forth-standard.org/standard/float/FEXPM1) [`FFIELD:`](https://forth-standard.org/standard/float/FFIELDColon) [`FLN`](https://forth-standard.org/standard/float/FLN) [`FLNP1`](https://forth-standard.org/standard/float/FLNP1) [`FLOG`](https://forth-standard.org/standard/float/FLOG) [`FS.`](https://forth-standard.org/standard/float/FSd) [`FSIN`](https://forth-standard.org/standard/float/FSIN) [`FSINCOS`](https://forth-standard.org/standard/float/FSINCOS) [`FSINH`](https://forth-standard.org/standard/float/FSINH) [`FSQRT`](https://forth-standard.org/standard/float/FSQRT) [`FTAN`](https://forth-standard.org/standard/float/FTAN) [`FTANH`](https://forth-standard.org/standard/float/FTANH) [`FTRUNC`](https://forth-standard.org/standard/float/FTRUNC) [`FVALUE`](https://forth-standard.org/standard/float/FVALUE) [`F~`](https://forth-standard.org/standard/float/Ftilde) [`PRECISION`](https://forth-standard.org/standard/float/PRECISION) [`S>F`](https://forth-standard.org/standard/float/S>F) [`SET-PRECISION`](https://forth-standard.org/standard/float/SET-PRECISION) [`SF!`](https://forth-standard.org/standard/float/SFStore) [`SF@`](https://forth-standard.org/standard/float/SFFetch) [`SFALIGN`](https://forth-standard.org/standard/float/SFALIGN) [`SFALIGNED`](https://forth-standard.org/standard/float/SFALIGNED) [`SFIELD:`](https://forth-standard.org/standard/float/SFIELDColon) [`SFLOAT+`](https://forth-standard.org/standard/float/SFLOATPlus) [`SFLOATS`](https://forth-standard.org/standard/float/SFLOATS)
LOCALS | | [`(LOCAL)`](https://forth-standard.org/standard/locals/LOCAL) [`TO`](https://forth-standard.org/standard/locals/TO)
LOCALS-EXT | | [`{:`](https://forth-standard.org/standard/locals/bColon)
LOCALS-EXT obsolescent | | [`LOCALS\|`](https://forth-standard.org/standard/locals/LOCALS)
MEMORY | | [`ALLOCATE`](https://forth-standard.org/standard/memory/ALLOCATE) [`FREE`](https://forth-standard.org/standard/memory/FREE) [`RESIZE`](https://forth-standard.org/standard/memory/RESIZE)
SEARCH | [`DEFINITIONS`](https://forth-standard.org/standard/search/DEFINITIONS)  [`FORTH-WORDLIST`](https://forth-standard.org/standard/search/FORTH-WORDLIST) [`GET-CURRENT`](https://forth-standard.org/standard/search/GET-CURRENT) [`GET-ORDER`](https://forth-standard.org/standard/search/GET-ORDER) [`SEARCH-WORDLIST`](https://forth-standard.org/standard/search/SEARCH-WORDLIST) [`SET-CURRENT`](https://forth-standard.org/standard/search/SET-CURRENT) [`SET-ORDER`](https://forth-standard.org/standard/search/SET-ORDER) [`WORDLIST`](https://forth-standard.org/standard/search/WORDLIST) |
SEARCH-EXT | [`ALSO`](https://forth-standard.org/standard/search/ALSO) [`FORTH`](https://forth-standard.org/standard/search/FORTH) [`ONLY`](https://forth-standard.org/standard/search/ONLY) [`ORDER`](https://forth-standard.org/standard/search/ORDER) [`PREVIOUS`](https://forth-standard.org/standard/search/PREVIOUS) |
STRING | [`/STRING`](https://forth-standard.org/standard/string/DivSTRING) [`BLANK`](https://forth-standard.org/standard/string/BLANK) [`CMOVE`](https://forth-standard.org/standard/string/CMOVE) [`CMOVE>`](https://forth-standard.org/standard/string/CMOVEtop) [`COMPARE`](https://forth-standard.org/standard/string/COMPARE) [`SLITERAL`](https://forth-standard.org/standard/string/SLITERAL) | [`-TRAILING`](https://forth-standard.org/standard/string/MinusTRAILING) [`SEARCH`](https://forth-standard.org/standard/string/SEARCH)
STRING-EXT | [`UNESCAPE`](https://forth-standard.org/standard/string/UNESCAPE) | [`REPLACES`](https://forth-standard.org/standard/string/REPLACES) [`SUBSTITUTE`](https://forth-standard.org/standard/string/SUBSTITUTE)
TOOLS | [`.S`](https://forth-standard.org/standard/tools/DotS) [`?`](https://forth-standard.org/standard/tools/q) [`DUMP`](https://forth-standard.org/standard/tools/DUMP) [`SEE`](https://forth-standard.org/standard/tools/SEE)[^partial] [`WORDS`](https://forth-standard.org/standard/tools/WORDS) |
TOOLS-EXT | [`AHEAD`](https://forth-standard.org/standard/tools/AHEAD) [`BYE`](https://forth-standard.org/standard/tools/BYE) [`CS-PICK`](https://forth-standard.org/standard/tools/CS-PICK) [`CS-ROLL`](https://forth-standard.org/standard/tools/CS-ROLL) [`N>R`](https://forth-standard.org/standard/tools/NtoR) [`NAME>COMPILE`](https://forth-standard.org/standard/tools/NAMEtoCOMPILE) [`NAME>INTERPRET`](https://forth-standard.org/standard/tools/NAMEtoINTERPRET) [`NAME>STRING`](https://forth-standard.org/standard/tools/NAMEtoSTRING) [`NR>`](https://forth-standard.org/standard/tools/NR>) [`TRAVERSE-WORDLIST`](https://forth-standard.org/standard/tools/TRAVERSE-WORDLIST) [`[DEFINED]`](https://forth-standard.org/standard/tools/BracketDEFINED) [`[ELSE]`](https://forth-standard.org/standard/tools/BracketELSE) [`[IF]`](https://forth-standard.org/standard/tools/BracketIF) [`[THEN]`](https://forth-standard.org/standard/tools/BracketTHEN) [`[UNDEFINED]`](https://forth-standard.org/standard/tools/BracketUNDEFINED) | [`;CODE`](https://forth-standard.org/standard/tools/SemiCODE) [`ASSEMBLER`](https://forth-standard.org/standard/tools/ASSEMBLER) [`CODE`](https://forth-standard.org/standard/tools/CODE) [`EDITOR`](https://forth-standard.org/standard/tools/EDITOR) [`SYNONYM`](https://forth-standard.org/standard/tools/SYNONYM)
TOOLS-EXT obsolescent | | [`FORGET`](https://forth-standard.org/standard/tools/FORGET)
XCHAR | | [`X-SIZE`](https://forth-standard.org/standard/xchar/X-SIZE) [`XC!+`](https://forth-standard.org/standard/xchar/XCStorePlus) [`XC!+?`](https://forth-standard.org/standard/xchar/XCStorePlusq) [`XC,`](https://forth-standard.org/standard/xchar/XCComma) [`XC-SIZE`](https://forth-standard.org/standard/xchar/XC-SIZE) [`XC@+`](https://forth-standard.org/standard/xchar/XCFetchPlus) [`XCHAR+`](https://forth-standard.org/standard/xchar/XCHARPlus) [`XEMIT`](https://forth-standard.org/standard/xchar/XEMIT) [`XKEY`](https://forth-standard.org/standard/xchar/XKEY) [`XKEY?`](https://forth-standard.org/standard/xchar/XKEYq)
XCHAR-EXT | | [`+X/STRING`](https://forth-standard.org/standard/xchar/PlusXDivSTRING) [`-TRAILING-GARBAGE`](https://forth-standard.org/standard/xchar/MinusTRAILING-GARBAGE) [`CHAR`](https://forth-standard.org/standard/xchar/CHAR) [`EKEY>XCHAR`](https://forth-standard.org/standard/xchar/EKEYtoXCHAR) [`X-WIDTH`](https://forth-standard.org/standard/xchar/X-WIDTH) [`XC-WIDTH`](https://forth-standard.org/standard/xchar/XC-WIDTH) [`XCHAR-`](https://forth-standard.org/standard/xchar/XCHARMinus) [`XHOLD`](https://forth-standard.org/standard/xchar/XHOLD) [`X\STRING-`](https://forth-standard.org/standard/xchar/XSTRINGMinus)
Extensions | [`BACKGROUND`](doc/mega65.md#background) [`BORDER`](doc/mega65.md#border) [`FOREGROUND`](doc/mega65.md#foreground) [`LATEST` `MON`](doc/internals.md#latest) [`SAVESYSTEM`](doc/mega65.md#savesystem) [`SP@`](doc/internals.md#sp) |

[^partial]: Partial support only (so far)

# TEST STATUS

The code is getting to the point where automating the test suite makes sense.  I do want to have some way to redirect output to make extracting results simpler.

Test | Status | Comments
:-- | :--: | :--
[Preliminaries](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/prelimtest.fth) | PASS | 1 expected[^petscii] error reported.
[BLOCK](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/blocktest.fth) | TBD | Too early to attempt
[CORE](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/core.fr), [CORE plus](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/coreplustest.fth) | **FAIL** | Some [problems](#ummod) with `UM/MOD`.  17 errors reported, 9 of which are expected[^petscii].
[CORE-EXT](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/coreexttest.fth) | PASS | 4 expected[^petscii] errors reported.
[DOUBLE](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/doubletest.fth) | TBD | Too early to attempt
[EXCEPTION](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/exceptiontest.fth) | PASS | 
[FACILITY](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/facilitytest.fth) | TBD | Too early to attempt
[FILE](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/filetest.fth) | TBD | Too early to attempt
[LOCALS](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/localstest.fth) | TBD | Too early to attempt
[MEMORY](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/memorytest.fth) | TBD | Too early to attempt
[SEARCH](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/searchordertest.fth) | PASS |
[STRING](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/stringtest.fth) | TBD | Too early to attempt
[TOOLS](https://github.com/gerryjackson/forth2012-test-suite/blob/master/src/toolstest.fth) | TBD | Too early to attempt

[^petscii]: Some failures will occur because we are using PETSCII and not ASCII.  See [this section](#petscii) for details.

## Failures to look at

The cause of these failures has not yet been determined.

From core plus:
```
INCORRECT RESULT: t[ 0 max-uint 0 ustep gd8 -> 256 ]t
INCORRECT RESULT: t[ 0 0 max-uint -ustep gd8 -> 256 ]t
INCORRECT RESULT: t[ 0 max-int min-int step gd8 -> 256 ]t
INCORRECT RESULT: t[ 0 min-int max-int -step gd8 -> 256 ]t
INCORRECT RESULT: t[ 0 0 0 ustep +uwrap? 256 gd9
INCORRECT RESULT: t[ 0 -max-int negate -max-int over gd8 -2> 2 ]t
INCORRECT RESULT: t[ 0 min-int 1+ 1 min-int gd8 -> 2 ]t
```

From string (looks like PETSCII things?):
```
INCORRECT RESULT: t[ s11 s12 compare -> 1 ]t
INCORRECT RESULT: t[ s12 s11 compare -> -1 ]t
```

## PETSCII

Because we are using the PETSCII character set and not ASCII some tests are expected to fail.  I'm not considering these as errors.

From preliminaries:
```
error #47: testing [CHAR]
```

From core:
```
INCORRECT RESULT: t[ char X -> 58 ]t
INCORRECT RESULT: t[ char HELLO -> 48 ]t
INCORRECT RESULT: t[ gc1 -> 58 ]t
INCORRECT RESULT: t[ gc2 -> 48 ]t
INCORRECT RESULT: t[ gc3 -> 58 ]t
INCORRECT RESULT: t[ gc4 drop dup c@ swap char+ c@ -> 58 59 ]t
INCORRECT RESULT: t[ gp1 -> <true> ]t
```

From core plus:
```
INCORRECT RESULT: t[ 'z' -> 122 ]t
INCORRECT RESULT: t[ 'z' -> 7a ]t
```

From core extensions:
```
INCORRECT RESULT: t[ ssq3 drop  1 chars + c@ ->   8 ]t \ \b BS Backspace
INCORRECT RESULT: t[ ssq3 drop  3 chars + c@ ->  12 ]t \ \f FF Form feed
INCORRECT RESULT: t[ ssq3 drop 14 chars + c@ ->  97 ]t \ a  a  Hex follow on
INCORRECT RESULT: t[ ssq3 drop 16 chars + c@ -> 120 ]t \ x  x  Non hex follow on
```

## `UM/MOD`

```
INCORRECT RESULT: t[ max-uint max-uint um* max-uint um/mod -> 0 max-uint ]t
```

# CREDITS

- Lots of the implementation comes from [FIG FORTH release 1.1](https://github.com/ptorric/figforth) (public domain).
- The implementations of some of the words comes from the [reference implementations](https://forth-standard.org/standard/implement).  These are [being released](https://forth-standard.org/proposals/licence-to-use-reference-implementations#contribution-158) under a [CC0 license](https://creativecommons.org/publicdomain/zero/1.0/).
- [Test suite](https://github.com/gerryjackson/forth2012-test-suite).
