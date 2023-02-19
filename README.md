
# MEGA65 Forth

An attempt at [Forth](https://forth-standard.org/standard/words) for the [MEGA65](https://mega65.org/).  This is heavily influenced by [FIG FORTH release 1.1](https://github.com/ptorric/figforth)

```
This publication has been made available by the 
Forth Interest Group, P. O. box 1105, San Carlos, CA 94070
```

![screenshot](doc/screenshot.png)

Note that we have actually switched to font A (the ASCII font).  The `\` word shows up correctly on screen but the screenshot from m65 assumes the default font.

This is *very* much in the early stages of developement:

- The interpreter is working well enough to support development.  It needs better error handling, etc.
- File input is mostly functional but slow.  Nested includes should work so long as the `INCLUDE` appears on a line by itself with the filename (`RESTORE-INPUT` into the middle of a line isn't working yet).
- Words can be defined (`:`, `;`, `CREATE`, `VARIABLE`, `CONSTANT`, etc are functional).  There isn't much support for control structures yet (conditionals, loops, etc) so these aren't particularly useful yet.

Control structures (conditionals, loops, etc) are the current priority.  Once these are in resonable shape, we can start running unit tests and do most of the remainder of the implementation in Forth.

My apologies if this isn't the greatest Forth implementation.  I don't really have much experience with Forth.  I'm doing this because I've always been intrigued by Forth, and with a new MEGA65 sitting on the dining room table, it seemed the perfect project to learn about Forth, shake decades of dust off my 6502 programming skills, and learn about some of the MEGA65-specific features.

An attempt will be made to:

- Be more ANS FORTH (94) compliant, and hopefully [Forth 2012](http://www.forth200x.org/documents/forth-2012.pdf)
- Provide access to MEGA65 features

# BUILDING

## Requirements

- [Acme](https://sourceforge.net/projects/acme-crossass):  I was using the one from [MEGA65 github](https://github.com/MEGA65/acme) but switched to the svn version to get access to some of the more recent features, but I'm not certain I ended up using them.  It could be that the MEGA65 one will still work.

There is a build script in `bin/build.sh` that can be used.  Right now it doesn't do a huge amount, but eventually I expect
that the build process will get more involved:

- Compiling the builtins
- Use a MEGA65 or xemu to compile the rest of the standard dictionary, possibly for multiple configurations (a minimal set, a complete set, etc)
- Run a test suite
- Run benchmarks
- Building tools (an assembler word set? an editor?)
- Assembling a disk image with various build configuration, source code, examples, tools

## Recommendations

- [Vice](https://vice-emu.sourceforge.io/) for c1541 and petcat to manipulate d81 images
- m65, mega65_ftp from [mega65-tools](https://github.com/MEGA65/mega65-tools)
- [m65dbg](https://github.com/MEGA65/m65dbg)
- [xemu](https://github.com/lgblgblgb/xemu)

# RANDOM TODOs

These should get us to the point of bootstrapping with a dictionary written in forth and running unit tests:
- Miscellaneous loose ends (these can likely be skipped for now)
  - [ ] Handle file access modes
  - [ ] Fix I/O status handling (when is status from READSS reset?)
  - [ ] Fix handling of I/O error cases
  - [ ] Honour the hidden flag in `SEARCH-NAMELIST`
  - [ ] Handle `RESTORE-INPUT` failures
- Implement more of the basic wordset
  - [ ] `DO`, `LOOP`
  - [ ] `DO`, `+LOOP`
  - [ ] `LEAVE`
  - [ ] `UNLOOP`, `EXIT`
  - [ ] `I`, `J`
- Bootstrapping with portions of the dictionary written in Forth
  - [ ] A "skeletal" configuration with just the builtins
  - [ ] A "minimal" configuration
  - [ ] A "complete" configuration
  - [ ] Move things we can from assembler to Forth
- Making `SAVESYSTEM` output deterministic? (it might not make sense to do this)
  - [ ] Move basepage to top of memory?
  - [ ] What to do about DMA lists?
- Tests
  - [ ] [Test suite](https://github.com/gerryjackson/forth2012-test-suite)
    - Needed for prelimtest
      - [ ] `[CHAR]`
      - [ ] `DO`, `LEAVE`, `LOOP`
      - [ ] `FIND` (I've just been using `PARSE`, `PARSE-NAME`)
  - [ ] Some reasonable way of capturing test results 
    - It might make sense to have a way to redirect `EMIT` to a file (this would need to persist across an `ABORT` though, so may need a compilation option) 
- Benchmarks
  - [ ] Compiling the dictionary?
  - [ ] Something without I/O?
  - [ ] A naive fibonnaci might be nice for inner interpreter performance

There'll be lots more to do after that.

# STATUS

Note that some of these are implemented in Forth and the bootstrap process is not yet automated.

Wordset | Implemented | Not (Yet?) Implemented
-- | -- | --
BLOCK | | [`BLK`](https://forth-standard.org/standard/block/BLK) [`BLOCK`](https://forth-standard.org/standard/block/BLOCK) [`BUFFER`](https://forth-standard.org/standard/block/BUFFER) [`FLUSH`](https://forth-standard.org/standard/block/FLUSH) [`LOAD`](https://forth-standard.org/standard/block/LOAD) [`SAVE-BUFFERS`](https://forth-standard.org/standard/block/SAVE-BUFFERS) [`UPDATE`](https://forth-standard.org/standard/block/UPDATE)
BLOCK-EXT | | [`EMPTY-BUFFERS`](https://forth-standard.org/standard/block/EMPTY-BUFFERS) [`LIST`](https://forth-standard.org/standard/block/LIST) [`SCR`](https://forth-standard.org/standard/block/SCR) [`THRU`](https://forth-standard.org/standard/block/THRU)
CORE | [`!`](https://forth-standard.org/standard/core/Store) [`(`](https://forth-standard.org/standard/core/p) [`*`](https://forth-standard.org/standard/core/Times) [`+`](https://forth-standard.org/standard/core/Plus) [`+!`](https://forth-standard.org/standard/core/PlusStore) [`,`](https://forth-standard.org/standard/core/Comma) [`-`](https://forth-standard.org/standard/core/Minus) [`.`](https://forth-standard.org/standard/core/d)[^partial] [`/`](https://forth-standard.org/standard/core/Div) [`0<`](https://forth-standard.org/standard/core/Zeroless) [`0=`](https://forth-standard.org/standard/core/ZeroEqual) [`1+`](https://forth-standard.org/standard/core/OnePlus) [`1-`](https://forth-standard.org/standard/core/OneMinus) [`2*`](https://forth-standard.org/standard/core/TwoTimes) [`2/`](https://forth-standard.org/standard/core/TwoDiv) [`2DROP`](https://forth-standard.org/standard/core/TwoDROP) [`2DUP`](https://forth-standard.org/standard/core/TwoDUP) [`:`](https://forth-standard.org/standard/core/Colon) [`;`](https://forth-standard.org/standard/core/Semi) [`<`](https://forth-standard.org/standard/core/less) [`=`](https://forth-standard.org/standard/core/Equal) [`>`](https://forth-standard.org/standard/core/more) [`>BODY`](https://forth-standard.org/standard/core/toBODY) [`>IN`](https://forth-standard.org/standard/core/toIN) [`>NUMBER`](https://forth-standard.org/standard/core/toNUMBER) [`>R`](https://forth-standard.org/standard/core/toR) [`?DUP`](https://forth-standard.org/standard/core/qDUP) [`@`](https://forth-standard.org/standard/core/Fetch) [`ABORT`](https://forth-standard.org/standard/core/ABORT) [`ABS`](https://forth-standard.org/standard/core/ABS) [`ACCEPT`](https://forth-standard.org/standard/core/ACCEPT) [`ALIGN`](https://forth-standard.org/standard/core/ALIGN) [`ALIGNED`](https://forth-standard.org/standard/core/ALIGNED) [`ALLOT`](https://forth-standard.org/standard/core/ALLOT) [`AND`](https://forth-standard.org/standard/core/AND) [`BASE`](https://forth-standard.org/standard/core/BASE) [`BEGIN`](https://forth-standard.org/standard/core/BEGIN) [`BL`](https://forth-standard.org/standard/core/BL) [`C!`](https://forth-standard.org/standard/core/CStore) [`C,`](https://forth-standard.org/standard/core/CComma) [`C@`](https://forth-standard.org/standard/core/CFetch) [`CELL+`](https://forth-standard.org/standard/core/CELLPlus) [`CELLS`](https://forth-standard.org/standard/core/CELLS) [`CHAR`](https://forth-standard.org/standard/core/CHAR) [`CHAR+`](https://forth-standard.org/standard/core/CHARPlus) [`CHARS`](https://forth-standard.org/standard/core/CHARS) [`CONSTANT`](https://forth-standard.org/standard/core/CONSTANT) [`COUNT`](https://forth-standard.org/standard/core/COUNT) [`CR`](https://forth-standard.org/standard/core/CR) [`CREATE`](https://forth-standard.org/standard/core/CREATE) [`DECIMAL`](https://forth-standard.org/standard/core/DECIMAL) [`DEPTH`](https://forth-standard.org/standard/core/DEPTH) [`DROP`](https://forth-standard.org/standard/core/DROP) [`DUP`](https://forth-standard.org/standard/core/DUP) [`ELSE`](https://forth-standard.org/standard/core/ELSE) [`EMIT`](https://forth-standard.org/standard/core/EMIT) [`EVALUATE`](https://forth-standard.org/standard/core/EVALUATE) [`EXECUTE`](https://forth-standard.org/standard/core/EXECUTE) [`FILL`](https://forth-standard.org/standard/core/FILL) [`HERE`](https://forth-standard.org/standard/core/HERE) [`I`](https://forth-standard.org/standard/core/I) [`IF`](https://forth-standard.org/standard/core/IF) [`IMMEDIATE`](https://forth-standard.org/standard/core/IMMEDIATE) [`INVERT`](https://forth-standard.org/standard/core/INVERT) [`KEY`](https://forth-standard.org/standard/core/KEY) [`LEAVE`](https://forth-standard.org/standard/core/LEAVE) [`M*`](https://forth-standard.org/standard/core/M*) [`MAX`](https://forth-standard.org/standard/core/MAX) [`MIN`](https://forth-standard.org/standard/core/MIN) [`NEGATE`](https://forth-standard.org/standard/core/NEGATE) [`OR`](https://forth-standard.org/standard/core/OR) [`OVER`](https://forth-standard.org/standard/core/OVER) [`POSTPONE`](https://forth-standard.org/standard/core/POSTPONE) [`QUIT`](https://forth-standard.org/standard/core/QUIT) [`R>`](https://forth-standard.org/standard/core/R>) [`R@`](https://forth-standard.org/standard/core/R@) [`REPEAT`](https://forth-standard.org/standard/core/REPEAT) [`ROT`](https://forth-standard.org/standard/core/ROT) [`S>D`](https://forth-standard.org/standard/core/S>D) [`SOURCE`](https://forth-standard.org/standard/core/SOURCE) [`SPACE`](https://forth-standard.org/standard/core/SPACE) [`SPACES`](https://forth-standard.org/standard/core/SPACES) [`STATE`](https://forth-standard.org/standard/core/STATE) [`SWAP`](https://forth-standard.org/standard/core/SWAP) [`THEN`](https://forth-standard.org/standard/core/THEN) [`TYPE`](https://forth-standard.org/standard/core/TYPE) [`UM*`](https://forth-standard.org/standard/core/UM*) [`UNTIL`](https://forth-standard.org/standard/core/UNTIL) [`VARIABLE`](https://forth-standard.org/standard/core/VARIABLE) [`WHILE`](https://forth-standard.org/standard/core/WHILE) [`XOR`](https://forth-standard.org/standard/core/XOR) [`[`](https://forth-standard.org/standard/core/[) [`]`](https://forth-standard.org/standard/core/]) | [`#`](https://forth-standard.org/standard/core/num) [`#>`](https://forth-standard.org/standard/core/num-end) [`#S`](https://forth-standard.org/standard/core/numS) [`'`](https://forth-standard.org/standard/core/Tick) [`*/`](https://forth-standard.org/standard/core/TimesDiv) [`*/MOD`](https://forth-standard.org/standard/core/TimesDivMOD) [`+LOOP`](https://forth-standard.org/standard/core/+LOOP) [`."`](https://forth-standard.org/standard/core/.") [`/MOD`](https://forth-standard.org/standard/core//MOD) [`2!`](https://forth-standard.org/standard/core/2!) [`2@`](https://forth-standard.org/standard/core/2@) [`2OVER`](https://forth-standard.org/standard/core/2OVER) [`2SWAP`](https://forth-standard.org/standard/core/2SWAP) [`<#`](https://forth-standard.org/standard/core/<#) [`ABORT"`](https://forth-standard.org/standard/core/ABORT") [`DO`](https://forth-standard.org/standard/core/DO) [`DOES>`](https://forth-standard.org/standard/core/DOES>) [`ENVIRONMENT?`](https://forth-standard.org/standard/core/ENVIRONMENT?) [`EXIT`](https://forth-standard.org/standard/core/EXIT) [`FIND`](https://forth-standard.org/standard/core/FIND) [`FM/MOD`](https://forth-standard.org/standard/core/FM/MOD) [`HOLD`](https://forth-standard.org/standard/core/HOLD) [`J`](https://forth-standard.org/standard/core/J) [`LITERAL`](https://forth-standard.org/standard/core/LITERAL) [`LOOP`](https://forth-standard.org/standard/core/LOOP) [`LSHIFT`](https://forth-standard.org/standard/core/LSHIFT) [`MOD`](https://forth-standard.org/standard/core/MOD) [`MOVE`](https://forth-standard.org/standard/core/MOVE) [`RECURSE`](https://forth-standard.org/standard/core/RECURSE) [`RSHIFT`](https://forth-standard.org/standard/core/RSHIFT) [`S"`](https://forth-standard.org/standard/core/S") [`SIGN`](https://forth-standard.org/standard/core/SIGN) [`SM/REM`](https://forth-standard.org/standard/core/SM/REM) [`U.`](https://forth-standard.org/standard/core/U.) [`U<`](https://forth-standard.org/standard/core/U<) [`UM/MOD`](https://forth-standard.org/standard/core/UM/MOD) [`UNLOOP`](https://forth-standard.org/standard/core/UNLOOP)  [`WORD`](https://forth-standard.org/standard/core/WORD) [`[']`](https://forth-standard.org/standard/core/[']) [`[CHAR]`](https://forth-standard.org/standard/core/[CHAR])
CORE-EXT | [`.(`](https://forth-standard.org/standard/core/) [`0<>`](https://forth-standard.org/standard/core/0<>) [`2>R`](https://forth-standard.org/standard/core/2>R) [`2R>`](https://forth-standard.org/standard/core/2R>) [`2R@`](https://forth-standard.org/standard/core/2R@) [`<>`](https://forth-standard.org/standard/core/<>) [`AGAIN`](https://forth-standard.org/standard/core/AGAIN) [`COMPILE,`](https://forth-standard.org/standard/core/COMPILE,) [`FALSE`](https://forth-standard.org/standard/core/FALSE) [`HEX`](https://forth-standard.org/standard/core/HEX) [`NIP`](https://forth-standard.org/standard/core/NIP) [`PAD`](https://forth-standard.org/standard/core/PAD) [`PARSE`](https://forth-standard.org/standard/core/PARSE) [`PARSE-NAME`](https://forth-standard.org/standard/core/PARSE-NAME) [`PICK`](https://forth-standard.org/standard/core/PICK) [`REFILL`](https://forth-standard.org/standard/core/REFILL) [`RESTORE-INPUT`](https://forth-standard.org/standard/core/RESTORE-INPUT) [`ROLL`](https://forth-standard.org/standard/core/ROLL) [`SAVE-INPUT`](https://forth-standard.org/standard/core/SAVE-INPUT) [`SOURCE-ID`](https://forth-standard.org/standard/core/SOURCE-ID) [`TRUE`](https://forth-standard.org/standard/core/TRUE) [`\`](https://forth-standard.org/standard/core/) | [`.R`](https://forth-standard.org/standard/core/.R) [`0>`](https://forth-standard.org/standard/core/0>) [`:NONAME`](https://forth-standard.org/standard/core/:NONAME) [`?DO`](https://forth-standard.org/standard/core/?DO) [`ACTION-OF`](https://forth-standard.org/standard/core/ACTION-OF)  [`BUFFER:`](https://forth-standard.org/standard/core/BUFFER:) [`C"`](https://forth-standard.org/standard/core/C") [`CASE`](https://forth-standard.org/standard/core/CASE) [`DEFER`](https://forth-standard.org/standard/core/DEFER) [`DEFER!`](https://forth-standard.org/standard/core/DEFER!) [`DEFER@`](https://forth-standard.org/standard/core/DEFER@) [`ENDCASE`](https://forth-standard.org/standard/core/ENDCASE) [`ENDOF`](https://forth-standard.org/standard/core/ENDOF) [`ERASE`](https://forth-standard.org/standard/core/ERASE) [`HOLDS`](https://forth-standard.org/standard/core/HOLDS) [`IS`](https://forth-standard.org/standard/core/IS) [`MARKER`](https://forth-standard.org/standard/core/MARKER) [`OF`](https://forth-standard.org/standard/core/OF) [`S\"`](https://forth-standard.org/standard/core/S\") [`TO`](https://forth-standard.org/standard/core/TO) [`TUCK`](https://forth-standard.org/standard/core/TUCK) [`U.R`](https://forth-standard.org/standard/core/U.R) [`U>`](https://forth-standard.org/standard/core/U>) [`UNUSED`](https://forth-standard.org/standard/core/UNUSED) [`VALUE`](https://forth-standard.org/standard/core/VALUE) [`WITHIN`](https://forth-standard.org/standard/core/WITHIN)
CORE-EXT-OBSOLESCENT | | [`#TIB`](https://forth-standard.org/standard/core/#TIB) [`CONVERT`](https://forth-standard.org/standard/core/CONVERT) [`EXPECT`](https://forth-standard.org/standard/core/EXPECT) [`QUERY`](https://forth-standard.org/standard/core/QUERY) [`SPAN`](https://forth-standard.org/standard/core/SPAN) [`TIB`](https://forth-standard.org/standard/core/TIB) [`[COMPILE]`](https://forth-standard.org/standard/core/[COMPILE])
DOUBLE | [`DABS`](https://forth-standard.org/standard/double/DABS) [`DNEGATE`](https://forth-standard.org/standard/double/DNEGATE) | [`2CONSTANT`](https://forth-standard.org/standard/double/2CONSTANT) [`2LITERAL`](https://forth-standard.org/standard/double/2LITERAL) [`2VARIABLE`](https://forth-standard.org/standard/double/2VARIABLE) [`D+`](https://forth-standard.org/standard/double/D+) [`D-`](https://forth-standard.org/standard/double/D-) [`D.`](https://forth-standard.org/standard/double/D.) [`D.R`](https://forth-standard.org/standard/double/D.R) [`D0<`](https://forth-standard.org/standard/double/D0<) [`D0=`](https://forth-standard.org/standard/double/D0=) [`D2*`](https://forth-standard.org/standard/double/D2*) [`D2/`](https://forth-standard.org/standard/double/D2/) [`D<`](https://forth-standard.org/standard/double/D<) [`D=`](https://forth-standard.org/standard/double/D=) [`D>S`](https://forth-standard.org/standard/double/D>S) [`DMAX`](https://forth-standard.org/standard/double/DMAX) [`DMIN`](https://forth-standard.org/standard/double/DMIN) [`M*/`](https://forth-standard.org/standard/double/M*/) [`M+`](https://forth-standard.org/standard/double/M+)
DOUBLE-EXT | | [`2ROT`](https://forth-standard.org/standard/double/2ROT) [`2VALUE`](https://forth-standard.org/standard/double/2VALUE) [`DU<`](https://forth-standard.org/standard/double/DU<)
EXCEPTION | | [`CATCH`](https://forth-standard.org/standard/exception/CATCH) [`THROW`](https://forth-standard.org/standard/exception/THROW)
EXCEPTION-EXT | |
FACILITY | [`PAGE`](https://forth-standard.org/standard/exception/PAGE) | [`AT-XY`](https://forth-standard.org/standard/exception/AT-XY) [`KEY?`](https://forth-standard.org/standard/exception/KEY?)
FACILITY-EXT | [`K-DELETE`](https://forth-standard.org/standard/facility/K-DELETE) [`K-DOWN`](https://forth-standard.org/standard/facility/K-DOWN) [`K-F1`](https://forth-standard.org/standard/facility/K-F1) [`K-F10`](https://forth-standard.org/standard/facility/K-F10) [`K-F11`](https://forth-standard.org/standard/facility/K-F11) [`K-F12`](https://forth-standard.org/standard/facility/K-F12) [`K-F2`](https://forth-standard.org/standard/facility/K-F2) [`K-F3`](https://forth-standard.org/standard/facility/K-F3) [`K-F4`](https://forth-standard.org/standard/facility/K-F4) [`K-F5`](https://forth-standard.org/standard/facility/K-F5) [`K-F6`](https://forth-standard.org/standard/facility/K-F6) [`K-F7`](https://forth-standard.org/standard/facility/K-F7) [`K-F8`](https://forth-standard.org/standard/facility/K-F8) [`K-F9`](https://forth-standard.org/standard/facility/K-F9) [`K-HOME`](https://forth-standard.org/standard/facility/K-HOME) [`K-INSERT`](https://forth-standard.org/standard/facility/K-INSERT) [`K-LEFT`](https://forth-standard.org/standard/facility/K-LEFT) [`K-RIGHT`](https://forth-standard.org/standard/facility/K-RIGHT) [`K-UP`](https://forth-standard.org/standard/facility/K-UP) | [`+FIELD`](https://forth-standard.org/standard/facility/+FIELD) [`BEGIN-STRUCTURE`](https://forth-standard.org/standard/facility/BEGIN-STRUCTURE) [`CFIELD:`](https://forth-standard.org/standard/facility/CFIELD:) [`EKEY`](https://forth-standard.org/standard/facility/EKEY) [`EKEY>CHAR`](https://forth-standard.org/standard/facility/EKEY>CHAR) [`EKEY>FKEY`](https://forth-standard.org/standard/facility/EKEY>FKEY) [`EKEY?`](https://forth-standard.org/standard/facility/EKEY?) [`EMIT?`](https://forth-standard.org/standard/facility/EMIT?) [`END-STRUCTURE`](https://forth-standard.org/standard/facility/END-STRUCTURE) [`FIELD:`](https://forth-standard.org/standard/facility/FIELD:) [`K-ALT-MASK`](https://forth-standard.org/standard/facility/K-ALT-MASK) [`K-CTRL-MASK`](https://forth-standard.org/standard/facility/K-CTRL-MASK) [`K-END`](https://forth-standard.org/standard/facility/K-END) [`K-NEXT`](https://forth-standard.org/standard/facility/K-NEXT) [`K-PRIOR`](https://forth-standard.org/standard/facility/K-PRIOR) [`K-SHIFT-MASK`](https://forth-standard.org/standard/facility/K-SHIFT-MASK) [`MS`](https://forth-standard.org/standard/facility/MS) [`TIME&DATE`](https://forth-standard.org/standard/facility/TIME&DATE)
FILE | [`BIN`](https://forth-standard.org/standard/file/BIN) [`CLOSE-FILE`](https://forth-standard.org/standard/file/CLOSE-FILE) [`INCLUDE-FILE`](https://forth-standard.org/standard/file/INCLUDE-FILE) [`INCLUDED`](https://forth-standard.org/standard/file/INCLUDED) [`OPEN-FILE`](https://forth-standard.org/standard/file/OPEN-FILE)[^partial] [`R/O`](https://forth-standard.org/standard/file/R/O) [`R/W`](https://forth-standard.org/standard/file/R/W) [`READ-FILE`](https://forth-standard.org/standard/file/READ-FILE) [`READ-LINE`](https://forth-standard.org/standard/file/READ-LINE) [`W/O`](https://forth-standard.org/standard/file/W/O) [`WRITE-FILE`](https://forth-standard.org/standard/file/WRITE-FILE) [`WRITE-LINE`](https://forth-standard.org/standard/file/WRITE-LINE) | [`CREATE-FILE`](https://forth-standard.org/standard/file/CREATE-FILE) [`DELETE-FILE`](https://forth-standard.org/standard/file/DELETE-FILE) [`FILE-POSITION`](https://forth-standard.org/standard/file/FILE-POSITION) [`FILE-SIZE`](https://forth-standard.org/standard/file/FILE-SIZE) [`REPOSITION-FILE`](https://forth-standard.org/standard/file/REPOSITION-FILE) [`RESIZE-FILE`](https://forth-standard.org/standard/file/RESIZE-FILE)
FILE-EXT | [`INCLUDE`](https://forth-standard.org/standard/file/INCLUDE) | [`FILE-STATUS`](https://forth-standard.org/standard/file/FILE-STATUS) [`FLUSH-FILE`](https://forth-standard.org/standard/file/FLUSH-FILE) [`REQUIRE`](https://forth-standard.org/standard/file/REQUIRE) [`REQUIRED`](https://forth-standard.org/standard/file/REQUIRED)
FLOATING | | [`>FLOAT`](https://forth-standard.org/standard/floating/>FLOAT) [`D>F`](https://forth-standard.org/standard/floating/D>F) [`F!`](https://forth-standard.org/standard/floating/F!) [`F*`](https://forth-standard.org/standard/floating/F*) [`F+`](https://forth-standard.org/standard/floating/F+) [`F-`](https://forth-standard.org/standard/floating/F-) [`F/`](https://forth-standard.org/standard/floating/F/) [`F0<`](https://forth-standard.org/standard/floating/F0<) [`F0=`](https://forth-standard.org/standard/floating/F0=) [`F<`](https://forth-standard.org/standard/floating/F<) [`F>D`](https://forth-standard.org/standard/floating/F>D) [`F@`](https://forth-standard.org/standard/floating/F@) [`FALIGN`](https://forth-standard.org/standard/floating/FALIGN) [`FALIGNED`](https://forth-standard.org/standard/floating/FALIGNED) [`FCONSTANT`](https://forth-standard.org/standard/floating/FCONSTANT) [`FDEPTH`](https://forth-standard.org/standard/floating/FDEPTH) [`FDROP`](https://forth-standard.org/standard/floating/FDROP) [`FDUP`](https://forth-standard.org/standard/floating/FDUP) [`FLITERAL`](https://forth-standard.org/standard/floating/FLITERAL) [`FLOAT+`](https://forth-standard.org/standard/floating/FLOAT+) [`FLOATS`](https://forth-standard.org/standard/floating/FLOATS) [`FLOOR`](https://forth-standard.org/standard/floating/FLOOR) [`FMAX`](https://forth-standard.org/standard/floating/FMAX) [`FMIN`](https://forth-standard.org/standard/floating/FMIN) [`FNEGATE`](https://forth-standard.org/standard/floating/FNEGATE) [`FOVER`](https://forth-standard.org/standard/floating/FOVER) [`FROT`](https://forth-standard.org/standard/floating/FROT) [`FROUND`](https://forth-standard.org/standard/floating/FROUND) [`FSWAP`](https://forth-standard.org/standard/floating/FSWAP) [`FVARIABLE`](https://forth-standard.org/standard/floating/FVARIABLE) [`REPRESENT`](https://forth-standard.org/standard/floating/REPRESENT)
FLOATING-EXT | | [`DF!`](https://forth-standard.org/standard/floating/DF!) [`DF@`](https://forth-standard.org/standard/floating/DF@) [`DFALIGN`](https://forth-standard.org/standard/floating/DFALIGN) [`DFALIGNED`](https://forth-standard.org/standard/floating/DFALIGNED) [`DFFIELD`](https://forth-standard.org/standard/floating/DFFIELD) [`DFLOAT+`](https://forth-standard.org/standard/floating/DFLOAT+) [`DFLOATS`](https://forth-standard.org/standard/floating/DFLOATS) [`F**`](https://forth-standard.org/standard/floating/F**) [`F.`](https://forth-standard.org/standard/floating/F.) [`F>S`](https://forth-standard.org/standard/floating/F>S) [`FABS`](https://forth-standard.org/standard/floating/FABS) [`FACOS`](https://forth-standard.org/standard/floating/FACOS) [`FACOSH`](https://forth-standard.org/standard/floating/FACOSH) [`FALOG`](https://forth-standard.org/standard/floating/FALOG) [`FASIN`](https://forth-standard.org/standard/floating/FASIN) [`FASINH`](https://forth-standard.org/standard/floating/FASINH) [`FATAN`](https://forth-standard.org/standard/floating/FATAN) [`FATAN2`](https://forth-standard.org/standard/floating/FATAN2) [`FATANH`](https://forth-standard.org/standard/floating/FATANH) [`FCOS`](https://forth-standard.org/standard/floating/FCOS) [`FCOSH`](https://forth-standard.org/standard/floating/FCOSH) [`FE.`](https://forth-standard.org/standard/floating/FE.) [`FEXP`](https://forth-standard.org/standard/floating/FEXP) [`FEXPM1`](https://forth-standard.org/standard/floating/FEXPM1) [`FFIELD:`](https://forth-standard.org/standard/floating/FFIELD:) [`FLN`](https://forth-standard.org/standard/floating/FLN) [`FLNP1`](https://forth-standard.org/standard/floating/FLNP1) [`FLOG`](https://forth-standard.org/standard/floating/FLOG) [`FS.`](https://forth-standard.org/standard/floating/FS.) [`FSIN`](https://forth-standard.org/standard/floating/FSIN) [`FSINCOS`](https://forth-standard.org/standard/floating/FSINCOS) [`FSINH`](https://forth-standard.org/standard/floating/FSINH) [`FSQRT`](https://forth-standard.org/standard/floating/FSQRT) [`FTAN`](https://forth-standard.org/standard/floating/FTAN) [`FTANH`](https://forth-standard.org/standard/floating/FTANH) [`FTRUNC`](https://forth-standard.org/standard/floating/FTRUNC) [`FVALUE`](https://forth-standard.org/standard/floating/FVALUE) [`F~`](https://forth-standard.org/standard/floating/F~) [`PRECISION`](https://forth-standard.org/standard/floating/PRECISION) [`S>F`](https://forth-standard.org/standard/floating/S>F) [`SET-PRECISION`](https://forth-standard.org/standard/floating/SET-PRECISION) [`SF!`](https://forth-standard.org/standard/floating/SF!) [`SF@`](https://forth-standard.org/standard/floating/SF@) [`SFALIGN`](https://forth-standard.org/standard/floating/SFALIGN) [`SFALIGNED`](https://forth-standard.org/standard/floating/SFALIGNED) [`SFIELD:`](https://forth-standard.org/standard/floating/SFIELD:) [`SFLOAT+`](https://forth-standard.org/standard/floating/SFLOAT+) [`SFLOATS`](https://forth-standard.org/standard/floating/SFLOATS)
LOCAL | | [`(LOCAL)`](https://forth-standard.org/standard/local/(LOCAL)) [`TO`](https://forth-standard.org/standard/local/TO)
LOCAL-EXT | | [`{:`](https://forth-standard.org/standard/local/{:)
LOCAL-EXT-OBSOLESCENT | | [`LOCALS\|`](https://forth-standard.org/standard/local/LOCALS\|)
MEMORY | | [`ALLOCATE`](https://forth-standard.org/standard/memory/ALLOCATE) [`FREE`](https://forth-standard.org/standard/memory/FREE) [`RESIZE`](https://forth-standard.org/standard/memory/RESIZE)
MEMORY-EXT | |
SEARCH | [`FORTH-WORDLIST`](https://forth-standard.org/standard/search/FORTH-WORDLIST) [`GET-CURRENT`](https://forth-standard.org/standard/search/GET-CURRENT)[^partial] [`SEARCH-WORDLIST`](https://forth-standard.org/standard/search/SEARCH-WORDLIST) | [`DEFINITIONS`](https://forth-standard.org/standard/search/DEFINITIONS) [`GET-ORDER`](https://forth-standard.org/standard/search/GET-ORDER) [`SET-CURRENT`](https://forth-standard.org/standard/search/SET-CURRENT) [`SET-ORDER`](https://forth-standard.org/standard/search/SET-ORDER) [`WORDLIST`](https://forth-standard.org/standard/search/WORDLIST)
SEARCH-EXT | | [`ALSO`](https://forth-standard.org/standard/search/ALSO) [`FORTH`](https://forth-standard.org/standard/search/FORTH) [`ONLY`](https://forth-standard.org/standard/search/ONLY) [`ORDER`](https://forth-standard.org/standard/search/ORDER) [`PREVIOUS`](https://forth-standard.org/standard/search/PREVIOUS)
STRING | [`CMOVE`](https://forth-standard.org/standard/string/CMOVE) [`CMOVE>`](https://forth-standard.org/standard/string/CMOVE>) [`COMPARE`](https://forth-standard.org/standard/string/COMPARE) | [`-TRAILING`](https://forth-standard.org/standard/string/-TRAILING) [`/STRING`](https://forth-standard.org/standard/string//STRING) [`BLANKS`](https://forth-standard.org/standard/string/BLANKS) [`SEARCH`](https://forth-standard.org/standard/string/SEARCH) [`SLITERAL`](https://forth-standard.org/standard/string/SLITERAL)
STRING-EXT | | [`REPLACES`](https://forth-standard.org/standard/string/REPLACES) [`SUBSTITUTE`](https://forth-standard.org/standard/string/SUBSTITUTE) [`UNESCAPE`](https://forth-standard.org/standard/string/UNESCAPE)
TOOLS | [`.S`](https://forth-standard.org/standard/tools/.S)[^partial] [`WORDS`](https://forth-standard.org/standard/tools/WORDS) | [`?`](https://forth-standard.org/standard/tools/?) [`DUMP`](https://forth-standard.org/standard/tools/DUMP) [`SEE`](https://forth-standard.org/standard/tools/SEE)
TOOLS-EXT | [`AHEAD`](https://forth-standard.org/standard/tools/AHEAD) [`BYE`](https://forth-standard.org/standard/tools/BYE) [`CS-PICK`](https://forth-standard.org/standard/tools/CS-PICK) [`CS-ROLL`](https://forth-standard.org/standard/tools/CS-ROLL) [`N>R`](https://forth-standard.org/standard/tools/N>R) [`NAME>COMPILE`](https://forth-standard.org/standard/tools/NAME>COMPILE) [`NAME>INTERPRET`](https://forth-standard.org/standard/tools/NAME>INTERPRET) [`NAME>STRING`](https://forth-standard.org/standard/tools/NAME>STRING) [`NR>`](https://forth-standard.org/standard/tools/NR>) [`TRAVERSE-WORDLIST`](https://forth-standard.org/standard/tools/TRAVERSE-WORDLIST) | [`;CODE`](https://forth-standard.org/standard/tools/;CODE) [`ASSEMBLER`](https://forth-standard.org/standard/tools/ASSEMBLER) [`CODE`](https://forth-standard.org/standard/tools/CODE) [`EDITOR`](https://forth-standard.org/standard/tools/EDITOR) [`SYNONYM`](https://forth-standard.org/standard/tools/SYNONYM) [`[DEFINED]`](https://forth-standard.org/standard/tools/[DEFINED]) [`[ELSE]`](https://forth-standard.org/standard/tools/[ELSE]) [`[IF]`](https://forth-standard.org/standard/tools/[IF]) [`[THEN]`](https://forth-standard.org/standard/tools/[THEN]) [`[UNDEFINED]`](https://forth-standard.org/standard/tools/[UNDEFINED])
TOOLS-EXT-OBSOLESCENT | | [`FORGET`](https://forth-standard.org/standard/tools/FORGET)
XCHAR | | [`X-SIZE`](https://forth-standard.org/standard/xchar/X-SIZE) [`XC!+`](https://forth-standard.org/standard/xchar/XC!+) [`XC!+?`](https://forth-standard.org/standard/xchar/XC!+?) [`XC,`](https://forth-standard.org/standard/xchar/XC,) [`XC-SIZE`](https://forth-standard.org/standard/xchar/XC-SIZE) [`XC@+`](https://forth-standard.org/standard/xchar/XC@+) [`XCHAR+`](https://forth-standard.org/standard/xchar/XCHAR+) [`XEMIT`](https://forth-standard.org/standard/xchar/XEMIT) [`XKEY`](https://forth-standard.org/standard/xchar/XKEY) [`XKEY?`](https://forth-standard.org/standard/xchar/XKEY?)
XCHAR-EXT | | [`+X/STRING`](https://forth-standard.org/standard/xchar/+X/STRING) [`-TRAILING-GARBAGE`](https://forth-standard.org/standard/xchar/-TRAILING-GARBAGE) [`CHAR`](https://forth-standard.org/standard/xchar/CHAR) [`EKEY>XCHAR`](https://forth-standard.org/standard/xchar/EKEY>XCHAR) [`X-WIDTH`](https://forth-standard.org/standard/xchar/X-WIDTH) [`XC-WIDTH`](https://forth-standard.org/standard/xchar/XC-WIDTH) [`XCHAR-`](https://forth-standard.org/standard/xchar/XCHAR-) [`XHOLD`](https://forth-standard.org/standard/xchar/XHOLD) [`X\STRING`](https://forth-standard.org/standard/xchar/X\STRING)
Extensions | `BACKGROUND` `BORDER` `FOREGROUND` `LATEST` `MON` `SAVESYSTEM` `SP@` |

[^partial]: Partial support only

# CREDITS

- Lots of the implementation comes from [FIG FORTH release 1.1](https://github.com/ptorric/figforth)
- [Test suite](https://github.com/gerryjackson/forth2012-test-suite)
