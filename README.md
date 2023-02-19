
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
BLOCK | | `BLK` `BLOCK` `BUFFER` `FLUSH` `LOAD` `SAVE-BUFFERS` `UPDATE`
BLOCK-EXT | | `EMPTY-BUFFERS` `LIST` `SCR` `THRU`
CORE | [`!`](https://forth-standard.org/standard/core/Store) `(` `*` `+` `+!` `,` `-` `.`[^partial] `/` `0<` `0=` `1+` `1-` `2*` `2/` `2DROP` `2DUP` `:` `;` `<` `=` `>` `>BODY` `>IN` `>NUMBER` `>R` `?DUP` `@` `ABORT` `ABS` `ACCEPT` `ALIGN` `ALIGNED` `ALLOT` `AND` `BASE` `BEGIN` `BL` `C!` `C,` `C@` `CELL+` `CELLS` `CHAR` `CHAR+` `CHARS` `CONSTANT` `COUNT` `CR` `CREATE` `DECIMAL` `DEPTH` `DROP` `DUP` `ELSE` `EMIT` `EVALUATE` `EXECUTE` `FILL` `HERE` `I` `IF` `IMMEDIATE` `INVERT` `KEY` `LEAVE` `M*` `MAX` `MIN` `NEGATE` `OR` `OVER` `POSTPONE` `QUIT` `R>` `R@` `REPEAT` `ROT` `S>D` `SOURCE` `SPACE` `SPACES` `STATE` `SWAP` `THEN` `TYPE` `UM*` `UNTIL` `VARIABLE` `WHILE` `XOR` `[` `]` | [`#`](https://forth-standard.org/standard/core/num) `#>` `#S` `'` `*/` `*/MOD` `+LOOP` `."` `/MOD` `2!` `2@` `2OVER` `2SWAP` `<#` `ABORT"` `DO` `DOES>` `ENVIRONMENT?` `EXIT` `FIND` `FM/MOD` `HOLD` `J` `LITERAL` `LOOP` `LSHIFT` `MOD` `MOVE` `RECURSE` `RSHIFT` `S"` `SIGN` `SM/REM` `U.` `U<` `UM/MOD` `UNLOOP`  `WORD` `[']` `[CHAR]`
CORE-EXT | `.(` `0<>` `2>R` `2R>` `2R@` `<>` `AGAIN` `COMPILE,` `FALSE` `HEX` `NIP` `PAD` `PARSE` `PARSE-NAME` `PICK` `REFILL` `RESTORE-INPUT` `ROLL` `SAVE-INPUT` `SOURCE-ID` `TRUE` `\` | `.R` `0>` `:NONAME` `?DO` `ACTION-OF`  `BUFFER:` `C"` `CASE` `DEFER` `DEFER!` `DEFER@` `ENDCASE` `ENDOF` `ERASE` `HOLDS` `IS` `MARKER` `OF` `S\"` `TO` `TUCK` `U.R` `U>` `UNUSED` `VALUE` `WITHIN`
CORE-EXT-OBSOLESCENT | | `#TIB` `CONVERT` `EXPECT` `QUERY` `SPAN` `TIB` `[COMPILE]`
DOUBLE | `DABS` `DNEGATE` | `2CONSTANT` `2LITERAL` `2VARIABLE` `D+` `D-` `D.` `D.R` `D0<` `D0=` `D2*` `D2/` `D<` `D=` `D>S` `DMAX` `DMIN` `M*/` `M+`
DOUBLE-EXT | | `2ROT` `2VALUE` `DU<`
EXCEPTION | | `CATCH` `THROW`
EXCEPTION-EXT | |
FACILITY | `PAGE` | `AT-XY` `KEY?`
FACILITY-EXT | `K-DELETE` `K-DOWN` `K-F1` `K-F10` `K-F11` `K-F12` `K-F2` `K-F3` `K-F4` `K-F5` `K-F6` `K-F7` `K-F8` `K-F9` `K-HOME` `K-INSERT` `K-LEFT` `K-RIGHT` `K-UP` | `+FIELD` `BEGIN-STRUCTURE` `CFIELD:` `EKEY` `EKEY>CHAR` `EKEY>FKEY` `EKEY?` `EMIT?` `END-STRUCTURE` `FIELD:` `K-ALT-MASK` `K-CTRL-MASK` `K-END` `K-NEXT` `K-PRIOR` `K-SHIFT-MASK` `MS` `TIME&DATE`
FILE | `BIN` `CLOSE-FILE` `INCLUDE-FILE` `INCLUDED` `OPEN-FILE`[^partial] `R/O` `R/W` `READ-FILE` `READ-LINE` `W/O` `WRITE-FILE` `WRITE-LINE` | `CREATE-FILE` `DELETE-FILE` `FILE-POSITION` `FILE-SIZE` `REPOSITION-FILE` `RESIZE-FILE`
FILE-EXT | `INCLUDE` | `FILE-STATUS` `FLUSH-FILE` `REQUIRE` `REQUIRED`
FLOATING | | `>FLOAT` `D>F` `F!` `F*` `F+` `F-` `F/` `F0<` `F0=` `F<` `F>D` `F@` `FALIGN` `FALIGNED` `FCONSTANT` `FDEPTH` `FDROP` `FDUP` `FLITERAL` `FLOAT+` `FLOATS` `FLOOR` `FMAX` `FMIN` `FNEGATE` `FOVER` `FROT` `FROUND` `FSWAP` `FVARIABLE` `REPRESENT`
FLOATING-EXT | | `DF!` `DF@` `DFALIGN` `DFALIGNED` `DFFIELD` `DFLOAT+` `DFLOATS` `F**` `F.` `F>S` `FABS` `FACOS` `FACOSH` `FALOG` `FASIN` `FASINH` `FATAN` `FATAN2` `FATANH` `FCOS` `FCOSH` `FE.` `FEXP` `FEXPM1` `FFIELD:` `FLN` `FLNP1` `FLOG` `FS.` `FSIN` `FSINCOS` `FSINH` `FSQRT` `FTAN` `FTANH` `FTRUNC` `FVALUE` `F~` `PRECISION` `S>F` `SET-PRECISION` `SF!` `SF@` `SFALIGN` `SFALIGNED` `SFIELD:` `SFLOAT+` `SFLOATS`
LOCAL | | `(LOCAL)` `TO`
LOCAL-EXT | | `{:`
LOCAL-EXT-OBSOLESCENT | | `LOCALS\|`
MEMORY | | `ALLOCATE` `FREE` `RESIZE`
MEMORY-EXT | |
SEARCH | `FORTH-WORDLIST` `GET-CURRENT`[^partial] `SEARCH-WORDLIST` | `DEFINITIONS` `GET-ORDER` `SET-CURRENT` `SET-ORDER` `WORDLIST`
SEARCH-EXT | | `ALSO` `FORTH` `ONLY` `ORDER` `PREVIOUS`
STRING | `CMOVE` `CMOVE>` `COMPARE` | `-TRAILING` `/STRING` `BLANKS` `SEARCH` `SLITERAL`
STRING-EXT | | `REPLACES` `SUBSTITUTE` `UNESCAPE`
TOOLS | `.S`[^partial] `WORDS` | `?` `DUMP` `SEE`
TOOLS-EXT | `AHEAD` `BYE` `CS-PICK` `CS-ROLL` `N>R` `NAME>COMPILE` `NAME>INTERPRET` `NAME>STRING` `NR>` `TRAVERSE-WORDLIST` | `;CODE` `ASSEMBLER` `CODE` `EDITOR` `SYNONYM` `[DEFINED]` `[ELSE]` `[IF]` `[THEN]` `[UNDEFINED]`
TOOLS-EXT-OBSOLESCENT | | `FORGET`
XCHAR | | `X-SIZE` `XC!+` `XC!+?` `XC,` `XC-SIZE` `XC@+` `XCHAR+` `XEMIT` `XKEY` `XKEY?`
XCHAR-EXT | | `+X/STRING` `-TRAILING-GARBAGE` `CHAR` `EKEY>XCHAR` `X-WIDTH` `XC-WIDTH` `XCHAR-` `XHOLD` `X\STRING`
Extensions | `BACKGROUND` `BORDER` `FOREGROUND` `LATEST` `MON` `SAVESYSTEM` `SP@` |

[^partial]: Partial support only

# CREDITS

- Lots of the implementation comes from [FIG FORTH release 1.1](https://github.com/ptorric/figforth)
- [Test suite](https://github.com/gerryjackson/forth2012-test-suite)
