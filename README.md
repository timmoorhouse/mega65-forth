
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
- File input is partially there.  We can include a file, but nested includes won't work yet (we need a stack of parse states).
- Words can be defined (`:`, `;`, `CREATE`, `VARIABLE`, `CONSTANT`, etc are functional).  There isn't much support for control structures yet (conditionals, loops, etc) so these aren't particularly useful yet.

File input and defining words are the current priorities.  Once these are in resonable shape, we can start running unit tests and do most of the remainder of the implementation in Forth.

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
- Basic file reading (just enough to get by for now)
  - [x] `OPEN-FILE`
  - [x] `CLOSE-FILE`
  - [x] `READ-LINE`
  - [x] `INCLUDE`
  - [x] `INCLUDED`
  - [x] `WRITE-FILE`
  - [x] `SAVESYSTEM` (gforth extension)
  - [x] Automatic selection of appropriate channel numbers and secondary addresses (first address will be hardcoded as 8 for now)
  - [x] Automatic selection of disk buffer
  - [ ] Handle file access modes (skip for now?)
  - [ ] `REQUIRE`, `REQUIRED` (skip for now?)
  - [ ] Fix I/O status handling (when is status from READSS reset?) (skip for now?)
  - [ ] Fix handling of I/O error cases (skip for now?)
- Defining words
  - [x] `,`
  - [x] `:`
  - [x] `;`
  - [x] `CREATE`
  - [x] `COMPILE,`
  - [x] `POSTPONE`
  - [x] Changes to `EVALUATE` for compilation state
  - [x] Get the alignment right in `CREATE`
  - [x] Get the alignment right for builtin words
  - [ ] Honour the hidden flag in `SEARCH-NAMELIST`
  - [x] Honour the hidden flag in `WORDS`
- Implement more of the basic wordset
  - [ ] `AHEAD`, `CS-PICK`, `CS-ROLL` (written but untested)
  - [ ] `IF`, `ELSE`, `THEN`
  - [ ] `BEGIN`, `AGAIN`
  - [ ] `BEGIN`, `WHILE`, `REPEAT`
  - [ ] `DO`, `LOOP`
  - [ ] `DO`, `+LOOP`
  - [ ] `LEAVE`
  - [ ] `UNLOOP`, `EXIT`
  - [ ] `I`, `J`
  - [x] `IMMEDIATE`
  - [ ] `RECURSE` (skip for now?)
  - [x] `VARIABLE`
  - [x] `CONSTANT`
  - [x] `.(`
- Parse state stack
  - [ ] `SAVE-INPUT`, `RESTORE-INPUT` (in progress)
    - I think these will just need one word (1 byte fileid, 1 byte `>IN`)
  - [ ] Update to `EVALUATE` (in progress)
  - [ ] Update to `INCLUDE-FILE` (in progress)
  - [ ] Sort out the buffer to use in `QUIT`
  - [ ] `REFILL`
  - [ ] Get rid of `TIB`, `TIB#` uses
  - [ ] Get `SOURCE-ID` and `SOURCE` right during all of this
- Bootstrapping with portions of the dictionary written in Forth
  - [ ] A "skeletal" configuration with just the builtins
  - [ ] A "minimal" configuration
  - [ ] A "complete" configuration
  - [ ] Move things we can from assembler to Forth
- Making `SAVESYSTEM` deterministic? (it might not make sense to do this)
  - [ ] Move basepage to top of memory?
  - [ ] What to do about DMA lists?
- Tests
  - [ ] [Test suite](https://github.com/gerryjackson/forth2012-test-suite)
    - Needed for prelimtest
      - [x] Case insensitive word names (map to lower case when defining/resolving)
      - [ ] `[CHAR]`
      - [ ] `IF`, `ELSE`, `THEN`
      - [ ] `DO`, `LEAVE`, `LOOP`
      - [ ] `FIND` (I've just been using `PARSE`, `PARSE-NAME`)
  - [ ] Some reasonable way of capturing test results 
    - It might make sense to have a way to redirect `EMIT` to a file (this would need to persist across an `ABORT` though, so may need a compilation option) 
- Benchmarks
  - [ ] Compiling the dictionary?
  - [ ] Something without I/O?
  - [ ] A naive fibonnaci might be nice for inner interpreter performance

There'll be lots more to do after that.

# CREDITS

- Lots of the implementation comes from [FIG FORTH release 1.1](https://github.com/ptorric/figforth)
- [Test suite](https://github.com/gerryjackson/forth2012-test-suite)
