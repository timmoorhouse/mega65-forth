
# MEGA65 Forth

An attempt at FORTH for the MEGA65.  This is heavily influenced by FIG FORTH release 1.1 (see https://github.com/ptorric/figforth)

                    This publication has been made available 
                    by the Forth Interest Group, 
                    P. O. box 1105,  San Carlos, CA 94070

This is *very* much in the early stages of developement:
- The inner interpreter is pretty much working, so I can execute words that are hardcoded in.
- The outer interpreter is almost entirely missing, but is the next priority.  So, no keyboard or file input yet.

An attempt will be made to:
- Be more ANS FORTH (94) compliant, and hopefully Forth 2012
- Provide access to MEGA65 features

RANDOM TODOs
- get monitor working via brk and mon/monitor word
- https://github.com/gerryjackson/forth2012-test-suite
- get rid of the user area
- switch everything to petscii? or ascii?
  - ascii would have the advantage of simplifying sharing source files
- some sort of RPICK (like PICK but for the return stack) for J, I, LEAVE, PLOOP, PPLOOP
- can we use +/- with ZBRANCH, BRANCH?

A.3.2.3.2

 AHEAD, CS-ROLL, CS-PICK?