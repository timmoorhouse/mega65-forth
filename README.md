
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

# BUILDING

## Requirements
- Acme from https://sourceforge.net/projects/acme-crossass/  I was using the one from https://github.com/MEGA65/acme but switched to the svn version to get access to some of the more recent features, but I'm not certain I ended up using them.  It could be that the https://github.com/MEGA65/acme one will still work.

## Recommendations
- Vice for c1541 to manipulate d81 images
- m65, mega65_ftp from mega65-tools
- m65dbg
- xemu


# RANDOM TODOs
- Should we make use of the kernel?  So far I've been keeping things completely independent of the kernel so we'll be able to bank it out (though that's not currently being done).
- get monitor working via brk and mon/monitor word
- https://github.com/gerryjackson/forth2012-test-suite
- get rid of the user area
- Some sort of RPICK (like PICK but for the return stack) for J, I, LEAVE, PLOOP, PPLOOP

A.3.2.3.2

 AHEAD, CS-ROLL, CS-PICK?