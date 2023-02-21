


## BACKGROUND (c --)

Sets the background colour to an index (0-255) in the colour palette.  This is equivalent to the BASIC command `BACKGROUND`.

## BORDER (c --)

Sets the border colour to an index (0-255) in the colour palette.  This is equivalent to the BASIC command `BORDER`.

## FOREGROUND (c --)

Sets the foreground colour to an index (0-255) in the colour palette.  This is equivalent to the BASIC command `FOREGROUND`.


## MON (--)

Enter the system monitor.

Note that exiting the monitor will (currently) exit to BASIC instead of returning to Forth.  This should be fixed eventually.

## SAVESYSTEM ("<spaces>filename" --)

Write the current dictionary to a file on drive 8.

Currently you need to append ,w,p to the filename.  Eventually this should not be necessary.

```
savesystem new-forth,p,w
```



