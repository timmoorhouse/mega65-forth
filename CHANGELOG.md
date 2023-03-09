
v0.2 alpha TBD
- Add `COMPILE-ONLY`.
- Catch exceptions in `AUTOBOOT`.
- Embed the bootstrap Forth code in forth-skeletal and parse out of memory instead of reading from a file.  This will allow `INCLUDE` and others to be implemented in Forth.
- Turn `AUTOBOOT` into a deferred, so what happens automatically at startup can be customized (by default this is a no-op - should it be something like `:NONAME INCLUDE AUTOBOOT.F ;` ?)
- Improve error reporting.  The source line number, line contents and parse position are displayed along with (if available) a message for the exception number.
- Added value `UNIT` used by `OPEN-FILE` as the unit number.  Defaults to 8.
- CORE: add `ENVIRONMENT?` and `MARKER`.  CORE is now feature complete (with the exception of the obsolescent `[COMPILE]`).
- SEARCH: add `ALSO`, `DEFINITIONS`, `FORTH`, `GET-CURRENT`, `GET-ORDER`, `ONLY`, `ORDER`, `PREVIOUS`, `SET-CURRENT` and `SET-ORDER`.  SEARCH is now feature complete.
- STRING: add `UNESCAPE`.

v0.1 pre-alpha 2023-03-04
- Initial version
