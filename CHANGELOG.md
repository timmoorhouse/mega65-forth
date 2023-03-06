
v0.2 alpha TBD
- Add `COMPILE-ONLY`.
- Catch exceptions in `autoboot.f`.
- Turn `AUTOBOOT` into a deferred, so what happens automatically at startup can be customized (by default this is a no-op - should this be left as `include autoboot.f`?)
- Improve error reporting.  The source line number, line contents and parse position are displayed along with (if available) a message for the exception number.
- CORE: add `ENVIRONMENT?`.
- SEARCH: add `GET-CURRENT`, `SET-CURRENT`.
- STRING: add `UNESCAPE`.

v0.1 pre-alpha 2023-03-04
- Initial version
