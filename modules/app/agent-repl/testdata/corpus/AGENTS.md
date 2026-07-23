# testdata/corpus/

The golden corpus: anonymized REAL harness artifacts backing converter and
store tests — at least one sample of every transcript line type, system
subtype, and attachment type; every toolUseResult shape; agent sidechain
lines; workflow journal pairs; shell spools (with and without clean endings);
and captured stream probes. `MANIFEST.md` documents each fixture's
provenance.

Contract: converters must decode every fixture with zero `UnparsedEvent`s and
zero unknown-field logs. A shape gap found later becomes a fixture here FIRST,
then a fix.

Dependencies: none (static fixtures).
