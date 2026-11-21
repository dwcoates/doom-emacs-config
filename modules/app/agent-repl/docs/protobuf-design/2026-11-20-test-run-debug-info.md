# Test runs link without debug information unless asked

A test run's Go link step is its CPU floor: about 0.41 CPU-s per daemon test
binary, measured in the merge gates of 2026-11-20. Linking without DWARF
(`-ldflags=-w`) cut a warm link of every daemon test binary from 88 to 71
CPU-s and the daemon program's link from 9.6 to 4.8 CPU-s. The owner ruled
(2026-11-20) that runs link without debug information by default, with an
opt-in that keeps it, documented as a temporary debugging measure only.

## Landed changes

### `testrun.v1.TestRunRequest.debug_info`

- WHAT: a new boolean, `debug_info = 7`. When set, the run links every Go test binary and every program it builds with DWARF. When unset (the default), it links them with `-ldflags=-w`.
- WHY: dropping DWARF saves link CPU in every run. A debugger is the only consumer of DWARF, and stack traces and failure file:line come from the line table, which `-w` keeps.
- ASSUMPTION (design mode is autonomous): a bool fits, like its siblings `remeasure`, `no_reuse` and `plan_only`. It selects a build option and does not change how any sibling field is read, so the two-arm oneof rule for interpretation-selecting bools does not apply.
- CONSEQUENCES:
  - `agent-repld test run` gains `-debug-info` (`daemon/cmd/agent-repld/testcli.go`).
  - testrun's `cli.Args` carries it into `suites.Layout`, which hands it to every test build (`gobuild.Options`) and every program build (`suites.ProgramBuild`).
  - The option changes no verdict, so reuse and recording treat a debug-info run like any other run.
