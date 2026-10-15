# A merge's test suite reused from an earlier green run

The test runner now skips a system's unit and integration suite when every
input it reads (each file, the toolchain, the environment) is byte-identical to
inputs an earlier run passed whole (`testrun/internal/reuse`). The merge gate's
Tests tab has to say so: a suite that did not run in this gate is neither
running, passed in this gate, nor failed, and a reader must be able to tell it
from a pass in this gate and see which earlier run vouches for it.

## Landed changes

### `frontend.v1.FeedMergeTestSuite.state` gains `reused`

- **What.** A fourth arm, `FeedMergeTestSuiteReused reused = 7`, carrying one
  element, `FeedMergeTestSuiteReusedSource source`, whose `text` is the
  daemon-resolved name of the earlier run: when it passed, on which branch and
  commit, and "with uncommitted changes" when that commit does not hold the
  passed bytes.
- **Why.** Approved by the parent workspace for the owner's suite-reuse design
  (Option E of the test-slimming program): a reused suite draws green in the
  feed and names its source, so a gate never reads as having run something it
  did not.
- **Consequences.**
  - The suite's `counts` stay unset for a reused suite: none of its tests ran
    in this gate.
  - The daemon reads the runner's line
    `<suite>: reused a green run (inputs <key12>, passed <time> on <source>)`
    (`daemon/internal/merge/testgate.go`), and the source text is the part
    after `passed`.
  - The webapp's Tests tab draws the arm (`webapp/src/feed/merge/tests-tab.ts`);
    its `default: unreachableArm` would otherwise throw on the new arm, so the
    webapp draws it before the daemon emits it.
  - **The footer draws no edge for a reused suite** (owner ruling via the
    parent, 2026-10-14): nothing started or ended, so
    `frontend.v1.FooterMergeStepSuite.edge` gains no arm.
  - A reused suite drawing green in the feed is a UX change, announced to the
    owner by the parent before it lands.

## Alternatives that lost

- **Folding reuse into `passed`.** Rejected: a reader could not tell a pass in
  this gate from a pass carried over, and the source run would have nowhere to
  live.
- **A footer `reused` edge.** Rejected by the owner: an edge marks a suite
  starting or ending, and a reused suite does neither.
