# Owner 6 (B17–18: detached indicator, link severed/recovered) — stand-down state

Reconstructed by the lead from git at the 2026-09-09 stand-down; the owner
left no STATE.md. Seven commits ahead of overhaul/integration after the lead's
WIP commit.

## Landed on this branch
- shim/fake: a detached run outlives its turn long enough to be caught (a79cf3a7a,
  5e9492ae2, 0ab8e4ade).
- daemon/workspace: a shim that is gone is not the workspace's client (160145f17), with
  an integration test that a prompt revives a workspace whose shim was killed.
- playtest: the tab says work is live, and says the route is gone (1689ca6eb).

## WIP (lead-committed, unreviewed, unrun)
- e2e/playtest_capture_test.go: settle on a 50ms UNCHANGED WINDOW instead of two agreeing
  reads, with the measured torn captures documented in the comment. This is the THIRD
  independent fix to capture settling (owner 7: rAF paint gate; owner 1: redisplay between
  reads). The lead reconciles all three into one substrate change before owners rerun.
