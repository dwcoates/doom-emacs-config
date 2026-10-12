# Merge bubble: nested rebasing and tests tabs

## Problem

The owner wants the merge bubble's strip to carry ONE tab for rebasing and ONE
for tests, instead of a flat run of rounds (`rebasing | conflicts |
rebasing (2) | tests | fixing | tests (2) | …`).

- **Rebasing** holds a subtab per replayed commit. A commit whose replay
  needed conflict resolution carries, inside its own subtab, the agent stream
  of that resolution.
- **Tests** holds a subtab per full suite run. Each run's subtab holds two
  sub-subtabs: the run's test output, and the agent stream of the fixing work
  that followed it.

Today `frontend.v1.FeedMergeTabRebasing` carries only a replay count and
narration lines, so no per-commit subtab can be drawn from the wire, and a
conflicts tab does not name the commit it resolves. The daemon
(`daemon/internal/merge/process.go`) knows every commit's sha and subject and
which commit each conflict stopped on.

## Systems involved

- `proto/src/frontend/v1/feed.proto` (the merge tab family).
- The daemon's merge run (`daemon/internal/merge`): tab rounds, the progress
  record and its resume paths.
- The webapp's merge body (`webapp/src/feed/merge/`).

## Landed changes

_None yet: awaiting the owner's ruling on the layout._
