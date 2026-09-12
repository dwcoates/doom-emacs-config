# Remediation changelog

One line per landed remediation, newest first. Kept BRIEF on purpose: it is read
back into context at every compaction, so it must stay cheap to carry.

Its job is regression watch. A perf win or an invariant established while
remediating one realtest must not be quietly undone while remediating a later
one, and this is the only artifact that spans the whole effort. Before landing
anything, scan it for a line the change would reverse.

Format: `- <area>: <what changed> (<finding>)`. One sentence. No rationale — the
commit and docs/REALTEST-JUDGEMENT-CALLS.md hold that.

## Startup and workspace sections (realtests 1-8), 2026-09-12

- daemon/roster: a repository's default branch is no longer read as a family link, so a workspace cut from it draws at the top level and priority can reorder the tab bar (realtest 8)
- daemon: every fleet adoption is dialed under the one adoption bound and records both its edges, so a revival can no longer hang forever with a prompt held and no turn recorded (realtest 7)
- elisp: the composer mounts at a height derived once per frame instead of a fraction of whatever window the mount split, so every workspace's input window is the same fixed height and stays it across remounts
- elisp/realtest: a quit arriving while a guarded section runs is delivered to the command loop afterwards, so C-g reaches the prompt the user aimed it at; a run clears pending input before it measures (DD, EE)
- daemon/elisp: registering a directory re-opens the closed row it names, and the roster stops offering repositories whose worktree is gone (AA, BB, GG)
- daemon: an inert surviving shim is no longer counted as a surviving session, removing all five cold-start harvest records (CC)
- realtest: a run forgets the workspaces it registered, via the command-file ingress (R)
- webview: the precreate hold gates a drain pass instead of each item, so losing focus mid-paint no longer strands the queue; 20.9s to under 1.5s (FF)
- realtest: a realtest reads a snapshot carrying the write-ahead log, never the owner's live database (Y)
- realtest/roster: both never-firing phase markers now fire and carry measured budgets (X)
- realtest: the sweep sequences each test's required world, so all eight can run in one invocation (T)
- daemon/elisp: departure means the boot claim is released, not the address file vanishing; fixes a restart that destroyed the daemon
- daemon: the host session identity is minted at creation, refused empty at the store, and healed in the existing rows (S)
- elisp: workspace switch and composer-landing records persist at info instead of being dropped at debug (U)
- realtest: phase budgets filled from 23 runs of manifest history; LOOSE at 3x and due for retightening (Q, V)
- logs: compact query modes (tally, sample, fields, timeline) plus stderr and Messages sources
- daemon/elisp: workspace log-sink resolution is total, and reopening a vanished directory is a named refusal (F, I, J)
- daemon/shim: the vendor guard forces a fake vendor instead of refusing the spawn (E, G, L)
- realtest: key delivery fails loudly instead of silently dropping, one shared show phase, realtest 4 bootstraps its third workspace (H, M, K)
- daemon/roster: the daemon outlives Emacs, and "loading workspaces" echoes on a hidden startup (C, D)
- wsm: the forget verb, registration's undo, reachable through the command-file ingress (B)

## Standing measurements to protect

- Cold start 1.80-1.95s from spawn to usable, every phase measured.
- Panels paint 216ms-1.5s after a single focus edge.
- Full elisp suite 4152 tests, ~36s.
