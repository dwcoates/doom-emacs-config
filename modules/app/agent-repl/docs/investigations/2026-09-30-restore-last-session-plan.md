# Restore the directory's last session at bring-up — plan (2026-09-30)

Written before a context compaction. Read this, then implement it on the
`seamless-restarts` branch (worktree `~/.config/doom-worktrees/seamless-restarts`)
without asking: the owner approved the design. No commits to `master`, no
deploys, no restarts of any running system: land nothing until the owner says.

## Where things stand

- Branch `seamless-restarts` carries 23 commits not on `master`, all green
  (daemon unit + integration, shim unit + integration + typecheck + lint,
  sidecar `-race`, webapp unit + integration + webkit + typecheck + lint, all
  ERT, proto/store/lock/logging/bin harnesses, and the e2e suite). See
  `docs/REMEDIATION-CHANGELOG.md`'s 2026-09-30 lines for what each fix is.
- Branch `merge-queue/spawn-argv-from-config-landing` (no worktree) is fully
  covered: its fixes are on `master` or cherry-picked onto `seamless-restarts`.
  It may be deleted once `seamless-restarts` lands.
- The owner's landing rule right now is cherry-pick onto `master` (the merge
  queue is suspended while another workspace fixes it); the metaprompt's
  merge-queue rule stays as written.

## The defect

When a workspace comes up and its session record names a conversation whose
transcript is not on disk, `claude-repld.internal.workspace.Fleet.classifySource()`
(`daemon/internal/workspace/sessions.go`, the "the recorded conversation has no
transcript on disk; the session comes up FRESH" branch) starts a FRESH session.
On 2026-09-30 that lost the master workspace's whole conversation after a
restart: the record named a stale vendor id (fixed on this branch by recording
every rotation, commit `4a360b65f`), and the conversation's real transcript sat
in the workspace's own directory, unused.

The owner's ruling: the fix is to restore the directory's last session, not to
block. Blocking is NOT the design.

## The fix

1. **A missing recorded transcript adopts the directory's newest transcript.**
   - In `classifySource`, the missing-transcript branch for an ENGAGED workspace
     (the `neverEngaged` branch stays as it is) runs the same adoption the
     no-record branch runs: `Fleet.adoptOrFresh()` (same file), which reads
     `Accounts.NewestTranscript(ctx, dir)`.
   - Log at INFO naming both ids: the recorded one that had no transcript and
     the adopted one.
   - Record the adopted id as the session's resume handle
     (`wsm.SetVendorSessionID`, added on this branch), so the next bring-up
     resumes it directly.
   - Extract the shared shape rather than calling `adoptOrFresh` with a
     different log message by hand: one helper decides "adopt the newest
     transcript or come up fresh" for both callers, each passing its reason.
     Per the metaprompt, the extraction is its own behavior-preserving commit
     first, with a unit test of the helper and a guard that both branches go
     through it.
2. **The idle guard is waived when this daemon reaped the previous shim.**
   - `TranscriptAdoptionIdleWindow` (45s) refuses a transcript touched in the
     last 45s, in case another writer (an interactive `claude`) still holds it.
   - Right after a restart or a shim swap the transcript was always just
     written, so the guard would refuse the very conversation being restored.
   - When the daemon itself stopped and reaped the shim that wrote it (a
     relaunch, a restart's stand-down, a known shim death), there is no other
     writer, so the guard does not apply. Thread that fact into the bring-up
     (the relaunch and the boot both know whether they reaped the shim); do not
     guess it from timing.
   - The guard still applies when the daemon cannot account for the writer
     (no record, or a shim it never held).
3. **Come up fresh only when the directory has no transcript at all**, keeping
   today's `conversation_abandoned` fault and WARN for an engaged workspace,
   since the recorded conversation really is gone then.
4. **Held transcripts are out of scope for now** (owner, 2026-09-30: "let's not
   worry about the transcript issue for now"). Do not add a skip for
   conversations another workspace's record names.

## Tests (each its own case, table-driven, AAA)

- An engaged workspace whose recorded transcript is missing adopts the
  directory's newest transcript, logs both ids at INFO, and records the
  adopted id via `SetVendorSessionID`.
- The same with the newest transcript touched inside the idle window AFTER this
  daemon reaped the shim: adopted (the guard is waived).
- The same with the newest transcript touched inside the idle window and no
  reap by this daemon: comes up fresh, as today, with the idle-guard INFO.
- An engaged workspace with no transcript in its directory at all: comes up
  fresh with the WARN and the `conversation_abandoned` fault, as today.
- A never-engaged workspace: unchanged (fresh, INFO, no fault).
- The no-record branch: unchanged behavior through the extracted helper.
- `SetVendorSessionID` failing during the adoption: recorded at ERROR through
  the workspace's logger, the session still resumes the adopted id.
- The helper's own unit tests, and the guard test that both branches use it.

## Docs

- `daemon/AGENTS.md`: where bring-up resolution is described, state the new
  order: recorded transcript, else the directory's newest (guard waived after
  this daemon's own reap), else fresh with `conversation_abandoned`.
- One line in `docs/REMEDIATION-CHANGELOG.md`.

## Gates before reporting

Daemon `make test` and `make integration` (through `bin/background.sh`, exit
codes checked; `make test` hides a gofmt failure, so read the exit code), plus
the e2e suite (`bin/test-e2e.sh -count=1`). No CPU load generation. Then a
final duplication sweep over the whole branch diff, and report to the owner
without landing.
