# Realtests — the mechanics

`docs/REALTEST-PLAN.md` is the CONTRACT: which realtests exist, what each one
measures, and the remediation loop they feed. A realtest that is not in it does
not run. This document is only how the machinery works, so that a reader of a
run's output can tell what was actually done and a reader of the code can tell
why it is shaped this way.

## What a realtest is not

It is not the e2e layer and it is not the retired playtest layer. Both of those
verified the module against a fixture: a bare Doom profile in a container, a
fake daemon, elisp-driven acts. A realtest has no fixture at all. It drives:

- the one `Emacs.app` process on the owner's Mac, launched the way the owner's
  Dock launches it;
- the real `~/.config/doom` on master — theme, treemacs, dashboard, personal
  bindings. The owner's environment is the FIXTURE, never a debugging target: a
  realtest that "fails" because of the owner's own configuration has found
  something about the product, because that configuration is the product's
  actual deployment;
- the real `~/.claude-emacs` state, the real store and sidecar under
  `~/.cache/agent-repl`, the daemon and shim deployed from master, and the
  owner's real `~/.claude` transcripts.

The ONE substitution is the vendor. `AGENT_REPL_FORBID_VENDOR_CALLS=1` is set on
the Emacs process and inherited by everything it spawns, so no real Claude call
can occur. The owner may lift it per test.

No picture is taken at this stage, which is why Emacs is never brought
frontmost.

## The entry point: `bin/realtest.sh`

The only supported way in, because the preflight is the part that cannot be
skipped.

```
bin/realtest.sh                                  every realtest
bin/realtest.sh -run TestRealtestStartTheEditor  one, by name

exit 0   the realtests ran
exit 77  DECLINED, and the message says why
other    a realtest failed
```

`77` is the autotools "skipped" convention, used rather than `0` so a run can
never report a green realtest that did not execute.

It refuses three things, and each refusal is there because the alternative is
worse than not running:

| refusal | why |
|---|---|
| a deployed system is not at this checkout's revision | a realtest against a stale daemon measures a build nobody has, and its findings send the owner after defects that were fixed days ago. `bin/readiness-report.sh` is the judge — the same `.source-tree` stamp comparison `bin/build-frontend.sh` rebuilds on, so a system this declines on is exactly a system a plain build will rebuild |
| an Emacs is running and `AGENT_REPL_REALTEST_TAKEOVER=1` is not set | a cold start has to quit the standing editor, and that is the owner's editor with the owner's unsaved work in it. The script does not make that decision |
| a daemon is running without the vendor guard in its environment | Emacs ADOPTS an answering daemon and never kills one, so the new Emacs would inherit it and it would spawn shims with the real SDK reachable. The refusal names the pid to stop |

The backups come BEFORE the takeover refusal. An operator who is told to set the
flag then re-runs against state that already has a copy.

Environment:

| variable | effect |
|---|---|
| `AGENT_REPL_REALTEST_TAKEOVER=1` | authorizes quitting the running Emacs |
| `AGENT_REPL_REALTEST_MEASURE=1` | the phase budgets are reported and NOT enforced (see "Budgets") |
| `AGENT_REPL_REALTEST_OUT` | where the run directory lands |
| `AGENT_REPL_REALTEST_EMACS_SOCKET` | the Emacs server socket, when it is not `$TMPDIR/emacs<uid>/server` |
| `AGENT_REPL_REALTEST_EMACSCLIENT` | the `emacsclient` to use; the script exports the one it used so its refusals and the run's probes cannot end up on two different clients |
| `AGENT_REPL_REALTEST=1` | set by the script; without it every `TestRealtest*` skips |

The gate is deliberately doubled: the `realtest` build tag keeps these tests out
of `go test ./...`, and the environment variable keeps them out of a tagged run
that did not come through the script.

## The backups

`bin/lib-realtest-backup.sh`, before anything is launched:

- `~/.claude-emacs/wsm.db` with its `-wal` and `-shm` siblings;
- `~/.cache/agent-repl/store/events.db` with the same siblings.

Each goes to a `.realtest-bak-<stamp>` sibling and every path is printed and
written to `backups.txt` in the run directory.

The `-wal` travels WITH the database because a WAL database is not one file:
restoring a `.db` without the log standing beside it discards every transaction
the daemon had committed and not yet checkpointed.

**The helper refuses to overwrite an existing backup.** A second run reusing a
stamp would replace the copy of the state as it was BEFORE the first run with a
copy of the state that run left behind — precisely the artifact somebody reaches
for once the first run has gone wrong. A refusal costs a re-run; a silent
overwrite costs the thing the backup was for.

## Launching without disturbing the owner

Two methods, both measured, because which one leaves focus alone on macOS is a
question about the operating system rather than about this code:

| method | how |
|---|---|
| `open -g -a /Applications/Emacs.app --env AGENT_REPL_FORBID_VENDOR_CALLS=1` | asks LaunchServices not to bring the application forward. Tried first, because it asks for the behavior instead of correcting for it |
| the bundle's own executable, spawned directly, with the frontmost application reactivated immediately | corrects a focus steal rather than preventing it, so the owner may see a flicker |

`--env` is not decoration. `open` hands the application to launchd, which does
NOT pass this process's environment along, so a variable merely exported by the
script would never reach Emacs — and a realtest that believed the vendor was
forbidden while the real SDK was one prompt away would spend the owner's tokens
finding out.

Focus is READ, not assumed: `System Events` is asked which application is
frontmost before and after each launch, and the run reports which method left it
alone. Realtest 1 uses the first method for cold start 1, the second for cold
start 2, and repeats whichever worked for cold start 3, so the answer arrives
with evidence on both sides.

The vendor guard is then VERIFIED against the kernel's copy of each process's
environment (`ps -Eww`), for the Emacs process and for the daemon — whether that
daemon was spawned by this Emacs or adopted. The launcher stating it, the elisp
passing it through (`agent-repl-daemon--environment` strips only
`AGENT_REPL_STATE_DIR` and `MULTI_REPO_ROOT`, to restate them) and the daemon
passing it on again (`spawnEnv` in `daemon/internal/shimclient/supervisor.go`)
are all readable in the source; none of them is the same fact as the process
standing here having it.

## Input is real key events

`e2e/realtest/keydriver.swift`, compiled with `swiftc` into the run directory,
posts key events with `CGEventPostToPid` addressed to the Emacs pid. The event
lands on that process's own input queue without the window server making it
frontmost, which is what lets a run drive Emacs while the owner keeps typing
somewhere else.

Elisp NEVER performs an act. An elisp call that performs the act tests the
function and says nothing about whether the chord reaches it, which is exactly
the class of defect a realtest exists to catch.

The helper checks accessibility trust as its own answer (`--check`) rather than
posting and hoping: an untrusted process's synthetic events are dropped by the
window server with no error, which is the worst failure mode a test can have.
The second mechanism, `osascript` / System Events `key code`, is implemented but
is NOT equivalent — it delivers to whatever is frontmost, so reaching Emacs with
it means bringing Emacs forward.

If neither works without focus, the run records exactly what failed and why and
stops. **There is no elisp fallback.** The owner rules on the alternative.

On this Emacs the Command key is `super` and Option is `meta` (the NS defaults,
which `~/.config/doom` does not override), so `s-}` is Command+Shift+`]`
(keycode 30) and `M-2` is Option+`2` (keycode 19).

## Reading state: elisp is read-only

`emacsclient` from inside the bundle, against the server socket. Answers travel
as JSON through a file rather than as `emacsclient`'s printed return value,
which is the lisp reader's spelling of the result and is re-quoted and truncated
for anything as large as `*Messages*`. The form is wrapped in a
`condition-case`, so a probe that signals comes back as an error carrying the
elisp message.

The e2e Emacs layer reaches the same conclusion for the same reason, but through
a helper INSTALLED in the sandbox profile. A realtest may not do that: the
owner's configuration gets nothing added to it, so the wrapper travels in the
form itself.

The ONE write is the takeover's `(kill-emacs)` — deliberately not
`save-buffers-kill-emacs`, which PROMPTS, and a prompt on a headless takeover
hangs the run holding the owner's editor open on a modal question nobody will
answer. It does not save; the human-in-Emacs refusal upstream is what protects
unsaved work.

## The phases

Every phase from spawn to usable is bounded by a record the module already
writes, with its own microsecond timestamp. Only the spawn is timed from
outside, because it precedes the process that would otherwise report it.

| phase | ends at |
|---|---|
| `doom-boot` | the first module record of the run — the earliest evidence the process reached lisp at all |
| `module-loaded` | `elisp.daemon.ensure-command` |
| `daemon-answered` | `elisp.daemon.adopted` or `elisp.daemon.booted`, reported as which |
| `link-up` | `elisp.link.up` |
| `first-roster` | `elisp.roster.reconcile:` |
| `tab-drawn` | `elisp.roster.tab-open:`, per workspace |
| `panel-painted` | `elisp.frontend.watch-load: load-changed`, per workspace — the only signal in the whole startup that comes from the PAGE, and a fact the widget emits rather than an answer to a question, which is what makes it trustworthy for a page too broken to answer one |
| `total` | the last marker of the run |

Two rules that are easy to get wrong:

- **Every phase is measured FROM SPAWN**, not from the phase before it. The user
  is waiting from the moment they launched the editor, so a phase that is fast
  in isolation but starts late is exactly as slow to them.
- **The log is what is read, never a poll.** A poll answers "by the time I
  asked, it had happened", which carries no timestamp. Emacs is polled only to
  decide WHEN TO STOP WAITING; every number reported comes from a record.

`daemon-answered` reports whether the daemon was adopted or booted because those
are different work, and comparing their times would be comparing different
things.

## Budgets, and why they ship unmeasured

`e2e/realtest/budgets.go` ships with every entry `unmeasured`, on purpose. A
bound invented before the first observation is not a bound; it is a guess that
will either pass everything or fail on something unrelated to the product. The
repo's standing rule is the same (`AGENTS.md`, "Test wait/timeout bounds are
measured, not guessed").

So realtest 1 is a measurement first and a gate second, and the two are
distinguished in the open:

- `AGENT_REPL_REALTEST_MEASURE=1` — the three cold starts run, every phase timing
  is reported at the site, the manifest is written, the LOG HARVEST is enforced,
  and the output says once that the phase budgets are not. Nothing green here
  can be mistaken for a passed budget.
- without it — the budgets are enforced, and while any entry is still
  `unmeasured` the test FAILS immediately, naming the file and the phases. A run
  cannot quietly skip a gate with no number in it.

Once measured, each entry carries the observed healthy maximum it is a multiple
of, the way the bounds table in `AGENTS.md` records the run behind every value
it holds.

The three OBSERVATION CEILINGS in the test (the server answering, the startup
finishing, a quit completing) are not budgets. They bound how long the run waits
before reporting that something did not happen, and they are generous on
purpose: a ceiling that fires turns a measurable slow startup into an
unmeasurable timeout, which throws away the evidence the run exists to collect.

## The log harvest — the remediation bar

`docs/REALTEST-PLAN.md`: a realtest is remediated if and only if ALL warnings and
errors across ALL logs are resolved. There is no allowlist. This is what makes
that a verdict.

The sources are the ones `logging-contract.md` names, and nothing else:

| source | what |
|---|---|
| the five canonical per-workspace links | `<workspace>/.claude/emacs/{emacs,daemon,shim,webapp,sidecar}.log`, read through the LINK and never through a target path constructed by a reader |
| the elisp global sink | `agent-repl-log-file-name` — `$TMPDIR/doom-agent-repl-<uid>/doom-agent-repl.log`, plus `.prev`. The pre-2026 default under `~/.claude-emacs` is retired and holds only historical records; no run harvests it |
| the daemon's global sink | `~/.claude-emacs/logs/daemon.run.log` and its rotation siblings |
| the two services' global sinks | `~/.cache/agent-repl/log/shim-store.log`, `shim-claude-sidecar.log`, with rotation siblings |
| the two services' stderr | `~/.cache/agent-repl/log/*.err.log` |
| Emacs's `*Messages*` | read through `emacsclient`; a cold start makes the whole buffer the run window |

The per-workspace TARGETS under `~/.claude-emacs/logs` and the OS temporary
directory are deliberately NOT enumerated: every one of them is already
reachable through the link that names it, and harvesting both would report every
record twice.

How a run window is selected:

1. Before the run, every source's identity and size is snapshotted. **The key is
   the device and inode, not the path.** Two things routinely move a log's bytes
   between paths mid-run — the daemon rotates its run log ON OPEN, and a
   restarting runtime replaces a workspace's canonical link with a new target —
   and a path-keyed offset reports both as losses that did not happen. An inode
   is the identity of the bytes: a renamed log is read from where the run left it
   under its old name, a fresh one is read whole, and the only thing that reads
   as a loss is the same inode holding fewer bytes than before, which IS one.
2. Sources are re-enumerated AFTER the run, so rotation siblings and workspace
   sinks the run itself created are read.
3. Both the current target and the snapshot's target are read for a relinked
   source, because a restarting runtime writes to its outgoing target right up
   to the swap.
4. Records are kept when their `timestamp` is inside the window, INCLUSIVE at
   both ends — a half-open interval's excluded end drops the last record a run
   wrote, which is routinely the one that says why it ended.
5. A line that does not parse is NOT skipped. `logging-contract.md` forbids
   human-formatted persisted records, so a non-record in a structured log means
   something wrote to it that should not have, and that is the one class of log
   defect nothing else in the system would notice.
6. Every line appended to a service's `.err.log` is a finding on its own. The
   contract permits emergency output only when the canonical sink cannot record
   its own failure, so a healthy service writes nothing there and the line's
   existence is the finding.

Attribution, per record: its own `workspace_id`/`workspace_dir` first, then the
workspace its SINK belongs to, then `global`. When the first two exist and
DISAGREE, that is reported in its own right — the contract's routing invariant
has been broken somewhere and neither value can be trusted after that, which is
a thing one merged field could not have said.

Everything lands in `MANIFEST.md` in the run directory, verbatim. Not
summarized, not deduplicated: step 3 of the loop is "surface them to the owner,
with evidence, verbatim. Nothing is fixed here", and a paraphrase is not
evidence. The test FAILS when the finding count is non-zero and fixes nothing.

Unexpected INFO is reported as counts per operation and never fails a run. A
count that jumps is a lead, not a verdict.

## What is expected to change

`docs/LOGGING.md` specifies `bin/logs.sh --harvest <from> <to>` as the ONE
reader, answering exactly this question. When it lands, the harvester here
should delegate to it rather than keep a second spelling of the source set —
two readers of the same layout is the drift the shared contract exists to
prevent. Until then this is the minimal reading of `logging-contract.md`, and it
is deliberately minimal for that reason.

The same document names gaps that bear directly on what a run can assert today:
`sidecar.log` is never written (the `ClientLog` seam hardcodes the webapp
runtime), 332 elisp sites pass a nil workspace so records that belong to a
workspace land globally, and the elisp sink truncates its oldest 80% at its cap
rather than rotating. A run's findings should be read with those in hand.

## Layout

```
bin/realtest.sh                the entry point and the three refusals
bin/lib-realtest-backup.sh     the state backups
bin/test-realtest.sh           hermetic tests for both, with stub siblings
e2e/realtest/
  doc.go                       what the package is and both of its gates
  logs.go                      the source set, read out of logging-contract.md
  harvest.go                   the snapshot, the window, the findings
  messages.go                  Emacs's own *Messages*, which has no timestamps
  phases.go                    the markers and the measurements
  budgets.go                   the table, and why it ships unmeasured
  emacsclient.go               read-only elisp, JSON through a file
  launch.go                    the two unfocused launches, and the guard check
  keys.go                      the chords and the two mechanisms
  keydriver.swift              CGEventPostToPid, with the trust check
  state.go                     what the state database holds, read-only
  manifest.go                  MANIFEST.md
  realtest_1_start_the_editor_test.go
```

The unit tests run under the same build tag and need none of the above: they
exercise the harvester, the phase reader and the source enumeration against
fixtures in `t.TempDir()`, touch no real path and start no process. That is what
`Env` states its roots explicitly for.

```bash
go -C e2e test -tags realtest ./realtest/ -count=1   # the unit tests
bash bin/test-realtest.sh                            # the script and the backups
```
