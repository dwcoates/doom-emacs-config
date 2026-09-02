# daemon/ — the rebuilt agent-repl daemon (Go)

Read `ARCHITECTURE.md` first: the package map, the seams, the conventions.
`integration/SPEC.md` is the integration suite's specification;
`ERROR-ARMS.md` is the ledger of refusal arms not yet landed in the contract.

## Build and test

- `go build ./... && go vet ./... && go test ./...` from this directory.
- Integration suite: `TMPDIR=/tmp go test -tags integration ./integration/...`
  (a real daemon subprocess against a fake shim.v1 server, fake git repos and a
  temp state root; the fake shim binary is `integration/fakeshim`).
  `TMPDIR=/tmp` IS REQUIRED on macOS: `t.TempDir()` otherwise roots the state
  under `/var/folders/...`, and `<state>/sock/<workspace-id>.sock` then exceeds
  the 103-byte unix socket path limit, so the daemon refuses the state root at
  boot before anything else runs.
- Every test process exports `AGENT_REPL_FORBID_VENDOR_CALLS=1`. No test
  ever calls the vendor.

## Command line (binding spellings; Go's flag package accepts one or two dashes)

Emacs launches `daemon/bin/claude-repld` with NO argv — state comes from the
environment. Every flag is optional.

| flag | meaning | default |
| --- | --- | --- |
| `--state-dir <dir>` | the state root | `$AGENT_REPL_STATE_DIR`, else `~/.claude-emacs` |
| `--fake` | force the shim's offline scripted SDK (`--fake`) onto every session, and the `-fake` classifier | off |
| `--joining <addr>` | start as the blue-green SUCCESSOR of the incumbent at `<addr>`: bind a fresh port, own no workspace, write `daemon.addr` only once every workspace is adopted | absent = incumbent |
| `--store-socket <uds>` | the store socket passed to every shim | `$AGENT_REPL_STORE_SOCKET`, else `~/.cache/agent-repl/sock/store.sock` |
| `--shim-main <path>` | the shim entry (`agent-shim/claude/shim/dist/main.js`) | resolved from the checkout the binary was deployed from |
| `--node <bin>` | the node binary that runs the shim | `node` on PATH |
| `--webapp-dist <dir>` | the webapp's built assets to serve at `/` | `webapp/dist` in the checkout |
| `--prompts-dir <dir>` | the prompts directory | `$AGENT_REPL_PROMPTS_DIR`, else `modules/app/agent-repl/prompts` in the checkout |
| `--default-config-dir <dir>` | the default account root | the CLI's default (`~/.claude`) |
| `--multi-repo-config-dir <dir>` | the account root for workspaces under `$MULTI_REPO_ROOT` | unset = the default root |
| `--idle-cutoff <duration>` | hibernate a session idle this long | the keep-alive idle cutoff |
| `--pprof <unix path or 127.0.0.1:port>` | opt-in local profiling surface, opened BEFORE any dependency; a wildcard or routable bind is refused, not opened | off |
| `--self-repo <dir>` | override the daemon's own checkout identity, which is what the merge orchestrator's two methods key on | the checkout the binary was deployed from |

## Run and boot order (binding; `cmd/claude-repld`)

`run` performs exactly this sequence, and every step's failure is fatal:

1. the four environment contracts, with `--fake` and `--state-dir` applied over
   them;
2. the state root's layout — every directory created, then the socket path
   budget checked, so an overlong root is refused here rather than at the first
   shim spawn;
3. the log surfaces; the run log's open failure is a BOOT FATAL, because a
   daemon that cannot write its own narrative cannot report what it then does
   wrong;
4. `--pprof`, BEFORE any dependency, so a boot wedged on one is still
   diagnosable through it;
5. the ONE loopback listener, bound FIRST as the boot-exclusivity claim (an
   exclusive kernel lock on `daemon.lock` beside `daemon.addr`); an UNFLAGGED
   second daemon loses there and exits without touching the incumbent's
   listener or its advertisement;
6. `daemon.addr`, written atomically by an incumbent; a `--joining` successor
   DEFERS it and instead writes `joining.addr` where the incumbent that spawned
   it is waiting, and `daemon.addr` is written only once every workspace is
   adopted (the rollout's `WriteDaemonAddr` hook);
7. the state client — `wsm.Open`, or `wsm.OpenReadOnly` for a joining daemon,
   which owns no workspace and must not be a second writer;
8. the component graph, then `boot.Sequence.Run`: adopt the shims whose
   workspace lock is still held (never kill-and-restart), reconcile the intent
   manifest (all four dispositions persisted as faults), restore the holds
   all-or-nothing, close the orphaned turns of the CLIENT-LESS workspaces in one
   transaction each (an adopted workspace's in-flight turns are re-opened by its
   sessionwatcher instead), recover the in-flight merges, and — for a successor
   — `rollout.Controller.Join`;
9. `server.New` behind `server.H2C` on the claimed listener;
10. an orderly exit on SIGINT/SIGTERM: the advertisement is withdrawn, the
    streams are closed, and the state client and the log sinks are closed.

A lock probe that could NOT TELL is never read as free: such a workspace is
neither adopted nor orphan-closed, and the boot report names it.

## Environment (process contracts and test knobs)

| variable | scope | meaning |
| --- | --- | --- |
| `AGENT_REPL_STATE_DIR` | contract | the one state root shared with Emacs, skills and tests |
| `AGENT_REPL_FAKE` | contract | the whole stack's fake mode: shims spawn with `--fake` and the classifier is scripted. `--fake` overrides it; the flag can only turn it ON |
| `AGENT_REPL_FORBID_VENDOR_CALLS` | contract | every vendor exec site refuses (classifier, login pty with the default binary, shim spawn without `--fake`) |
| `AGENT_REPL_OWNED=1` | contract | propagated into every shim so vendor hooks recognize our processes |
| (shim spawn env) | contract | the daemon's OWN environment passed through, with CLAUDE_CONFIG_DIR, AGENT_REPL_OWNED, AGENT_REPL_STATE_DIR, SHIM_BUILD_SHA, AGENT_REPL_SESSION_ID (the HostSessionId, log correlation only) set/overridden; the store socket rides argv — never a curated allowlist |
| `AGENT_REPL_STORE_SOCKET` | contract | the store socket (a flag beats it) |
| `MULTI_REPO_ROOT` | contract | a workspace whose main repo is under it uses the multi-repo account root |
| `AGENT_REPL_SELF_REPO_DIR` | test only | overrides the daemon's own-checkout identity for the merge-method split; the self-reload trigger stays ON (test safety comes from `AGENT_REPL_DEPLOY_SCRIPT` naming a fake deploy script, so landed range → rollout trigger → deploy is assertable end to end) |
| `AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS` | test only | compresses the idle cutoff |
| `AGENT_REPL_LOCK_DIR` | test only | overrides `~/.cache/agent-repl/run` for the kernel-lock probes (the fake shim honors it too) |
| `AGENT_REPL_BROWSER_CMD` | operator/test | the external browser launcher command for OpenExternal |
| `AGENT_REPL_CLAUDE_BIN` | test only | the `claude` binary for the login pty and the real classifier (a fake script in tests) |
| `AGENT_REPL_DEPLOY_SCRIPT` | test only | overrides `bin/deploy-all.sh` for the self-reload trigger |
| `AGENT_REPL_TEST_ALL_SCRIPT` | test only | overrides `bin/test-all.sh` for the merge test gate (invoked as `bash <script> --suites <a,b>` in the merge TARGET worktree; exit 0 = pass; per-suite state parsed from the script's own `<suite>: passed in <N>s` / `<suite> failed after <N>s with exit code <rc>` lines; output archived under `<state>/merge-logs/`) |
| `AGENT_REPL_PROMPTS_DIR` | operator | the prompts directory (the `--prompts-dir` flag beats it) |
| `AGENT_REPL_CHECKOUT` | operator | the agent-repl module root (`modules/app/agent-repl`) the binary was deployed from. It is resolved without this: the executable's own ancestors are walked first, and the path this daemon's source was COMPILED from answers when the binary was built outside the tree (`go build -o <tmp>`, which every test harness does). `--shim-main`, `--webapp-dist` and `--prompts-dir` default beneath it; `proto/vocab/` (the render colors and paint classes) and `daemon/bin/.built-sha` are read from it and have NO flag |

## The `-fake` classifier (deterministic)

A held prompt whose first word is `stop` or `abort` (the explicit-interrupt
fast path, also without `--fake`) or whose text contains `[interject]` is
classified `interject`; every other prompt is `hold_for_turn_end`.

## Kernel locks (shim-held; the daemon only probes)

`~/.cache/agent-repl/run/workspace-<md5hex(clean abs dir)[:8]>.lock` (probed
with `open + flock(LOCK_EX|LOCK_NB)`, released at once) and
`session-<vendor session id>.lock` (never probed by the daemon).

## State root layout

See ARCHITECTURE.md "State root layout": `daemon.addr`, `wsm.db`,
`logs/daemon.run.log`, `sock/<workspace-id>.sock`, `intent/manifest.json`,
`output/workspace_commands_*.json`, `merge-logs/`.

## Wiring (wave 3: the graph is complete)

`cmd/claude-repld`'s `buildGraph` builds every component and returns the
server's and the boot sequence's dependencies, the LATE BINDINGS and the
background loops. `graph.go`'s `unwired` list is EMPTY: every Deps field has a
landed producer. The list stays so a future dependency with no producer is
declared there and fails the boot loudly, naming it, rather than being filled
with a stand-in at the composition root.

Two edges point backwards and are closed with FORWARDERS in
`cmd/claude-repld/forward.go`, bound by `run` immediately after `server.New`
and before anything is served: the rollout's and the drain's pushes
(`WorkspacePusher`, `ParticipantSource`, the announcers) and the workspace
verbs' `HostRelay` (`srv.Relay()`). The merge orchestrator's guidance route and
the queue's parked route read the orchestrator out of a forwarder for the same
reason. The background loops — the drain sweep and the command-file ingress —
start after the bindings, because each of them can push.

The one collaborator with NO PRODUCER is the feed's image origin: nothing in
the daemon serves an image reference as a fetchable `src`, so the resolver is
wired with `feed.UnproducedImageResolver`, which refuses loudly and names the
missing producer. `/todos` and `/mcp` have no producer either, and `/agents`
and `/help` are ruled unproduced: `server.Panels` answers `/context` (the
topbar resolver's context tree) and `/status` (the daemon's build stamp plus
the resolver's spliced account/model/mode facts) and fails loudly for every
other recognized panel command.

## Deploy chain

`bin/deploy-all.sh` is the ONE chain, in the order proto → bindings → shim →
webapp → daemon → store/sidecar; its step 5 evaluates
`(agent-repl-runtime-restart-await)` in `lisp/services.el` via emacsclient (the
old `agent-repl-frontend-daemon-restart-await` is dead), and
`bin/build-frontend.sh` builds `daemon/bin/claude-repld` from
`./cmd/claude-repld`. The rollout invokes the same chain with `--no-bounce` and
never a second build path. `agent-shim/wire` is DELETED: nothing in the rebuilt
daemon imports it, and its `bin/test-all.sh` roster entry is gone.

## Logging

Only `internal/dlog`. Every logical branch logs (DEBUG ordinary, WARN
warnings, ERROR errors) with `operation = daemon.<package>.<verb>` and
structured context, per `../logging-contract.md`. Workspace-bound records
go to `<workspace>/.claude/emacs/daemon.log`; failing to resolve the
workspace is an invariant violation, never a global write.

## Conventions

Table-driven tests, Arrange/Act/Assert, one test file per source file, one
edge case per test, no `time.Sleep` for synchronization. GIT IS NEVER CALLED
DURING TESTING (user directive): every package above the git client tests
against a fake `gitclient.Git`; the integration harness scripts every git
fact (commits, conflicts, landed ranges, worktree lists) as fixture data;
the git-client leaf's own tests exercise its one spawn point against a
scripted fake `git` executable placed first on PATH (recording argv/env,
answering from a fixture table); the merge test gate is a scripted fake
script in tests. No `git init`, no temp repositories, anywhere in tests. Unlanded refusal
arms are answered at the transport as `intended arm: <Rpc>Error.<arm>: …`,
logged at WARNING under `daemon.refusal.unlanded_arm`, and recorded in
`ERROR-ARMS.md`.

## Coverage deliberately not attainable under the no-git-in-tests directive

The git client's tests pin argv, env scrubbing, `-C` selection and output
parsing against a scripted fake `git`; they can no longer prove git's OWN
behavior: that a `--no-ff` merge yields a two-parent commit, that the
landed range equals the source branch, that a conflicted merge leaves
unmerged index entries and MERGE_HEAD, that a revert removes the content
in one commit, that `worktree prune` clears a stale registration, the
exact `status --porcelain` markers, that git honors GIT_DIR over `-C`, and
real-git version compatibility (`rev-list --no-commit-header` needs
git >= 2.33). Those are e2e facts now (the project lead's suite).
