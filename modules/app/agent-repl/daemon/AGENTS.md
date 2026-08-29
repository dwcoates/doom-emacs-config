# daemon/ — the rebuilt agent-repl daemon (Go)

Read `ARCHITECTURE.md` first: the package map, the seams, the conventions.
`integration/SPEC.md` is the integration suite's specification;
`ERROR-ARMS.md` is the ledger of refusal arms not yet landed in the contract.

## Build and test

- `go build ./... && go vet ./... && go test ./...` from this directory.
- Integration suite: `go test -tags integration ./integration/...` (a real
  daemon subprocess against a fake shim.v1 server, fake git repos and a temp
  state root; the fake shim binary is `integration/fakeshim`).
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
| `--pprof <unix path or 127.0.0.1:port>` | opt-in local profiling surface | off |

## Environment (process contracts and test knobs)

| variable | scope | meaning |
| --- | --- | --- |
| `AGENT_REPL_STATE_DIR` | contract | the one state root shared with Emacs, skills and tests |
| `AGENT_REPL_FORBID_VENDOR_CALLS` | contract | every vendor exec site refuses (classifier, login pty with the default binary, shim spawn without `--fake`) |
| `AGENT_REPL_OWNED=1` | contract | propagated into every shim so vendor hooks recognize our processes |
| (shim spawn env) | contract | the daemon's OWN environment passed through, with CLAUDE_CONFIG_DIR, AGENT_REPL_OWNED, AGENT_REPL_STATE_DIR, SHIM_BUILD_SHA and the store socket set/overridden — never a curated allowlist |
| `AGENT_REPL_STORE_SOCKET` | contract | the store socket (a flag beats it) |
| `MULTI_REPO_ROOT` | contract | a workspace whose main repo is under it uses the multi-repo account root |
| `AGENT_REPL_SELF_REPO_DIR` | test only | overrides the daemon's own-checkout identity for the merge-method split; the self-reload trigger stays ON (test safety comes from `AGENT_REPL_DEPLOY_SCRIPT` naming a fake deploy script, so landed range → rollout trigger → deploy is assertable end to end) |
| `AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS` | test only | compresses the idle cutoff |
| `AGENT_REPL_LOCK_DIR` | test only | overrides `~/.cache/agent-repl/run` for the kernel-lock probes (the fake shim honors it too) |
| `AGENT_REPL_BROWSER_CMD` | operator/test | the external browser launcher command for OpenExternal |
| `AGENT_REPL_CLAUDE_BIN` | test only | the `claude` binary for the login pty and the real classifier (a fake script in tests) |
| `AGENT_REPL_DEPLOY_SCRIPT` | test only | overrides `bin/deploy-all.sh` for the self-reload trigger |
| `AGENT_REPL_PROMPTS_DIR` | operator | the prompts directory |

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

## Logging

Only `internal/dlog`. Every logical branch logs (DEBUG ordinary, WARN
warnings, ERROR errors) with `operation = daemon.<package>.<verb>` and
structured context, per `../logging-contract.md`. Workspace-bound records
go to `<workspace>/.claude/emacs/daemon.log`; failing to resolve the
workspace is an invariant violation, never a global write.

## Conventions

Table-driven tests, Arrange/Act/Assert, one test file per source file, one
edge case per test, no `time.Sleep` for synchronization. Unlanded refusal
arms are answered at the transport as `intended arm: <Rpc>Error.<arm>: …`,
logged at WARNING under `daemon.refusal.unlanded_arm`, and recorded in
`ERROR-ARMS.md`.
