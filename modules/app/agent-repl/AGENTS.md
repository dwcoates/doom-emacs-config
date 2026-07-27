# agent-repl/

The Claude REPL: an Emacs frontend (`*.el`), a resident Go daemon (`daemon/`), a
per-session TypeScript shim driving the Claude SDK (`agent-shim/claude/shim/`), a
browser GUI (`webapp/`), and two OS-managed services carrying the file plane
(`agent-shim/shim-store/`, `agent-shim/claude/shim-sidecar/`).

The repo-wide rules in the top-level `AGENTS.md` apply here in full; this file
covers only deploying and running THIS module.

## Committing to master means bouncing what you changed

Every component here is a built artifact, and every running process keeps
serving the binary it started with — so a commit deploys nothing on its own. A
change is finished when the process serving the user is running it, and your
report says what you rebuilt and what you restarted.

A merged-but-undeployed fix looks exactly like a fix that does not work, except
the correct code sitting in `git log` makes it harder to diagnose.

The two deploy paths have OPPOSITE bounce policies. Follow each as written
rather than re-deciding per change.

**1. `bin/build-frontend.sh` — shim, webapp bundle, daemon. ALWAYS bounce the
daemon afterwards.** Build-if-stale, so it is cheap to run unconditionally:

```sh
modules/app/agent-repl/bin/build-frontend.sh
```

Then bounce claude-repld — every time, without asking and without weighing
whether this particular change merits it. Rebuilding the binary is not deploying
it, and the bounce is also what remounts webviews, which is how a webapp rebuild
reaches the user. The top-level "Daemon bounce policy (claude-repld)" section
governs HOW (never mid-turn; prefer the Emacs restart path) — never whether.

**2. Hand-deployed launchd services — shim-store, shim-claude-sidecar. Leave
these IN FLIGHT; bounce only when the user asks.** They carry the file plane for
every live session, so restarting them is disruptive in a way a daemon bounce is
not. After landing an important change to either, ASK the user whether to bounce
— an unbounced service means they never see the change, so silence is not the
safe option either.

`build-frontend.sh` does NOT touch them. They run out of
`~/.cache/agent-repl/bin/` under `com.agentrepl.shim-store` and
`com.agentrepl.shim-claude-sidecar` (plists in `launchd/`), so changes under
`agent-shim/shim-store/` or `agent-shim/claude/shim-sidecar/` deploy nothing
until:

```sh
cd modules/app/agent-repl/agent-shim/shim-store
go build -o ~/.cache/agent-repl/bin/shim-store .
cd ../claude/shim-sidecar
go build -o ~/.cache/agent-repl/bin/shim-claude-sidecar .

launchctl kickstart -k gui/$(id -u)/com.agentrepl.shim-store
# WAIT for ~/.cache/agent-repl/sock/store.sock before the next line
launchctl kickstart -k gui/$(id -u)/com.agentrepl.shim-claude-sidecar
```

**That ordering is mandatory.** Restarting both at once makes the sidecar's
cursor recovery fail against a socket not yet listening; it then starts cold and
silently re-reads every watched transcript from offset zero (observed
2026-07-25, thousands of files re-ingested).

## Verify the deploy rather than assuming it

`KeepAlive` restarts a failing service forever, so a broken deploy presents as a
service that is "running" while doing nothing. Read the tail of
`~/.cache/agent-repl/log/shim-{store,claude-sidecar}.log` and confirm the steady
state — for the sidecar, `store link UP`, not a repeating `store link DOWN`.
