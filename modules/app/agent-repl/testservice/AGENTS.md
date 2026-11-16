# testservice/

The test service, `com.agentrepl.tests`: a singleton process the platform's
service manager keeps running, like the store and the sidecar, that owns
every merge queue gate's run and serves `testservice.v1.Tests`
(`proto/src/testservice/v1/`) over Connect on a unix socket. The daemon is
its only caller (`daemon/internal/testsclient`). Its service entry is
`requirements.json`, rendered as `services/AGENTS.md` says.

## Why it exists

A gate run as the daemon's own child died with the daemon, so every deploy
restarted the head's gate from its start. Here, the run outlives every
daemon:

- The gate starts its run here and records the run's id in the merge's
  progress (`daemon/internal/merge/gateservice.go`) before it watches it.
- A merge resumed on the next daemon, or after a pause, a transfer or a
  handover, watches the same run again from its first line to its end.
- A gate whose merge was taken back, or whose resume found the target moved,
  stops its run.

## Glossary

| Term | Meaning | Known synonyms / code identifier |
|---|---|---|
| run | One command the service started for a gate, in its own process group, with its output kept by line. | code: `runs.Spec` (what it runs), `runs.End` (how it ended) |
| run registry | The directory holding every run: `<root>/<id>/` with `run.json` (its spec and group), `output.log` and, once it ended, `end.json`. | code: `runs.Registry` |
| watch | A read of a run's lines from any line, following it live until it ends. | |
| lost | A run a previous life of the service left without an end: the next life kills its group and settles it as lost. | |

## Rules

- **A run ends only with its command, a Stop, or the service's next life.**
  The service's own shutdown leaves its runs going; the next life settles
  each one it finds unended as lost, killing its group.
- **A stop kills the whole group**, until no live member is left
  (`agentrepl/logging/childgroup.KillGroup`): a script's background children
  die with it.
- **Ended runs are pruned after `runs.Retention` (24h)**, when the registry
  opens.
- **Every record goes through `recordlog`**, under the runtime `tests`;
  `bin/logs.sh` reads it.
- **A test harness hands the service its socket** with `--listen-fd`, so a
  client's first connect queues until the service accepts and there is no
  readiness to wait for (`daemon/integration/harness/testservice.go`).

## Tests

The suite is `testservice` in the roster (`testrun/roster/roster.go`); run
only the tests a change adds or changes, through `agent-repld test run`, as
the `agent-repl-tests` skill says.
