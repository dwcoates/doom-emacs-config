# agent-shim/shim-lock/

`shim-lock` HOLDS one kernel `flock(2)` on behalf of a parent that cannot take
one itself. It is the shim's session and workspace claim, spelled as a child
process.

## Why it exists

The daemon PROBES the shim's claims with Go's
`syscall.Flock(LOCK_EX|LOCK_NB)` (`daemon/internal/sessionlock`), so whatever
holds one must be a real kernel flock ON THE SAME PATH. Node has no flock. The
shim used to reach one through `open(2)`'s `O_EXLOCK`, which exists only on
macOS/BSD — so on Linux the shim refused to start ANY session and every Linux
deployment was dead in the water.

This binary is ONE code path on both platforms. It is deliberately not a
library: the property that makes the claim trustworthy is that a kernel lock
dies with the process holding it, and only a process can be that holder.

## The protocol

`shim-lock <lock-path>` — the shim's `src/locks.ts` is its only speaker.

| channel | meaning |
| --- | --- |
| argv | exactly one argument, the lock file path (its directory is created) |
| stdout | the single line `locked`, written ONLY once the flock is held |
| stdin | read and discarded; EOF is the release |
| stderr | structured JSON records (`internal/logging`) |
| exit 0 | stdin reached EOF and the lock was released |
| exit 1 | anything failed |
| exit 2 | the arguments were not one lock path |
| exit 3 | the lock is HELD BY ANOTHER PROCESS (`EWOULDBLOCK`) |

Three rules the protocol rests on, none of them negotiable:

- **Only the shim's end releases the lock.** The holder ignores SIGTERM,
  SIGINT and SIGHUP: it sits in the shim's process group, and a group stop
  (SIGTERM) starts the shim's graceful stand-down, during which its session
  still runs. Released then, the lock read free under a live session and a
  booting daemon adopted it as inert (2026-10-03). Stdin's EOF -- the shim's
  exit or deliberate release -- and SIGKILL still release it.
- **The ready line comes after the flock, never before it.** The shim reads it
  as proof the claim is made; announcing intent would let a session start over
  a lock nobody holds.
- **Exit 3 is distinct from exit 1.** The shim turns 3 into a typed
  `conversation_owned` StartSession refusal and 1 into
  `lock_holder_unavailable` naming the exit code.
  Collapsing them would make an unwritable lock directory look like a live
  duplicate.
- **stdout is a protocol channel, not a log sink.** Every diagnostic goes to
  stderr, which the shim drains into its own `shim-session-lock` records.

## Release on death, however death comes

The shim keeps the write end of this process's stdin. When the shim exits —
cleanly, on a crash, on SIGKILL — the kernel closes that pipe, this process
reads EOF and exits, and the kernel drops the lock with its last descriptor.
There is no stale lock to reap and no PID-reuse hazard.

## Where the binary lives

`bin/build-frontend.sh lock` builds it to `~/.cache/agent-repl/bin/shim-lock`,
beside `shim-store` and `shim-claude-sidecar`, and `lock` is in the script's
DEFAULT target set because the shim cannot start a session without it. The
shim resolves it from `AGENT_REPL_SHIM_LOCK_BIN` when set, and from that cache
path otherwise; every suite that spawns a real shim sets the override at a
binary it built itself.

## Tests

```bash
../../bin/background.sh go test ./...   # tests run only at background priority
```

The suite re-executes its own test binary as the holder (`TestMain` honors
`SHIM_LOCK_TEST_HELPER_ARGS`), so it drives REAL child processes and REAL
kernel locks with no build step. It synchronizes on the ready line and on
process exit — never on a clock.

`make coverage` runs `bin/report-nonlisp-coverage.sh lock`.
