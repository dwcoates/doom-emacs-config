# Skills changed mid-session

A running session never picked up a skill installed, changed or removed after
it started. The vendor loads its skills once, at its process's start
(measured 2026-10-23 with the shim's own `@anthropic-ai/claude-agent-sdk`
0.3.280: a skill written into `<config>/skills/` or `<cwd>/.claude/skills/`
after the start is not listed by `supportedCommands()` 2s or 10s later), so
re-reading the catalog at every turn end can never surface it. The live
symptom was `agentic-puzzle-gns-spec`, whose catalog lacked every agent-repl
skill linked into `~/.claude-chesscom/skills` after it started.

Every proto change here is pre-approved by the owner's standing ruling
(2026-10-13: decide contract changes and carry them through, report after).

## Landed changes

### `conversation.v1.SessionUpdate.skills_changed` (new arm, tag 39)

- WHAT: `conversation.v1.SessionSkillsChanged { repeated string paths }`, the
  paths whose change made the vendor's skill set stale.
- WHY A STANDING LEVEL, restated on every WatchSession open: the daemon acts
  on it by replacing the shim at the next idle point, and a daemon that
  attaches after the statement (a handover's successor) must still learn it,
  or the replacement is lost with the daemon that heard it.
- WHY NEVER WITHDRAWN: only a new vendor process loads the current set, and
  the daemon answers by replacing the shim, whose successor starts with a
  fresh watch and no level.
- WHY THE SHIM WATCHES, NOT THE DAEMON: the shim alone knows exactly what its
  vendor loads from (the `CLAUDE_CONFIG_DIR` it spawned it with, and its cwd),
  and Node's `fs.watch` is kernel-driven with no new dependency. The daemon
  keeps the one replacement path it already has: `rollout.Controller.BounceShim`
  under a new reason, `skills_changed`, unforced, so it defers behind running
  work and a merge's lease exactly as a stale build's does.
- NO CHANGE IS MISSED: the watch is armed before the vendor query is created,
  so a change after the vendor loaded its skills is seen, and one before it is
  in the set it loaded. A change during a replacement lands either before the
  successor's vendor starts (it loads it) or after its watch is armed (it
  states it).
