# Identity correlation

Use this playbook to construct an identity spine across runtimes. Keep each
identifier in its own namespace:

- Absolute `workspace_dir`.
- Stable `workspace_id`.
- Daemon-owned `agent_repl_session_id`, commonly shaped like `s_...`.
- Vendor-owned `claude_session_id`, commonly a UUID and also the transcript
  filename.
- OS-process `pid`.
- Browser `connection_id`.
- Command or protocol `request_id`.
- Event `seq`, task ID, and transcript path when relevant.

Never compare unlike identifiers merely because both are called a session ID.

## Start from the strongest known identity

Prefer this order:

1. Absolute workspace directory.
2. A dedicated identity field in a canonical JSONL record.
3. A daemon registry record.
4. A session socket or process ID.
5. Message text only when no structured field exists.

Canonicalize a supplied workspace before using it:

```sh
cd /absolute/workspace && pwd -P
```

## Resolve from workspace logs

List the workspace's canonical runtime logs:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace
```

Filter all workspace runtimes by either session namespace:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --session "$SESSION_ID" \
  --tail 2000
```

Filter one process:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --runtime shim \
  --pid 12345 \
  --tail 2000
```

The resolver matches `--session` against both
`agent_repl_session_id` and `claude_session_id`. Treat the matching field in
each JSON object as the namespace declaration.

Extract identity fields without searching message text:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh \
  --workspace /absolute/workspace \
  --runtime daemon \
  --session "$SESSION_ID" \
  --tail 2000 |
jq -c '{timestamp,runtime,workspace_dir,workspace_id,agent_repl_session_id,claude_session_id,pid,connection_id,request_id}'
```

## Cross-check the daemon registry

The daemon registry is
`~/.claude-emacs/claude-repld-sessions.json`. Inspect it read-only:

```sh
jq '.' ~/.claude-emacs/claude-repld-sessions.json
```

Registry records carry:

- `session_id` for the agent-repl session.
- `cwd` for the workspace.
- `claude_session_id` once durable vendor evidence exists.
- Model, permission, terminal, checkpoint, and replay fields.

Filter by workspace:

```sh
jq --arg ws "/absolute/workspace" \
  '.sessions[] | select(.cwd == $ws)' \
  ~/.claude-emacs/claude-repld-sessions.json
```

Several records can share one workspace. Do not select the first record
without checking terminal state, timestamps, live socket evidence, and recent
logs.

## The store is authoritative for session provenance

WHEN ASKING WHETHER A CONVERSATION EXISTED, OR WHAT IT CONTAINED, THE STORE
ANSWERS — not the shim log, not the Claude transcript, not the Emacs
memory-state dump. The store is agent-repl's own durable event record, written
independently of the vendor's transcript file, so it survives the transcript
being missing, unwritten, or unreadable.

```sh
DB=~/.cache/agent-repl/store/events.db
# Did this conversation ever carry content?
sqlite3 "$DB" "select count(*) from event where session_id='<claude_session_id>';"
# What, and when?
sqlite3 "$DB" "select seq, kind, datetime(produced_at/1000,'unixepoch','localtime')
               from event where session_id='<claude_session_id>' order by seq;"
```

Read it this way:

- A handful of `ClaudeStreamMessage` records all stamped within the same second
  as bring-up is a HANDSHAKE, not a conversation. That session ran no turns.
- Absence of a Claude transcript with substantial store events means the
  transcript was LOST. Absence of both means the conversation never had content.
- The store is keyed by `claude_session_id` (the vendor conversation), not by
  the agent-repl session id.

THE TRAPS, each of which has produced a wrong provenance conclusion:

- **The shim log is not evidence of absence.** It records the shim's own
  lifecycle and framing traffic, which is voluminous even for an idle session.
  A quiet shim log does not establish that no conversation existed.
- **`memory-state.el` fields can belong to a PREVIOUS conversation in the same
  workspace.** `:last-prompt-time` and `:last-prompt-summary` are workspace
  scoped and survive across sessions, so a summary there may describe a
  conversation the current session has nothing to do with. Convert the
  timestamp and compare it against the session's own creation before using it.
- **A workspace's transcripts are not all reachable.** `resume-resolve` excludes
  conversations it will not resume, and logs one `EXCLUDING uuid=… — <reason>`
  line per candidate. A workspace can hold large, intact transcripts that are
  all excluded, after which a fresh empty conversation is created. Read those
  exclusion lines before concluding a workspace had no prior conversation:

```sh
modules/app/agent-repl/scripts/agent-repl-log-discovery.sh   --workspace /absolute/workspace --runtime daemon |
  grep 'resume-resolve:'
```

- **Count the transcript's records** rather than trusting its presence or size:

```sh
wc -l ~/.claude*/projects/<slug>/<uuid>.jsonl
```

## Correlate sockets and processes

Runtime surfaces live under:

- `~/.cache/agent-repl/sock/daemon-frontend.sock`.
- `~/.cache/agent-repl/sock/daemon-shim.sock`.
- `~/.cache/agent-repl/sock/session-<agent-repl-session-id>.sock`.
- `~/.cache/agent-repl/run/session-<agent-repl-session-id>.lock`.

List them read-only:

```sh
ls -la ~/.cache/agent-repl/sock/ ~/.cache/agent-repl/run/
```

A registry record without its expected session socket means the daemon has
durable identity but cannot reach that session's shim. A socket without a
matching current registry record is an orphan candidate, not proof of a live
session.

Use the shim log's structured `pid` to separate multiple shim processes that
share one workspace `shim.log`. Use `connection_id` for browser instances.
Use `request_id` to connect one command across daemon, shim, and webapp
boundaries.

## Build the case identity table

Maintain a small internal table:

| Namespace | Value | Source | Confidence |
|---|---|---|---|
| Workspace directory | value | user, registry, or log field | known or inferred |
| Workspace ID | value | log field | known |
| Agent-repl session | value | registry or log field | known |
| Vendor session | value | registry, log field, or transcript name | known |
| PID | value | log field or process evidence | known |
| Connection or request | value | log field | known |

Mark unresolved links explicitly. A missing correlation field is an
observability gap. Do not repair the chain by guessing from chronology alone.
