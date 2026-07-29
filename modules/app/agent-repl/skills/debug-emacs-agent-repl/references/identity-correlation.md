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
