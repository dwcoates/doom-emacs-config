---
name: profile
description: Run a time-boxed Emacs profiling session. Starts the profiler, schedules an automatic stop after N minutes via ScheduleWakeup, then analyzes the resulting profiler-report. Use when the user wants a hands-off profile around a slow editor operation, or invokes /profile.
---

# Profile (orchestrated workspace profiling)

This skill orchestrates a complete profiling session against the running Emacs:

1. Dispatch `workspace-profile` enable.
2. `ScheduleWakeup` for the chosen wait duration.
3. On wake, dispatch `workspace-profile` disable.
4. Receive the `profiler-report` text via Emacs's existing return-address pipeline (no file polling, no Emacs changes).
5. Analyze the report and present findings.

Every step appends a JSON-lines record to a per-workspace session log so wait-duration policy can be tuned post-hoc.

Use `/profile` when:

- The user asks for a profile and wants the stop to happen automatically.
- The user describes a slow editor operation and expects a hands-off capture-and-analyze flow.
- A debug workflow needs profiling around a fixed time window without the user manually toggling stop.

Use plain `/workspace-profile` instead when:

- The user wants to start/stop manually with no automation.
- The agent has no way to schedule a wakeup (rare; if `ScheduleWakeup` is unavailable, fall back to `/workspace-profile` and tell the user).

`/debug-logs` covers a different problem (reading existing logs / recommending instrumentation). If the suspect is performance and you want fresh sampling data, use `/profile`. If the suspect is logic or state and you want history, use `/debug-logs`.

## Wait-duration policy

Pick the wait duration `N` (seconds) using the first rule that matches:

1. **Explicit user duration** wins. Forms accepted: `/profile 45s`, `/profile 3m`, `/profile manual`.
   - `manual` mode = start only, no auto-stop; user calls `/workspace-profile` disable themselves. Skip all the wakeup/stop steps below.

2. **Cue inference** from the user's prompt:

   - "startup" / "init" / "doom load" / "boot" → 30s
   - "scroll" / "tab through" / "while I type" / "interactive" → 90s
   - "compile" / "indexing" / "lsp warmup" / "first project open" → 240s
   - "merge" / "git" / "magit" → 120s

3. **Default** when no cue and no explicit duration → 120s (2 minutes).

4. **Hard cap = 600s (10 minutes).** If inference or an explicit arg exceeds this, clamp to 600 and warn the user in your reply.

Record which rule fired in the JSONL log's `wait_source` field (`explicit-arg` | `inferred:<cue>` | `default` | `manual`).

## Session log

Path: `<project-root>/.claude/emacs/profile-sessions.jsonl`.

Format: append-only JSON-lines, one record per event. Records are joined across events by `session_id`.

Schema (v1):

```json
{"schema": 1, "ts": "<ISO8601>", "session_id": "<workspace>-<unix_ts>", "event": "<event>", ...event-specific fields}
```

Events and their fields:

- `start` — written immediately before dispatching enable.
  - `planned_wait_s` (int)
  - `wait_source` (`explicit-arg` | `inferred:<cue>` | `default` | `manual`)
  - `mode` (`cpu` | `mem` | `cpu+mem`)
  - `workspace` (string)
  - `prompt_excerpt` (string, capped at 200 chars)

- `reschedule` — written when the user asks to extend mid-run.
  - `new_planned_wait_s` (int — total from start, not delta)
  - `reason` (string, e.g. `user-extended`)

- `stop` — written after dispatching disable (auto-wakeup or manual).
  - `actual_wait_s` (int — wall-clock from start to stop)
  - `stopper` (`auto-wakeup` | `user-manual` | `early-cancel`)
  - `report_size_bytes` (int, when known; omit if not yet received)

- `outcome` — written after analyzing the report.
  - `label` (`found-culprit` | `inconclusive` | `noise`)
  - `user_feedback` (`too-short` | `too-long` | `fine` | `null`)

Always include `schema: 1` in every record so future format bumps stay backward-compatible.

## Steps

### 1. Parse intent

Determine:

- Mode: `cpu` | `mem` | `cpu+mem` (default `cpu+mem`).
- Wait policy outcome: `N` (seconds) and `wait_source`, per the policy above.
- Whether this is `manual` mode.

Resolve the workspace name once: `ws="$(basename "$PWD")"`.

Resolve the session id: `session_id="${ws}-$(date +%s)"`.

### 2. Append the `start` record

```bash
LOG="$PWD/.claude/emacs/profile-sessions.jsonl"
mkdir -p "$(dirname "$LOG")"

jq -cn \
  --argjson schema 1 \
  --arg ts "$(date -u +%FT%TZ)" \
  --arg sid "$session_id" \
  --arg event "start" \
  --argjson planned_wait_s "$N" \
  --arg wait_source "$wait_source" \
  --arg mode "$mode" \
  --arg ws "$ws" \
  --arg excerpt "$prompt_excerpt" \
  '{schema:$schema, ts:$ts, session_id:$sid, event:$event,
    planned_wait_s:$planned_wait_s, wait_source:$wait_source,
    mode:$mode, workspace:$ws, prompt_excerpt:$excerpt}' \
  >> "$LOG"
```

Cap `prompt_excerpt` to the first 200 characters of the user's request. Strip newlines.

### 3. Dispatch enable

Invoke the existing `workspace-profile` skill via its `run.sh`. Do not duplicate its JSON contract here — it owns that:

```bash
bash ~/.claude/skills/workspace-profile/run.sh << EOF
[
  {"type": "profile", "enabled": true, "mode": "$mode"}
]
EOF
```

(If `~/.claude/skills/workspace-profile/run.sh` is missing or fails on `uuidgen`, stop and ask the user to run `.claude/install.sh` — same failure mode as `/workspace-profile`.)

### 4. Branch on mode

- **`manual` mode:** tell the user the profiler is running and that they should invoke `/workspace-profile` disable when ready. Do NOT schedule a wakeup. Do NOT proceed to step 5. The session log will be closed when the user manually stops, at which point you should append a `stop` event with `stopper: user-manual` and `actual_wait_s` measured from the `start` record.

- **Timed mode (everything else):** continue to step 5.

### 5. Schedule the auto-stop

Call `ScheduleWakeup` with `delaySeconds=N` and a self-contained `prompt` that re-enters this skill at step 6. The wake-up prompt must carry the data needed to close the session without re-deriving it:

```
/profile __resume__ session_id=<session_id> ws=<ws> planned_wait_s=<N> start_ts=<unix_ts>
```

Reason: "auto-stopping profiler at T+<N>s for session <session_id>".

### 6. On wake-up: dispatch disable

When the resume prompt fires (recognized by `__resume__` token), do:

```bash
bash ~/.claude/skills/workspace-profile/run.sh << EOF
[
  {"type": "profile", "enabled": false, "workspace": "$ws"}
]
EOF
```

Then append the `stop` record:

```bash
actual_wait_s=$(( $(date +%s) - start_ts ))
jq -cn \
  --argjson schema 1 \
  --arg ts "$(date -u +%FT%TZ)" \
  --arg sid "$session_id" \
  --arg event "stop" \
  --argjson actual_wait_s "$actual_wait_s" \
  --arg stopper "auto-wakeup" \
  '{schema:$schema, ts:$ts, session_id:$sid, event:$event,
    actual_wait_s:$actual_wait_s, stopper:$stopper}' \
  >> "$LOG"
```

Tell the user: "Profiler stopped after Ns. Report will arrive as a follow-up message and I'll analyze it then."

### 7. When the report arrives

The Emacs return-address pipeline delivers `profiler-report` text as a user message. When that arrives:

- Read it.
- Identify the top 3–5 hot frames (CPU) and/or the largest allocators (mem), grouped by module/file where possible.
- Call out anything in `modules/app/claude-repl/` specifically — that's the primary target audience.
- Append an `outcome` record (use `null` for `user_feedback` until the user gives one):

```bash
jq -cn \
  --argjson schema 1 \
  --arg ts "$(date -u +%FT%TZ)" \
  --arg sid "$session_id" \
  --arg event "outcome" \
  --arg label "$label" \
  '{schema:$schema, ts:$ts, session_id:$sid, event:$event,
    label:$label, user_feedback:null}' \
  >> "$LOG"
```

Where `label` is `found-culprit` | `inconclusive` | `noise` based on whether your analysis identified one or more plausible hot paths.

### 8. Mid-run extensions

If the user comes back before the wakeup fires and asks for more time:

- Append a `reschedule` record with the new total `planned_wait_s` and `reason: user-extended`.
- Call `ScheduleWakeup` again with the new delta from now (the runtime treats this as the active wakeup; the prior one is superseded).

## What NOT to do

- Do NOT toggle the profiler yourself. Always go through `~/.claude/skills/workspace-profile/run.sh`. The Emacs side is the only legitimate toggle path.
- Do NOT write the profiler-report to disk. The report arrives via the existing return-address pipeline; you do not need to read or polling-check a file.
- Do NOT exceed the 600s hard cap. Clamp and warn instead.
- Do NOT skip the JSONL log appends — the post-mortem policy tuning depends on them.
- Do NOT invent `wait_source` values outside the documented set. If you add a new cue, add it to this doc first.
