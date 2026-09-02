# Intended-but-unlanded error arms

The ledger of refusals the daemon must make for which the contract has no
typed error arm yet.

When a state must be refused and `<Rpc>Error` has no arm for it, the handler
answers a Connect error — `CodeFailedPrecondition` for a state refusal,
`CodeNotFound` for an unknown id — whose message is EXACTLY:

```
intended arm: <RpcName>Error.<arm_name>: <reason>
```

and logs the intended arm at WARNING with operation
`daemon.refusal.unlanded_arm`. `server.UnlandedArm` is the one helper that does
both; nothing else spells the message by hand.

Every such site is recorded here. The daemon teamlead batches the table to the
project lead, who lands the arms; a landed arm is deleted from this table in
the same commit that switches the handler onto it.

Every arm the daemon had recorded here landed in the landing-4 contract batch,
and each row was deleted as its rpc's `<Rpc>Error` gained the arm; the table
below holds what landing 5 opened. Record the next one the same way — a row
here, then the row deleted in the commit that switches the handler onto the
landed arm.

| rpc | arm | condition | package |
| --- | --- | --- | --- |

One thing the batch did NOT land: `CloseWorkspaceError.blocked` exists but
`CloseWorkspaceBlocked` is still an EMPTY message, so the composed reason the
close refusal carries (turn_in_flight, live_work, held_prompts, merge_queued
and its sentence) has no field to ride in. The arm is usable; the evidence is
not yet expressible.
| SubmitPrompt (the one-shot finish hook) | `brief_missing` | `prompts/oneshot-create-pr-then-close-followup.md` is absent or will not splice when the one-shot's turn concludes | workspace |
| Interrupt / AnswerPermission / AnswerQuestion | `not_deliverable` (landing 3, `UpdateAgentFailure.kind.not_deliverable`) | the SDK has no route to the addressed subagent; answered honestly, the control is not hidden this wave | workspace |
| Interrupt / AnswerPermission / AnswerQuestion | `unknown_agent`, `no_open_ask`, `answer_mismatch`, `no_session` | the shim's own `UpdateAgentFailure` arm, propagated by NAME rather than collapsed into a sentence | workspace |
| Interrupt | `unknown_work` | the shim's `StopBashFailure.unknown_work`: the addressed detached shell is stale | workspace |
| Interrupt | `live`, `not_the_open_turn`, `no_session` | the shim's `KillTurnFailure` cause, propagated by name | workspace |
| Interrupt / AnswerPermission / AnswerQuestion | `unspecified` | a shim failure whose `kind` oneof is unset — illegal on the wire, surfaced rather than guessed at | workspace |

## Landing-4 batch must also answer (e2e seam, project lead request)

1. Final names of the arms the suite asserts: `transferring_away{address}`,
   `not_yet_adopted{}`, and the DaemonFault / SessionFault / HostFault kind
   arms.
2. The merge test-gate invocation: command line, cwd, env, how pass/fail is
   read, and its AGENT_REPL_* fake knob.
3. The `.claude.json` key path the daemon reads (and writes, if any) for the
   CLAUDE_CONFIG_DIR project entry.
4. The metaprompt sentinels (exact strings) the daemon wraps around a held or
   merged prompt.

## Drain and rollout refusal arms (this wave, all LANDED)

The drain controller and the rollout controller make four refusals, and every
one of them already has a typed arm in the landing-4 contract. They are recorded
here as the mapping the server handlers switch on, not as arms still owed:

| rpc | arm | daemon-side refusal | package |
| --- | --- | --- | --- |
| UpdateShutdownSchedule | `nothing_scheduled` | `drain.ErrNothingScheduled` — Cancel with nothing in force | drain |
| AdoptHostWorkspace / AdoptWebWorkspace | `no_transfer_announced` | `rollout.ErrNoTransferAnnounced`. On the WEB side this is the ORDINARY page boot (the web side never redials, so every non-handover boot makes the call) and is recorded at INFO, never WARN and never a fault | rollout |
| AdoptHostWorkspace / AdoptWebWorkspace | `not_yet_adopted` | `rollout.ErrNotYetAdopted` — an expected participant has not called yet; the caller retries with backoff | rollout |
| AdoptHostWorkspace / AdoptWebWorkspace | `participant_not_expected` | `rollout.ErrParticipantNotExpected` — the caller's stream was not open at announcement | rollout |

NOTHING NEW IS OWED by drain or rollout: no refusal either makes lacks an arm.

## Landing 6 batch, opened by the server handlers (wave 3a)

Every row below is a refusal the server MAKES and the contract has no arm for.
Each answers through `server.UnlandedArm`.

| rpc | arm | condition | package |
| --- | --- | --- | --- |
| WatchFeed | `unknown_token` | a `FeedWatchToken` this daemon never minted, or one whose mint site is gone (`feed.ErrUnknownToken`) | server / feed |
| WatchFeed | `token_expired` | a token whose pinned start is no longer retained (`feed.ErrTokenExpired`) — the client must re-open the feed | feed |
| WatchLoginTerminal | `no_login_open` | a login terminal watch on a workspace with no standing login pty (`login.ErrNoSession`). The unary `SendLoginInput` HAS the arm; the stream has no error message at all | login |
| WatchFooter / WatchTopbar / WatchDaemonHolds / WatchHostWorkspace / WatchWebWorkspace / WatchLoginTerminal | `unknown_workspace`, `workspace_ref_mismatch`, `transferring_away`, `not_yet_adopted` | every per-workspace STANDING STREAM refuses an unknown, mismatched or unowned workspace, but a `Watch*` rpc has NO `<Rpc>Error` message — a refused open is a Connect error before any frame — so all four ownership arms are unlanded for the streams | server |

One consequence is recorded as a row rather than lost: the SHIM still refuses a
bubble-addressed submit with its own `turn_already_open`, propagated by name.

| rpc | arm | condition | package |
| --- | --- | --- | --- |
| SubmitPrompt (the bubble path) | `turn_already_open` | the shim's `StartTurnFailure.turn_already_open` for a subagent whose turn runs, propagated by NAME rather than collapsed into a sentence. `SubmitPromptError` no longer carries the arm | workspace |

`SubmitPromptError.turn_already_open` is RETIRED (landing 6, tag 8 reserved):
it never had a producer — the session watcher answers the MAIN turn's flight and
nothing in the daemon tracks a subagent's own — so the sentinel, the mapping and
the arm all went away together.

## Panel commands with no producer (server, wave 3a)

`server.Panels` is the prompt handler's panel source. Only `/context` has a
producer (the topbar resolver's context tree). `/status`, `/todos`, `/mcp` have
no resolver at all, and `/agents` and `/help` are ruled UNPRODUCED (Q1) — they
answer as `command_refused` before recognition ever reaches a panel. A panel
command with no producer fails LOUDLY out of the handler rather than drawing an
empty card.
