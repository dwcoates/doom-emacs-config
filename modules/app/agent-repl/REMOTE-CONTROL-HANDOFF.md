# Remote control — work handoff

Transient takeover doc. DELETE before landing. Branch: `remote-control`
(off `master`). Working directory for all paths below:
`modules/app/agent-repl/`.

## The task

Add "remote control" to agent-repl: make a workspace's session — which runs
locally (daemon → per-workspace TypeScript shim → `@anthropic-ai/claude-agent-sdk`
→ vendor CLI) — reachable from claude.ai and the Claude mobile app, via the
vendor CLI's own Remote Control bridge. This is NOT cloud execution: the agent
loop, tools and files stay on this machine; claude.ai attaches to the local
session.

## Owner rulings (settled — do not relitigate)

1. **Bypassing the shim's prompt queue is fine.** Prompts typed on claude.ai go
   straight into the vendor CLI.
2. **Per-workspace mode, daemon-owned, persisted in wsm.** Default ON for a
   repository's main worktree (`wsm.IsMainWorktree`), OFF for every other
   workspace. Any explicit toggle is recorded and wins over the default. Both
   main and non-main can be toggled (even main → off).
3. **A remote-control workspace does NOT park its shim between keep-alives.** It
   still receives every keep-alive; it just isn't stood down afterward (no
   keep-alive-cycle park, no settings-visit stand-down), so it never needs the
   pre-beat restore. **Hibernation by the idle cutoff is UNCHANGED** — it still
   hibernates these workspaces. The mode is read when each stand-down is
   decided.
4. **Chip look & feel = the persistent-wifi chip**, immediately to its LEFT,
   with a phone-outline glyph (my choice). Green disc = on, white = off.
   Clicking toggles (same for main and non-main).
5. **The bridge's claude.ai URL is an implementation detail** — not surfaced
   anywhere (not in the links menu, nowhere).
6. On every shim start, if the mode is on, re-enable the bridge, reusing the
   stored claude.ai session id so the same session is reattached rather than a
   new one listed.

## The contract (FROZEN — committed, generated, do not edit/regenerate)

Commits `e5cab96`, `b5d7c59`. Generated Go in `proto/gen/go`, TS in
`proto/gen/ts`.

- `proto/src/agentrepl/v1/endpoint_update_remote_control_mode.proto` —
  `UpdateRemoteControlMode(workspace, oneof{on,off,toggle})` →
  `success{on}` | `error{unknown_workspace, workspace_ref_mismatch,
  transferring_away, not_yet_adopted, vendor_refused{detail}}`.
- `proto/src/shim/v1/endpoint_set_session_remote_control.proto` —
  `SetSessionRemoteControl{enabled, name, reattach_bridge_session_id?}` →
  `success{enabled, bridge_session_id}` | `failure{no_session|vendor_refused, detail}`.
  Its comments are normative; the shim holds the state and reapplies it on
  every vendor process it opens.
- `proto/src/frontend/v1/topbar.proto` — `TopbarView.remote_control = 16`
  (`TopbarRemoteControl{oneof mode{on,off}; tooltip{text}}`), always set.
- `proto/src/conversation/v1/prompt_origin.proto` —
  `PROMPT_ORIGIN_REMOTE_CONTROL = 45`: a prompt typed on claude.ai, adopted by
  the shim as a turn's first row with `said` = the text, drawn as a prompt the
  user said.
- `proto/src/conversation/v1/permission.proto` —
  `AgentPermissionSuccess.answered_remotely` (tag 5): the ask was answered on
  claude.ai; the decision itself is not relayed (the gated call's own outcome
  says which way).

### How the vendor supports it (researched from bundled SDK/CLI; UNVERIFIED against a real bridge)

- `Query.enableRemoteControl(enabled, name?, opts?)` exists in `sdk.mjs` but is
  NOT in `sdk.d.ts` (internal). Sends control request
  `{subtype:"remote_control", enabled, name, reattach_session_id,
  keep_session_on_exit, work_secret}`. Response (CLI side) carries
  `{session_url, connect_url, environment_id, bridge_epoch, bridge_session_id}`.
  Typed at the SDK boundary in `src/sdk/types.ts`.
- Phone prompts appear on the stream only with CLI flag `--replay-user-messages`
  (passed via `Options.extraArgs`). Echoes are `{type:"user", isReplay:true,
  uuid, ...}`. The shim's OWN prompts echo too, with the uuid it sent — those
  must be dropped; a replay with any other uuid is a remote prompt.
- Permission answered on claude.ai first → CLI sends `control_cancel_request` →
  SDK aborts the `AbortSignal` passed to `canUseTool` (`options.signal`, which
  `permission-gate.ts` previously ignored).
- Alternative not used: the `@anthropic-ai/claude-agent-sdk/bridge` export
  (`attachBridgeSession`, `fetchRemoteCredentials`) — more code, needs an OAuth
  token from the keychain. Only fall back to it if `enableRemoteControl` fails.

**The SDK version (0.3.311, pinned) already has this; no upgrade needed.**

## Commits on the branch (pushed)

- `e5cab96`, `b5d7c59` — proto contract.
- `e3c023b` — docs: `docs/developer/remote-control.md` (new), keep-alive rule
  added to `docs/developer/shim-lifecycle.md`, pointer in `AGENTS.md`.
- `0c5438c` — Emacs: `agent-repl-remote-control-toggle` (`SPC j C`).
- `6cf6a31` — webapp: the chip + `docs/USER-GUIDE.md` entry.

## Status by part

- **proto** — DONE, committed.
- **docs** — DONE, committed (developer ref + user guide + shim-lifecycle).
- **Emacs** — WRITTEN, committed (`0c5438c`), **UNRUN** (no `emacs` in the
  cloud container). ~35 ERT cases across `lisp/test-{wire-verbs,verbs,rpc,core,
  keybindings,wire-common,input-help}.el`. Key `SPC j C`. Open: 60s command
  deadline is a guess; messages/`test-core` order unconfirmed.
- **webapp** — DONE, committed (`6cf6a31`). typecheck clean, eslint clean,
  1250 topbar+styles tests pass. WebKit suite UNRUN (no webkit browser here);
  one pre-existing integration failure (`page-mux … lets go of the page's one
  stream`) unrelated to this work.
- **daemon** — IN WORKING TREE, UNCOMMITTED (agent finishing/finished). Must
  make `go build ./...` pass (server + fakeshim need the two new methods).
  Covers: wsm layer (new migration after 90 `cache_cold`), default resolution,
  `UpdateRemoteControlMode` verb + server wiring, drain skip for `CycleDown`
  and `VisitDown` while mode on (hibernation unchanged), reapply on every shim
  bring-up (incl. adoption paths), topbar chip resolution + push,
  `PROMPT_ORIGIN_REMOTE_CONTROL` feed drawing, `answered_remotely` handling.
- **shim** — IN WORKING TREE, UNCOMMITTED (agent finishing/finished). Covers:
  `SetSessionRemoteControl` route/validate/failures, `enableRemoteControl`
  typing, `--replay-user-messages` + own-uuid filtering, foreign-replay turn
  adoption with REMOTE_CONTROL origin, `canUseTool` signal → `answered_remotely`,
  fake vendor modelling, reapply on reopen with reattach id.

## Environment quirks (cloud container — a local machine won't have these)

- **No `protoc`.** I built a protoc-compatible driver so `make -C proto all`
  regenerates the committed tree byte-for-byte (it leaves exactly one unrelated
  file,`gen/ts/conversation/v1/slash_command_pb.ts`, changed — revert it after
  any regen). A local machine with real `protoc` 29.x + the pinned
  `protoc-gen-go` 1.36.11 / `protoc-gen-connect-go` 1.17.0 / protoc-gen-es
  (per `proto/package.json`) should just use `make -C proto all`.
- **No `agent-repld` daemon running**, so `agent-repld test run` (the normal,
  required test path per the `agent-repl-tests` skill) is UNAVAILABLE here.
  Agents ran targeted tests directly at `nice -n 19`. On a real agent-repl
  host, **use `agent-repld test run` instead** — it enforces the CPU governor.
- **No `emacs`**, **no WebKit browser** (`/opt/pw-browsers/webkit-*` absent).

## What a local takeover must do (in order)

1. `git fetch && git checkout remote-control`.
2. Review the uncommitted daemon + shim trees (both agents' output). Confirm
   `cd daemon && go build ./...` and `cd agent-shim/claude/shim && npx tsc
   --noEmit -p .` pass. Commit each part separately (daemon, then shim) with the
   attribution footer below.
3. Run the real test suites via `agent-repld test run`, naming the changed
   packages/files (daemon unit + the one integration wiring test; shim vitest
   unit + integration; the Emacs ERT files; webapp vitest incl. WebKit;
   `go vet ./...` and the daemon source-scan tests). Fix what the cloud couldn't
   run: **the Emacs suite and the WebKit suite have never executed.**
4. **The go/no-go spike that nothing here could do:** on a real Mac with the
   account logged in, start a shim session, enable remote control, open the
   session on a phone, and verify (a) a prompt typed there runs locally and
   streams back as a user turn, (b) a permission answered there closes the
   Emacs/webapp card as `answered_remotely`, (c) a stop there ends the turn,
   (d) reattach reuses the same claude.ai session across a shim restart. All the
   vendor-behaviour claims above are read from minified code and UNVERIFIED.
5. Reconcile `docs/developer/remote-control.md` with what the daemon/shim agents
   actually built (e.g. whether enabling can bring up the vendor process with no
   first prompt — an open shim question), delete this handoff file, land via the
   merge queue (`agent-repl-merge-queue`).

## Attribution footer for commits/PRs on this branch

```
Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01XJaqUchPMkBqRBkprHDNVR
```
