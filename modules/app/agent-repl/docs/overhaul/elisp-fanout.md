# Elisp overhaul — fanout spec (teamlead-authored, binding for every elisp agent)

This is the shared contract the parallel implementation agents code against.
Function names and representations below are FIXED so agents in separate
worktrees agree without seeing each other's code. Behavior comes from the
protos (authoritative) and docs/overhaul/elisp.md; this file only pins names,
shapes, seams and ownership. When a name here conflicts with a proto comment,
the proto wins and the agent reports the conflict.

## 0. Ground rules (every agent)

- Work only in your assigned worktree; verify `git rev-parse --show-toplevel`
  and `git branch --show-current` first. Never touch ~/.config/doom, never
  hot-load into any running Emacs. Batch tests only.
- Every test process exports `AGENT_REPL_FORBID_VENDOR_CALLS=1`. No real
  vendor call ever; `prompt-summary.el` (a `claude -p` exec site) refuses
  under that variable.
- Logging: every logical branch of production code logs through core.el's
  canonical API: `agent-repl--log` (debug), `agent-repl--info`,
  `agent-repl--warn` (WARNING), `agent-repl--error` (ERROR; added by the
  dead-code pre-pass). Operation names: `elisp.<module>.<operation>`.
  Dynamic values go in the context, never only in the message.
- Validation invariant: a push or response missing a non-optional field, an
  unset oneof, a oneof with two arms set, or an unknown field/arm is a
  contract breach: signal `agent-repl-wire-error` and log ERROR. Requests are
  built only from complete values; an incomplete request errors before send.
- Tests: ERT, one test file per source module (`lisp/test-<module>.el`),
  table-driven, AAA, ONE edge case per test. Old tests are not truth.
- Commit atomically on your branch; tests ride with the change they cover.
- Surface (do not improvise) any UX/API gap you hit; the teamlead rules or
  escalates.
- Proto→code mapping: one base decode/encode function per message with
  validation once; one dedicated function per non-primitive use site
  delegating to the child's base; primitives get no wrappers.

## 1. Module map (final tree of lisp/)

New files:
- `connect.el` — Connect-over-HTTP/1.1 transport (curl subprocess).
- `wire-common.el`, `wire-host.el`, `wire-roster.el`, `wire-verbs.el` — codec.
- `rpc.el` — one function per agentrepl.v1 rpc Emacs calls.
- `daemon-link.el` — connection lifecycle, WatchDaemon, handover, drain.
- `host.el` — Register/Select/WatchHostWorkspace/Adopt, treatments.
- `roster.el` — WatchWorkspaceRoster consumer: tabs, paint, finish edge.
- `verbs.el` — workspace + admin verbs as thin wrappers, health output.
- `popup.el` — the one shared editor-popup subroutine.
- `notes.el` — org notes (extracted from tasks.el).
- `testsupport/fakedaemon/` (Go) + `test-integration-*.el` — the suite.

Deleted by the pre-pass (with their `test-*.el`): rename, codex, backend,
explain-config, readiness, recovery-slo, connection-notice,
hide-project-dirs, memory-state, output-nav, sentinel, install,
external-browser, workspace-status-export, tasks (org notes → notes.el),
open-fence, failure, frontend-uds, frontend-state, frontend-client, sidebar,
permission, transcripts, context, context-cost, ai-title.

Kept and adapted (owner in §15): core, prompts, workspace, frontends,
notifications, history, status, autosave, input, clipboard-image, commands,
session, daemon, prompt-queue, services, frontend, webview-recovery,
prompt-summary, window, sibling-popup, panels, open-progress, worktree,
keybindings, magit, emoji, prevent-select, close-panels-on-open,
interaction-record. `merge-handlers.el` and `workspace-create-client.el`
are deleted by the verbs agent once verbs.el replaces them.

## 2. Wire representation (binding)

- Parse: `(json-parse-string s :object-type 'alist :array-type 'list
  :null-object :null :false-object :false)` → alists keyed by SYMBOLS
  spelled exactly as on the wire (protojson lowerCamel: `atMs`,
  `shimAttached`, `shutdownAnnounced`, `reloadWebapp`, `idleAsync`).
- Serialize: `json-serialize` on alists with symbol keys (lowerCamel);
  `t` / `:false` for bools; integers for int64 (Go accepts numbers);
  vectors for repeated fields (`[]` when empty); an EMPTY MESSAGE is `nil`
  (serializes to `{}`), so an empty oneof arm encodes as `(arm . nil)`.
- Decoding int64: accept an integer OR a decimal string (Go protojson emits
  strings for int64). Optional scalars absent → nil; non-optional scalars
  absent → the proto3 default (protojson omits defaults); non-optional
  MESSAGE fields absent → `agent-repl-wire-error`, except where the proto
  comment states presence is the fact (RosterRowDetail's three lines;
  RosterRowWhen's oneof may be unset).
- Decoded elisp shape: a plist with kebab-case keywords per field
  (`:at-ms`, `:shim-attached`); a oneof decodes to `(:arm KEYWORD :value V)`
  where KEYWORD is the arm's kebab keyword (`:shutdown-announced`,
  `:merge-parked`, `:idle-async`) and V is the decoded arm message (nil for
  an empty arm); repeated → list; optional message absent → nil.
- Unknown keys anywhere → `agent-repl-wire-error` (unknown-field refusal,
  the same strictness generated clients have). Unknown oneof arm → error.
- Naming: `agent-repl-wire-decode-<message-kebab>` /
  `agent-repl-wire-encode-<message-kebab>` (base, validation lives here);
  `agent-repl-wire-decode-<message-kebab>-<field-kebab>` per non-primitive
  use site. Examples: `agent-repl-wire-decode-host-workspace`,
  `agent-repl-wire-decode-host-session-live-composer`,
  `agent-repl-wire-encode-create-workspace-request`.
- `agent-repl-wire-error` is a `define-error` in wire-common.el with data
  `(MESSAGE-NAME FIELD REASON)`.

## 3. connect.el (transport)

- Discovery: `(agent-repl-connect-daemon-addr-file)` = `<state
  dir>/daemon.addr` using core.el's existing state-dir resolver
  (`$AGENT_REPL_STATE_DIR`, default `~/.claude-emacs`);
  `(agent-repl-connect-read-daemon-addr)` → `"127.0.0.1:PORT"` or nil when
  the file is absent (the legal no-daemon state); malformed content signals
  `agent-repl-connect-error`.
- `(agent-repl-connect-open ADDRESS)` → a connection object (cl-defstruct
  `agent-repl-connect-connection`: address, streams, alive-p).
- Unary: `(agent-repl-connect-unary CONN METHOD JSON-STRING &key
  on-response on-failure timeout)` async; `on-response` receives the parsed
  alist; `on-failure` receives an `agent-repl-connect-error` datum. Also
  `(agent-repl-connect-unary-sync CONN METHOD JSON-STRING &optional
  TIMEOUT)` → alist or signal. Default timeout `agent-repl-connect-unary-
  timeout-seconds` (10). METHOD is the bare rpc name ("RegisterWorkspace");
  path `/agentrepl.v1.AgentRepl/<METHOD>`; headers `Content-Type:
  application/json`, `Connect-Protocol-Version: 1`. Any non-200 → parse the
  Connect error body `{code,message}` → failure. Never retried here.
- Streaming: `(agent-repl-connect-stream CONN METHOD JSON-STRING ON-PUSH
  ON-CLOSE)` → stream object (`agent-repl-connect-stream`: process, method,
  conn). Request `Content-Type: application/connect+json`, body = one
  envelope (flag byte 0x00, u32 big-endian length, JSON). Response =
  envelopes; flag 0x02 = EndStreamResponse with JSON `{}` or
  `{"error":{...}}`. No compression negotiated. `ON-PUSH` receives each
  parsed alist. `ON-CLOSE` receives `(:cancelled)`, `(:ended)` (end frame
  without error — for a standing stream the caller treats it as a failure),
  or `(:error DETAIL)` (end frame with error, HTTP failure, or process death
  without an end frame — logged ERROR here). `(agent-repl-connect-stream-
  cancel STREAM)` kills the curl process; that is the graceful close.
- Mechanism: `curl --http1.1 -sS --no-buffer` via `make-process` with a
  filter; envelope parsing is a pure function `(agent-repl-connect-envelope-
  feed PARSER BYTES)` → list of `(FLAGS . PAYLOAD-STRING)` frames, unit-
  tested without processes. HTTP status is read from `-D -` headers or an
  equivalent; the implementer chooses and documents.
- ON-PUSH exceptions are caught at the filter boundary: log ERROR with the
  payload in context; the stream stays open.
- LANDED SHAPES (connect.el as merged): the failure datum handed to
  `:on-failure` and carried in `(:error DETAIL)` is the plist
  `(:kind K :code CODE :status STATUS :message MSG)` with `:kind` one of
  `:http`, `:transport`, `:timeout`, `:malformed`, `:malformed-addr`,
  `:no-end-frame`. `(agent-repl-connect-close CONN)` marks the connection
  dead and cancels every standing stream as `(:cancelled)` — daemon-link's
  teardown primitive. `agent-repl-connect--spawn-curl` is the single spawn
  point, registered in `agent-repl--external-boundary-functions`. The HTTP
  status is read from `curl -D -` header blocks for unary and streams alike.
- LANDED SHAPES (rpc.el as merged): request encoders are called for EMPTY
  request messages too (`agent-repl-wire-encode-watch-daemon-request`,
  `-watch-workspace-roster-request`, `-daemon-health-request` receive nil);
  `agent-repl-rpc-watch-host-workspace` hands its encoder `(:workspace REF)`.

## 4. rpc.el

- `agent-repl-rpc-<method-kebab>`: unary verbs take CONN plus the decoded
  request plist, with `&key on-response on-failure`; `on-response` receives
  the DECODED response plist `(:arm :success :value ...)` /
  `(:arm :error :value ...)`. A `-sync` variant exists for each (tests,
  doctor). Streams: `(agent-repl-rpc-watch-host-workspace CONN REF ON-PUSH
  ON-CLOSE)`, `(agent-repl-rpc-watch-daemon CONN ON-PUSH ON-CLOSE)`,
  `(agent-repl-rpc-watch-workspace-roster CONN ON-PUSH ON-CLOSE)`; ON-PUSH
  receives the decoded push plist. A push that fails decoding is logged
  ERROR (`elisp.rpc.push-invalid`) with the raw JSON in context and dropped;
  the stream continues.
- REF is the decoded WorkspaceRef plist `(:id "..." :dir "...")`, echoed
  verbatim; never constructed from a path.

## 5. Codec scopes

- wire-common.el: WorkspaceRef, RepositoryRef (both directions); UserSaid,
  UserContent, UserContentBlock, TextBlock, ImageBlock(+Path/Url,
  media_type) (ENCODE; Emacs produces, never decodes); PromptOrigin (ENCODE
  as the enum's string name, e.g. `"PROMPT_ORIGIN_USER_SENT"`; the elisp
  value is the kebab keyword `:user-sent`; UNSPECIFIED is refused before
  send); DrainReason + arms (both); WorkspacePriority (encode; decode not
  needed); TurnId (decode); shared helpers (oneof, int64, uint32, optional,
  unknown-key check); `agent-repl-wire-error`.
- wire-host.el: RegisterWorkspace{Request,Response,Success,Error};
  SelectWorkspace{...}; WatchHostWorkspaceRequest, WatchHostWorkspaceResponse
  with the whole HostWorkspace tree (session none|existing; existing.id +
  standing live|terminal; live: generation, shim_attached, vendor_info
  claude, backfill 4 arms, composer 5 arms, faults; naming), Host
  WorkspaceNotification + HostNotificationKind arms, Transferred,
  ReloadWebapp, and the landed arm `open_in_editor {path, optional uint32
  line}` (decoded to `(:path P :line L-or-nil)`); AdoptHostWorkspace{...}; WatchDaemonRequest,
  WatchDaemonResponse (shutdown_announced with cause arms, drain_scheduled,
  drain_cancelled).
- wire-roster.el: WatchWorkspaceRosterRequest/Response and the whole
  frontend.v1 WorkspaceRoster tree: RosterRepositoryView, RosterTaskView,
  RosterRepoSection (RosterRepoKey), RosterTaskSection (RosterTaskKey,
  RosterTaskSectionHeader, RosterTaskDone), RosterMergedSection,
  RosterSectionHeader, RosterLabel, RosterRows, RosterRow (workspace,
  optional attention, optional priority badge, name, the 23 status arms,
  current, children recursive, when with 2 arms or unset, detail with
  presence-optional lines, closed), RosterCurrentWorkspace.
- wire-verbs.el: CreateWorkspace (request with both forms, parent+fork,
  model, priority, allow_ungated, merge_actions, base_ref, name,
  initial_prompt; response); Open/Close(blocked arm)/Kill/Nuke/Merge/
  Restart(force)/SetWorkspacePriority (absent priority = clear); SubmitPrompt
  (request said + idempotency_key + REQUIRED origin, feed omitted;
  response: success turn {TurnId} | command_panel — decode only the ARM
  KEYWORD and keep the panel payload as the raw alist |
  command_refused{command} (decoded `(:command "/agents")`) | error
  merging);
  UpdateShutdownSchedule (3 arms); UpdateMergeQueue (3 arms);
  DaemonHealth (healthy | unhealthy{faults[{detail}]}); SessionHealth.
- Empty error messages decode to `(:arm :error :value nil)`; a future arm
  is an unknown key → loud error (by design: the teamlead threads new arms).

## 6. daemon-link.el

- `(agent-repl-link-connect)`: read daemon.addr; nil → run
  `agent-repl-link-no-daemon-functions` (daemon.el hooks cold start) and
  return nil; else open the conn, start WatchDaemon; on the stream's first
  successful open run `agent-repl-link-up-functions` with CONN.
- `(agent-repl-link-primary)`, `(agent-repl-link-successor)`,
  `(agent-repl-link-up-p)`.
- The WatchDaemon stream closing other than by cancel = link down: run
  `agent-repl-link-down-functions` (CONN), then reconnect: poll daemon.addr
  every `agent-repl-link-reconnect-interval-seconds` (1, backing off to 5)
  until a conn opens and WatchDaemon stands; then the up hooks run again
  (host.el re-registers and re-subscribes; roster.el re-subscribes).
- `shutdown_announced` with address: open the successor conn, WatchDaemon on
  it, record it, run `agent-repl-link-handover-functions` (OLD NEW). When
  the old conn's daemon stream later closes after a handover, PROMOTE the
  successor to primary silently (no down/up hooks: workspaces were
  adopted). Without address (plain bounce): set the quiet-until instant
  `minted_at_ms + expected_outage_ms` (ms epoch, compared against
  `(* 1000 (float-time))`); the reconnect loop waits until then before
  polling; the indicator reads "daemon restarting (<cause>)".
- `drain_scheduled` / `drain_cancelled`: keep `agent-repl-link-drain` (nil or
  `(:at-ms N :reason PLIST)`), run `agent-repl-link-drain-functions`, and
  draw the standing indicator as a `global-mode-string` segment
  `agent-repl-link-drain-segment`: "drain HH:MM · deploy" / "· maintenance"
  / "· <operator note>". This is the teamlead's choice of indicator.
- daemon-link never issues Register/Select/verbs.

## 7. host.el

- State: `agent-repl-host--by-name` hash WS-NAME → plist `(:ref REF :conn
  CONN :stream STREAM :host HOST-PLIST)`. Accessors:
  `(agent-repl-host-ref WS)`, `(agent-repl-host-conn WS)`,
  `(agent-repl-host-state WS)`, `(agent-repl-host-backfill WS)` → arm
  keyword or nil, `(agent-repl-host-faults WS)`.
- `(agent-repl-host-register CONN DIR ON-DONE)`: RegisterWorkspace; success
  → ON-DONE receives REF; error arm → `agent-repl--error` and ON-DONE nil.
- `(agent-repl-host-select WS)`: SelectWorkspace on the conn owning WS;
  records `agent-repl-host-last-selected-id`. Called from workspace.el's
  perspective-activated hook (`agent-repl--ws-add-activated-hook`).
- `(agent-repl-host-subscribe CONN WS REF)` / `(agent-repl-host-unsubscribe
  WS)`: one WatchHostWorkspace per open workspace; unsubscribe cancels.
- `(agent-repl-host-composer-gate WS)` → `:open` | `:merge-parked` |
  `:merging` | `:draining` | `:restarting` | `:no-session` | `:terminal` |
  `:unknown` (no push yet). Fixed treatments (input.el enforces): `:open`
  send; `:merge-parked` send, with the input mode-line badge "merge parked —
  prompts go to the resolution agent"; `:merging` refuse "composer closed: a
  merge owns this session"; `:draining` refuse "composer closed: daemon
  draining"; `:restarting` refuse "composer closed: restarting";
  `:no-session` / `:terminal` SEND (ruled: SubmitPrompt has no
  precondition; the daemon starts or revives the session implicitly);
  `:unknown` (no host push yet) SEND as well, logging INFO — the daemon is
  the authority and answers with its own refusal arms.
- Naming: tab label = the ROSTER row name (roster.el); buffer titles use
  `naming.title`, else `naming.slug`, else the row name. `(agent-repl-host-
  display-title WS)` exposes it.
- `agent-repl-host-update-functions` (WS HOST-PLIST) runs after every host
  push. shim_attached=false has NO treatment (parked is invisible by design).
- Notification policy, at the arm, per the proto comment: Emacs unfocused
  (`agent-repl--emacs-focused-p` nil) → `agent-repl--notify` with the text;
  its click raises the frame and `agent-repl--ws-switch`es to WS. Focused
  and WS not the selected tab → `(agent-repl-status-blink-tab WS)`. Selected
  → log only. `permission_requested` follows the same policy (the text is
  daemon-composed; tool_name goes into the log context).
- `transferred`: NEW = `(agent-repl-link-successor)`; nil → ERROR log
  (`elisp.host.transferred-without-successor`), keep the stream. Else call
  AdoptHostWorkspace on NEW; success → cancel the old stream, subscribe on
  NEW, update `:conn`; error arm → ERROR log, keep the old stream.
- `reload_webapp` → `(agent-repl-frontend-reload-webview WS)`.
- `open_in_editor` (Q2 ruling) → `(agent-repl-popup-open PATH LINE)` — the
  ONE shared subroutine; a directory opens in dired. Log INFO with the path.
- On `agent-repl-link-up-functions`: for every live workspace
  (`agent-repl--live-ws-names`) register its dir, then subscribe. On link
  down: mark streams gone; keep the last host state.

## 8. roster.el and status.el

- `(agent-repl-roster-subscribe CONN)`; `agent-repl-roster-view` holds the
  last decoded roster; `agent-repl-roster-update-functions` (ROSTER).
- Tab reconciliation `(agent-repl-roster-reconcile ROSTER)`: walk
  `repository.sections` in order and rows depth-first (row, then its
  children), then `recently_merged.rows`. A row with `closed` false → ensure
  a tab exists (workspace.el creates it with `:ref`, `:dir` = ref.dir,
  `:name`); `closed` true → ensure no tab (teardown is idempotent). Tab
  order = walk order, strictly; no local ordering. The task view is ignored
  (the same rows regrouped). A row whose ref id matches an existing tab is
  the same workspace whatever its name; a rename of the row renames the tab.
- Tab naming: the row's name text; on collision within the roster, append
  "·<repo label>".
- `current`: if `current.workspace.id` differs from the selected tab's ref
  id AND from `agent-repl-host-last-selected-id`, switch to that tab (R8;
  the resulting SelectWorkspace is idempotent, no loop).
- Paint: `(agent-repl-status-tab-state WS)` → the row's status arm keyword.
  `agent-repl-status-color-table` maps every one of the 23 arms to exactly
  one of blue/purple/red/yellow/green/none and is ASSERTED row for row
  against `proto/vocab/render-colors.json` (landed on this branch):
  `roster_status` (arm → color) composed with
  `surface_overrides.emacs_tab_bar` (merge_enqueuing, merge_queued, merging
  → purple; vendor_blocked → blue), `merge_glyphs` (queue / recycle /
  conflict / failed / check → the tab glyph), and `precedence` (blue purple
  red yellow green). Teal and RENDER_STATE_* are gone from the file and
  from elisp. inactive → none with a "?" glyph. Attention present → blink
  once (below) then a steady marker until the marker leaves the row.
  Priority badge label draws before the name. Teal, hibernated, the local
  state machine, poll timers, git ticks, spread and stale thresholds are
  deleted; the ready-shout-then-fade dwell and bracket-only paint stay as
  local modifiers.
- Blink: `(agent-repl-status-blink-tab WS)` implements frontend.v1
  RosterRowAttention's cadence exactly — marker on at 0 ms, off at 500,
  on at 1000, off at 1500, steady on from 2000 — and its docstring cites
  the message. The test asserts the exact timer schedule.
- Finish edge: RUNNING = {submitting thinking clearing compacting
  permission}; SETTLED = {ready done interrupted idle-async}. A row moving
  RUNNING → SETTLED runs `agent-repl-roster-finish-functions` (WS) once.
  Registered reactions: (1) unfocused desktop banner "Agent ready: <name>";
  (2) cross-workspace echo `message` when WS is not the selected tab;
  (3) magit-status refresh for the dir; (4) deferred-prompt drain
  (prompt-queue.el registers this one).
- The render-colors.json assertion test reads the file at test time and
  fails on any row divergence or any arm missing on either side.

## 9. verbs.el

- `(agent-repl-verb-close WS)`, `-kill`, `-nuke`, `-open` (REF of a closed
  row), `-merge`, `(agent-repl-verb-restart WS FORCE)`,
  `(agent-repl-verb-create REPO-REF FORM &rest FACTS)`,
  `(agent-repl-verb-set-priority WS PRIORITY-OR-NIL)`,
  `(agent-repl-verb-shutdown-schedule ACTION)`,
  `(agent-repl-verb-merge-queue ACTION)`. Each resolves REF via
  `agent-repl-host-ref` (nil → `user-error`), CONN via `agent-repl-host-
  conn` (falls back to `agent-repl-link-primary`), sends async, and on the
  ack: success → the editor-state update; error arm → `agent-repl--warn` +
  `message`; transport failure → `agent-repl--error` + `message`.
- Editor-state updates: Close success → tear the tab down (workspace.el;
  idempotent against the roster's reconciliation); Close `blocked` → log
  INFO + `message "close blocked — see the workspace footer"`, no dialog;
  Kill/Nuke success → tear the tab down; Merge success → `message "merge
  enqueued"`, nothing else (Emacs holds no merge state); Restart success →
  `message`; Create success → nothing (the roster push opens the tab).
- Interactive commands and keys: `agent-repl-close-workspace` (SPC j x),
  `agent-repl-kill-workspace`, `agent-repl-nuke-workspace` (y/n confirm:
  data destruction), `agent-repl-open-workspace` (completing-read over
  closed rows), `agent-repl-merge-workspace` (SPC TAB M),
  `agent-repl-restart-workspace` (SPC o C-c; prefix arg = force),
  `agent-repl-create-workspace` (repo from the roster's sections, default
  the current workspace's; prompt, optional name/base_ref; prefix arg =
  child of the current workspace; fork/model/priority via arguments),
  `agent-repl-create-oneshot-workspace` and variants replacing the
  doom/explanation-engine one-shot commands (self_merge, open_pr with
  self_certified/add_to_merge_queue; model from
  `agent-repl-oneshot-model-candidates`), `agent-repl-set-priority`,
  `agent-repl-daemon-shutdown-schedule` / `-cancel` / `-now`,
  `agent-repl-merge-queue-pause` / `-resume` / `-evict`,
  `agent-repl-daemon-health` and `agent-repl-session-health` (render into
  `*agent-repl-health*`: verdict, each fault's detail, plus the host
  stream's standing faults for the workspace).
- worktree.el is slimmed to what has no wire successor and is blessed:
  the eval helpers (`agent-repl--eval-code-string`, reachable via
  emacsclient for /runtime-eval-code), print-branch helpers; every creation
  flavor, git removal, gns close, clipboard, PGN, profiler and heartbeat
  code dies. workspace-create-client.el and merge-handlers.el are deleted.

## 10. input.el (composer) and commands.el

- `agent-repl--send` pipeline: text → `agent-repl--prepare-input`
  (metaprompt prepend with the sentinel markers via `agent-repl--meta-wrap`,
  prefix/postfix variants) → `(agent-repl-host-composer-gate WS)` treatment
  → UserSaid `(:content (:blocks (...)))` where the blocks are the text
  block(s) plus one `(:arm :image :value (:location (:arm :path :value
  (:path P)) :media-type M))` per image attached through clipboard-image.el
  (ruled: pasted images travel as ImageBlock{path}; the composer keeps a
  per-buffer list of attached images and their MIME types, drawn as the
  existing thumbnail overlay, cleared on a successful send) →
  `agent-repl-rpc-submit-prompt` with `(:said SAID :idempotency-key
  (agent-repl--uuid) :origin ORIGIN)` (RFC 4122 v4 from `random`; ORIGIN
  is the send site's keyword, REQUIRED) → success `:turn` →
  clear the input, push history, run `agent-repl-send-posthooks`; success
  `:command-panel` or `:command-refused` → "answered, nothing to await":
  log INFO (`elisp.input.command-answered` with the arm), clear the input
  (the webapp draws the panel or refusal as feed rows; Q3 ruling); error `:merging` → keep the text, `message` + a
  mode-line flash "refused: merge in flight"; transport failure → keep the
  text and offer it to prompt-queue.el (drained on link-up).
- Prompt origins ride the wire (landed: SubmitPromptRequest.origin is
  REQUIRED, never UNSPECIFIED): each send site passes its own keyword,
  also logged in the submit's context. Sites: user-sent, user-sent-and-hide,
  user-sent-with-metaprompt, user-sent-with-postfix, user-sent-with-prefix,
  metaprompt-read, command-diff-analysis, command-explain-context,
  command-explain-prompt, command-update-pr, command-rebase,
  command-create-or-update-pr, deferred-prompt. Dead sites get no constant.
- Composer slash/skill completion dies. The output-nav bindings die. The
  input history (persistence, fuzzy search, glyph) stays.
- prompt-queue.el keeps two roles: explicit deferral
  (`agent-repl-queue-deferred-prompt`, drained on the finish edge through
  `agent-repl-roster-finish-functions`) and the outage queue (drained on
  `agent-repl-link-up-functions`). Its liveness gate is
  `agent-repl-link-up-p` and the composer gate.
- commands.el: the canned-prompt families (explain / diff / PR / rebase /
  tests / lint) compose text and send through the composer pipeline with
  their site's origin; `agent-repl-link-code` opens via
  `agent-repl-popup-open`; snapshot save/load/archive, push/pull tab,
  paste-clipboard, switch-to-N and the restore machinery die.
- clipboard-image.el keeps capturing the pasteboard image to the workspace
  dir and drawing the thumbnail, but registers the file as an attached
  ImageBlock{path, media_type} on the input buffer instead of inserting a
  path token. prompt-summary.el stays with the FORBID guard.

## 11. daemon.el and services.el (cold start)

- `(agent-repl-daemon-ensure &optional ON-READY)`: hooked on
  `agent-repl-link-no-daemon-functions` and run at Emacs startup when
  `agent-repl-frontend-auto-start`. Sequence: daemon.addr present → probe
  DaemonHealth over a fresh conn; any ANSWER (healthy or unhealthy) = a
  daemon is there → adopt it (INFO `elisp.daemon.foreign-adopted` when this
  Emacs did not spawn it; unhealthy faults go to `*agent-repl-health*` as a
  WARNING); a transport failure = a stale file → WARNING
  `elisp.daemon.stale-addr`, treated as absent. Absent → build via
  `agent-repl-daemon-build-script` (bin/build-frontend.sh, its own
  build-if-stale) → start `agent-repl-daemon-command` (default the module's
  `daemon/bin/claude-repld`, no argv) with `AGENT_REPL_STATE_DIR` exported
  explicitly → wait for daemon.addr up to `agent-repl-daemon-boot-timeout-
  seconds` (30) polling with a timer, no sleeps → `agent-repl-link-connect`.
  Build failure → `*agent-repl-build-frontend*` shown, WARNING, `message`,
  the mode-line segment "daemon: build failed"; no automatic retry;
  `agent-repl-frontend-daemon-ensure` (interactive) retries.
- Emacs never kills a daemon that answers. `agent-repl-frontend-daemon-stop`
  = UpdateShutdownSchedule{now, operator "emacs"}; `-restart` = stop then
  ensure. Legacy `agent-repl-daemon-addr`, sentinel-era expected-restart
  bookkeeping and all UDS probing die.
- services.el keeps launchd management of store/sidecar with UDS/readiness
  references removed; `agent-repl-runtime-restart` = build script + store/
  sidecar bounce + daemon stop + ensure.

## 12. frontend.el, webview-recovery.el, open-progress.el, panels.el, popup.el

- `(agent-repl-frontend-webview-url WS)` =
  `http://<address of (agent-repl-host-conn WS)>/?workspace=<url-hexify
  id>&dir=<url-hexify dir>` — both values verbatim from the WorkspaceRef
  (RegisterWorkspace's answer / the roster). Nothing else rides the URL
  (no composer flag).
- The pool: pre-creation and staggering stay (`agent-repl-webview-precreate-
  stagger-seconds`), scheduled after link-up; each webview is bound to its
  workspace buffer for life; `agent-repl-frontend-rescue-webview` (SPC o L)
  survives. `(agent-repl-frontend-reload-webview WS)` navigates the widget
  to the current URL. Every `xwidget-webkit-execute-script` call and every
  script helper (tail, chess, close-menus, text-size, copy-selection,
  recovery probe) is deleted; the stale-webview sweep dies.
- open-progress.el stages: `:requested` (verb sent) → `:acked` → `:host-
  state` (first host push for WS) → `:loaded` (xwidget load finished); the
  stall diagnosis names the first missing stage.
- panels.el's `agent-repl` entry: ensure the current workspace is registered
  and subscribed (host.el), mount its webview, show the input buffer.
- popup.el: `(agent-repl-popup-open PATH &optional LINE)` — directory →
  dired; file → `find-file-noselect` then goto LINE; shown with
  `display-buffer-in-side-window` on the right at half the frame width.
  ONE implementation; commands.el's link-code and notes.el call it. No
  wire caller until Q2.

## 13. Dead pre-pass

Deletes the files in §1 and their tests; removes their entries from
config.el's load list and doctor.el; removes dead keybindings
(rename SPC TAB r, hibernate SPC o z, explain-config SPC j h c/C/n,
hide-project-dirs SPC o H, push/pull tab SPC TAB p/P, switch-to-N SPC TAB
1..0, the whole agent-repl-debug/* family and dump renderer, output-nav
bindings); extracts tasks.el's org-notes helpers into notes.el
(`agent-repl-notes-open` keyed by workspace name, popup via
`agent-repl-popup-open` once it exists — until then `find-file`); adds
`agent-repl--error` to core.el (level "error", persisted and displayed like
`agent-repl--warn`) with tests; removes core.el's symlink migration helpers
and UDS probes; prunes `agent-repl--external-boundary-functions` and
test-helpers.el's clean-state macro and stubs of deleted symbols; rewrites
magit.el's browse-url consumers to plain `browse-url`; removes teal/
hibernated from status.el's tables and faces; updates test-agent-repl.el to
list exactly the surviving suites. Definition of done: `emacs -batch -Q -l
ert -l lisp/test-helpers.el` loads config.el with zero load errors,
test-agent-repl.el references only existing files, test-core.el and
test-notes.el pass. Other surviving suites may be red (their owners rewrite
them).

## 14. Integration suite (fake daemon)

- `lisp/testsupport/fakedaemon/` — a Go program (module
  `agentrepl/fakedaemon`; `replace agentrepl/proto => ../../../proto/gen/go`;
  `connectrpc.com/connect v1.17.0`; `golang.org/x/net v0.43.0` (h2c);
  `google.golang.org/protobuf v1.36.11`; `go 1.23` directive; builds
  OFFLINE with `GOFLAGS=-mod=mod GOPROXY=off go build`). It binds
  127.0.0.1:0, writes `$AGENT_REPL_STATE_DIR/daemon.addr` exactly per the
  common contract (address plus newline, atomic replace; removed on orderly
  exit), serves agentrepl.v1 through the generated Connect handler
  (JSON codec, HTTP/1.1 and h2c), and records every request. Control plane
  on the same mux under `/_fake/`: `POST /_fake/script` sets canned unary
  responses per method (protojson bodies, validated by unmarshalling into
  the generated types); `POST /_fake/push` `{stream, workspace_id?,
  message}` pushes a protojson message to matching open subscribers
  (WatchHostWorkspace by workspace id; WatchDaemon; WatchWorkspaceRoster);
  `POST /_fake/end` `{stream, workspace_id?, error?, abort?}` ends a
  stream with an end frame (optionally carrying a Connect error) or, with
  `abort`, drops the TCP connection without one; `GET /_fake/calls` returns
  the recorded requests in order `[{method, body}]`; `GET /_fake/subscribers`
  lists open streams; `POST /_fake/exit` exits orderly. Unknown fields in
  elisp-sent requests are refused by protojson (that is the round-trip
  check). Two instances can run at once (handover scenarios).
- `lisp/test-integration-helpers.el` (batch-only, like test-helpers.el):
  builds the binary once per run, starts an instance with a private state
  dir, exports `AGENT_REPL_FORBID_VENDOR_CALLS=1`, and provides
  `agent-repl-itest--with-fake-daemon`, `--script`, `--push`, `--end`,
  `--calls`, `--wait-until` (polls with `accept-process-output` under a
  deadline; no sleeps), `--start-second-daemon`. A missing `go` toolchain
  FAILS the suite loudly (no skip). Fake webview factory and fake notifier
  backend record calls.
- Suites, one per production module: `test-integration-connect.el`,
  `-host.el`, `-link.el`, `-roster.el`, `-composer.el` (input.el),
  `-verbs.el`, `-daemon.el`. Scenarios (from the API):
  1. daemon.addr read; RegisterWorkspace round-trip; camelCase keys; id
     echoed verbatim; error arm → loud error; unknown-field refusal.
  2. Re-register + re-subscribe after a daemon restart (second instance on
     a new port rewrites daemon.addr).
  3. Host stream: snapshot then pushes; every composer arm → gate value;
     naming → display title; faults → health buffer; terminal / none →
     blocked; shim_attached false → still open.
  4. notification: unfocused → notifier called with the text and the click
     selects the tab; focused+unselected → the exact blink schedule;
     selected → nothing; permission_requested same policy.
  5. Handover: shutdown_announced{address} → dual attach; transferred →
     AdoptHostWorkspace on the new instance, then the old stream is
     cancelled (old instance observes the disconnect), then re-subscribe on
     the new; order asserted; old stream closing promotes the successor.
  6. Plain bounce (no address): no dual attach; quiet window honored;
     reconnect after daemon.addr reappears.
  7. reload_webapp → the webview reload is called for that workspace only.
  8. drain_scheduled → indicator text per reason arm; drain_cancelled →
     removed; a late subscriber receives the standing schedule.
  9. Roster: all 23 arms decode; unknown arm, unset oneof, two arms → ERROR
     logged, push dropped, stream continues.
  10. Tab reconciliation: rows appear → tabs in walk order; closed → tab
      torn down; reorder → tabs reorder; daemon-originated current change →
      tab switch; Emacs's own switch → exactly one SelectWorkspace.
  11. Finish edge: thinking→done fires the four reactions once;
      permission→thinking does not.
  12. SubmitPrompt: text→UserSaid; uuid key; success → history push and
      cleared input; merging error → text preserved; gate merging → no rpc;
      merge_parked → rpc sent; metaprompt markers; prefix/postfix.
  13. Verbs: each sends the right request echoing the ref; Close blocked →
      no dialog, tab stays; Restart{force}; Create standard and both
      one-shot forms with parent/fork/model/priority; SetWorkspacePriority
      clear omits the field; schedule requires a reason; DaemonHealth
      unhealthy → faults printed.
  14. Cold start: no daemon.addr → build script (stub) invoked → daemon
      command (stub that starts the fake) → link up; stale addr → treated as
      absent; build failure → buffer + WARNING, no start; an answering
      daemon → adopted, no build.
  15. Validation: a HostWorkspace push without `naming` → ERROR, dropped.
  16. Streams: producer close without end frame → ERROR + reconnect; end
      frame with error → ERROR.

## 15. Ownership and seams

| Agent | Owns | Defines (others call) | Calls (others define) |
|---|---|---|---|
| pre-pass | deletions, core.el `agent-repl--error`, notes.el, keybindings prune, test-helpers prune | `agent-repl--error` | — |
| connect | connect.el, rpc.el | §3, §4 | wire-* (by name) |
| wire-a | wire-common.el, wire-host.el, wire-roster.el | §5 | — |
| wire-b | wire-verbs.el | §5 | wire-common (by name) |
| link | daemon-link.el | §6 | rpc, connect |
| host | host.el, notifications.el adaptation | §7 | rpc, link, status blink, frontend reload, workspace.el |
| roster | roster.el, status.el, workspace.el, session.el (finish reactions) | §8 | rpc, host, workspace.el |
| composer | input.el, commands.el, history.el, prompt-queue.el, prompts.el, clipboard-image.el, prompt-summary.el | §10 | rpc, host gate, popup |
| verbs | verbs.el, worktree.el, doctor.el, delete merge-handlers.el + workspace-create-client.el | §9 | rpc, host, link, workspace.el |
| cold-start | daemon.el, services.el, startup wiring in config.el/panels.el boot | §11 | rpc (DaemonHealth), link |
| webview | frontend.el, webview-recovery.el, open-progress.el, panels.el, window.el, popup.el | §12 | host, link |
| integration | testsupport/fakedaemon, test-integration-*.el | — | everything (by name) |

Keybindings.el is shared: each owner edits only its own commands' lines; the
teamlead resolves merge seams.

## 16. Escalations sent to the project lead (defaults in force meanwhile)

- E1 RESOLVED: SubmitPromptRequest.origin landed, REQUIRED.
- E2 RESOLVED: the trimmed vocabulary landed (cherry-pick of 24740ae4f);
  §8 keys from it.
- E3 RULED as the default: `daemon/bin/claude-repld`, no required argv,
  state root via env; adopt any answering daemon; never kill one.
- E4 CONFIRMED: permission.el dies; the notification policy is the whole
  reaction.
- E5 RULED: merged, closed and killed rows carry closed=true; nuked rows
  leave the roster; tabs derive from closed=false rows in roster order.
- E6 CONFIRMED: no task verbs in Emacs; org notes stay local.
- Soft spots RULED: submitting on a none/terminal session simply submits;
  pasted images travel as ImageBlock{path}.
- E7 transcripts.el (resume choice), ai-title.el (naming.title replaces),
  workspace-status-export.el (the roster stream replaces; the
  create-or-update-workspace skill's status source dies) removed by API
  absence.
