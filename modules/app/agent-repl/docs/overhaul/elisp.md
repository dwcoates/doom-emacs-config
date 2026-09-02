# Elisp implementation planning

## Dead code to remove
- The `:hibernated` render state, whole: its decode, teal color
  (`agent-repl--color-hibernated-teal`), tab palette row, and face in
  lisp/status.el — referenced across ~29 elisp files. Hibernation left the
  contract; a parked workspace presents as live with shim_attached=false.
- The keep-alive origin machinery: `agent-repl--context-cost-keep-alive-origin`
  and `--keep-alive-p` in lisp/context-cost.el match a retired enum value and
  now silently downgrade the keep-alive alarm — LIVE correctness issue, remove
  with the alarm's re-derivation from the new contract.
- The UDS frame/command envelope tables in lisp/frontend-uds.el
  (`--uds-known-frame-fields`, `--uds-known-command-fields`,
  `--uds-ignored-frame-fields`, ~25 arms): the whole push/command envelope has
  no successor; the transport is re-pointed at the agentrepl.v1 Connect rpcs.
- The `FailureKind` triage lists (`agent-repl-failure-machinery-kinds` /
  `-vendor-kinds` / `-client-kinds`, ~62 arms vs the contract's 17): the
  entry-correlated arms moved into feed.proto per-entry error arms; re-derive
  the coloring from the frozen vocabulary.
- `proto/vocab/render-colors.json`'s 24 RENDER_STATE_* values (CROSS-SYSTEM:
  coordinate with webapp+daemon orchestrators; the palette contracts to the
  five live colors, teal dies).

## Replacement integration-test specs
(Seeded from reconciliation's deleted pins; unit specs deliberately absent.)
- The agentrepl.v1 protojson round-trip: elisp encodes each host-section
  request (RegisterWorkspace, SelectWorkspace, WatchHostWorkspace) and decodes
  responses/pushes against the real frozen schema, including the
  WatchHostWorkspace push oneof {host | notification}.
- Roster status decode covers EVERY RosterRow.status arm the frozen contract
  declares and REFUSES unknown arms loudly (replaces the deleted
  hibernated-inclusive pin).

## Removals ruled 2026-08-28 (merge-variants + account rulings)
- Emacs's durable merged/merge-failed memory across restart: REMOVED
  (session.el's saved merge-completed restore, the re-classification
  probe); the daemon's pushed views are the only merge state.
- The merged-tab hiding/greying (tab-bar filtering, sidebar greying of
  merged workspaces): REMOVED — the information is deliberately not
  provided to Emacs.
- agent-repl-doom-multi-repo-mode: KILLED — path-under-$MULTI_REPO_ROOT
  is the only account rule; no Emacs-side widening exists.
- The intake side-effect machinery (auto-decline of parked permission
  asks on prompt, owed-redelivery cancellation): dropped for this
  project; the landed permission/question API is the only path.

## Removals ruled 2026-08-29 (final-audit triage)
- WORKSPACE RENAME (rename.el, SPC TAB r, 577 lines): DEAD — names are
  daemon-minted at creation and permanent.
- CODEX as a second backend (backend.el, codex.el): DEAD for now — the
  vendor oneof stays extensible; a future vendor is a future shim.
- explain-config (the read-only config Q&A popup): DEAD.
- The pre-close /gns-sockets agent round-trip: DEAD.
- The readiness mode-line segment AND the recovery-SLO machinery
  (readiness.el, recovery-slo.el): DEAD — blue-green + reconnect-is-reopen
  replace both.
- The durable Emacs workspace-roster snapshot: DEAD — the DAEMON is the
  source; on connect Emacs opens tabs from the roster stream; only local
  display prefs may persist locally.
- The daemon-link-degraded banner and connection-notice retraction
  machinery: DEAD.
- Client-authored tab ORDERING and HIDING: DEAD — tabs follow roster order
  strictly (the resolver orders, priority included); push/pull-tab
  commands, priority auto-reordering, and hide-project-dirs all die;
  sidebar folding affects the sidebar only.
- Composer slash/skill completion: DEAD (no data source; the panels'
  producer gap is a separate wave escalation).
- The stale-webview sweep on link-up: DEAD (reload_webapp replaces it).
- Emacs-side external-browser pinning of browse-url: DEAD (the daemon's
  OpenExternal verb is the one pinned path).
- The debug keymap (SPC j h), memory-state.el's continuous dump, and the
  sentinel reset/nuke recovery commands: DEAD.
- The skill-symlink auto-installer + pre-commit hook installer: DEAD —
  provisioning is install.sh's job.
- ALL webview key affordances installed via JS eval (copy chords, chess
  stepping, text-size controls): DEAD — no window.agentRepl* hook surface
  exists at all; the webview is purely daemon-driven.
- Per-workspace clipboard slot, arbitrary data payloads, the PGN board
  popup, and the profiler-report dump: DEAD (/runtime-eval-code SURVIVES —
  debugging depends on it).
- Sidebar keyboard navigation and the single-prompt-at-a-time guard: DEAD.
- The output-feed navigation commands (webview bubble cycling): DEAD.
- Emacs task store, org notes files, and task gestures move to the wire:
  the task-verbs increment (CreateTask/UpdateTask/AssignWorkspaceTask)
  owns tasks; org NOTES files stay Emacs-local.

## Host-native behaviors blessed 2026-08-29 (survive as Emacs-local)
- PROMPT COMPOSITION IS FREE: Emacs may compose/decorate prompt text
  freely BEFORE submission — the metaprompt prepend, the SPC j canned
  prompts (explain/test/lint/PR families), prefix/postfix send variants.
  "Verbatim" means no post-submission rewriting, not no composition; what
  is submitted is what is drawn (the daemon strips the sentinel-marked
  metaprompt spans from the DRAWN row).
- ONE-SHOTS ride the wire now: Emacs supplies {prompt, model, parentage}
  through the dedicated one-shot creation form; the DAEMON owns naming,
  worktree, decoration, and merge/PR postprocessing.
- THE OPEN-PROGRESS LADDER survives as host-native UX over
  Emacs-observable stages (request sent, ack, page load, render) with the
  stall diagnosis; no wire change.
- EMACS OWNS COLD START of the daemon: auto-start on Emacs boot,
  stale-binary rebuild-and-start, foreign-daemon handling, build-failure
  surfacing. Everything after boot is the daemon's own blue-green.
- LOCAL PRESENTATION AXIS: Emacs may compose its tab treatment from the
  pushed state arm × local-only facts (panels-dismissed bracket-only
  paint, the ready-shout-then-fade dwell) — the fixed-treatment rule
  governs the ARM's meaning, not host-local modifiers.
- WEBVIEW POOL: pre-creation and staggering are blessed performance
  machinery ("bound from FIRST MOUNT for life"); the manual rescue
  command survives as an escape hatch.
- Editor-local conveniences kept: tab-bar geometry, panel/window
  discipline, commit-emoji + hook, magit integrations, interaction
  record/replay, the autosave sweep, composer input history
  (persistence, fuzzy search, glyph), copy-reference/copy-name/
  revert-and-eval/reload-config/print-branch.

## Turn state and reactions (ruled 2026-08-29)
- EMACS SUBSCRIBES WatchWorkspaceRoster: the roster's per-row state/dot
  vocabulary is the ONE source for tab coloring and the sidebar dot (no
  HostWorkspace lifecycle axis exists; the palette's five live colors
  paint from roster arms).
- THE FINISH EDGE is the roster row's turn-running→idle transition; all
  four reactions ride it as Emacs-local policy: the unfocused "Agent
  ready" desktop banner, the cross-workspace echo, the magit-status
  refresh, and the deferred-prompt drain.
- Tab REHYDRATION: on connect, Emacs opens tabs for the workspaces the
  roster lists — the daemon is the source of which workspaces exist.

## Code-level consistency requirements (from the conventions walk)
- ONE shared subroutine backs every open-a-file affordance: "open
  path[:line] in a doom popup, right side, half width" — the plan
  bubble's edit button, every findings location, the worktree
  separation paths (dired for a directory) all call it.
- THE BLINK CADENCE is implemented exactly from the one spec on
  frontend.v1 RosterRowAttention (two blinks, 500 ms on/off, then
  steady); divergence from the webapp sidebar is a defect.


## Contract context (for implementers)

Orientation for implementation agents working the elisp side. The protos under
`proto/src/` are the contract; their comments are the authoritative
documentation — read the actual `.proto` files you work against. Cross-cutting
conventions (response spelling, echo tokens, identity vocabulary, validation
and logging invariants, the proto→code mapping) live in
`the teamlead prompt (standing conventions) and the proto comments` and are not repeated here. The
full history and rationale is the design record, which is the PROJECT LEAD's context — escalate rather than reading it.
Implementers never change protobufs — a needed change is a request up the
orchestration chain.

### What Emacs is in this system

- Emacs is a HOST, not an author. Its whole workspace contribution is two
  verbs: REGISTER a workspace (hand the daemon a dir; the daemon mints and
  returns the identity) and SELECT a workspace (the user switched tabs).
  Everything else about a workspace — parentage, branch, repo, naming, status
  — the daemon derives and pushes.
- Emacs REACTS to streams; the daemon never calls Emacs. There is no
  daemon→host command loop, no action inbox, no report-back ("I finished
  setting up" has no listener). A workspace appearing on the roster means
  open its buffers; a close means tear down — the same reactive model as the
  webapp.
- Emacs commands are THIN WRAPPERS: send the request, await the daemon's ack,
  update editor state. Every piece of real machinery (git, worktrees, session
  lifecycle, merge orchestration) is the daemon's.
- Emacs is the daemon's SINGULAR CLIENT MULTIPLEXER: one process holding the
  one WatchDaemon stream plus one WatchHostWorkspace stream per open
  workspace. Each open workspace also hosts one xwidget WKWebView (the
  webapp), bound to its buffer for life, with its own connection; the
  composer is HOST-native (the webview runs with the composer disabled).
- Transport is Connect (HTTP/2), not gRPC — chosen for exactly these clients.
  Commands are ordinary unary requests; component streams are
  server-streaming rpcs multiplexed over one connection. PROTOJSON IS THE
  ELISP CODEC: Connect serves binary or JSON per client; elisp speaks the
  same schema as the webapp in JSON. One schema, two codecs — Emacs and the
  webapp call the identical rpcs.
- Stream lifecycle: cancelling a stream IS the graceful close (no
  CloseXConnection verbs exist); a reconnect re-opens and re-pulls — streams
  are "now", never "since", and no resume token exists. After a daemon
  restart Emacs re-registers (idempotent by dir) and re-subscribes; that is
  the normal path, not an error.

### Package map, from the elisp seat

- `agentrepl.v1` — the ONLY service Emacs calls. Seven sections on one
  `service AgentRepl`: feed / sidebar / topbar / footer / daemon-hold tray
  (webapp-facing), daemon admin, host, plus the web-link section (webview
  only). One `endpoint_<rpc>.proto` per rpc; `service.proto` is the index.
- `workspace.v1` — the identity leaf: `WorkspaceRef {id, dir}` and
  `RepositoryRef {id, dir}`. Daemon-minted echo tokens: a path is never an
  identity (paths have many spellings); `id` is opaque, compared byte-wise,
  handed back verbatim; `dir` is display/normalized, never used as a key.
  Emacs obtains a ref from RegisterWorkspace's success (or the roster) and
  echoes it on every per-workspace call.
- `frontend.v1` — the webapp's drawn-component vocabulary. Emacs touches it
  in exactly ONE place: `sidebar.proto`'s `RosterRowAttention`, whose comment
  is the canonical blink-cadence spec (below). Everything else in frontend.v1
  is the webview's business.
- `conversation.v1`, `shim.v1`, `store.v1` — never Emacs's; listed only so
  nobody goes looking.

### The HOST section (agentrepl.v1)

Three rpcs plus the handover pair.

- `RegisterWorkspace { dir }` → `{ WorkspaceRef }`. Emacs provides the path in
  whatever spelling it has; the daemon normalizes, mints, returns. IDEMPOTENT
  BY DIR — re-registration after reconnect/daemon restart reconciles, one
  success answer.
- `SelectWorkspace { WorkspaceRef }` → empty success. Fired on tab switch;
  the daemon stamps `current` and last-selected, and CLEARS the workspace's
  attention marker; the roster stream reflects it. Idempotent — re-selecting
  the current workspace succeeds.
- `WatchHostWorkspace { WorkspaceRef }` — one subscription per OPEN
  workspace: snapshot first, then whole-replace per push. Closing a
  workspace cancels its stream. The response is a push oneof:
  - `host` — the `HostWorkspace` state, whole (below).
  - `notification` — an EVENT {text, at_ms}, fired not state (presentation
    policy below). TYPED KINDS (increment ruled 2026-08-29): the push
    gains a kind oneof (agent_addressed | permission_requested | ...)
    beside the composed text; a permission ask FIRES this push and sets
    the attention marker — Emacs's focus policy needs no new logic.
  - `transferred` — daemon handover: this (old) daemon released the
    workspace (below).
  - `reload_webapp` — webapp-only rollout: reload this workspace's xwidget
    against the SAME daemon; empty by design (no address — the daemon is not
    changing; a combined daemon+webapp rollout never sends it because the
    handover re-attach pulls fresh assets).

The flow, as designed: Emacs connects → RegisterWorkspace per known worktree
→ per OPEN workspace one WatchHostWorkspace subscription → closes cancel → a
daemon restart drops the streams and Emacs re-registers and re-subscribes.
There is no global host-workspace stream — workspace-dependent and
workspace-independent channels are distinct types by ruling, which is why
WatchDaemon exists separately.

### HostWorkspace, abstractly

The host-facing state of one workspace: what Emacs needs to correlate
processes, gate its composer, and manage buffers — nothing a webview draws.
Emacs renders FIXED TREATMENTS PER ARM, never mapping values (ordinary oneof
rendering; the old composed "gate sentence" died for this).

- `session` oneof: `none` (registered, no session ever created) |
  `existing`.
- `existing` hoists the shared `HostSessionId` (daemon-minted; sessions
  rotate under one workspace — this is what Emacs correlates transcripts,
  health probes and fault windows against) over a `standing` oneof:
  `live` | `terminal {rehydratable}`.
- `live` carries:
  - `generation` (controller generation; rotates on daemon-side restart
    without the session id changing; fault windows scope to it);
  - `shim_attached` — false while the daemon is between shim starts;
  - `vendor_info` oneof, arm = the vendor: `claude {session_id,
    config_dir}` — config_dir names which account/login the conversation
    belongs to;
  - `backfill` — the never-blue signal, arm = state: none | pending | done
    | failed{detail} (known gap carried in the comment: a non-parse sidecar
    read error still manifests as pending-forever);
  - the `composer` gate oneof (below);
  - `faults` — standing generation-scoped `HostFault {detail,
    opened_at_ms}` for doctor output (typed kind arms arrive with their
    first derived producers).
- `naming {optional slug, optional title}` sits BESIDE the session oneof —
  buffers need a name in every standing; both fields unset until derived.

Composer gating: the resolved arm IS the gate, and it exists only on the
LIVE arm (the other standings are blocked by their own nature). Arms:
`open` | `merging` (the merge lease owns the session — composer closed) |
`draining` (scheduled shutdown) | `restarting` (graceful RestartWorkspace)
| `merge_parked` (the merge gave up and wants guidance: the composer is
OPEN WITH CONTEXT — everything submitted while parked is delivered to the
merge's resolution agent, never refused and never queued as the session's
own turn). The composer is host-native, so this gate is Emacs's to enforce.

### Hibernation does not exist on the wire

- Hibernation is entirely a daemon implementation detail (an idle-shutdown
  sweep). NO frontend word says hibernation anywhere: no host arm, no roster
  status, no footer state, no hibernate/revive verbs.
- A parked workspace presents as `live` with `shim_attached = false` — "the
  session exists and serves on demand". The composer stays open; typing
  revives under the hood. The only user-visible cost story is the webapp
  feed's cold-context gate, which is not Emacs's surface.
- Consequence for elisp: the teal treatment, 💤 glyph, hibernated decoders,
  hibernate command and its rpc plumbing are all dead (see the dead-code
  inventory above); the frontend cannot distinguish parked from idle, on
  purpose.

### The daemon handover (graceful rollout)

A daemon self-rollout is blue-green: the old daemon spawns the rebuilt one
(joining mode, fresh socket), announces, and transfers workspaces one by one
at freeness (no in-flight turn, no live detached work). There is no
daemon↔daemon channel — coordination is CLIENT RELAY + durable facts +
per-workspace kernel locks, and Emacs is the relay.

- `WatchDaemon {}` — the one daemon-level stream. Push arm:
  `shutdown_announced {address}`. On it, Emacs DUAL-ATTACHES: open a second
  connection to `address` while KEEPING the old one — each workspace's
  updates keep flowing from the daemon that currently owns it.
- Per workspace, the old daemon pushes `transferred` on that workspace's
  WatchHostWorkspace stream when it releases it. `transferred` is a PUSH,
  never a terminal frame — the stream stays standing until the CLIENT
  cancels (the standing-stream convention). Emacs's obligation, in order:
  call `AdoptHostWorkspace {WorkspaceRef}` on the NEW connection, then
  cancel the old stream and re-subscribe on the new connection.
- The adopt rendezvous: `AdoptHostWorkspace` and `AdoptWebWorkspace` are
  SIBLING VERBS so the verb itself identifies the participant (no
  self-declared kind field a confused client could get wrong). Expected
  participants = holders of the workspace's two streams at announcement
  time; the new daemon completes adoption (kernel lock, shim adoption,
  drain of held intake) only when every expected participant has called,
  and all calls succeed together. Headless workspaces transfer with zero
  rendezvous.
- Ordering is enforced BY REFUSAL, not convention: per-workspace rpcs for
  an unowned workspace are refused. Two derived error arms are owed to the
  wave — `transferring_away {address}` on the old daemon's verbs (a lagging
  client self-heals from the refusal) and `not_yet_adopted {}` on the new
  daemon's — two arms because wrong-daemon and right-daemon-too-early are
  different facts.
- Prompts arriving during the window are HELD (never errored) and replay in
  order on the new daemon.
- The old daemon times the adoption window; expiry surfaces as that
  workspace's own error and is NOT hardened machinery — no abort/retry
  exists.

### Notifications and the attention marker (presentation policy is Emacs's)

- The daemon publishes the FACT (`notification` push per workspace); each
  surface applies the policy it alone has the knowledge for. The daemon
  never asks "is Emacs focused".
- Emacs's policy, stated at the arm:
  - Emacs UNFOCUSED → post an OS desktop notification; its click raises the
    frame and selects the workspace's tab (plain elisp — decider and actor
    are one process, no daemon round-trip).
  - Focused, tab NOT selected → blink that tab-bar entry per the canonical
    cadence.
  - Tab selected → nothing (the footer's activity line already shows it).
- THE CANONICAL BLINK CADENCE is specified ONCE, on frontend.v1
  `RosterRowAttention`: two blinks — 500 ms on, 500 ms off, twice — then a
  steady marker until cleared. The webapp sidebar and the Emacs tab-bar
  both implement exactly that spec and CITE THE MESSAGE; a divergent
  cadence is a defect (code-level consistency is user-mandated).
- The marker's lifecycle is daemon-owned: set on notification, cleared by
  the existing SelectWorkspace verb — Emacs's ordinary tab switch is the
  clearing act; no dedicated ack verb exists.

### The workspace verbs Emacs wraps

Vocabulary note: doom-speak was renamed to match the contract — doom's old
"kill" (remove from the editor) is CLOSE; doom's old "nuke" is KILL; NUKE is
reserved for the verb that destroys data.

- `CloseWorkspace` (SPC j x) — the user's close, a VIEW act: fast ack, tab
  gone, the daemon↔shim session UNTOUCHED. REQUIRES QUIET: no turn in
  flight, no live async work, NO HELD PROMPTS (undelivered user intent may
  never be silently discarded; the user clears a hold via the tray's
  release/drop). A refusal manifests in the WEBAPP FOOTER (status closing ·
  close blocked, daemon-composed reasons), not as an Emacs dialog — the
  response carries only the blocked cause arm.
- `KillWorkspace` — the big red button: forced session death (connections
  and the shim). Never blocks, never warns; worktree and branch survive.
- `NukeWorkspace` — data destruction: kill first if live, then delete the
  worktree and branch.
- `OpenWorkspace` — just opens; any revival happens under the hood.
- `MergeWorkspace` — success means ENQUEUED; the merge's whole life from
  there is the webapp feed's merge bubble and the roster/footer. Emacs
  holds NO merge state: no durable merged/failed memory, no merged-tab
  filtering — the pushed views are the only merge state.
- `RestartWorkspace {force}` — SPC o C-c calls this and nothing else; the
  DAEMON owns everything the restart entails, INCLUDING bouncing the
  webapp view (via reload_webapp after the ack if Emacs coordination is
  needed). Graceful: wait for quiet, holding prompts in the tray
  meanwhile. Forced: interrupt and bounce; the agent is NOT resumed
  afterwards — continuing is the user's next prompt.
- `CreateWorkspace {RepositoryRef, optional initial_prompt, optional
  base_ref}` — THE DAEMON names and creates everything (slug → branch →
  worktree), registers it, and the workspace appears on the roster; no host
  materialization round-trip exists. THE CREATION-FACTS INCREMENT (ruled
  2026-08-29, see daemon.md) adds: merge actions, priority, fork-from,
  the ungated-consent flag, user name, parentage, model, and the one-shot
  form. The RepositoryRef comes from the
  roster's repo sections. No account field exists: the account is
  DETERMINED by the repo-under-root rule, never selected.

### Daemon-admin verbs (Emacs is merely today's caller)

These are not host-natured; elisp is the plumbing that reaches them.

- `UpdateShutdownSchedule { schedule{at_ms, reason} | cancel | now }` —
  deploy tooling's drain-and-exit control; the reason is REQUIRED on
  schedule (increment ruled 2026-08-29) and a drain-scheduled WatchDaemon
  push carries reason + at_ms so every client can draw the standing
  banner; other consequences ride existing surfaces (tray holds, footer
  status).
- `UpdateMergeQueue { pause | resume | evict{WorkspaceRef} }` — operator
  control; visible state rides the merge bubble's queue tab and the roster.
- `DaemonHealth {}` / `SessionHealth {WorkspaceRef}` — health pulls;
  UNHEALTHY IS AN ANSWER (a success arm carrying typed fault lists with
  dynamic detail strings), never a transport error. Two deliberately
  separate fault vocabularies (DaemonFault, SessionFault).
- `ClientLog` — Emacs NEVER calls it; it exists for console-less clients
  (the xwidget webapp) and Emacs has durable logs of its own.

### The shared editor-popup subroutine

- ONE elisp subroutine backs every open-a-file affordance the webapp raises
  to the host: "open path[:line] in a doom popup, right side, half width" —
  dired when the path is a directory.
- Its callers, all mandated to share the one implementation: the plan
  bubble's ✎ edit button (opens the plan file; the round-trip is
  save-then-tell-the-agent — the vendor cannot observe disk edits, and no
  edited marker exists), every findings-row location jump, and the worktree
  separation-divider paths.
- Divergence between call sites is a defect: the consistency requirement is
  code-level, the same ruling as the blink cadence.

### Gotchas worth knowing before touching elisp

- Echo refs verbatim: constructing or parsing a WorkspaceRef id — or using
  `dir` as a key — is typed-as-wrong by the contract's opacity comments.
- Empty error messages are DELIBERATE: error arms are derived from real
  refusal sites at implementation time, never invented ahead; an empty
  `<Rpc>Error` today is the correct current shape, and new arms land as the
  daemon's refusal sites are written.
- Success is empty wherever the new state arrives as a push (select,
  adopt, the workspace verbs): do not expect state in unary answers; the
  streams are the authority.
- A stream the PRODUCER ends without a terminal frame is a transport
  failure; a CLIENT cancel is the normal close. `transferred` and
  `reload_webapp` are pushes, not endings.
- No keepalive frames exist anywhere; connection death is detected at the
  transport, and unary calls fail loudly. Staleness machinery (fences,
  revisions, boot ids) is gone — per-stream ordering is the only ordering.
- The roster is READ-ONLY for Emacs: the daemon authors it wholesale
  (Emacs's old sidebar.el status table and roster publishing have no
  successor); Emacs consumes it, if at all, only for tab bookkeeping — and
  its two inputs to the roster are exactly Register and Select.
- Push cadence, stated on the frontend views and true of the host stream
  alike: event-driven, whole-replace, no ticks — push whole on any resolved
  change, push nothing on no change; clients tick clocks locally from
  shipped instants.

## Kickoff increments and rulings (2026-08-29, project lead)

- LANDED: WatchHostWorkspace push arm `open_in_editor {path, optional
  line}` — Emacs reacts with the ONE shared editor-popup subroutine (dired
  for a directory); nothing acks. SubmitPromptSuccess gains
  `command_refused` — both non-turn arms mean "answered, nothing to await";
  the webapp draws panels and refusals. SubmitPromptRequest.origin is
  REQUIRED: every Emacs send site sends its own PromptOrigin value.
- The webview URL is `http://<daemon.addr>/?workspace=<id>&dir=<dir>`.
- R8: a roster `current` change Emacs did not originate is a tab-switch
  request (re-selection is idempotent). Tabs derive from `closed = false`
  rows in roster order (repository sections depth-first, then recently
  merged); the daemon sets `closed = true` on merged/closed/killed rows.
- Cold start (ruled): Emacs adopts any daemon that answers DaemonHealth
  (healthy or unhealthy), never kills one, and only builds + starts
  `daemon/bin/claude-repld` (no required argv; state root via the env) when
  daemon.addr is absent or answers nothing. Everything after boot is the
  daemon's own blue-green.
- No permission-answering surface in Emacs (permission.el dies; the
  notification policy is the whole reaction); no task verbs in Emacs (org
  notes stay local). Submitting on a workspace with no/terminal session
  simply submits — the daemon starts or revives the session implicitly.
  Pasted images travel as `ImageBlock{path}` in UserSaid. Pending: the
  create-or-update-workspace skill's `status` verb loses its
  workspace-status.json source (follow-up outside this wave).

- CROSS-SYSTEM PROCESS CONTRACTS (project lead, kickoff): one state root
  `$AGENT_REPL_STATE_DIR` (default ~/.claude-emacs); the daemon binds ONE
  loopback TCP listener serving Connect (HTTP/1.1 + h2c, binary + JSON) and
  the webapp assets on one origin, writes `127.0.0.1:<port>` to
  `$AGENT_REPL_STATE_DIR/daemon.addr` (atomic replace; removed on orderly
  exit; a joining successor writes it only after it owns every workspace);
  the webview URL is `http://<daemon.addr>/?workspace=<id>&dir=<dir>`
  (`&composer=1` only in dev mode); the shim is spawned as `node
  agent-shim/claude/shim/dist/main.js --listen <uds> --store-socket <uds>
  --log-fd 3 [--fake]` with CLAUDE_CONFIG_DIR, AGENT_REPL_OWNED=1,
  AGENT_REPL_STATE_DIR, SHIM_BUILD_SHA (tests add
  AGENT_REPL_FORBID_VENDOR_CALLS=1), cwd = the workspace; session facts
  travel only in StartSession; readiness = the first healthy `diagnostics`
  push on WatchSession; the store serves on ~/.cache/agent-repl/sock/
  store.sock (tests: env AGENT_REPL_STORE_SOCKET, a flag beats it); kernel
  locks live in ~/.cache/agent-repl/run/ — `workspace-<md5hex(clean abs
  dir)[:8]>.lock` (shim-held from startup; the daemon probes ONLY this one,
  flock LOCK_EX|LOCK_NB) and `session-<vendor session id>.lock` (taken
  inside StartSession; pre-minted on a fresh start); proto/vocab/
  render-colors.json + paint-classes.json are the daemon's, consumed by
  webapp and Emacs; Go modules pin connectrpc.com/connect v1.17.0 and
  golang.org/x/net v0.43.0 (Go 1.24 on this machine; every module stays
  `go 1.23`).

## Landing 4 relay (2026-08-29, project lead)

- The host decoder accepts every new `<Rpc>Error` arm on the rpcs Emacs calls and HostFault.kind's eight arms; `transferring_away{address}` is the handover redial signal.

## Landing 6 relay (2026-09-01, project lead)

- SubmitPromptSuccess.command_acted is a third non-turn success arm ("answered, nothing to await"); the composer clears. SubmitPromptError.duplicate_submission is a refusal that keeps the text and states the key was already accepted.
- SubmitPromptError.turn_already_open is retired; the decoder's arm table drops it.
- UpdateMergeQueueError.unknown_repository for the operator merge-queue verbs.

## Landing 7 relay (2026-09-02, project lead)

Adapt to protos ab7e681f2 / bindings c10714a41 (see PROTO-CHANGES.md):
- SubmitPromptError.bubble_refused{detail, kind}: the host's submit error
  path names the kind (not_deliverable | agent_busy) and echoes `detail`.
- CloseWorkspaceBlocked now carries fields; the host command still keys off
  the footer's close-blocked state for its message and may use `summary`
  for the echo-area line.
- FeedMergeAbandoned.summary: render on the collapsed merge line as the
  failed summary is rendered.
