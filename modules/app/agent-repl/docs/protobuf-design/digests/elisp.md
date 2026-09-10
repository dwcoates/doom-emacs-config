# The Emacs client, derived

DERIVED from figma-to-idl-redesign.md at 86fd2b543 — the canonical record WINS on any conflict. Do not edit; regenerate.

Scope: every standing decision in the redesign record that bears on the Emacs
(elisp) client. Standing facts only — superseded positions appear only where
the record keeps them visible as a correction. No proto text; message and
field names are cited so the schema can be looked up.

---

## 1. What the Emacs client IS, under the settled model

- Emacs is a HOST, not an author. Its whole contribution to workspace state is
  two commands: REGISTER a workspace and SELECT a workspace. The daemon tracks
  everything else (parentage, dir, status, branch, merge facts, summaries).
  - WHY: today Emacs authors the roster in `sidebar.el` and re-derives each
    row's status into a second vocabulary that the webapp maps a third time —
    three spellings of one status, and the source of the done-vs-interrupted
    class of bug.

- Emacs REACTS to streams; the daemon never gives it orders and never calls
  it. A workspace appearing on a stream means open its buffers; disappearing
  means tear them down — exactly the webapp's discipline.

- Emacs owns the COMPOSER. The webview is launched with `composer=0`; prompt
  intake is host-native, which is why the host stream must tell Emacs whether
  the composer may be open.

- Rendering topology, recorded as standing fact: ONE WKWebView xwidget per
  workspace, bound for life, in its own pinned Emacs buffer, each with its own
  already-workspace-scoped socket. Consolidating to one webview was REFUSED
  (buffer/xwidget binding, per-workspace socket, `SPC .` alignment, and no
  cross-view sharing of process memory anyway). Endpoint-per-component is not
  connection-per-component: component streams multiplex over the one socket a
  webview already opens.

## 2. Transport and encoding

- `agentrepl.v1` is a CONNECT service, not gRPC, BECAUSE of its two clients:
  an xwidget WebKit view and elisp.

- PROTOJSON is the elisp codec. Connect serves binary or JSON per client, so
  ONE schema serves both callers with two codecs; the workspace verbs are one
  RPC shape for the webapp's roster clicks and for Emacs alike. This carries
  forward from the deleted `service.proto` header as a standing fact.

- NO KEEPALIVE FRAMES anywhere (the 3b keepalive convention was RETRACTED).
  A client never times frame cadence to judge liveness. The layering that
  replaces it: source-of-truth silence is the DAEMON's observation, a dead
  daemon↔client pipe fails the connection and unary calls fail loudly, and a
  wedged publisher behind a healthy connection is a daemon-internal fault its
  own watchdog surfaces.

- PUSH CADENCE, the standing convention for every `frontend.v1` view and the
  host stream: EVENT-DRIVEN, WHOLE-VIEW, NO TICKS. Push whole on any resolved
  change, push nothing on no change, tick clocks CLIENT-SIDE from shipped
  instants. Bursts are coalescible because the wire carries states, not events.

- No paint attestation: no response ever claims anything was rendered. No
  `request_id`/`client_id` envelope — Connect's unary response IS the
  correlation.

## 3. Workspace identity (`workspace.v1`)

- A PATH IS NEVER AN IDENTITY — "directories can be represented in many ways."
  `RegisterWorkspace` PROVIDES the path (any spelling) and the DAEMON MINTS the
  identifier, returned on success.

- `WorkspaceRef { id; dir }` — `id` is the sole supported identifier and is
  OPAQUE; `dir` is the normalized directory and must NOT be used as an
  identifier. `RepositoryRef` is the same construction for a repo's main
  worktree; clients get one from the roster's repo sections.

- These live in a NEW LEAF PACKAGE `workspace.v1`, not in `agentrepl.v1`,
  because `frontend.v1` must not import `agentrepl.v1`.

- The refs are TYPED ECHO TOKENS: the daemon mints, the client echoes back
  unchanged. A client that CONSTRUCTS an id rather than echoing one is wrong in
  intent (the opacity comment is the contract, not a mechanical guard). The
  daemon owns normalization.

## 4. The HOST section — exactly three RPCs

`RegisterWorkspace`, `SelectWorkspace`, `WatchHostWorkspace`.

- `RegisterWorkspace`: request is JUST the dir — no name, parent, or branch.
  IDEMPOTENT BY DIR: re-registration after a reconnect or a daemon restart is
  the NORMAL path and answers success. Returns the minted `WorkspaceRef`.

- `SelectWorkspace`: request is the ref; success is empty (the roster stream
  carries the new `current`). IDEMPOTENT — re-selecting the current workspace
  is a success. The daemon stamps `current` and `last_selected` on select, and
  also CLEARS the roster row's attention marker there (see §7).

- `WatchHostWorkspace`: request is the ref; the response wraps the host state
  whole. Per-workspace, never global.

- `ReportHostAction` NEVER EXISTS. The daemon→host command loop is deleted:
  `HostActionCompleted` (Emacs reporting on a daemon-dispatched action) and
  `WorkspaceMaterialized` (Emacs reporting its buffer/perspective bookkeeping
  done) both die with NOTHING in their place. The inbox was an abstraction leak
  of the old push stream, and the daemon never waits on Emacs's buffers, so
  "I finished setting up" has no listener. A real daemon→host ask, if one ever
  surfaces, re-enters as its own designed verb — never a generic action
  envelope.

- No host materialization round-trip exists on creation either:
  `CreateWorkspace` has the DAEMON derive the slug, branch and worktree dir,
  run the git itself and register the workspace; Emacs simply sees it on the
  roster and opens its buffer.

## 5. The host stream's flow and shape

THE FLOW, recorded because the user asked for it twice:

1. Emacs connects.
2. `RegisterWorkspace(dir)` per known worktree → a minted ref each.
3. Per OPEN workspace, ONE `WatchHostWorkspace(ref)` subscription — snapshot
   first, then WHOLE-REPLACE on every change.
4. Closing a workspace cancels its stream.
5. A daemon restart drops the streams; Emacs re-registers and re-subscribes.
6. The daemon NEVER calls Emacs.

- A GLOBAL host stream was REJECTED: workspace-dependent and
  workspace-independent channels must be distinct types, so the ref moved from
  the stream's entries into the request. No daemon-level host stream exists
  until a daemon-level PUSHED fact needs one.

THE SHAPE (`HostWorkspace`), as landed:

- `HostWorkspace` = `oneof session { none | existing }` plus `naming`.
- `HostWorkspaceNaming { optional slug; optional title }` — replaces the bare
  slug/title strings the user called out.
- `HostSessionExisting` HOISTS the shared `HostSessionId` (the user's
  factoring) over `oneof standing { live | terminal { rehydratable } }`.
- `HostSessionLive` carries: generation; `shim_attached`; `oneof vendor_info
  { HostVendorClaude { session_id; config_dir } }` (the user's restructure of
  bare vendor strings); `HostBackfill` (the old `BackfillState` enum converted
  to arms — none | pending | done | failed{detail}); the composer arm; and the
  generation-scoped faults.
- The composer arm and the faults were RELOCATED INTO the live arm at the
  user's prompting, on adjacent exclusivity: a non-live session with an open
  composer was representable nonsense as a sibling, and a fault window dies
  with its generation.
- `host_surface_pending.proto` is DELETED entirely — every message in it was
  placed or ruled dead (`WorkspaceState` with its fence and SSM snapshot
  fields, `SessionView`, `RuntimeFault`, `BackfillState`, the merge fields).

WHAT LEFT the host surface, each having a drawn home elsewhere:
model/model_options, total tokens/cost/context window, permission mode and
pending permissions, merge status, merged_at. Also dead: `fence` (stage 2
deleted fencing outright — one ordered stream per component means a push
cannot overtake a newer one) and the merge-dequeue offer (it is the tray's
`HeldOffer`).

`BackfillState`'s known limitation is carried, not solved: a sidecar read
error that is not a malformed line manifests as PENDING forever.

## 6. The composer gate

- COMPOSER GATING IS A RESOLVED ELEMENT on the host stream, never raw booleans
  (`merge_lease_held`, `hibernated`) that Emacs maps itself. This is the
  figma→idl gray-zone ruling: what Emacs DRAWS arrives resolved; what it merely
  COORDINATES with (session id, generation, backfill, lifecycle booleans it
  does not draw) stays coordination residue and is exempt.

- The landed shape is `oneof composer { open | merging | draining |
  restarting }` inside `HostSessionLive`.

- `HostComposerGate` / `HostComposerBlocked` and the daemon-composed gate
  SENTENCE all DIE. The other lifecycle arms are blocked BY THEIR OWN NATURE,
  and Emacs rendering a fixed treatment per arm is ordinary oneof rendering —
  no prose needs to ride the wire for it.

- A parked (shim-less) session presents as `live` with `shim_attached = false`
  — "the session exists and serves on demand" — so THE COMPOSER STAYS OPEN and
  typing revives the workspace implicitly.

## 7. Push notifications — presentation policy and the blink cadence

- The daemon publishes the FACT; each surface applies the policy it alone has
  the knowledge for. The user's first sketch had the DAEMON deciding by Emacs
  focus state; the settled model moves policy to where the knowledge lives —
  THE DAEMON NEVER ASKS "IS EMACS FOCUSED".

- `WatchHostWorkspaceResponse` becomes a PUSH ONEOF: the whole-state `host`
  arm as before, PLUS a `notification` EVENT arm `{ text; at_ms }`.

- EMACS OWNS PRESENTATION POLICY, stated at the arm:
  - Frame UNFOCUSED → an OS notification whose CLICK raises the frame and
    selects the tab (plain elisp — decider and actor are one process).
  - Frame focused, workspace UNSELECTED → TAB-BAR BLINK.
  - Workspace SELECTED → nothing.

- `RosterRow.attention` is an empty PRESENCE MARKER: the daemon sets it on the
  notification and clears it on the EXISTING `SelectWorkspace` verb — no new
  verb.

- THE CANONICAL BLINK CADENCE is specified ONCE, on `RosterRowAttention`:
  TWO BLINKS, 500 ms ON/OFF, THEN STEADY. The webapp sidebar and the Emacs
  tab-bar both implement exactly that spec and CITE the message; divergence is
  a defect. This is user-mandated code-level consistency, on the
  editor-popup precedent.

- The footer also gains `FooterStatusActivityNotification` — a composed line
  shown until the next activity replaces it. Notification OUTRANKS every other
  standing activity in the footer's precedence ladder, and is an ACTIVITY in
  every status arm, never a status of its own (a status arm would knock the
  real status off the strip).

- The vendor's own phone push is INDEPENDENT of our local presentation; the
  conversation-level record carries the vendor's delivery outcome
  (sent / not_sent{config_off | user_present | no_transport}) faithfully.

## 8. The shared editor-popup subroutine

- THE SUBROUTINE, stated as a code-level prescription that must survive into
  the fanout planning docs: ONE shared Emacs subroutine — "open `path[:line]`
  in a doom popup, RIGHT SIDE, HALF WIDTH" — and ONE shared webapp link
  component, used by EVERY affordance that opens a file.

- Its callers, as landed:
  - PLAN MODE's ✎ EDIT BUTTON on the purple plan bubble. Present iff the plan
    exit named the plan file. The WEBAPP RAISES THE CLICK TO THE HOST and Emacs
    opens `FeedPlanEditTarget.path`. The bubble itself is READ-ONLY; revisions
    otherwise go through the composer.
  - REPORT FINDINGS: EVERY finding's location is a jump target, and the user
    mandated code-level consistency with the plan button explicitly here.
  - THE WORKTREE separation divider's path — DIRED when the path is a
    directory.

- The plan round-trip is SAVE-THEN-TELL-THE-AGENT: the vendor cannot observe
  disk edits, so nothing auto-informs it. The user DECLINED a host-side
  "edited" marker.

## 9. Workspace verbs, and the close / kill / nuke vocabulary

THE TRIAD, all with Emacs commands that are THIN WRAPPERS — send the request,
await the daemon's ack, then tear the tab down; every piece of real machinery
is the daemon's:

- `CloseWorkspace` (`SPC j x`) — the USER's close, a VIEW act: fast ack, tab
  gone, the daemon↔shim session UNTOUCHED (keepalives continue; the workspace
  is merely unviewed).
  - It REQUIRES QUIET. A busy workspace REFUSES, and the refusal manifests in
    the FOOTER — status `closing`, sub-status `close blocked`, and the daemon's
    composed plain-English reasons as the activity line ("a turn is in flight;
    2 subagents and a shell are running"). The response carries only the
    `blocked` cause arm; the footer owns the reasons. The user waits or
    interrupts the work normally.
  - CLOSABLE = no turn in flight, no live async work, NO HELD PROMPTS. A held
    prompt is undelivered user intent and a close may never silently discard
    it; the user clears a hold via the tray's release/drop verbs.
  - NON-blocking, explicitly: a standing cold gate, the task tracker's
    contents, and a parked (shim-less) session.

- `KillWorkspace` — THE BIG RED BUTTON: forced session death (connections AND
  the shim itself). Never blocks, never warns, checks nothing. Worktree and
  branch SURVIVE. It is the better-named replacement for the `StopAgentShim`
  idea, which never landed.

- `NukeWorkspace` — DATA DESTRUCTION: kill first if live, then delete the
  worktree and branch. "Nuke" is reserved for exactly the verb that destroys
  data.

THE DOOM-SPEAK RENAME, which is elisp-only work (Go/TS/protos were already
right): doom's "kill" (remove the workspace from the editor — tab-bar, buffers,
editor state; worktree survives) becomes CLOSE; doom's "nuke" (the harder
destroy) becomes KILL. The old emacs "nuke" never destroyed data, which is why
the word was free to be re-pointed.

The distinction that motivated the design: KILL targets WORK the user is
looking at, so naming is free and refusal is senseless; CLOSE targets the
CONTAINER and the collision with live work is incidental — the refusal's job
is to redirect attention to work the user was NOT thinking about, which is why
it earns a designed surface rather than a bare error.

Other workspace verbs Emacs may call (each `{WorkspaceRef}` → empty
success / error):

- `OpenWorkspace` — just opens, reviving under the hood when needed.
- `MergeWorkspace` — success means ENQUEUED; the merge's life from there is the
  feed's bubble. It targets the workspace's parent by definition; there are NO
  parameters and no target override.
- `RestartWorkspace { force }` — bounces ONLY that workspace's shim (rebuild if
  out of date + restart the process). `force=false` is GRACEFUL: wait until no
  turn is in flight and no async/background work runs, holding incoming prompts
  via a tray-visible daemon hold meanwhile. `force=true` is FORCED: interrupt
  the current turn and background tasks, bounce immediately, and do NOT resume
  the agent — continuing is the user's, with a subsequent prompt. Never the
  webapp, never the daemon.
- `HibernateWorkspace` and `ReviveWorkspace` are both DELETED (see §10).
- DELETE-WITHOUT-CLOSE IS DEAD: no `DeleteSession` returns.

`SubmitPrompt` gains its first derived refusal arm: a prompt arriving AFTER a
merge began is REFUSED outright (never held) — once the workspace merges it
closes, so post-merge-start work would be orphaned. Prompts already held when
the merge began stay held.

## 10. Hibernation leaves the contract entirely

- THE RULING: "I'm not sure that emacs should know about hibernation at all…
  all the useful information that's practically cared about by the user is
  implicit in the compaction warning." A hibernated workspace only matters
  because keepalives are gone, which only matters because of the context cache,
  which the COLD GATE already fully surfaces.

- WHAT DIED, all four contract sites: `ReviveWorkspace` (endpoint + rpc —
  revival is now IMPLICIT: a prompt to a parked workspace revives it under the
  hood, cost story = the cold gate); the host stream's `hibernated` standing arm
  and the whole `HostHibernation*` family; `FooterStatusAsleep`; and
  `RosterRowStatusHibernated`. `HibernateWorkspace` died earlier — hibernation
  is an INACTIVITY policy, "i don't think it should ever be done explicitly."

- THE COMMITTED CONSEQUENCE, stated plainly: the frontend can no longer
  distinguish a parked workspace from an idle one ANYWHERE. Parked presents as
  `live` with `shim_attached = false`, the footer shows `idle`, the roster
  shows the ordinary dot. The distinction surfaces only as the cold gate, when
  it has a cost.

- The daemon's hibernation MACHINERY is untouched — it just stopped being a
  wire fact; what survives is an idle-cutoff shutdown sweep plus the ordinary
  resume path, and it is "entirely an implementation detail of the daemon to
  save memory": NO frontend knowledge, NO daemon API surface.

- The one remaining contract trace was NEUTRALIZED: `HeldPromptRevivalHold`
  became `HeldPromptSessionStartingHold` (arm `session_starting`) — "the
  session is still coming up" — with the same no-classifier / no-force /
  loud-drop semantics. No frontend word says hibernation anywhere now.

- THE ELISP REMOVAL INVENTORY, named implementation-wave work: the TEAL TAB
  TREATMENT for hibernated workspaces; the 💤 GLYPH; the roster label and its
  decoders; the `RENDER_STATE` / `CONNECTIVITY` hibernated decoders; the
  hibernate COMMAND and its `SPC o z` binding; the RPC client path and its
  error copy; the open-progress arm; roughly EIGHT TEST BLOCKS.
  - Ambiguity dispositions: `SPC o c` bring-up behavior is KEPT with
    generalized wording; the frontend-state live-branch guard dies with the
    decoder; TEAL DIES OUTRIGHT (the palette contracts to five colors,
    cross-system); log and diagnostic prose is kept and reworded lazily.

## 11. What Emacs no longer authors

- THE ROSTER. `WorkspaceRoster` becomes a daemon-RESOLVED `frontend.v1` view;
  "Emacs has nothing to do with `WorkspaceRoster` under the settled model: it
  can only register a workspace, and select a workspace."
  - Deleted with it: Emacs's `publishWorkspaceRoster` path; the daemon's roster
    retainer; `PublishWorkspaceRoster` as an RPC; `WorkspaceRoster.revision`
    and `boot_id` with the whole epoch/monotonicity rule set (they guarded an
    out-of-order STATE publish — a command has no stale roster to resurrect,
    and the daemon's own stream ordering replaces them); the `sidebar.el` STATUS
    TABLE (24 arms) — the daemon's roster resolver coarsens its state machine
    onto the sidebar dot vocabulary ONCE.
  - `RosterRow.current` and `last_selected` are stamped by the daemon on
    `SelectWorkspace`; `closed` vs gone is marked by the daemon from the
    open/close/unregister traffic it already brokers.
  - A `WorkspacePresence` per-row snapshot alternative was PROPOSED and
    REJECTED in favor of commands; recorded so it is not re-proposed.
  - The roster stream is GLOBAL (no workspace on the request); every webview
    watches the same roster.
  - Roster UI PREFERENCES are WEBVIEW-LOCAL, not daemon-held and not Emacs's:
    grouping mode, section folds and the nav cursor left `WorkspaceRoster`
    entirely (`RosterNavCursor`, `RosterFold`, the view oneof are deleted).
    `SetWorkspaceRosterView` never exists. The roster carries BOTH groupings
    fully resolved and the client draws the one its local preference picks.

- MERGE ROUND-TRIPS. `agentrepl.v1/shared.proto` is deleted: `MergeStatus` →
  the feed's merge bubble; `MergeDequeueOffer` → the tray's `HeldOffer`,
  answered by `AnswerHeldOffer`; `HibernationDetail` → dead with hibernation.
  The merge queue's visible state rides the merge bubbles' queue tabs and the
  roster's status arms; `UpdateMergeQueue` is purely INBOUND.

- PROMPT IDENTITY. `TurnId` is DAEMON-MINTED and returned at submission;
  clients reconcile nothing and mint nothing. Optimistic rows and client-side
  pending-request maps go away.

- Merge is DAEMON-orchestrated and can never be "detached" — the agent is not
  orchestrating it.

## 12. Daemon-admin verbs whose caller happens to be Emacs

The former Host section SPLIT: HOST (the three RPCs above) and DAEMON ADMIN.
The admin verbs are NOT host-natured — "Emacs is just today's caller."

- `UpdateShutdownSchedule { schedule{at_ms} | cancel | now }` — DEPLOY
  TOOLING's drain-and-exit control, purely inbound. NO UX motivation exists;
  elisp is merely today's plumbing to reach it, and nothing about the schedule
  is pushed outward. Its user-visible consequences ride surfaces already
  modeled (held prompts in the tray during the drain, footer status).
- `UpdateMergeQueue { pause | resume | evict{WorkspaceRef} }`.
- `DaemonHealth` and `SessionHealth` — UNHEALTHY IS AN ANSWER inside success,
  never an error. Faults are typed classes plus a dynamic detail string;
  `DaemonFault` and `SessionFault` are DELIBERATELY SEPARATE TYPES (different
  producers, not one shared vocabulary).
- `ClientLog` — EMACS NEVER CALLS IT. It exists because the webapp runs in an
  xwidget whose JS console is invisible and unpersisted, so without this relay
  a webapp malfunction leaves no evidence anywhere. Its `context` is an
  untyped Struct, accepted as the stated exception.

## 13. Conventions that bind the elisp implementation

- UNSET NON-OPTIONAL FIELDS ARE ILLEGAL, EVERYWHERE, IMMEDIATELY.
  - A REQUEST carrying an unset non-optional field is answered with an ERROR to
    the producer at once — never "handled", never defaulted.
  - A RESPONSE or STREAM PUSH with an unset non-optional field makes the
    CONSUMER RAISE A LOUD ERROR itself (on a stream there is no producer to
    answer). Sized to be caught during integration remediation.

- PROTO→CODE MAPPING, per language, both directions:
  - Every MESSAGE has ONE core "base" function; validation lives there ONCE.
    An unset oneof is an ERROR BY DEFAULT — a fallback only where the schema
    comment explicitly sanctions absence.
  - Every NON-PRIMITIVE use site (message-typed field, oneof arm) gets its own
    dedicated, TESTABLE function delegating to the child's base;
    ancestry-named specializations go one layer deeper only where a path has
    real site-specific behavior.
  - PRIMITIVES get no wrappers. The producer side is symmetric (build functions
    with the same validation, per-site builders on top).
  - NO class-per-message mandate — dedicated testable functions and separated
    concerns, not a shape. The anti-goal is a million unnamespaced
    `Handle<A><B><C>` functions.

- LOGGING: a DEBUG statement on every logical branch; warnings at WARNING,
  errors at ERROR. Integration/e2e orchestration turns on ≥WARNING BEFORE tests
  run and PERUSES the logs EVEN WHEN TESTS PASS; every warning is remediated to
  zero (fixed or deliberately downgraded), never left standing.

- THE TEST RULE for reconciliation: any test referencing a DELETED symbol, or a
  RESPELLED one (pointing at a genuinely different structure, not a mere
  rename), is DELETED — never adapted. Pure renames adapt mechanically. An
  adaptation that would require deciding what behavior should now be is a
  SURFACED GAP, not an adaptation.
  - Replacement coverage is prescribed only for INTEGRATION and E2E tests; unit
    coverage falls out of the mapping convention above and is deliberately NOT
    prescribed.

- Implementers NEVER change protobufs. A needed change is a REQUEST to the
  subsystem orchestrator, who triages to the lead; on approval the lead
  broadcasts PAUSE, lands the change, rebuilds bindings, and broadcasts RESUME
  carrying the NEW FOUNDATION COMMIT SHA. Every request and its ruling gets a
  line in the design record.

- DEAD CODE the redesign stranded is NAMED WORK in
  `docs/overhaul/elisp.md`, never left for discovery. That document also
  carries elisp's integration replacement specs and reconciliation gotchas.

## 14. Reconciliation status (standing fact)

- The design FROZE at `2d79f7501`; bindings were regenerated from clean for all
  six packages and the Makefile's proto list went dynamic.
- The ELISP subsystem reconciled GREEN in an isolated worktree and merged
  (5,614 tests), with its dead-code inventory, blockers and integration
  replacement specs seeded into `docs/overhaul/elisp.md`. Shim, webapp,
  store and sidecar merged alongside it; the DAEMON is a fanout subject rather
  than a reconciliation one.

## 15. The graceful-rollout handover (post-freeze increment)

- `WatchDaemon {}` is Emacs's daemon-level stream (HOST section); its
  `shutdown_announced { address }` push starts a rollout: Emacs opens a
  second connection to the address while keeping the first.
- Per workspace, the OLD daemon pushes `transferred` on that workspace's
  `WatchHostWorkspace` at freeness (a push, never a terminal frame); Emacs
  then calls `AdoptHostWorkspace { WorkspaceRef }` on the NEW connection,
  and on success CANCELS the old stream and re-subscribes on the new.
- The adopt is a RENDEZVOUS with the webview's `AdoptWebWorkspace`: all
  expected participants (the per-workspace stream holders at announcement)
  succeed together; the old daemon times the window and surfaces expiry as
  the workspace's error (not an invariant to harden).
- `reload_webapp` on `WatchHostWorkspace` is the webapp-only rollout:
  Emacs reloads the workspace's xwidget against the SAME daemon (empty arm
  — no address; the webview's default first-page load is the recovery).

## 16. The merge_parked composer arm (post-freeze increment)

- HostSessionLive's composer oneof gains `merge_parked` (10): the merge
  parked for the user's guidance — the composer OPENS with context, and
  everything submitted is delivered to the merge's resolution agent
  (never refused, never queued as the session's own turn). `merging`
  stays closed as before.
