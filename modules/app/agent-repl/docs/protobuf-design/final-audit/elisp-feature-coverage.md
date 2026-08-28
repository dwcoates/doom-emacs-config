# Elisp feature-coverage audit — `docs/overhaul/elisp.md`

Method: read `docs/overhaul/elisp.md` in full, then enumerated the user-facing
feature surface of `lisp/*.el` (47k lines, 56 sources, ~152 interactive
commands, the `SPC o` / `SPC j` / `SPC TAB` / chord keymaps, the tab-bar,
the sidebar, notifications, and the workspace lifecycle). Each feature was
checked against the plan for **explicit** accounting (ported, ruled removed,
named dead code) or **implicit** accounting (subsumed by a stream/verb the
plan does specify, or by the frozen `.proto` comments the plan points at).
A FINDING is a real, currently-shipping user-facing feature with **no**
accounting of either kind.

Contract evidence is cited from `proto/src/` (the plan names the protos as
authoritative); no design-record or `docs/protobuf-design/` material outside
this file was consulted.

---

## Findings

### 1. No turn/lifecycle signal reaches Emacs at all — the tab-bar state coloring, the sidebar dot, and every "the agent finished" reaction lose their data source

- **Feature.** Every workspace tab is painted by the workspace's live agent
  state (red = turn running, green = ready/idle, yellow = detached async
  work, purple = merging, blue = broken), repainted at 1 Hz and on focus
  regain; the sidebar rail draws the same vocabulary as a dot.
- **Evidence.** `lisp/status.el:404`, `lisp/status.el:428`, `lisp/status.el:446`,
  `lisp/status.el:461`, `lisp/status.el:468`, `lisp/status.el:486`,
  `lisp/status.el:523`, `lisp/status.el:571`, `lisp/status.el:979`,
  `lisp/status.el:2851`; `lisp/sidebar.el:115`; poll timer
  `lisp/status.el:29`.
- **Why unaccounted.** The plan fixes Emacs's stream inventory at "one
  WatchDaemon stream plus one WatchHostWorkspace stream per open workspace"
  (elisp.md, *Package map* / *The HOST section*), and `HostWorkspace` carries
  **no** lifecycle or turn axis: `session{none|existing}` →
  `standing{live|terminal}` → `generation`, `shim_attached`, `vendor_info`,
  `backfill`, `composer`, `faults`, plus `naming`
  (`proto/src/agentrepl/v1/endpoint_watch_host_workspace.proto:69-82`,
  `:100-138`). `composer=open` is equally true while a turn runs and while the
  agent is idle. The roster *does* carry the dot
  (`proto/src/frontend/v1/sidebar.proto:184` region), but the plan explicitly
  says "The roster is READ-ONLY for Emacs: Emacs consumes it, **if at all**,
  only for tab bookkeeping" and never lists `WatchWorkspaceRoster` among
  Emacs's streams. So the plan's dead-code entry ("the palette contracts to
  the five live colors") preserves the *palette* while leaving the *source*
  unspecified.
- **Confidence.** High. This is the load-bearing gap: several findings below
  are its downstream consequences.

### 2. "Agent finished" reactions have no edge to fire on

- **Feature.** Turn completion drives four distinct behaviors: the
  "*ws*: Agent ready" desktop banner when Emacs is unfocused, the
  "Agent finished in workspace: X" echo when the finish is in a non-current
  workspace, an automatic `magit-status` refresh for that repo, and the
  deferred-prompt-queue drain.
- **Evidence.** `lisp/session.el:684`, `lisp/session.el:709`,
  `lisp/session.el:794`, `lisp/session.el:733`, `lisp/session.el:761`,
  `lisp/session.el:805`, `lisp/session.el:830`.
- **Why unaccounted.** The plan's notification policy covers the
  `notification` push only, and that arm is documented as "A notification the
  **agent addressed to the user**, forwarded per workspace" with `{text,
  at_ms}` (`endpoint_watch_host_workspace.proto:59-64`) — an agent-authored
  event, not a turn boundary. Combined with finding 1, there is no
  turn-completion edge on any stream Emacs holds. The plan's *Notifications*
  section also enumerates exactly three treatments (desktop / blink /
  nothing), so the non-current-workspace echo is an unlisted fourth.
- **Confidence.** High.

### 3. `agent-repl-rename-workspace` — a 577-line feature with no successor verb and no removal ruling

- **Feature.** `SPC TAB r` renames a workspace: validates against collisions
  and in-flight cherry-pick/merge/rebase, renames the git branch, moves the
  worktree on disk, re-points open buffers, and rewrites other workspaces'
  source back-references.
- **Evidence.** `lisp/rename.el:549`, `lisp/rename.el:65-104`,
  `lisp/rename.el:105`, `lisp/rename.el:134`, `lisp/rename.el:259`,
  `lisp/rename.el:379`, `lisp/rename.el:240`; binding
  `lisp/keybindings.el:729`.
- **Why unaccounted.** `service.proto` has no rename rpc (28 rpcs, none
  naming). The plan says naming is daemon-derived (`naming{slug, title}`) and
  that "THE DAEMON names and creates everything", but never states that user
  renaming is removed — and the plan's own *Removals ruled 2026-08-28* list,
  which is where such a ruling would live, omits it.
- **Confidence.** High.

### 4. `CreateWorkspace` cannot express the creation flows Emacs actually ships

- **Feature.** Five distinct creation gestures: user-supplied workspace
  **name**; creation **from a chosen source workspace** (`C-u`), which
  establishes parentage and inherits the source's priority; creation from
  local `master` as a separate command; **session forking** (`--fork-session`
  resumes the source's conversation in the new worktree); and **model
  selection at creation** for the one-shot variants.
- **Evidence.** name `lisp/worktree.el:1323`; source pick
  `lisp/worktree.el:1261`, `lisp/worktree.el:1300`; priority inheritance
  `lisp/worktree.el:1339`; from-master `lisp/worktree.el:1626`; fork
  `lisp/worktree.el:1638`, `lisp/worktree.el:1656`, `lisp/worktree.el:1672`;
  model read `lisp/worktree.el:1589`, `lisp/worktree.el:1604`.
- **Why unaccounted.** `CreateWorkspaceRequest` is exactly
  `{RepositoryRef repository, optional UserSaid initial_prompt, optional
  string base_ref}` (`endpoint_create_workspace.proto:18-28`) — no name, no
  parent/source workspace, no fork-session, no model, no priority. `base_ref`
  covers the from-master variant; `SetModel` (`service.proto:118`) covers
  model *after* creation. Nothing covers name, parentage, or fork. The roster
  nevertheless renders "Nested workspaces (a spawned family under its
  parent)" (`sidebar.proto:267`), so parentage is a live concept with no
  stated input.
- **Confidence.** High for name/fork/parentage; medium for model (SetModel is
  a plausible two-step successor the plan does not state).

### 5. The whole one-shot workspace family

- **Feature.** Six commands (`SPC j o`, `O`, `C-o`, `C-S-o`, `M-o`, `M-S-o`):
  repo-pinned one-shot workspaces off master whose prompt is decorated with a
  self-merge or open-a-PR suffix, an AI-generated workspace name via
  `claude -p --model haiku`, a bespoke prompt minibuffer where `RET` submits
  as typed and `C-RET` appends ". dont take action", one-shot prompt history
  that survives an abort, and an "amend the in-flight one-shot" verb that
  queues onto the workspace still being generated.
- **Evidence.** `lisp/worktree.el:1517`, `:1547`, `:1558`, `:1604`, `:1614`;
  prompt UI `:1366`, `:1375`, `:1439`, `:1414`, `:1509`; name generation
  `:1524`; amend `:833`, `:864`, `:879`, `:891`, `:781`; bindings
  `lisp/keybindings.el:827-832`.
- **Why unaccounted.** Not named anywhere in elisp.md — not as a port, not in
  the removals list. The daemon now owns naming, so the haiku name generation
  is implicitly displaced, but the prompt decoration, the amend-in-flight
  verb, and the two-key submit variants have no wire counterpart
  (`SubmitPrompt` carries `UserSaid` verbatim, and command recognition is
  "the daemon's, transparently" —
  `endpoint_submit_prompt.proto:1-5`).
- **Confidence.** High.

### 6. Composer prompt decoration: metaprompt, prefix/postfix sends, and workspace-command injection

- **Feature.** The composer auto-prepends a per-workspace metaprompt (with an
  exemption list), offers send-with-metaprompt-reread, send-with-postfix
  ("what do you think? do NOT code, just analyze."), send-with-prefix ("just
  answer, dont take action: "), send-and-hide, and injects the source
  workspace into `/wor…` commands.
- **Evidence.** `lisp/input.el:261`, `:344`, `:658`, `:703`, `:93`, `:722`,
  `:98`, `:651`, `:365`, `:379`.
- **Why unaccounted.** The plan makes the composer host-native and Emacs the
  gate enforcer, but says nothing about what Emacs may do to the text.
  `SubmitPromptRequest.said` is "The composed prompt — one canonical form
  client → daemon → tray → shim → record" and the endpoint header states the
  submission is the composer's, **verbatim**
  (`endpoint_submit_prompt.proto:1`, `:24-27`). Client-side rewriting is at
  minimum in tension with that; the plan neither blesses nor removes it.
- **Confidence.** High that it is unaccounted; medium on whether the intent is
  removal.

### 7. Slash-command / skill completion-at-point in the composer has no data source

- **Feature.** The Emacs composer offers completion over the workspace's
  available slash commands and skills.
- **Evidence.** `lisp/input.el:476`, `lisp/input.el:518`.
- **Why unaccounted.** The composer is host-native by ruling, so Emacs needs
  the command list; the only place the vendor's supported-commands answer
  appears on the contract is the `/help` **panel**, which is explicitly
  webapp-owned ("Daemon-resolved; the WEBAPP owns the rendering" —
  `proto/src/frontend/v1/help_panel.proto:7-8`). No host-section rpc or
  `HostWorkspace` field exposes it.
- **Confidence.** High.

### 8. The open-progress ladder and its stall diagnosis

- **Feature.** Pressing the open key paints an "Opening *ws*…" placeholder
  before any async work, then a six-rung stage ladder (asking the daemon →
  daemon ready → openWorkspace sent → acknowledged → loading the conversation
  → rendering) with ✓/▸ marks and a ticking elapsed counter; at 12 s with no
  progress it turns red with the last stage reached and a Try list; a failed
  open replaces the ladder with a named cause and stays on screen.
- **Evidence.** `lisp/open-progress.el:256`, `:288`, `:120-127`, `:183`,
  `:232`, `:71`, `:199`, `:338`, `:324`, `:404-419`, `:359`, `:262-268`.
- **Why unaccounted.** The plan says `OpenWorkspace` "just opens", that
  "Success is empty wherever the new state arrives as a push", and that
  Emacs commands are thin wrappers that "send the request, await the daemon's
  ack, update editor state". There is no progress channel and no per-stage
  fact on any stream Emacs holds, and the plan never rules the ladder removed.
- **Confidence.** High.

### 9. Codex as a second backend, and `agent-repl-select-backend`

- **Feature.** A whole second vendor: `agent-repl-select-backend` picks the
  backend per workspace (or session-default under `C-u`), refuses to switch
  mid-turn, persists across restart, and resumes the prior backend's earlier
  conversation on switch-back; codex carries its own model, `CODEX_HOME`,
  managed-vs-personal permission flags, doctor checks, and an intentionally
  empty title segment.
- **Evidence.** `lisp/backend.el:239`, `:257`, `:275`, `:279`, `:254`, `:284`,
  `:111`; `lisp/codex.el:41`, `:50`, `:58`, `:66`, `:97`, `:318`, `:344`.
- **Why unaccounted.** `HostSessionLive.vendor_info` is a oneof whose "ARM IS
  THE VENDOR" and it declares exactly one arm — `HostVendorClaude claude = 3`
  (`endpoint_watch_host_workspace.proto:108-112`). The plan's own summary
  repeats "`vendor_info` oneof, arm = the vendor: `claude {session_id,
  config_dir}`". No arm, no verb, and no removal ruling for codex.
- **Confidence.** High.

### 10. Emacs supervises the daemon process itself

- **Feature.** Emacs starts the daemon (auto-start on Emacs startup,
  toggleable, plus a force-start command), stops it while preserving session
  shims, restarts the whole runtime, detects and rebuilds a **stale daemon
  binary**, detects and terminates a **foreign/incompatible daemon already on
  the port** with a grace period, optionally echoes daemon output into Emacs,
  and reports build/deploy failure with exit code and captured output.
- **Evidence.** `lisp/daemon.el:104`, `:2476`, `:2490`, `:2508`, `:1815`,
  `:598`, `:2143`, `:2184`, `:2055`, `:112`, `:1167`, `:541`, `:562`;
  bindings around `lisp/keybindings.el:694`.
- **Why unaccounted.** The plan treats the daemon as already running and
  covers only reaction (reconnect, re-register) and the daemon's own
  blue-green self-rollout. `UpdateShutdownSchedule` is inbound drain control
  only. Nothing addresses who spawns the daemon, port arbitration, or
  stale-binary rebuild — and the handover design *presumes* the old daemon
  spawns the new one, which does not cover cold start.
- **Confidence.** High.

### 11. The scheduled-restart **reason**, and the drain lease

- **Feature.** `agent-repl-frontend-daemon-restart-scheduled` reads a
  **mandatory reason** (blank is refused loudly) that other clients see in
  their drain banner, takes a drain lease, and bounces once every workspace
  is quiet; a companion command cancels the schedule and releases the lease,
  failing loudly if there is none.
- **Evidence.** `lisp/daemon.el:2549`, `:2566`, `:2569`, `:2582`.
- **Why unaccounted.** `UpdateShutdownScheduleRequest` is
  `{schedule{at_ms} | cancel | now}` with no reason string and no lease
  concept (`endpoint_update_shutdown_schedule.proto:14-30`), and the plan
  describes it as "purely inbound; consequences ride existing surfaces". The
  reason text is a user-visible fact today with nowhere to go.
- **Confidence.** High.

### 12. Tab ordering and folding are client-authored, against an explicit "clients do not re-sort"

- **Feature.** Workspaces are auto-reordered in the tab bar by priority;
  `SPC TAB p` pushes a workspace to second-to-last and `SPC TAB P` pulls it to
  second; closing via `SPC o C` pushes the tab to second-to-last; clicking a
  repo header in the sidebar folds the section **and hides its workspaces
  from the tab bar**; `SPC o H` hides every workspace under configured path
  prefixes by *killing* them so the `SPC <n>` indices stay contiguous, then
  re-establishes them at the front of the tab bar on toggle-off, persisting
  the toggle and hidden set across restarts.
- **Evidence.** `lisp/workspace.el:1271`, `:1341`, `:1386`, `:874`, `:888`;
  `lisp/commands.el:3609`, `:3633`; `lisp/panels.el:646`;
  `lisp/sidebar.el:1560`, `:1584`; `lisp/hide-project-dirs.el:323`, `:191`,
  `:49`, `:145`, `:225`, `:297`, `:375`.
- **Why unaccounted.** The roster states "Order is the resolver's; clients do
  not re-sort" twice (`sidebar.proto:96`, and the repository view's twin), and
  the plan gives Emacs exactly two roster inputs (Register, Select). The plan
  kills `agent-repl-doom-multi-repo-mode` but is silent on `hide-project-dirs`,
  on priority-driven ordering, and on the fold→tab-bar coupling. The
  hide-by-killing implementation is additionally hazardous under the new
  vocabulary, where KILL is forced session death.
- **Confidence.** High.

### 13. Priority as a workspace attribute

- **Feature.** `SPC j P p` sets/clears a priority level; the badge renders as
  an inline PNG in the tab, persists across restart, is inherited by children,
  and reorders the tab bar.
- **Evidence.** `lisp/keybindings.el:323`, `:304`, `:275`;
  `lisp/status.el:13`, `:181`, `:1061`; `lisp/worktree.el:1339`;
  `lisp/session.el:120`.
- **Why unaccounted.** No priority field exists anywhere in `proto/src/`
  (grep: only ordering prose in `sidebar.proto`), no verb sets it, and the
  plan neither ports nor removes it. Note the tab-bar renderer's own comment
  already says the priority "was announced by the daemon"
  (`lisp/status.el:1061` region) — i.e. today's code expects a daemon-side
  home the contract does not provide.
- **Confidence.** High.

### 14. Emacs-owned tasks: the store, the org notes, and the assignment gestures

- **Feature.** A user-defined task list persisted to `~/.claude-emacs/tasks.el`,
  each task owning an org notes file under `~/.claude-emacs/tasks/` opened in a
  right-side popup and saved on dismissal; workspaces are assigned to tasks and
  children inherit their parent's task; the sidebar's Task view offers create
  (minibuffer title), toggle-done, open-notes, and add-workspace-to-task.
- **Evidence.** `lisp/tasks.el:98`, `:118`, `:145`, `:166`, `:200`, `:227`,
  `:260`, `:272`; `lisp/sidebar.el:1665`, `:1678`, `:1686`, `:1697`, `:1707`,
  `:1738`.
- **Why unaccounted.** The roster renders a full task grouping —
  `RosterTaskView`, `RosterTaskSection`, `RosterTaskKey{task_id}`,
  `RosterTaskSectionHeader`, `RosterTaskDone`
  (`sidebar.proto:95-165`) — so tasks are a daemon-authored concept now, but
  `service.proto` has **zero** task rpcs (create, rename, done, assign). The
  plan says Emacs's sidebar has no successor, which disposes of the *rendering*
  but not of the task store, the org notes files, or the four gestures.
- **Confidence.** High.

### 15. The `explain-config` read-only Q&A popup — a second agent session outside the daemon

- **Feature.** `SPC j h c` reads a question at an orange 🤖 prompt and answers
  it in a webkit popup that takes over the agent-output window at a
  configurable width fraction; re-running continues the same conversation, the
  popup has its own composer, `C-u` starts fresh, every first turn is wrapped
  in a read-only preamble forbidding mutating actions, and two companion
  commands dismiss the popup (keeping the conversation) or reset it (killing
  the session). Model, config dir, and permission mode are configurable.
- **Evidence.** `lisp/explain-config.el:579`, `:593`, `:582`, `:586`, `:114`,
  `:589`, `:104`, `:257`, `:274`, `:626`, `:638`, `:71`, `:78`, `:85`.
- **Why unaccounted.** An entire second conversation surface Emacs runs
  itself, outside the daemon, the roster, and the workspace model. elisp.md
  never mentions it in any capacity.
- **Confidence.** High.

### 16. The canned-prompt keymap: `SPC j` explain / test / lint / coverage / PR

- **Feature.** ~30 bindings that compose a prompt and send it: explain
  line/region/hunk (prompt and canned variants), explain diff (worktree /
  staged / uncommitted / HEAD / branch), run tests / lint / all across the same
  five scopes, test-quality and test-coverage across the same five scopes,
  four create-or-update-PR variants (with and without `--self-certified`, plus
  paste variants), update-PR-description, and rebase-onto-origin/master.
- **Evidence.** `lisp/keybindings.el:852-900`, `:834`, `:835`, `:862-865`;
  implementations `lisp/commands.el:374`, `:426`, `:438`, `:713`, `:738`,
  `:790`, `:810`, `:819`, `:828`.
- **Why unaccounted.** These are Emacs composing prompt text from editor
  context (point, region, diff scope) — the same tension as finding 6, and at
  much larger scale. The plan's "Emacs commands are THIN WRAPPERS" framing does
  not cover them, and they appear in neither the port nor the removal list.
- **Confidence.** High that unaccounted; the *right* answer may well be
  "these are just prompts, they survive untouched" — but the plan does not say so.

### 17. Emacs's pre-teardown agent round-trip (`/gns-sockets close`) versus CloseWorkspace's quiet requirement

- **Feature.** Closing or merging a workspace first sends `/gns-sockets close`
  to its agent and polls for idle (30 s cap) so held sockets are released
  before teardown, proceeding with a warning on timeout.
- **Evidence.** `lisp/worktree.el:1936`, `:1889`, `:1913`, `:1929-1932`.
- **Why unaccounted.** The plan makes `CloseWorkspace` a fast-ack VIEW act
  requiring quiet, with refusal surfacing in the **webapp footer**, and makes
  every piece of real machinery the daemon's. A host-initiated agent
  round-trip before close contradicts both, and no successor (a daemon-side
  pre-close hook, or a ruling that it dies) is stated.
- **Confidence.** High.

### 18. Readiness and recovery-SLO surfaces (1,674 lines)

- **Feature.** A mode-line readiness segment polling every 15 s with per-system
  cells (`X✓` ready, `X↓3` three commits behind, `X↯` stale running binary,
  `X?` unknown), tooltips carrying commits/minutes behind and the stale pid,
  a "…" pre-first-poll state and a trailing `!` on stale reports; plus a
  per-workspace recovery window with a 3 s budget that, on breach, forces the
  breaching workspace's page repair and session re-ensure exactly once.
- **Evidence.** `lisp/readiness.el:68`, `:300`, `:324`, `:341`, `:365-372`;
  `lisp/recovery-slo.el:169`, `:553`, `:614`, `:978`, `:1010`, `:986-1000`,
  `:1024-1034`, `:698`.
- **Why unaccounted.** `DaemonHealth` / `SessionHealth` are described as health
  *pulls* with typed fault lists — a different fact from source-vs-running
  deploy freshness, and the plan supplies no client-side recovery-budget
  concept. The plan does say staleness machinery (fences, revisions, boot ids)
  is gone, but that ruling is about ordering, not about the deploy-freshness
  indicator or the recovery SLO.
- **Confidence.** Medium-high (the recovery SLO may be intended to die with
  the reconnect model; the readiness segment is a distinct, uncovered surface).

### 19. Emacs-side durable workspace roster — the thing that makes tabs come back

- **Feature.** A durable snapshot of every workspace (including tombstoned and
  hidden ones) persisted across Emacs restarts, with explicit save / update /
  load / load-from-archive commands, plus per-workspace display state (model,
  backend, tab index, source workspace, repl-state) rehydrated on restart.
- **Evidence.** `lisp/workspace.el:1698`, `:1713`, `:622`, `:642`, `:656`,
  `:670`; `lisp/session.el:120`, `:188-199`, `:238-248`;
  `lisp/commands.el:1602`, `:1638`, `:2302`, `:2580`.
- **Why unaccounted.** The plan's flow opens with "Emacs connects →
  RegisterWorkspace **per known worktree**" without saying how Emacs knows
  them after a restart. It rules out durable *merge* memory specifically, and
  says the pushed views are the only merge state — but does not say whether the
  workspace roster snapshot survives, is replaced by a daemon pull, or dies.
- **Confidence.** Medium-high.

### 20. Panels-dismissed (`:repl-state :inactive`) is an Emacs-only state with a visual treatment

- **Feature.** A workspace whose panels were dismissed keeps its tab but paints
  only the `[N]` bracket in its state color (name region falls back to default),
  and its sidebar row greys out; the `:ready` state additionally "shouts"
  full-green then fades to bracket-only after ~2 s of the user sitting in it,
  resetting on the next non-ready state.
- **Evidence.** `lisp/status.el:836`, `:1154`, `:1083`, `:1115`, `:1137`;
  `lisp/sidebar.el:249`, `:526`; `lisp/panels.el:916`, `:625`.
- **Why unaccounted.** The plan mandates "FIXED TREATMENTS PER ARM, never
  mapping values", and neither panels-dismissed nor viewed-ness is an arm on
  any contract message. The ready-fade in particular is derived from local
  dwell time, which the daemon by design cannot know — so it is neither an arm
  treatment nor a ruled removal.
- **Confidence.** Medium-high.

### 21. Desktop notification on **permission request**

- **Feature.** A "*ws*: permission requested — *tool*" banner fires on a
  permission ask, gated on Emacs being unfocused, and re-delivered items after
  a reconnect deliberately do not re-notify.
- **Evidence.** `lisp/permission.el:227`, `:241`, `:200`.
- **Why unaccounted.** The plan's presentation policy is written entirely
  around the `notification` push arm, whose proto comment scopes it to "a
  notification the agent addressed to the user"
  (`endpoint_watch_host_workspace.proto:59`). A permission ask arrives on the
  feed/permission plane, which the plan treats as webview business. Whether a
  permission ask should still raise an OS banner is unstated.
  (`AnswerPermission` itself, including the free-text deny reason, IS
  accounted — see the accounted list.)
- **Confidence.** Medium-high.

### 22. The `agent-repl-restart-session` compound: shim restart **plus** webapp rebuild/redeploy

- **Feature.** `SPC o C-c` hard-restarts the shim keeping the same
  conversation, non-blocking, while rebuilding and redeploying the webapp in
  parallel, bouncing the webview only once both succeed; a failed build is
  surfaced loudly and the page stays on its existing bundle. `services.el`
  additionally builds and bounces launchd-managed store/sidecar services with
  tunable readiness/health timeouts, and exposes a synchronous
  restart-and-wait surface `bin/deploy-all.sh` drives over `emacsclient`.
- **Evidence.** `lisp/frontend.el:1499`, `:1520`; `lisp/services.el:526`,
  `:537`, `:280`, `:54`, `:59`, `:67`, `:78`, `:553`.
- **Why unaccounted.** `RestartWorkspace {force}` is scoped to "bounce only
  the workspace's shim". The build/deploy half — and Emacs's role as the
  deploy driver for store and sidecar — has no place in the plan, which never
  mentions deploy tooling beyond `UpdateShutdownSchedule`.
- **Confidence.** Medium-high.

### 23. Webview lifetime: pre-creation, staggering, and the stale-bundle sweep

- **Feature.** Webviews are pre-created ahead of use and staggered so the frame
  does not stall; stale webviews are automatically swept back onto the deployed
  bundle on daemon link-up and on deploy, debounced so reconnect storms do not
  thrash pages; `agent-repl-frontend-rescue-webview` brings a webview that
  navigated away back home and reports where it went.
- **Evidence.** `lisp/webview-recovery.el:102`, `:150`, `:330`, `:407`, `:87`;
  `lisp/frontend.el:1381`, `:1399`.
- **Why unaccounted.** The plan gives exactly one webview lifecycle statement —
  "one xwidget WKWebView … bound to its buffer for life" — plus the
  `reload_webapp` push for rollout. Pre-creation before a buffer exists is in
  direct tension with "bound to its buffer for life", and the link-up sweep and
  the rescue path are neither ported nor ruled out.
- **Confidence.** Medium.

### 24. The daemon-link degraded banner and retractable connection notices

- **Feature.** A right-aligned red " DAEMON LINK DEGRADED " banner in the tab
  bar, suppressed during an expected daemon restart; and a retractable
  connection-notice system that takes its own warnings back down on reconnect
  (exact-region retraction, echo-area text only cleared if it is still this
  module's own).
- **Evidence.** `lisp/status.el:944`, `:949`, `:2229`;
  `lisp/connection-notice.el:53`, `:82`, `:142`, `:46`, `:119`.
- **Why unaccounted.** The plan removes keepalive and staleness machinery and
  says "connection death is detected at the transport, and unary calls fail
  loudly" — that specifies *detection*, not the standing user-visible
  indicator or its retraction. `FailureDaemonUnreachable`
  (`proto/src/frontend/v1/failure.proto:125`) covers the classified card, but
  a card is a settled fact while the banner is a live condition, which is
  exactly the distinction `connection-notice.el` exists to make.
- **Confidence.** Medium.

### 25. Editor-local behaviors with no contract bearing and no mention

Grouped; each is real and user-facing, none is named in elisp.md. Low
significance individually — listed so the plan's "deliberate ignore" set is
explicit rather than assumed.

- Tab-bar geometry: pinned two-row height, centered rows, anchored window with
  `+N` elision badges, hard truncation, hidden close/new-tab buttons —
  `lisp/status.el:1270`, `:1740`, `:1609`, `:1674`, `:1504`, `:1400`, `:2232`.
- Panel/window discipline: orphan-window sweeping, two-panel self-healing,
  side-window preservation, `next-buffer`/`previous-buffer` skipping panel
  buffers, auto-close-panels-on-file-open, sibling bottom popups —
  `lisp/panels.el:777`, `lisp/window.el:513`, `lisp/prevent-select.el:23`,
  `lisp/close-panels-on-open.el:93`, `lisp/sibling-popup.el:125`.
- Commit-emoji decoration and its CLI hook installer — `lisp/emoji.el:330`,
  `:241`, `:72`, `:78`, `:391`.
- Magit integrations: tag-ref toggle, merge-base section, open-commit-in-GitHub,
  copy-commit-link, open-workspace-PR, workspace `magit-status`, panel
  auto-hide on magit RET actions — `lisp/magit.el:97`, `:170`, `:228`, `:252`,
  `:295`, `:372`, `:444`.
- External-browser pinning of `browse-url` to a named Chrome profile —
  `lisp/external-browser.el:103`, `:156` (the daemon has its own Go twin, so
  only the Emacs half is uncovered).
- Interaction record/replay, including `AGENT_REPL_RECORD_INTERACTIONS=1` —
  `lisp/interaction-record.el:98`, `:212`, `:281`, `:404`, `:466`.
- Autosave sweep of every modified buffer every 5 minutes —
  `lisp/autosave.el:62`, `:110`.
- Debug keymap (`SPC j h`): dump workspace, clear/obliterate state, refresh
  state, toggle logging, set durable log level, toggle verbose-to-disk, cancel
  timers, set owning workspace — `lisp/keybindings.el:400-643`.
- `memory-state.el`'s continuous out-of-process state dump —
  `lisp/memory-state.el:117`, `:26`.
- Sentinel watcher reset/nuke recovery commands — `lisp/sentinel.el:372`,
  `:388`.
- Skill-symlink provisioning and the repo pre-commit hook installer, with
  auto-install-on-startup-when-doctor-complains — `lisp/install.el:180`,
  `:190`, `:198`, `:205`, `:285`.
- Webview keyboard affordances: `y`/`C-c` copy selection, `h`/`l` chess board
  stepping, text-size increase/decrease/reset — `lisp/frontend.el:545`, `:575`,
  `:763`, `:773`, `:783`.
- Per-workspace clipboard slot, arbitrary data payloads, PGN board popup,
  profiler-report file, `/runtime-eval-code` — `lisp/worktree.el:2093`,
  `:2177`, `:2118`, `:2212`, `:2354`.
- Sidebar keyboard navigation (`C-S-n` / `C-S-p` / `C-S-RET`) and the
  single-prompt-at-a-time guard — `lisp/sidebar.el:1424`, `:1457`, `:1611`.
- Input-history persistence per project root, fuzzy history search, the ⏎
  glyph for multi-line entries — `lisp/history.el:195`, `:577`, `:560`.
- Output-feed navigation commands (six, wrapping) — `lisp/output-nav.el:129`.
- `agent-repl-copy-reference`, `agent-repl-copy-workspace-name`,
  `agent-repl-revert-and-eval-buffer`, `agent-repl-reload-config`,
  `agent-repl-print-git-branch` — `lisp/commands.el:992`, `:1006`,
  `lisp/keybindings.el:361`, `:389`, `lisp/core.el:2348`.
- **Confidence.** High that each is unmentioned; low significance, since none
  crosses the wire.

---

## Checked and accounted

Features verified as covered — explicitly or implicitly — and therefore NOT findings.

**Explicitly ruled removed / dead by the plan**
- The `:hibernated` render state, teal color, tab palette row and face, the 💤
  glyph, hibernated decoders, `agent-repl-hibernate-workspace` (`SPC o z`,
  `lisp/frontend.el:1579`) and its rpc plumbing — named dead code; the
  *Hibernation does not exist on the wire* section is exhaustive on this.
- `agent-repl--context-cost-keep-alive-origin` / `--keep-alive-p` and the
  keep-alive alarm's re-derivation — named dead code (`lisp/context-cost.el:216`).
- The UDS frame/command envelope tables (`lisp/frontend-uds.el`) — named dead
  code; the transport re-points at Connect rpcs.
- The `FailureKind` triage lists (`lisp/failure.el:145-190`) — named dead code,
  re-derived from the frozen 17-arm vocabulary
  (`proto/src/frontend/v1/failure.proto:93-130`).
- Durable merged / merge-failed memory across restart, and the
  re-classification probe (`lisp/session.el:258-276`) — REMOVED by ruling.
- Merged-tab hiding/greying and sidebar greying of merged workspaces
  (`lisp/merge-handlers.el:372`, `lisp/workspace.el:914`, `lisp/sidebar.el:452`)
  — REMOVED by ruling; the information is deliberately not given to Emacs.
- `agent-repl-doom-multi-repo-mode` (`lisp/session.el:548`) — KILLED by ruling.
- Auto-decline of parked permission asks on prompt, and owed-redelivery
  cancellation — dropped by ruling.
- `sidebar.el`'s status table and roster publishing (all of `lisp/sidebar.el`'s
  authoring half, `lisp/sidebar.el:702`, `:742`, `:603`) — "no successor",
  stated in *Gotchas*.
- The merge-phase echo narration (`lisp/merge-handlers.el:161-244`) — the
  merge's whole life is the webapp feed's merge bubble and the roster/footer.

**Ported / subsumed by a named verb, stream, or proto comment**
- Register a workspace by directory, including `agent-repl-add-project-workspace`
  (`lisp/commands.el:3290`) → `RegisterWorkspace`, idempotent by dir.
- Tab switch clearing the attention marker, and all 20 jump chords /
  `SPC 1..0` / `s-1..9` / `M-1..9` / `agent-repl-switch-left|right`
  (`lisp/commands.el:3443`, `:3490-3585`, `lisp/keybindings.el:734`) →
  `SelectWorkspace`, which the plan names as the clearing act.
- Blink-on-notification and the OS banner with its click-raises-and-selects
  behavior (`lisp/notifications.el:249`, `:331`, `:429`, `lisp/session.el:685`)
  → the `notification` push plus the three-treatment policy; cadence pinned to
  `RosterRowAttention` (`proto/src/frontend/v1/sidebar.proto:69-76`), whose
  comment carries the two-blink / 500 ms spec verbatim.
- Notification backend selection, coalescing, the 60 s clickable window, the
  10 s hung-tool kill, and the lazy Emacs server
  (`lisp/notifications.el:372`, `:342`, `:29`, `:52`, `:236`) — implementation
  detail beneath "post an OS desktop notification".
- Close (tab gone, worktree kept) — `lisp/worktree.el:1792` → `CloseWorkspace`,
  including the quiet requirement and footer-surfaced refusal.
- `agent-repl-kill-workspace` / `-kill-all-workspaces`
  (`lisp/commands.el:889`, `:916`) → `KillWorkspace`, under the doom→contract
  vocabulary rename the plan states.
- `finish` / worktree+branch removal (`lisp/worktree.el:1812`) →
  `NukeWorkspace`.
- Open a workspace, including revival of a parked one
  (`lisp/worktree.el:2052`, `lisp/open-progress.el:426`) → `OpenWorkspace`;
  "any revival happens under the hood".
- Merge request and its enqueued ack (`lisp/worktree.el:1842`) →
  `MergeWorkspace`; success means ENQUEUED.
- Merge-conflict continuation (`lisp/worktree.el:2962`,
  `lisp/merge-handlers.el:125`) → the `merge_parked` composer arm, whose
  comment routes everything typed to the merge's resolution agent.
- Merge dequeue offer on interrupt (`lisp/merge-handlers.el:309`, `:352-370`)
  → `AnswerHeldOffer` / the held-offer merge-dequeue body.
- The refusal to tear down mid-merge (`lisp/workspace.el:1105`) → the
  `merging` composer arm plus CloseWorkspace's quiet requirement.
- Interrupt (`lisp/commands.el:648`, `lisp/input.el:196`) → `Interrupt`
  (`service.proto:72`).
- Permission answering with a free-text deny reason
  (`lisp/permission.el:334`, `:365`) → `AnswerPermission` with
  `AnswerPermissionDenyReason{text}`
  (`endpoint_answer_permission.proto:35-41`).
- Clipboard-image attach (`lisp/clipboard-image.el:183`) → `UserContentBlock`'s
  `ImageBlock` arm (`proto/src/conversation/v1/user.proto:44`).
- AI conversation title in the mode line (`lisp/ai-title.el:191`) →
  `HostWorkspaceNaming.title`
  (`endpoint_watch_host_workspace.proto:169-175`).
- Prompt summary and its "X ago" stamp (`lisp/prompt-summary.el:282`, `:260`)
  → the roster row's last-prompt-summary element; the daemon authors it.
- Context/cost alarm rendering (`lisp/context-cost.el:211`, `:202`) — the plan
  keeps the alarm and only re-derives its keep-alive origin.
- Session-death / resume-failure prose (`lisp/failure.el:344-430`) →
  daemon-composed reasons plus the frozen failure vocabulary; the plan states
  Emacs is render-only for anything on `frontend.v1`.
- Client-classified failures for daemon unreachability and Emacs's own
  boot/decode faults (`lisp/failure.el:60`, `:467`) → the client-owned arms
  `daemon_unreachable`, `boot_failed`, `control_plane_failed`,
  `frame_undecodable`, `stale_bundle`
  (`proto/src/frontend/v1/failure.proto:125-130`).
- Terminal-continuity fencing and its once-only retry
  (`lisp/open-fence.el:63`, `:95`, `:119`) → the `terminal {rehydratable}`
  standing arm.
- Doctor output for standing faults (`doctor.el`, `lisp/daemon.el:294`) →
  `HostSessionLive.faults` ("for doctor output") plus
  `DaemonHealth`/`SessionHealth`, where UNHEALTHY IS AN ANSWER.
- `agent-repl-session-health` (`lisp/frontend-client.el:755`) →
  `SessionHealth`.
- Transcript selection and resume (`lisp/transcripts.el:171`, `:199`) → the
  `existing`/`terminal{rehydratable}` session model plus `backfill`; the plan
  names the `HostSessionId` as exactly what Emacs correlates transcripts
  against.
- Reconnect / daemon-restart re-establishment, including the
  "session did not come back" announcement
  (`lisp/workspace-create-client.el:485`) → "After a daemon restart Emacs
  re-registers (idempotent by dir) and re-subscribes; that is the normal path,
  not an error", plus the handover rendezvous.
- Prompts held across a link outage and drained on link-up
  (`lisp/prompt-queue.el:59`, `:173`, `:295`) → "Prompts arriving during the
  window are HELD (never errored) and replay in order on the new daemon",
  plus the daemon-hold tray (`WatchDaemonHolds` / `UpdateHeldPrompt`).
- Composer gating (`lisp/input.el:621` and the send guards) → the `composer`
  oneof; the plan states the gate is Emacs's to enforce, host-native.
- `SPC o l` webview reload for a rebuilt bundle (`lisp/frontend.el:1330`) →
  the `reload_webapp` push arm.
- Opening a file/plan/finding location in a right-side half-width popup, dired
  for a directory (`lisp/tasks.el:200`, `lisp/explain-config.el:104`) → the
  shared editor-popup subroutine, mandated by both the plan and
  `proto/src/frontend/v1/feed.proto:323`, `:1497`, `:1530`.
- The 1 Hz tab repaint (`lisp/status.el:979`) → superseded by the stated push
  cadence: event-driven, whole-replace, no ticks; clients tick clocks locally.
- Roster reads for tab bookkeeping generally → "Emacs consumes it, if at all,
  only for tab bookkeeping — and its two inputs to the roster are exactly
  Register and Select."
