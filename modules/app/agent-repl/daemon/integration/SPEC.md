# Daemon integration suite — specification

Owner: the daemon teamlead. Implemented by ONE dedicated subagent that never
runs it; the teamlead runs it. It exercises a REAL daemon process against
FAKES of every neighbor: a fake shim.v1 server, fake git repositories, a
temp state root, a fake store socket path, a fake webapp dist. It never
runs the real shim, the real store, Emacs or the webapp, and never calls
the vendor (`AGENT_REPL_FORBID_VENDOR_CALLS=1` in every process).

## Harness (`daemon/integration/harness`)

- Build once per `go test` run: `go build -o <tmp>/claude-repld ./cmd/claude-repld`
  and `go build -o <tmp>/fakeshim ./integration/fakeshim` (a `TestMain`).
- `StartDaemon(t, opts) *Daemon`: temp state root; env
  `AGENT_REPL_STATE_DIR`, `AGENT_REPL_FORBID_VENDOR_CALLS=1`; flags `-fake`,
  `-node <fakeshim>`, `-shim <tmp>/main.js` (a placeholder file), `-webapp
  <tmp dist with index.html + assets/app.js>`, `-store-socket <tmp>/store.sock`
  (nothing listens), `-prompts-dir <copy of prompts/>`, `-default-config-dir
  <tmp root A with .claude.json {oauthAccount:{emailAddress:"a@x"}}>`,
  `-multi-repo-config-dir <tmp root B>`, env `MULTI_REPO_ROOT=<tmp>/multi`.
  Wait for `daemon.addr` to appear (poll the file, bounded by the test
  context; no fixed sleeps) and dial it with a Connect client (binary
  codec by default; one test uses JSON). Capture the daemon's stderr and
  expose the run log + per-workspace daemon.log for assertions. `Stop()`
  sends SIGTERM and waits; `Kill()` for crash simulation.
- `NewRepo(t) *Repo`: a FAKE repository — a directory with a `.git` marker and
  a row in the test's fixture file, carrying one commit on `main`. GIT IS NEVER
  CALLED: a scripted `git` (`integration/fakegit`) is placed first on the
  daemon's PATH and answers every command the git leaf issues out of that
  fixture file, so commits, branches, worktree lists, conflicts, landed ranges,
  changed paths and cleanliness are all fixture data. No `git init` and no real
  git binary anywhere. Helpers: `Commit(file, content)`, `Branch(name)`,
  `Checkout(name)`, `Head()`, `Worktrees()`, `Branches()`, `HasBranch`,
  `HasWorktree`, `LogSubjects(ref)`, `AddWorktree(name)`, `CommitIn`, and the
  scripting verbs `ScriptConflict(worktreeDir, branch, paths...)`,
  `ScriptFailure(dir, exit, stderr, match...)`, `SetDirty`, `SetPaths`.
- `Register(t, d, repo) WorkspaceRef` via RegisterWorkspace.
- Fake shim (`daemon/integration/fakeshim`, a Go `main`): accepts the real
  argv (`<main.js> --listen <uds> --store-socket <p> --log-fd 3 [--fake]`),
  takes the two kernel locks exactly as the shim contract states (workspace
  lock keyed by cwd; session lock keyed by the vendor session id once
  StartSession assigns one), writes JSONL to fd 3, serves shim.v1 on the
  UDS, and opens a CONTROL listener at `<uds>.ctl` (a tiny line-JSON
  protocol) through which the test SCRIPTS it: `push_session_update {…}`,
  `push_agent_frame {agent, frame}`, `push_bash {work, frame}`, `answer
  <rpc> {response}` (queue the next answer for a verb), `expect <rpc>`
  (returns the next received request), `exit <code>` (die), `hang` (stop
  answering), `drop_stream <name>`. Default behaviors: StartSession answers
  `success{SessionStarted{vendor_session_id: fresh uuid, runtime{shim_build_sha:
  <env FAKESHIM_BUILD_SHA or "fake">}, effective_model, permission_mode,
  model_catalog:[opus, sonnet, haiku]}}`; WatchSession immediately pushes
  `diagnostics{healthy}` then stays open; StartTurn answers success with
  the AgentPrompt echo and an empty page; WatchAgent answers an empty page
  and stays open; UpdateAgent/KillTurn/KillSession succeed; Hibernate
  succeeds; ReadHistory answers an empty floor page. Every request is
  recorded and retrievable via `expect`.
- The harness exposes `d.Shim(ws)` (the fake's control client for that
  workspace's UDS) and helpers to read views: `d.WatchFooter(ws)` etc.
  returning channels; `AwaitView(t, ch, pred)` bounded by the test context.

## Suites and tests (one `_test.go` file per suite; one edge case per test)

### boot_test.go
- daemon writes `daemon.addr` with `127.0.0.1:<port>\n` after binding and
  removes it on SIGTERM
- a second unflagged daemon on the same state root exits non-zero without
  disturbing the incumbent (addr file unchanged, incumbent still serving)
- `-joining` daemon binds a fresh port and does NOT write daemon.addr while
  it owns no workspace
- boot refuses loudly (exit non-zero, run log names the misconfig) when the
  state root is unwritable
- boot opens WSM fresh (no state.db import): a pre-existing `state.db` is
  left untouched and `wsm.db` is created
- pprof: `-pprof 127.0.0.1:0` serves /debug/pprof; `-pprof 0.0.0.0:6060` is
  refused at boot
- run log is JSONL per logging-contract.md (timestamp pattern, runtime
  "daemon", pid, operation, context)

### register_select_test.go
- RegisterWorkspace mints an id; re-registering the same dir in another
  spelling (trailing slash, `~`, symlink) returns the same id
- RegisterWorkspace on a dir that is not a git worktree answers the
  intended-arm refusal (Connect error naming `RegisterWorkspaceError.<arm>`)
  and logs the arm at WARNING
- SelectWorkspace stamps `current` on the roster push and the row's
  last_selected; re-selecting is success and produces no duplicate push
- a per-workspace rpc with an unknown WorkspaceRef answers NotFound naming
  the intended arm; a ref whose `dir` disagrees with the registry for its
  `id` is refused (workspace_ref_mismatch)
- a request with an unset non-optional field answers InvalidArgument naming
  the field (SubmitPrompt without `said`)

### roster_test.go
- WatchWorkspaceRoster delivers the latest roster first to a late
  subscriber (subscription invariant)
- two subscribers receive identical sequences
- registering a workspace produces exactly one roster push with the new row
  under its repo section with status `none`
- a workspace with a live session reports `ready` after readiness, `thinking`
  during a turn, `done` after the turn concludes, `permission` while a
  permission is open, `idle_async` with detached work live and no turn
- priority ordering: rows sort P05 < P1 < P2 < P3 < unprioritized;
  SetWorkspacePriority reorders and carries the badge label; clearing
  removes the badge
- attention marker set on a notification push, cleared by SelectWorkspace
- closed workspace draws `closed:true`; nuked workspace leaves the roster
- recently_merged lists a merged workspace with `when.merged`
- task view: CreateTask/UpdateTask/AssignWorkspaceTask group rows under the
  task section with the done check; unassign returns the row to the repo
  grouping; blank title refused with the intended arm

### session_lifecycle_test.go
- OpenWorkspace spawns the fake shim with the contracted argv and env
  (assert on the fake's recorded argv: `--listen`, `--store-socket` = the
  explicitly passed socket (flag beats env AGENT_REPL_STORE_SOCKET),
  `--log-fd 3`, `--fake`; env CLAUDE_CONFIG_DIR, AGENT_REPL_OWNED=1,
  AGENT_REPL_STATE_DIR, SHIM_BUILD_SHA, AGENT_REPL_FORBID_VENDOR_CALLS) and
  cwd = the workspace dir
- readiness gates on the first healthy diagnostics: with the fake told to
  delay the diagnostics push, HostWorkspace shows `shim_attached:false`
  until it arrives, then `true`
- a fake shim that exits during bring-up ends bring-up immediately with the
  exit code and stderr in the host stream's faults and the footer's
  `disconnected.start_failed`
- StartSession(fresh) is sent only for a workspace with no prior
  conversation; a workspace whose session record carries a vendor session
  id gets StartSession(resume{vendor_session_id})
- resume of a vendor transcript that is missing on disk is refused before
  spawn (no fake shim process is started; the host stream shows the
  intended fault)
- StartSession(resume) answering `cold` produces a FeedColdGate standing row
  with the shim's facts, footer `waiting.cold_gate`, and no re-open until
  AnswerColdGate; AnswerColdGate{pay} re-opens with `cold_remediation{pay}`;
  {compact{model,scope}} echoes exactly; a scope the menu never served is
  refused with the intended arm
- the config dir is DETERMINED: a workspace under MULTI_REPO_ROOT spawns
  with CLAUDE_CONFIG_DIR = the multi-repo root, another with the default;
  the topbar account reads each root's .claude.json email; a root without
  it draws `logged_out`
- KillWorkspace sends KillSession{force:true}, the shim process is reaped,
  HostWorkspace shows `terminal{rehydratable:true}`, the roster shows `dead`
- CloseWorkspace with nothing live succeeds and leaves the shim running
  (session untouched); with a turn in flight answers `blocked` and the
  footer shows `closing.blocked` with the composed reason; with a held
  prompt refuses; with a queued merge refuses; with a standing cold gate
  succeeds
- NukeWorkspace kills, removes the worktree and branch (assert on the fake
  git repo), and removes the row
- RestartWorkspace{force:false} prelaunches a second fake shim (inert: no
  StartSession until freeness), waits for the running turn to end, sends
  KillSession to the old, reaps it, then StartSession(resume) on the new;
  prompts submitted meanwhile appear in the tray with `build_refresh`/restart
  hold and drain after readiness; {force:true} interrupts first
- build-staleness bounce: a fake shim reporting a different
  `shim_build_sha` than the daemon's stamp is relaunched at freeness
- hibernation: with `-idle-cutoff 1s`, an idle session gets Hibernate then
  KillSession; the roster shows the parked state per the sidebar arms; the
  next SubmitPrompt revives (StartSession(resume)) and delivers
- crash-boot adoption: kill the daemon with SIGKILL while a fake shim
  holds its locks; a new daemon probes the workspace lock, adopts the
  running shim (no second spawn; the fake sees a new WatchSession), and
  reconciles: the intent manifest absent → reports UNKNOWN/PRESERVED per
  session in the host faults, never a count

### prompt_test.go
- SubmitPrompt on an idle session mints a TurnId, mirrors a user_prompt
  row stamped with it on WatchFeed (metaprompt sentinel spans stripped from
  the drawn row), sends StartTurn with the same TurnId, said, and origin
  WEBAPP_USER_SENT / the origin implied by the caller
- duplicate idempotency_key answers the same TurnId and sends no second
  StartTurn
- SubmitPrompt while a turn is in flight is HELD: tray shows the HeldPrompt
  with `classifying` then a verdict from the -fake heuristic; a prompt
  beginning with "stop" takes the fast path to `interject`; the running
  turn is interrupted (KillTurn), the footer shows `waiting.interrupting`
  immediately, and the held prompt is delivered only after the fake pushes
  the turn's terminal
- hold_for_turn_end delivers FIFO after the turn ends; two held prompts
  deliver in order
- UpdateHeldPrompt{drop} removes the entry durably (survives a daemon
  restart: restart and assert it is gone); {release} delivers now
- a held prompt survives a daemon restart (all-or-nothing restore) and a
  corrupted held_prompts row makes the restore load nothing and log ERROR
- a prompt submitted during a merge lease answers `merging` refusal;
  prompts held before the merge stay held
- SubmitPrompt with `feed` set to a subagent bubble delivers via
  UpdateAgent.prompt addressed to that agent
- `/status` answers a StatusPanelView inline with no StartTurn AND mirrors a
  non-durable `command_panel` row onto the root feed (absent after a daemon
  restart's first page); `/agents` and `/help` answer `command_refused`
  (also mirrored) and never reach the shim; an unknown slash command falls
  through as a prompt; `/clear`
  and `/compact` go through the queue as session acts and produce a
  separation row on the ContextCut record; `/model` with an argument
  submits the model change; bare `/model` is refused/absorbed; 
- a second submit while a turn runs on the same agent through the bubble
  path answers the daemon-fault refusal
- SetModel with a catalog token sends SetSessionModel and the topbar
  selector updates only when the shim pushes model_changed; a token not in
  the catalog is refused with the intended arm; SetPermissionMode
  likewise; an ungated mode without creation consent is refused
- Interrupt{turn} with live detached agents answers confirm_required with
  the count; resend with confirm_agents stops them; Interrupt with nothing
  running answers nothing_running
- AnswerPermission{allow_once|allow_standing|deny{reason}} forwards the
  correct AgentPermissionDecision (standing echoed from the daemon-held
  offer, never from the client); allow_standing on a card without
  standing_offered is refused
- AnswerQuestion echoes served texts/labels; an unserved label is refused;
  multi-pick on single_select is refused

### feed_test.go
- OpenFeed(root) answers the newest page + token; WatchFeed with that token
  tails exactly after the page (no gap, no overlap) while frames arrive
  between open and watch
- WatchFeed with an unminted token is refused at the transport
- GetFeedPage{next} with no walk standing is refused; {first} then {next}
  walks older pages; the walk is per connection (a second client's walk is
  independent) and never persisted (restart → next refused)
- a growing response re-pushes the same FeedId with accumulated prose
  (`update`), then `success` with the whole text on the terminal frame; a
  lost fragment self-corrects on the terminal
- tool cards: read → `code` output with paint spans and `omitted` for a
  head cut; write/edit → `diff` lines; grep → `lines` with omitted; glob;
  bash foreground → `text`; a `progress` frame re-pushes `running.last_progress`;
  a failure → `returned.failed`; a denied permission → `denied`
- skill card from exactly the two frames (start, success{document}); no
  later row parents under it unless the resolver chooses
- plan-mode enter then exit coalesce onto ONE FeedId (`planning` →
  `planned{prose, edit}`); exit without enter is legal
- worktree enter/exit draw separation dividers with the token delta unset
- an AgentUpdate `context_cut` arm (cleared/compacted/compaction_failed)
  draws the separation row with formatted before/after; compaction_failed
  draws no separation and surfaces the error
- permission start → `permission.open` row + footer `waiting.permission`
  + host `notification{permission_requested}`; answered → `answered` re-push
- question start → `question.open`; answers → `answered` with echoed labels
- a subagent spawn draws a bubble head (label, description, runtime);
  OpenFeed on the bubble's FeedId serves the sub-feed; frames carrying the
  created agent id route to that sub-feed; settled → `settled.succeeded`
  with tokens; a detached subagent gets `detached_subagent` and its own
  WatchAgent opened EAGERLY (assert the fake saw it before any OpenFeed)
- detached bash: `detached_shell` head, spool tail from WatchBash deltas,
  `settled.completed{exit}`; a spool gap (from_offset mismatch) is refused
  and logged
- turn terminal: `turn_ended.concluded{answer}` stamps the answering
  response; api_request_failed arms respell to the FeedTurnEndedErrored
  arms (429 with retry_after, 401, refusal, max_tokens, query died);
  interrupted → `interrupted`
- hook blocked/failed draw cards; succeeded draws nothing
- artifact publish draws the purple bubble; list draws nothing
- findings draw rows in served order
- unmodeled tool draws NO row and adds one topbar warning per distinct name

### footer_topbar_test.go
- footer pushes are whole views, deduplicated (an identical frame yields no push)
- every panel arrives populated on every push (agents/tasks/shells/monitors/crons)
- status tree: idle.ready → thinking.submitting (on StartTurn) → thinking →
  idle.done; `interrupted` is retired by a daemon-side dwell into the
  successor push with no client action; `loading` likewise
- tokens cell formats input misses (written+unwritten) and excludes cache
  reads; usage stamped once per response is not double-counted across the
  response's units; verdict `incomplete` when a response carried no usage
- live-work chips: agents count, shells count, tasks done/total from
  task_act state, monitors, crons from AgentCron listed; unset when zero
- wakeup: schedule_wakeup scheduled → `waiting.wakeup` only when nothing
  else stands; a real status wins
- rate_limited and context_budget precedence: notification outranks both
- topbar: title from naming; model selector from the catalog with the
  effective model selected; context chip from context_usage push; warnings
  for session faults (diagnostics unhealthy) retracted on the next healthy
  push; degraded windows drawn open then closed; connectivity tone/glyph
  from the vocabulary file; account email
- the /context panel resolves from the same context_usage fact
- permission_mode_picker serves the switchable set with the current mode;
  a permission_mode_changed push updates `current`
- an `api_error` page-line arm mid-turn draws the footer `thinking.retrying`
  style evidence and does NOT end the turn; the turn's terminal is authoritative
- link death: dropping the fake's WatchSession stream flips footer to
  `disconnected.severed` and the roster to `severed`; the daemon redials
  forever with backoff; the fake exiting flips to `dead` and stops redials

### merge_test.go (self-repo path with fake git; the daemon's own checkout identity is injected via `-self-repo <dir>` or equivalent test hook)
- MergeWorkspace on a workspace without layout facts is refused pre-state
- enqueue: queue tab row on the merge bubble's sub-feed, footer
  `merging.queued{position,depth}`, roster `merge_queued`; a second
  workspace in the same repo queues behind; pause/resume/evict via
  UpdateMergeQueue
- Interrupt while queued raises the dequeue HeldOffer; AnswerHeldOffer
  {release} evicts, {keep} keeps
- the emacs-repo method: pre-prompt tab (when configured) runs the session
  under the lease with origin MERGE_BEFORE_ACTION and its rows parented to
  the tab (output address); merge tab narrates the no-ff commit; a
  conflicting branch opens the conflicts tab, the agent is prompted once
  with the brief from prompts/ (assert the placeholders were spliced),
  then parked: host composer `merge_parked`, footer `merging.parked`, a
  SubmitPrompt while parked lands in the conflicts tab (not refused, not a
  session turn)
- tests tab runs `bin/test-all.sh --suites …` (a fake test-all.sh in the
  repo records its args and exits per a control file): pass → settled; fail
  → fixes tab with the brief, then parked on escalation; no re-run on flake
- post-prompt failure never fails the run (rides the terminal)
- landed: FeedMergeSuccess{commit}, footer `merging.merged`, roster `merged`,
  recently_merged, worktree removed only after the terminal push; the
  merge ledger holds the tab intervals; a landed range whose target is the
  self-repo triggers rollout.Trigger (assert via the fake deploy script)
- the non-emacs repo method runs only pre/post prompts
- a merge in flight across a daemon restart is resumed or loudly failed
  (never a stuck lease)
- missing brief file fails the step loudly

### drain_rollout_test.go
- UpdateShutdownSchedule{schedule{at, reason}} pushes drain_scheduled on
  every WatchDaemon subscriber (Emacs + N webviews); {cancel} pushes
  drain_cancelled; reason blank note refused
- during a drain, new prompts are held with `shutdown` hold; the daemon
  exits after the in-flight turn ends and never interrupts the vendor;
  refusal logs are rate-limited (N refusals → one WARN with counts)
- {now{reason}} announces shutdown_announced{cause immediate, no address}
- handover: `rollout.Handover` test hook spawns a `-joining` successor;
  WatchDaemon pushes shutdown_announced{address}; a busy workspace is not
  transferred until its turn ends; at freeness the old daemon pushes
  `transferred` on WatchHostWorkspace and WatchWebWorkspace{address};
  per-workspace rpcs on the old answer transferring_away (intended arm);
  on the new before adoption answer not_yet_adopted; AdoptHostWorkspace +
  AdoptWebWorkspace both called → both succeed together, the new daemon
  adopts the running fake shim (new WatchSession seen), the held intake
  drains in order; headless workspace transfers without any adopt call;
  the old daemon exits after the last transfer and the successor writes
  daemon.addr
- a never-free workspace leaves both daemons up with a periodic WARN
- reload_webapp: the webapp-only trigger pushes reload_webapp with no address
- asset origin: GET / serves index.html with Cache-Control: no-store;
  rewriting index.html on disk is served on the next request without
  restart; /assets/* is served without no-store

### health_clientlog_login_test.go
- DaemonHealth healthy; with an open fault record unhealthy{faults}
- SessionHealth for a live session healthy; with the fake pushing
  diagnostics unhealthy → unhealthy{faults}; unknown workspace → error
- ClientLog writes a JSONL record into the workspace's webapp.log sink
- OpenLogin spawns the pty (a fake `claude` on PATH that prints a marker
  and echoes input), WatchLoginTerminal (server stream) replays scrollback
  then streams; SendLoginInput{keystrokes} is echoed back on the stream;
  {resize} is applied; CloseLogin ends the stream with `closed`; a second
  OpenLogin joins the same pty
- OpenInEditor{path, line} relays an `open_in_editor` push onto that
  workspace's WatchHostWorkspace stream; an unknown workspace is refused
- OpenExternal invokes the configured browser launcher with the url
- workflow rpcs / arms answer the typed not-implemented refusal

### commandfile_test.go
- a `workspace_commands_*.json` file with a create entry materializes a
  workspace exactly like CreateWorkspace; a merge entry enqueues; a prompt
  entry submits; a malformed file is quarantined and logged, never
  ingested; ingestion is atomic (a half-written file is not claimed)

### create_test.go
- CreateWorkspace standard: slug derived from the initial prompt, branch
  and worktree created off the default branch, layout facts recorded,
  registration only after materialization, initial prompt submitted with
  origin WORKSPACE_CREATED; base_ref honored; a bad base_ref refused; name
  supplied wins; a parent makes the child nest under it and its merge
  target the parent's worktree; fork ports the parent's transcript (the
  fake transcript file appears under the child's config root project dir)
  and StartSession(resume) is sent
- one-shot: prompt decorated from prompts/ (assert the splice), finish
  action recorded; self_merge enqueues on completion; open_pr runs the PR
  post-prompt
- merge_actions recorded and read back by a later merge
- ungated permission mode without allow_ungated refused

## Log discipline
Every test runs with the daemon at ≥WARNING terminal mirror; the harness
FAILS a test that produced any WARN/ERROR record it did not explicitly
expect (`d.ExpectWarnings(ops...)`), so the remediation loop drives
warnings to zero.
