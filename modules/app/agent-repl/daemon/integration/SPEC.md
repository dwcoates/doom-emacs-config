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
  `HasWorktree`, `AddWorktree(name)`, `CommitIn`, and the
  scripting verbs `ScriptConflict(worktreeDir, branch, paths...)`,
  `ScriptFailure(dir, exit, stderr, match...)`, `SetPaths`.
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
- STATE-DATABASE CORRUPTION: `d.WithDB(func(*sql.DB))`, `d.CorruptRow(table,
  column, keyColumn, key, value)` and `d.CountRows(table)` open `wsm.db`
  directly with the daemon's own sqlite driver. They exist for exactly one
  thing — producing the half-written row no rpc can produce, so a restart can
  be watched refusing it — and the daemon must be STOPPED while they run.
- ORDERING PROOFS: the fake shim records every verb on one timeline in its
  durable sink, and `harness.ShimVerbOrder(t, dir)` /
  `d.AwaitShimVerbOrder(dir, verbs...)` read it back. The in-memory recorder
  answers per verb and so cannot say whether Hibernate preceded KillSession;
  this can.
- ALREADY-RUNNING WORK: `ShimProfile.LiveWork` (built with
  `harness.EncodeLiveWork(t, items...)`) is what the fake's `SessionStarted`
  states as `live_work`. It is a startup PROFILE rather than a scripted
  answer because the opening is the daemon's first request, which a
  control-socket script would be racing.
- CODECS: `Opts.JSONCodec` dials the daemon with the JSON codec instead of
  the binary one, so one test proves both are served on the one origin.
- PAGE SIZE: `harness.FeedPageSize` IS the daemon's own
  `internal/resolve/feed.DefaultPageSize`, never a copy. A walk test pushes
  `FeedPageSize + 1` rows and FAILS if `has_more` is unset; it never skips on
  "the page size is unknown".
- TRANSCRIPTS: `harness.TranscriptPath` and `harness.HasTranscript` read back a
  `<vendor session id>.jsonl` under either account root, which is how
  account-switch PORTING is watched under a root no session has ever run in
  (the fake shim writes one at every StartSession). `d.RemoveTranscripts` is
  the other side (a resume whose transcript is gone).
- LAUNCHER FAILURE: the fake browser and every other recorder executable take
  `SetExitCode(n)`, so `OpenExternal`'s `launch_failed` arm is driven by a
  launcher that really exits non-zero rather than by a stub.
- VENDOR-GUARD REFUSAL SITES: `Opts.NoFake` starts the daemon WITHOUT `--fake`
  and withholds `AGENT_REPL_CLAUDE_BIN`, so the classifier's headless run and
  the login pty reach their real implementations and `envc.VendorGuard` refuses
  them naming the site. It is the only way to exercise a guarded site; the fake
  shim is unaffected, since `--node` still names it.
- THE Watch* FAMILY AS ONE TABLE: `harness.WatchKinds()` reduces every view
  stream to `{Name, PerWorkspace, Open}` with the pushes type-erased to
  `proto.Message`, so an invariant stated over the WHOLE family (the
  subscription invariant; flush-on-accept) is one table-driven test rather than
  seven copies. `WatchFeed` and `WatchLoginTerminal` are deliberately outside
  it: the first is a tail from a minted token and the second carries pty bytes,
  so "the first push is the last-published view" is not their contract.
- FLUSH-ON-ACCEPT: `Stream.AwaitHeaders` reads Connect's response headers,
  which arrive only when the server FLUSHES them. Nothing else on the wire
  separates a stream that is open with no view yet from one that never
  answered.
- `harness.DialAt(t, addr)` builds a client against an address other than
  `daemon.addr`, for the joining daemon that publishes its own `joining.addr`.

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
- SelectWorkspace stamps `current` on the roster push, and does NOT drive the
  when-column (which shows last activity, not last viewing): a never-active
  selected workspace's when falls back to `created`, never `last_selected`;
  re-selecting is success and produces no duplicate push
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
- attention marker set on a notification push, cleared by SelectWorkspace and
  by the last open ask settling
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
  delay the diagnostics push, HostWorkspace carries NO session at all
  (`host.none`) until bring-up finishes — Fleet.Start only calls f.remember
  (what HostSessionFacts reads) after bringUpClient returns, and
  Supervisor.Spawn itself blocks internally until the shim's first healthy
  diagnostics arrives, so there is no session record for any arm — including
  `existing.live.shim_attached:false` — to carry while diagnostics is
  withheld. Once it arrives, HostWorkspace shows
  `existing.live.shim_attached:true`.
- a fake shim that exits during bring-up ends bring-up immediately with the
  footer's `disconnected.start_failed`; the exit code and stderr are NOT on
  the host stream — bring-up died before any session record existed, so
  there is no HostSessionLive arm to carry `faults` on — but they ARE
  readable from SessionHealth, which needs only a registered workspace (not
  a live session) and reports `shim_start_failed{exit_code, stderr_tail}`
  among the unhealthy faults.
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
- a `forget` command-file entry on a CLOSED workspace removes the row and,
  when it was the last workspace under its repository, that repository's
  section too, touching no files; on an OPEN one the file is quarantined and
  the row stands (the rpc is unlanded, so the command-file ingress is the only
  route -- see ERROR-ARMS.md)
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
  draws its own divider (label "compaction failed", the error verbatim,
  `tokens` UNSET) and also surfaces the error on the turn's terminal
- the run's own five terminals (max_turns, budget_exhausted,
  execution_error, structured_output_retry_exhausted, stop_hook_prevented)
  draw their own FeedTurnEndedErrored arms with composed headlines; the four
  FailureVendor* ones also resolve the roster PURPLE
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

### image_origin_test.go
- an attached `ImageBlock{path}` draws as an image block with a src and the
  file's own name as alt text -- never the `unsupported block: image`
  placeholder a missing resolver produced
- a GET of that src off the daemon's own listener answers the file's bytes
  under the RECORD's media type
- an id no conversation registered is 404: the origin serves only what a feed
  already drew, so an arbitrary host path has no id a page could ask for

### footer_topbar_test.go
- footer pushes are whole views, deduplicated (an identical frame yields no push)
- every panel arrives populated on every push (agents/tasks/shells/monitors/crons)
- status tree: idle.ready → thinking.submitting (on StartTurn) → thinking →
  idle.done; `interrupted` is retired by a daemon-side dwell into the
  successor push with no client action; `loading` likewise
- tokens cell is the main agent's context growth: one `context_usage` push
  moves the topbar's chip ("118.2k") and the cell ("18.2k in", against the
  100k the turn opened on) together; the tokens PANEL's summed input line
  formats input misses (written+unwritten) and excludes cache reads; usage
  stamped once per response is not double-counted across the response's
  units; verdict `incomplete` when a response carried no usage
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
- one-shot: prompt decorated from the repository's policy (assert the
  preamble, the commission and the completion directive); no finish action is
  recorded and none is taken on completion
- merge_actions recorded and read back by a later merge
- ungated permission mode without allow_ungated refused

### host_notifications_test.go
- a `question_asked` host notification carries its `header` and sets the
  workspace's attention marker; `agent_addressed` carries its text (skipped —
  no production route, see the settled-behaviors section)

### Coverage the second adversarial audit added

Folded into the suites above rather than listed twice; recorded here so the
audit's charge can be reconciled against the files.

- COMMAND-FILE INGRESS beyond create/merge/prompt/task-create: one test per
  verb (send, close, open, switch, task-toggle-done, task-add-workspace),
  `dir` addressing proven equivalent to `workspace`, one-shot decoration,
  `base_ref`, and a file with ONE invalid entry applying NOTHING.
- HOLD ARMS: `uninterruptible_turn` (a /clear's turn open) reaches the tray
  with NO classifying push and refuses release; a revival-time entry carries
  `session_starting` and refuses release; `AnswerHeldOffer` with nothing
  standing answers `no_offer_standing`; `UpdateMergeQueue{evict}` clears a
  standing dequeue offer and the tray heading counts down.
- ACCOUNT ROUTING: the topbar email and `HostVendorClaude.config_dir` are each
  asserted for a workspace inside the multi-repo root and one outside it; a
  fork created outside the root does not inherit its parent's account.
- TASK ARMS: `set_title`, `set_open`, `set_done` twice → `no_change`, a bogus
  ref → `unknown_task` on both UpdateTask and AssignWorkspaceTask, a blank
  title → `blank_title`, and tasks plus assignments surviving a restart.
- FAULT KINDS AND CLOSURE: `prompts_dir_missing.path`, its repair flipping
  healthy, `shim_died{exit_code}`, `link_severed`, and a restart NOT reopening
  a closed fault.
- RELAY REFUSALS: `path_escapes_workspace`, a directory path relaying with
  `line` unset, `invalid_url`, and `launch_failed` driven by a launcher that
  really exits non-zero.
- EXACT FIGURES rather than shapes: the context chip reads "142.3k", the
  tokens panel's input line "1k", breakdown rows carry `share_permille`/`emphasized`, the
  model selector's options are exactly the catalog in order, and the wakeup
  cell carries the scheduled instant itself.
- EITHER/OR REFUSALS SETTLED: the unknown-workspace and no-login-open refusals
  and bare `/model` each assert one outcome, never "an error or an arm".

### Coverage the third adversarial audit added

Folded into the suites above rather than listed twice; recorded here so the
audit's charge can be reconciled against the files.

- THE SUBSCRIPTION INVARIANT AND FLUSH-ON-ACCEPT ARE STATED OVER THE WHOLE
  FAMILY, in `subscriptions_test.go`, table-driven over `harness.WatchKinds()`:
  a late subscriber's first push is the last-published view and everything
  after it arrives in order; the response headers flush at accept before any
  push. `WatchWebWorkspace` is skipped in the first table, naming why: its only
  push arm is `transferred`, and by the time one is published
  `resolveStreamRef` has already flipped the workspace to `transferring_away`,
  so no reachable window has a late subscriber and a standing view at once.
- PUSH CADENCE IS ASSERTED NEGATIVELY: an identical `context_usage` re-push
  produces no topbar push, a repeated hold accept produces no second tray, and
  a re-composed identical host view produces no host push.
- PER-WORKSPACE Watch* TRANSPORT-CLOSED REFUSALS are table-driven over Footer,
  Topbar, DaemonHolds, Host, Web and LoginTerminal, for a bogus id and for a
  ref whose `dir` disagrees with the registry: the refusal lands before any
  frame and logs `daemon.refusal.transport_closed` at INFO, never WARN.
- STATE-DATABASE CORRUPTION AT BOOT is one test per table: a bumped
  `layout.version`, a corrupt `tasks` row, a corrupt `sessions` row of an
  adopted workspace and a corrupt `creation_jobs` row of an admitted merge.
- THE SOCKET-PATH BUDGET REFUSES THE BOOT: a state root past the unix-socket
  path limit exits non-zero with stderr naming `sock/`. The long root is passed
  as a second `--state-dir` through `ExtraArgs`, because the harness's own
  pre-flight would otherwise fatal the test rather than the daemon.
- THE JOINING DAEMON REALLY SERVES: it is dialed at its own `joining.addr` and
  answers `DaemonHealth`, rather than only being observed to leave
  `daemon.addr` alone. The JSON codec likewise carries a SERVER STREAM
  (`WatchWorkspaceRoster`), not just unary verbs.
- A REFUSED HIBERNATE DEFERS THE STAND-DOWN: both a transport failure and a
  typed `turn_in_flight` refusal leave `KillSession` uncalled.
- SPAWN-ON-MOUNT REVIVAL: `OpenWorkspace` on a hibernated row sends
  `StartSession(resume)`; on a terminally deleted record it answers
  `session_deleted`.
- COLD-GATE AND INTERRUPT ARMS: `no_cold_gate` on a resolved gate, `no_session`
  on a shim reporting none, and `shim_refused{detail}` for a refused
  `KillTurn`.
- LOGIN IS IDEMPOTENT PER ACCOUNT, not per workspace: two workspaces under one
  account root share one pty and one banner; one inside `MULTI_REPO_ROOT` and
  one outside get two. `CloseLogin` with nothing open succeeds, and a vendor
  binary that cannot be spawned answers `spawn_failed`.
- THE VENDOR GUARD'S TWO REFUSAL SITES are exercised under `Opts.NoFake`: a
  prompt needing classification is HELD with `hold_for_turn_end` (never the
  producer-less `classification_error`) and an ERROR under
  `daemon.promptqueue.classify`; `OpenLogin` is refused naming "login".
- SELF-RELOAD NEEDS BOTH HALVES: a sibling worktree of the self repository runs
  the Emacs method but triggers no deploy, and a one-shot's merge on any
  other repository triggers none either.
- NUKEWORKSPACE KILLS BEFORE IT REMOVES: `KillSession` reaches the shim with no
  `worktree remove` yet recorded by the scripted git, and a scripted failure of
  that removal answers `git_failed{detail}`.
- CLOSEWORKSPACE IS BLOCKED BY LIVE DETACHED WORK even with no turn open.
- ONE-SHOT COMPLETION DIRECTIVE: the decorated prompt carries the framing
  sentence and the REPOSITORY'S own directive text, a repository missing only
  the directive is refused with `one_shot_policy_missing` naming just that
  file, and a concluded one-shot turn enqueues NO merge — the daemon takes no
  finish action at all.
- THE ROSTER'S STATUS ARMS ARE ASSERTED ONE PER TEST, never inside a
  disjunction: `submitting`, `clearing`, `compacting`, `interrupted`,
  `degraded`, `vendor_blocked` and `merge_failed`, each driven from its own
  real cause.
- THE DISPLACED TURN'S NON-BOUNCE HALF is expressed by count: the displaced
  turn is captured at lease acquisition and resubmitted EXACTLY ONCE at release
  (`StartTurn` reaches exactly two, never three), with the resubmission's
  origin `PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME`. Only the crash-window
  half stays skipped.
- HANDOVER ARMS: `participant_not_expected` for a client that was not open at
  the announcement, and `no_transfer_announced` for an `AdoptWebWorkspace` on a
  plain boot, logged at INFO.
- `shutdown_announced` ENRICHMENT: `expected_outage_ms`, `minted_at_ms` and the
  `scheduled_drain` cause are asserted, including a schedule that actually
  fires.
- NITS: no `.daemon.addr.*` temp sibling survives a boot, pprof serves over a
  unix socket, and `AGENT_REPL_SESSION_ID` is asserted in the spawn env.

## Settled behaviors the daemon states differently from an earlier reading

These are recorded here rather than argued in a test comment, per the audit's
ruling that a divergence between this spec and the daemon's settled behavior is
a spec edit and never a rationalization in the suite.

- A CORRUPTED `held_prompts` ROW FAILS THE WHOLE BOOT. The restore is
  all-or-nothing AND the boot sequence fails every step loudly
  (`internal/boot/sequence.go`), so the daemon does not come up with an empty
  tray: it exits non-zero having logged exactly one ERROR under
  `daemon.promptqueue.restore_holds`. The test asserts that.
- A `workspace`-package REFUSAL LOGS `daemon.refusal.typed` AT INFO.
  `internal/workspace/refusal.go`'s `refuse`/`refuseWith` records the verb's
  refusal as the ordinary answer it is; `daemon.refusal.unlanded_arm` at
  WARNING belongs to `server.UnlandedArm` alone, so that operation stays usable
  for reconciling ERROR-ARMS.md. A test therefore declares that operation only
  when the arm it exercises is genuinely unlanded per ERROR-ARMS.md.
- CLOSEWORKSPACE EVICTS THE LOG SINK AND LEAVES THE LINK. daemon.md's LOG
  SURFACES ruling prescribes "eviction on workspace close": the SINK HANDLE is
  released, and the canonical link and its target stay on disk
  (`internal/workspace/close.go`; pinned by `internal/dlog/surfaces_test.go`'s
  `TestEvictLeavesTheCanonicalLinkAndTargetOnDisk`), because a closed
  workspace's log is still the record of what it did. The close record is
  written through that same sink and therefore lands BEFORE the eviction
  releases it.

- A `workspace_commands_*.json` FILE IS QUARANTINED AT PARSE TIME AND AT APPLY
  TIME. daemon.md's file route for a refused verb is quarantine, so an entry
  the daemon refuses while applying retires the file there with the refusal
  recorded: the rpc route answers the refusal to the caller who made it, and a
  file has no caller to answer, so a file left in `ClaimedDir` would be
  invisible — neither applied, nor swept again, nor anywhere a person looks.

- THE THREE CLAIMS THAT ONCE HAD NO PRODUCTION HOOK NOW DO, and their tests
  run:
  - ACCOUNT-SWITCH TRANSCRIPT PORTING. `internal/workspace/sessions.go`'s
    `Fleet.Start` recomputes `Accounts.ConfigDirFor(dir)` at EVERY start
    (daemon.md 10a) and, when a resume's recorded ConfigDir disagrees, calls
    `Accounts.MoveTranscript` before `StartSession(resume)`.
  - `agent_addressed` HOST NOTIFICATIONS. `internal/sessionwatcher/route.go`
    routes an `AgentPushNotification` START frame to
    `sinks.Lifecycle.OnNotification` as `agent_addressed`, carrying the pushed
    message as the notification's text.
  - `no_browser_configured`. `cmd/claude-repld/graph.go` leaves the Browser
    dependency NIL under `--no-browser`, or when neither
    `$AGENT_REPL_BROWSER_CMD` nor the pinned default launcher exists.

- THE MERGE LEDGER HAS NO WIRE SURFACE. No rpc serves it: it exists only as
  the `merge_ledger` / `merge_tab_intervals` rows in `wsm.db`. The tab-interval
  test therefore stops the daemon and reads the database through `d.WithDB`,
  which is that helper's documented contract. A clean landing records the
  `queue`, `merge` and `tests` intervals: the queue is a tab like every other,
  opened at admission and closed when the run leaves it for its first phase.
  The missing wire surface is recorded for the teamlead as a decision owed, not as suite
  defects.

- FEEDMERGEERROR IS A STALE SHAPE (owed to landing 7). A queued merge can end
  three distinct ways -- evicted by another merge, dequeued by its own release,
  or abandoned by the run giving up -- and `FeedMergeError` carries only
  `failed` and `abandoned`, so the three causes are NOT distinguishable on the
  wire. `dropQueued` now PRODUCES the `abandoned` terminal (remediated): the
  head row is drawn against the bubble's ledger identity before that identity
  is dropped, and the footer and roster shed their merge arms behind it.
  `FeedMergeAbandoned` is an EMPTY message, so the cause rides only in the log.
  The third cause still has no reachable producer — `dropQueued`'s only callers
  are Evict and the dequeue release — so that case stays skipped. A landing-7
  arm per cause, and a field on `FeedMergeAbandoned`, are the changes owed.

- TWO FURTHER CONTRACT CLAIMS HAVE NO PRODUCTION HOOK, recorded with the three
  above rather than silently dropped:
  - A DRAIN SCHEDULE NOW SURVIVES A RESTART (remediated).
    `drain.Controller.Republish` re-announces and re-arms the persisted
    schedule, and the boot's `Prime` step calls it once the push surface is
    bound and before anything is served.
  - A FEED WATCH TOKEN CAN NOW BE EXPIRED ON PURPOSE. `token_expired`
    (`feed.ErrTokenExpired`) fires when a token's pinned start falls out of the
    retained publication log, and `feed.Deps.TailRetention` is now set from
    `--feed-tail-retention` / `$AGENT_REPL_FEED_TAIL_RETENTION` in
    `cmd/claude-repld/graph.go`. The suite compresses the retention to one row
    and asserts the refusal.

- A CORRUPT `creation_jobs` ROW REFUSES THE BOOT (remediated).
  `recoverAdmitted` matches `*wsm.DecodeError` and fails the recovery loudly,
  on the workspace sink and in the run log, rather than folding corruption into
  "the workspace's merge geometry is gone" and serving on.

- `InterruptError.shim_refused` HAS A PRODUCER (remediated). A KillTurn
  failure that names no landed arm — a transport failure, or a typed failure
  whose kind oneof is unset — relays as `shim_refused` carrying the shim's own
  words (`shimclient.Detail` strips the transport's code prefix). A refusal the
  shim DID name still propagates by name.

- `Fleet.Shim` READS LIVENESS FROM THE CLIENT (remediated). A reaped client is
  no session, so a verb against a workspace whose shim process is gone answers
  the typed `no_session` refusal instead of a raw transport error.

## Log discipline
Every test runs with the daemon at ≥WARNING terminal mirror; the harness
FAILS a test that produced any WARN/ERROR record it did not explicitly
expect (`d.ExpectWarnings(ops...)`), so the remediation loop drives
warnings to zero.

THE SWEEP IS UNCONDITIONAL. `StartDaemon` arms it for every daemon with an
EMPTY expected set, so a test that never calls `ExpectWarnings` still gets the
assertion; `ExpectWarnings` only WIDENS that set. There is no escape hatch --
the `"*"` allow-all was deliberately deleted so every list stays exact -- and a
record on a GREEN path is a daemon defect to fix, never something to declare
away.

IT SWEEPS THE STATE ROOT'S OWN TARGETS, NOT THE WORKSPACES' SYMLINKS, and it
sweeps EVERY workspace this daemon wrote for rather than an opted-in list.
`<workspace>/.claude/emacs/daemon.log` is a symlink, and a landed merge takes
the worktree — link and all — with `git worktree remove`; a sweep that read
through it found nothing for exactly the workspaces whose merge was the
subject. The sweep therefore globs
`<state>/logs/agent-repl-*-daemon-*.log` (dlog's own target names) and keeps
the records whose `pid` is this daemon's, which is what keeps an incumbent's
and a successor's records attributable over one shared state root.
`TestAWarningLoggedAfterTheWorktreeIsGoneIsStillSwept` pins it, and the old
opt-in `WatchWorkspaceLogs` is gone: nothing declares which workspaces are
swept, so nothing can forget to.
