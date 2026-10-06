# Emacs Lisp system

The repository and module `AGENTS.md` files apply here. Never hot-load these
sources into a running Emacs while implementing or testing a change.

## Logging

`core.el` owns the Emacs logging boundary. Every production log site calls one
of `agent-repl--log`, `agent-repl--log-verbose`, `agent-repl--info`,
`agent-repl--warn`, `agent-repl--warn-once`, `agent-repl--error`, or
`agent-repl--fatal`. Those rungs call `agent-repl--emit-log-record`; no other
function may build a JSONL record or invoke `agent-repl--do-log-to-file`.

Workspace records are written to a runtime-owned temporary target reached
through `<workspace>/.claude/emacs/emacs.log`. The active file is capped at
64 MiB and retains five completed generations, `.1` newest through `.5`
oldest. The canonical symlink always names the active target path. Central
records -- everything logged under the `:agent-repl-central` scope, such as
creation, fork, kill, teardown and daemon administration -- go to the durable
central sink `<state>/logs/emacs.central.log` (`agent-repl-log-file-name`),
beside the daemon's `daemon.run.log`, through the same rungs and the same
rotation. Its directory is held to the real-directory rule the workspace
targets are minted under, and the retired OS-temporary default is redirected
to it on reload.

Every record contains timestamp, runtime, PID, level, verbosity, operation,
message, and structured context. Workspace records also contain
`workspace_dir` and `workspace_id`, plus `agent_repl_session_id` and
`claude_session_id` whenever the latest host push provides them. Request edges
bind `request_id` through `agent-repl--with-log-context` so asynchronous
callbacks retain the same identity.

A nil workspace argument is not permission to use the central sink. The
logger resolves the request's dynamic workspace, the current buffer owner, or
the current workspace. A genuinely process-wide call must match a prefix in
`agent-repl--central-log-format-prefixes`, whose entry states the reason it is
central. An explicit workspace without a durable sink emits a central routing
error, displays a warning, and aborts; it never reroutes the original record.

`AGENT_REPL_LOG_LEVEL` is read at module load. Its exact vocabulary is
`debug|info|warn|error`, with `info` when unset; an invalid value aborts load.
`agent-repl-log-file-level` is the live Elisp knob and is reset from the
environment on reload. Verbose records use `level=debug` and
`verbosity=verbose`, so the same threshold governs their persistence.

Direct `message` calls are only for text the user must read. The source audit
in `test-core.el` explicitly lists each permitted file, owning function,
message template, and user-facing reason. Diagnostics use the logging rungs;
adding an unlisted `message` call fails the ERT suite. The same source audit
rejects record-builder/file-writer bypasses and unclassified literal-nil
workspace log sites.

## A workspace's link always follows the live daemon

`host.el` moves a workspace onto a daemon through ONE walk,
`agent-repl-host--reattach`, whatever started it. Its CLAIM is how the new
daemon is asked; everything after the claim is shared:

| trigger | claim |
| --- | --- |
| `transferred` push; `transferring_away` / `not_yet_adopted` refusal; successor accepted | `:adopt` (AdoptHostWorkspace, the handover rendezvous; the webview is reloaded BESIDE the adopt) |
| host stream lost (`stream-lost`); host stream ended after the daemon's planned ending (`planned-ending`); a call finding its connection closed (`dead-connection`); a promotion that left the workspace on the old daemon (`promotion`); `link-up` | `:register` (RegisterWorkspace, idempotent by dir; the webview is reloaded after the subscribe) |

Both end in `agent-repl-host--reattached`: the old stream cancelled, the
host stream re-subscribed on the new daemon (which moves `:conn`/`:ref`, and
so the page URL), INFO `elisp.host.reattached ... trigger=`, and
`agent-repl-host-reattached-functions`.

- The target is ALWAYS `agent-repl-link-live`: the accepted primary the link
  resolved from `daemon.addr` or promoted from an announced successor. A
  workspace never follows an address of its own.
- `agent-repl-host-conn` NEVER answers a dead connection: it starts the
  reattach and answers the live daemon, or nil with no link.
- With no link a workspace is marked `:detached` and INFO
  `elisp.host.reattach-awaiting-link` is written; the link-up edge (or a
  promotion) walks it. Nothing polls.
- A loss on a daemon that is itself handing over waits for the successor's
  promotion (`agent-repl-host-on-link-promote`), which walks every workspace
  still on the old connection and every detached one.
- Every walk carries a token; only the newest walk for a workspace may
  finish, and a close of a stream the workspace already left is stale.
- A PLANNED end is told apart by the daemon's own last frame: the host,
  daemon and roster streams each carry an `ending` arm
  (`DaemonStreamEnding`) that a planned stand-down sends immediately
  before its clean end. The consumer marks the stream that carried it, and
  a clean `(:ended)` of THAT stream is INFO (`elisp.host.stream-ended-planned`,
  `elisp.link.down-planned`, `elisp.roster.stream-close: reason=planned-ending`)
  followed by the very same walk a loss takes. A clean end without the
  frame, or an error after it, stays the loss it always was
  (`elisp.host.stream-lost` ERROR, `elisp.link.down` WARN, the roster's
  ERROR).
- The roster stream follows the same rule (`agent-repl-roster--follow-live-daemon`):
  an accepted stream that is lost is re-subscribed on the live daemon; with no
  link, or on a stream never accepted, the link's own edge re-subscribes.

Regression, 2026-09-27: a daemon exited without transferring a workspace,
the link was promoted onto its successor, and the workspace kept its dead
connection — calls to `127.0.0.1:61043` and a blank webview.

## A prompt the daemon did not take is held on disk, never in memory

Owner ruling, 2026-09-28: held prompts survive outages and restarts of
Emacs, the daemon and the shim. A submission that gets no answer, or a
handover refusal, goes down ONE path, `agent-repl--input-hold`, which writes
it through `held-ingress.el` into the daemon-owned ingress at
`$AGENT_REPL_STATE_DIR/held-prompts/` (format: daemon ARCHITECTURE.md
"heldingress"), under the failed attempt's own idempotency key, atomically
(temp name, then rename). The daemon ingests it into its held queue once it
serves; Emacs never re-sends it and keeps no copy in memory. The composer's
mode line shows "N prompts waiting for the daemon", counted from the
directory at composer birth, on each write and on each host push while the
line stands. There is no outage queue and no link-up, promotion or reattach
release edge; `test-prompt-queue.el` fails on any production source naming
one.

The user's explicit deferral (`SPC j RET`, `prompt-queue.el`) is held by the
DAEMON too, never by Emacs: it is submitted at once with
`SubmitPromptRequest.delivery = SUBMIT_PROMPT_DELIVERY_DEFERRED` (the
`:delivery :deferred` request key), and the daemon holds it in the held tray,
never classifies or interjects it, and runs it as its own turn once the
running one ends. Every hold path carries the delivery into the ingress
entry, and a deferral refused by a merge, a cold gate or a session still
coming up (`agent-repl--input-deferred-held-arms`) is held on disk rather
than drawn as a refusal, because it already asked for "later". There is no
in-memory deferral queue, finish-edge release or liveness gate; the same
source scan fails on any of their names.

## A failed deploy reaches the minibuffer however it was started

The daemon tells every Emacs its standing loud faults on the one
daemon-level stream Emacs holds, `WatchDaemon`, as `faults_standing`
(`agentrepl.v1.DaemonFaultsStanding`): the whole set, replayed to a
resubscribing stream. "Loud" is exactly what the topbar's warning strip
carries: a failed deploy, and a failed deploy's rollback.

- `agent-repl-link--faults-standing` (`daemon-link.el`) surfaces each
  fault id ONCE: an ERROR record `elisp.link.daemon-fault` carrying the id,
  the line and the typed fault, and one `agent-repl: <line>` echo for the
  push's new faults together.
- `agent-repl-link--surfaced-faults` remembers the ids across resubscribes,
  reconnects, `agent-repl-link-teardown` and reloads; a replay is silent.
- A deploy this Emacs asked for (`agent-repl-deploy`) that fails at its
  build, install or service restart is ALSO such a fault, so the pushed line
  is the one echo: the verb records its refusal at ERROR and does not echo
  it (`agent-repl-verbs--deploy-fault-arms`). Every other refusal is still
  echoed by the verb.

## Workspace create/open/register progress

Every gesture that makes or restores a workspace -- `SPC TAB n`, `N`, `c`,
`C`, `f`, `o`, `O`, `C-n` and `SPC j .` -- reports each step it reaches
through exactly one function, `agent-repl-workspace-progress-report` in
`mutation-progress.el`. Its sentences live in one table,
`agent-repl-workspace-progress-phases`, keyed by KIND (`:create`, `:open`,
`:register`, `:register-repository`) and PHASE. No entry point words its own
feedback: a new one supplies a kind and a phase, never a `message` call.

Every kind carries `:requested` (Emacs is sending), `:completed` and
`:failed`; everything between them is a stage the DAEMON reported reaching,
and a kind lists exactly the stages its rpc can push. A phase with no
template is reported as a caller bug and never invented, so a stage added to
`WorkspaceCreateStage` or `WorkspaceOpenStage` without a sentence here fails
loudly rather than reaching the user as an enum name. A create's stages are
`:deriving-name` (only for a daemon-minted name), `:creating-worktree` and
`:starting-session` (every create past the worktree, until its terminal
step); an open's are `:checking-worktree`, `:restoring-worktree` (only for a
workspace whose deleted worktree is recreated from its branch),
`:starting-session`, `:reviving` (only
for a parked workspace), `:clearing-closed` (only for a closed one) and
`:checking-build`. `wire-host.el` decodes each kind's stage from its
`entered_stage` oneof arm and refuses an arm it does not hold, an unset
oneof, or the retired field-1 `stage` as a logged contract breach.

The echo goes through `agent-repl--backend-phase`, the same startup-phase
channel the daemon build and bounce use: one call produces both the durable
record and the minibuffer line. Details ride as FORMAT ARGUMENTS, never
pasted into the sentence, because the log record's operation name is derived
from the template and a runtime value must never become part of one. That is
also why these sites are absent from the `message` audit above -- they reach
the user through `agent-repl--emit-message`, which the audit already permits.

A create is acked and worked in the daemon's background, so its terminal
outcome arrives on the progress stream and the correlation seat retires the
op there. A create also carries `:accepted`, echoed when the ack lands
unless a daemon stage already overtook it. EVERY REFUSAL OF A CREATE IS ITS
`:failed` PHASE -- synchronous or streamed, `naming_failed` and
`one_shot_policy_missing` included -- so it is an ERROR record and an
`agent-repl: workspace creation FAILED: ...` minibuffer line, never a
WARNING left in *Messages*; only the two handover arms go to the handover
instead. Every create mode (plain, child, static, fork, one-shot) sends
through `agent-repl-verb-create`, so none has its own failure path. An OPEN is answered synchronously, so only its stages ride the
stream: the verb retires the op itself on success, on refusal, and on a
transport failure (`agent-repl-verbs--send`'s `:on-transport-failure`).
Nothing on the stream would ever retire it.

## A close lands on the workspace selected before it

Owner ruling, 2026-09-30. When a workspace closes, for ANY reason (the
user's close, kill or nuke, or an implicit close such as the daemon closing
a workspace after its merge lands), ONE rule decides what Emacs selects:

- Every close path tears the tab down through `agent-repl--ws-land-then-kill`:
  the verbs' teardown (`agent-repl--kill-one-workspace`) and the roster
  push's teardown of a closed or vanished row
  (`agent-repl-roster--tear-down-tab`). Nothing else chooses a landing.
- `agent-repl--land-before-teardown` moves the user ONLY when they stand on
  the closing workspace; standing on another workspace changes nothing.
- The target (`agent-repl--teardown-landing-target`) is the first open
  workspace in THE ONE selection-recency order,
  `agent-repl-roster-selection-recency-order` (roster.el):
  - this session's `agent-repl--workspace-history` first (maintained on every
    perspective activation, carried across renames by
    `agent-repl--ws-rename-state`);
  - then the roster row's durable `last_selected` instant, newest first, so a
    fresh Emacs still lands where the user was before;
  - then tab order, only for workspaces never selected at all.
- `agent-repl--roster-recent-names` (`SPC … R`) orders by the same helper;
  there is no second recency rule. The retired when-column `last_selected`
  arm is never read.
- The source that decided (`history`, `roster`, `tab-order` or
  `foreign-persp`) is logged at INFO.
- The webapp decides nothing: its highlight is the daemon's `current`, which
  the landing's own SelectWorkspace moves.

## Verification

Run from `modules/app/agent-repl/`, always through the host suite slot, which
also puts the run at background priority (`bin/background.sh`; a bare
`emacs -batch` load of `test-helpers.el` refuses to start):

```bash
bin/suite-slot.sh emacs -batch -Q -l ert -l lisp/test-<source>.el -f ert-run-tests-batch-and-exit
bin/suite-slot.sh emacs -batch -Q -l ert -l lisp/test-agent-repl.el -f ert-run-tests-batch-and-exit
bin/suite-slot.sh bin/byte-compile.sh
```

Every changed source has its matching `test-<source>.el` suite. Tests are
table-driven where cases share a contract, use Arrange/Act/Assert structure,
and keep one edge case per test. Tests never launch external processes or
mutate state outside `temporary-file-directory`; use the repository fakes and
single-purpose boundary stubs.
