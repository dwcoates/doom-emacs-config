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
loudly rather than reaching the user as an enum name.

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
