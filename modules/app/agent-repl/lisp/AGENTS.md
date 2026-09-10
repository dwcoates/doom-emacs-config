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
records use the private global Emacs log under the UID-scoped OS temporary
directory.

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

## Verification

Run from `modules/app/agent-repl/`, always through the host suite slot:

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
