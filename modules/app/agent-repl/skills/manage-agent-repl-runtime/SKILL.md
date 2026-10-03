---
name: manage-agent-repl-runtime
description: Operate the agent-repl runtime — build it, bounce its backends, hard bounce everything including Emacs, roll a deploy out, hot-reload elisp into the running Emacs, check health and readiness, read logs, and send any unary request to the running daemon (restart or interrupt a workspace, select one, ask its health). Use whenever the user asks to build, deploy, bounce, hard bounce, restart, hot-reload, or poke agent-repl, or names one of the terms in this skill's terminology table.
argument-hint: "<build|byte-compile|bounce|hard-bounce|deploy|hot-reload FILE...|call METHOD ...|doctor|readiness|logs> or a plain-English request"
allowed-tools: Bash(.claude/skills/manage-agent-repl-runtime/run.sh:*)
lineage_root: user.dodge.skills.manage-agent-repl-runtime
---

## What This Skill Does

Runs agent-repl's build, bounce, deploy, hot-reload, health and daemon-request operations through one script, and fixes the vocabulary for them.

## Arguments

| Argument | Behaviour |
|---|---|
| `build [--force] [component...]` | Build every stale component, or every component with `--force`. Starts and stops nothing. |
| `byte-compile` | Byte-compile the elisp as a warning gate; any warning fails it. |
| `bounce` | Rebuild everything and stand the daemon and its shims down. Emacs stays up and starts the fresh daemon, whose boot restarts a stale store and sidecar before any shim starts. |
| `hard-bounce` | `byte-compile`, then hot-load the fresh elisp into the running Emacs when it is stale (a failed load stops with nothing stopped), then `bounce`, then quit and relaunch Emacs.app, so the editor and every backend run the fresh build. Refuses while Emacs holds unsaved file buffers. |
| `deploy [-force]` | Rolling deploy: the running daemon rebuilds what is stale and moves every workspace onto it without killing turns; `-force` ends them. |
| `hot-reload FILE.el...` | Load changed elisp files into the running Emacs. Refuses any `test-*.el`. |
| `call METHOD [-workspace DIR] [JSON]` | Send one unary request to the running daemon and print its answer; `-workspace DIR` fills the request's workspace from that workspace directory. |
| `doctor` | Read-only health sweep of the running services. |
| `readiness` | Report whether each component's running build matches its source. |
| `logs [args...]` | Read the canonical logs. |
| *(plain-English request)* | Resolved to one argument above through the terminology table in step 1. |

## Steps

1. Resolve the request to exactly one argument from the table above.
  - a. Map the user's words through this terminology table:

    | The user says | Argument | What happens |
    |---|---|---|
    | "hard bounce agent-repl" (or "hard bounce", "bounce everything") | `hard-bounce` | Rebuild everything, byte-compile, hot-load stale elisp, stand the daemon and its shims down AND restart Emacs.app, unless the user says to leave Emacs running, in which case use `bounce`. |
    | "bounce" / "force bounce the backends" | `bounce` | The same without restarting Emacs. |
    | "deploy" / "roll out" | `deploy` | Rolling deploy; running turns continue. |
    | "restart the workspace" / "bounce this workspace" | `call RestartWorkspace -workspace DIR` | Immediate: the workspace's shim is relaunched on the same session and its webapp page reloads. The user's own key is `SPC o C-c`. |
    | "interrupt / stop the turn in DIR" | `call Interrupt -workspace DIR '{"target":{"turn":{}}}'` | Stops the running turn. The user's own key is `C-c C-k` in the input window. |
    | "switch to / select workspace DIR" | `call SelectWorkspace -workspace DIR` | Makes it current; Emacs and the webapp follow. |
    | "is the daemon healthy" | `call DaemonHealth` | Prints the daemon's health and identity. |
    | "hot reload" / "reload the lisp" | `hot-reload FILE.el...` | Loads the changed non-test `.el` files into the running Emacs. |
    | "reload the page" / "reload the webview" | *(no argument)* | The user's own key is `SPC o l`; suggest it, do not act. |

  - b. CRITICAL: there is NO graceful daemon hot reload. When the user asks for one, say so and offer `deploy` (rolling) or `bounce` instead.
  - c. When the request names none of these, STOP and ask which argument they mean.

2. Decide whether to act or to suggest the user's own control.
  - a. When the request is a single workspace operation the user can do with a key (`SPC o C-c`, `C-c C-k`, `SPC o l`) and the user is at the editor, name the key and offer to run it instead of running it unasked.
  - b. CRITICAL: ONLY the lead session (the one the user is talking to directly) may run `bounce`, `hard-bounce` or `deploy`. A dispatched subagent NEVER runs them; it reports back instead.
  - c. IMPORTANT: before `bounce`, `hard-bounce` or `deploy`, confirm the user asked for it in this conversation. These restart what the user is working in.

3. Run the argument.
  - a. Call `.claude/skills/manage-agent-repl-runtime/run.sh --<argument> [args...]`, passing the argument's own flags, files, method and JSON through verbatim.
    - `EXIT CODE 0:` Continue to step 4.
    - `EXIT CODE 1:` The operation failed or refused. Surface its output verbatim. For a `hard-bounce` refused for unsaved buffers, list them and ask the user to save or revert them. Do not retry on your own.
    - `EXIT CODE 2:` Usage error. IMMEDIATELY terminate and surface the raw error.
  - *NOTE*: `hot-reload` takes the changed file paths. Collect them from what the session changed; NEVER include a `test-*.el`.

4. Verify after anything that restarts services.
  - a. If the argument was `bounce`, `hard-bounce` or `deploy`, call `.claude/skills/manage-agent-repl-runtime/run.sh --doctor`.
    - `EXIT CODE 0:` Report that the runtime is back up and summarize the sweep.
    - `EXIT CODE 1:` Surface the failing checks verbatim, then diagnose with the `/debug-emacs-agent-repl` skill.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
  - b. For every other argument, report the operation's output and stop.

## Notes

- **CRITICAL NOTE: Changes to build, deploy, bounce or daemon-request infrastructure update this skill in the same commit.** That includes a new verb, a changed verb, and a new daemon request worth naming in the terminology table, and it updates agent-repl `AGENTS.md` in the same commit too.
- **CRITICAL: Only the lead session restarts Emacs or the backends.** Subagents never run `bounce`, `hard-bounce` or `deploy`.
- **IMPORTANT NOTE: Health diagnosis is `/debug-emacs-agent-repl`'s job.** This skill runs operations; when one fails or the sweep reports a failure, hand the investigation to that skill.
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.
- **IMPORTANT NOTE: NEVER hot-reload a `test-*.el` file into the running Emacs.** Test files are a batch-only harness.
