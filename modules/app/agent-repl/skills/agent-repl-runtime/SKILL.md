---
name: agent-repl-runtime
description: 'Operate the agent-repl runtime — build it, bounce its backends, hard bounce everything including Emacs, roll a deploy out, hot-reload elisp into the running Emacs, check health and readiness, read logs, hold or release a repository''s deploys, prune agent-repl''s stored data, evaluate elisp in the running Emacs and read the answer back, arrange the sidebar, and send any unary request to the running daemon. Use whenever the user asks to build, deploy, bounce, hard bounce, restart, hot-reload, prune, or poke agent-repl, or names one of the terms in this skill''s terminology table; whenever a hand-deployed build (a profiling build) must not be replaced by the next landing; whenever live editor state must be inspected or changed rather than reasoned about from source; and whenever the sidebar should be organized.'
argument-hint: "<build|byte-compile|bounce|hard-bounce|deploy|hot-reload FILE...|call [-workspace DIR] METHOD ...|doctor|readiness|logs|deploy-hold hold|release|eval TEXT|sidebar> or a plain-English request"
allowed-tools: Bash(~/.claude/skills/agent-repl-runtime/run.sh:*), Bash(~/.claude/skills/agent-repl-runtime/agent-repld:*)
lineage_root: user.dodge.skills.agent-repl-runtime
---

## What This Skill Does

Runs agent-repl's build, bounce, deploy, hot-reload, health and daemon-request operations through one script, and fixes the vocabulary for them.

## Arguments

| Argument | Behaviour |
|---|---|
| `build [--force] [component...]` | Build every stale component, or every component with `--force`. Starts and stops nothing. |
| `byte-compile` | Byte-compile the elisp as a warning gate; any warning fails it. |
| `bounce` | EMERGENCY ONLY (step 2b). Rebuild everything and stand down the daemon, every other daemon serving the same state root, and every shim. Emacs stays up and starts the fresh daemon, whose boot restarts a stale store and sidecar before any shim starts. |
| `hard-bounce` | EMERGENCY ONLY (step 2b). `byte-compile`, then hot-load the fresh elisp into the running Emacs when it is stale (a failed load stops with nothing stopped), then `bounce`, then quit and relaunch Emacs.app, so the editor and every backend run the fresh build. Refuses while Emacs holds unsaved file buffers. |
| `deploy [-force]` | Rolling deploy: the running daemon rebuilds what is stale and moves every workspace onto it without killing turns. This is how the runtime is brought up to date. `-force` ends every turn and is EMERGENCY ONLY (step 2b). |
| `hot-reload FILE.el...` | Load changed elisp files into the running Emacs. Refuses any `test-*.el`. |
| `call [-workspace DIR] METHOD [JSON]` | Send one unary request to the running daemon and print its answer; `-workspace DIR`, which comes BEFORE the method, fills the request's workspace from that workspace directory. |
| `doctor` | Read-only health sweep of the running services. |
| `readiness` | Report whether each component's running build matches its source. |
| `logs [args...]` | Read the canonical logs. |
| *(plain-English request)* | Resolved to one argument above through the terminology table in step 1. |

## Steps

1. Resolve the request to exactly one argument from the table above.
  - a. Map the user's words through this terminology table:

    | The user says | Argument | What happens |
    |---|---|---|
    | "hard bounce agent-repl" (or "hard bounce", "bounce everything") | `hard-bounce` | EMERGENCY ONLY (step 2b). Rebuild everything, byte-compile, hot-load stale elisp, stand down every daemon of the state root and every shim AND restart Emacs.app, unless the user says to leave Emacs running, in which case use `bounce`. |
    | "bounce" / "force bounce the backends" | `bounce` | EMERGENCY ONLY (step 2b). The same without restarting Emacs. |
    | "deploy" / "roll out" | `deploy` | Rolling deploy; running turns continue. |
    | "restart the workspace" / "bounce this workspace" | `agent-repld workspace restart WS` | Immediate: the workspace's shim is relaunched on the same session and its webapp page reloads. The user's own key is `SPC o C-c`. |
    | "park WS" / "park every workspace" / "stand the shims down between keep-alives" | `agent-repld workspace park WS` or `agent-repld workspace park -all` | Immediate: the workspace's shim is stood down as the keep-alive cycle stands one down; the daemon restores it a minute before its next keep-alive, or at once for a prompt, a look or a history read. Never the selected, a hidden or closed workspace; a busy one parks when it falls free. |
    | "interrupt / stop the turn in WS" | `agent-repld workspace interrupt WS` | Stops the running turn. The user's own key is `C-c C-k` in the input window. |
    | "switch to / select workspace WS" | `agent-repld workspace select WS` | Makes it current; Emacs and the webapp follow. |
    | "is the daemon healthy" | `call DaemonHealth` | Prints the daemon's health and identity. |
    | "prune agent-repl's storage" / "free up agent-repl's disk" | `agent-repld runtime prune-storage [-preview]` | Runs the storage sweep now and prints what it deleted; `-preview` deletes nothing. |
    | "prune / clean up the worktrees" / "reap landed worktrees" | `agent-repld runtime reap-worktrees [-preview] [-force]` | Runs the landed-worktree reaper now and prints what it removed; `-preview` changes nothing; `-force` takes closed workspaces' worktrees without waiting. |
    | "hot reload" / "reload the lisp" | `hot-reload FILE.el...` | Loads the changed non-test `.el` files into the running Emacs. |
    | "reload the page" / "reload the webview" | *(no argument)* | The user's own key is `SPC o l`; suggest it, do not act. |

  - b. CRITICAL: there is NO graceful daemon hot reload. When the user asks for one, say so and offer `deploy` (rolling) instead. `bounce` is never an alternative to it (step 2b).
  - c. When the request names none of these, STOP and ask which argument they mean.

2. Decide whether to act or to suggest the user's own control.
  - a. When the request is a single workspace operation the user can do with a key (`SPC o C-c`, `C-c C-k`, `SPC o l`) and the user is at the editor, name the key and offer to run it instead of running it unasked.
  - b. CRITICAL: a FORCED STOP is an EMERGENCY LAST RESORT. Forced stops are `bounce`, `hard-bounce`, `deploy -force`, `call UpdateShutdownSchedule` with `now`, and Emacs's `agent-repl-frontend-daemon-restart` and `agent-repl-runtime-restart`.
    - i. Each one ends every running turn and every subagent, shell and monitor in every workspace.
    - ii. Run one ONLY to unstick a broken runtime that an unforced `deploy` cannot fix.
    - iii. Run one ONLY when the user asks for that forced stop in the current conversation, in that moment. A request in an earlier conversation, a memory, a runbook or a standing routine NEVER authorizes one.
    - iv. NEVER run one as a routine step: not to pick up a new build, not after a merge or a landing, not to start a loop iteration, not to verify a change. An unforced `deploy` brings the runtime up to date, and a merge-queue landing already rolls out that way.
  - c. CRITICAL: ONLY the lead session (the one the user is talking to directly) may run a forced stop or `deploy`. A dispatched subagent NEVER runs them; it reports back instead.
  - d. IMPORTANT: before an unforced `deploy`, confirm the user asked for it in this conversation. It restarts what the user is working in.

3. Run the argument.
  - a. Call `~/.claude/skills/agent-repl-runtime/run.sh --<argument> [args...]`, passing the argument's own flags, files, method and JSON through verbatim.
    - `EXIT CODE 0:` Continue to step 4.
    - `EXIT CODE 1:` The operation failed or refused. Surface its output verbatim. For a `hard-bounce` refused for unsaved buffers, list them and ask the user to save or revert them. Do not retry on your own.
    - `EXIT CODE 2:` Usage error. IMMEDIATELY terminate and surface the raw error.
  - *NOTE*: `hot-reload` takes the changed file paths. Collect them from what the session changed; NEVER include a `test-*.el`.

4. Verify after anything that restarts services.
  - a. If the argument was `bounce`, `hard-bounce` or `deploy`, call `~/.claude/skills/agent-repl-runtime/run.sh --doctor`.
    - `EXIT CODE 0:` Report that the runtime is back up and summarize the sweep.
    - `EXIT CODE 1:` Surface the failing checks verbatim, then diagnose with the `/agent-repl-debug` skill.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
  - b. For every other argument, report the operation's output and stop.

## The agent API's runtime commands

These run through the INSTALLED front door,
`~/.claude/skills/agent-repl-runtime/agent-repld`. The daemon owns their flags
and refusals: `agent-repld runtime` and `agent-repld editor` with no verb print
the usage.

- `runtime deploy-hold hold -repository here|DIR` holds ONE repository's
  deploy stage, durably: its deploys keep running their pre-deploy stage
  (agent-repl's merges keep landing on master) and their post-deploy hooks,
  and skip their deploy hook (for agent-repl, the runtime's rollout). Hold
  agent-repl before hand-deploying a build that must stay running (a
  profiling build). `runtime deploy-hold release -repository here|DIR` ends
  the hold and, when deploys skipped their hook meanwhile, runs a script
  deploy hook once (a prompt deploy hook has no workspace to run in, and is
  reported as not run). Whoever holds a repository releases it when that work
  ends.
- `runtime roll-out-landing [-deploy LEDGER]` is a deploy hook's call: with
  the deploy's ledger (`$AGENT_REPL_DEPLOY_LEDGER`) the rollout fires once the
  deploy has reported; with none it starts at once. NEVER run it by hand:
  `deploy` is the rolling deploy.
- `runtime health` prints the daemon's health and identity.
- `runtime prune-storage [-preview]` runs the daemon's storage sweep now
  (it also runs once a day): it deletes forgotten and landed workspaces'
  shim state, the transcript backups of workspaces closed over thirty days,
  and month-old profiling sessions and test-reuse records, and prints what
  went. `-preview` prints what a sweep would delete and deletes nothing.
- `runtime reap-worktrees [-preview] [-force]` runs the daemon's
  landed-worktree reaper now (it also runs once a day): it removes worktrees
  whose changes have landed on the default branch (and their branches),
  preserves and removes worktrees that never landed after a week of
  repository activity, prunes stale registrations, and removes leftover
  directories beside the worktrees that hold nothing but log links, then
  prints what went. `-preview` prints what a sweep would do and changes
  nothing. `-force` takes a CLOSED workspace's worktree at once, landed or
  not (an unlanded one is preserved first and its branch kept); an open
  workspace's worktree is never touched.

## Evaluating elisp in the running Emacs (`editor eval`)

`editor eval [-workspace WS] TEXT` hands TEXT to the running Emacs, which reads
and evaluates it in its ONE error-catching handler with WS's composer current
(this workspace's by default), and prints the answer: the last form's value as
`prin1` prints it and whether it reads back, or the read error, or the
evaluation error, plus the `*Messages*` lines the evaluation added. The call
waits for the answer (a bounded deadline answers `timed_out`).

- Reach for it PROACTIVELY whenever the user suggests or implies unexpected
  runtime state: read the live state (the face, the variable, the workspace
  plist key) instead of reasoning from source. The source says what should
  happen; only the running editor says what does.
- Always prefer it over asking the user to run elisp themselves, except for
  genuinely dangerous code (destructive or irreversible), which you surface to
  the user to decide.
- After any mutating change, verify it landed: re-read the state it targets and
  read the logs for its record. A clean answer only says the text ran.
- Keep each snippet small and returning data. Text with several forms runs in
  order and answers the last one's value.
- `message` lines come back in the answer; return a value for anything else.
- Useful reads:
  - Workspaces and one key each: `(mapcar (lambda (ws) (list ws :priority
    (plist-get (gethash ws agent-repl--workspaces) :priority)))
    (agent-repl--live-ws-names))`. Use `agent-repl--live-ws-names`, never the
    raw hash keys, which include tombstones.
  - Link the user to code, opening it beside them without taking focus:
    `(agent-repl-link-code "<absolute-path>" START END "<workspace-name>")`.
    Reach for it proactively whenever code answers what the user asked.
  - Reopen a workspace by its root: `(agent-repl-switch-to-project
    (agent-repl--ws-dir "<workspace-name>"))`.
  - Dump `*Messages*` to a file when its whole history matters:
    `(with-current-buffer "*Messages*" (write-region (point-min) (point-max)
    "~/.claude-emacs/messages-dump.txt" nil 'silent))`.
- **CRITICAL: Never evaluate elisp any other way** (no `emacs --batch`, no
  `emacsclient`). The running editor's one handler is the only evaluator.
- **IMPORTANT: Send no mutation-heavy snippet without a clear user request.**

## Arranging the sidebar

The daemon's `GetSidebarArrangement` and `ArrangeSidebar` order the sidebar and
the tab bar, which follows it repository by repository: inside each repository
its entries in drawn order (unsectioned workspaces and subsections interleaved,
a subsection's workspaces where it stands), each family kept together. They run
through `run.sh --call` (a typed `agent-repld` verb comes once they land on master):

- `run.sh --call GetSidebarArrangement '{}'` reads every repository in order,
  its `entries` in drawn order (each a `workspace` family or a `subsection`
  holding its families), as trees.
- `run.sh --call ArrangeSidebar '<protojson>'` applies ONE change:
  `createSubsection {repository, name}` (answers the new subsection's ref under
  `created`), `renameSubsection {subsection, name}`, `deleteSubsection
  {subsection}` (its workspaces return to the repository where it stood),
  `moveSubsection {subsection, before|beforeWorkspace|end}` (`beforeWorkspace`:
  a root in no subsection of the same repository), `moveRepository
  {repository, before|end}`, `placeWorkspace {workspace,
  noSubsection|subsection, before|beforeSubsection|end}` (`beforeSubsection`
  only into `noSubsection`; `end` into `noSubsection` is the bottom of the
  repository), or the all-or-nothing `placeWorkspaces {workspaces: [...],
  ...}`, which keeps the given order. A placement clears the placed families'
  one-shot marks unless it sets `keepOneShot: true`.
- `createAgentSubsection {createdBy, name, workspaces: [...]}` creates a
  subsection as a workspace's agent, with its workspaces, in one change; the
  daemon removes it once none of its workspaces is open.
- To file workspaces into a subsection by name, or gather them into a new
  one, use the agent API instead: `agent-repld workspace subsections WS` and
  `agent-repld workspace place WS... -subsection NAME|-new-subsection NAME`
  (the agent-repl-workspaces skill).
- Copy every ref verbatim from `GetSidebarArrangement`'s answer. A family
  moves whole: placing any workspace places its root, with every ancestor and
  descendant, and a workspace moves only within its own repository
  (`other_repository`).
- Use it when the user asks to organize the sidebar, or after creating several
  child workspaces that belong together, to group them in a named subsection.

## Notes

- **CRITICAL NOTE: Changes to build, deploy, bounce or daemon-request infrastructure update this skill in the same commit.** That includes a new verb, a changed verb, and a new daemon request worth naming in the terminology table, and it updates agent-repl `AGENTS.md` in the same commit too.
- **CRITICAL: A forced stop is an emergency last resort, run only on the user's request in the moment (step 2b).** Only the lead session restarts Emacs or the backends; subagents never do.
- **IMPORTANT NOTE: Health diagnosis is `/agent-repl-debug`'s job.** This skill runs operations; when one fails or the sweep reports a failure, hand the investigation to that skill.
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.
- **IMPORTANT NOTE: NEVER hot-reload a `test-*.el` file into the running Emacs.** Test files are a batch-only harness.
