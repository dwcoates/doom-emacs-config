# Merge-variants investigation reports (2026-08-27)

Three Opus subagent reports (merge-queue variants; Emacs vs non-Emacs
merge handling; MULTI_REPO_ROOT behavior), persisted verbatim per meta
rule 21 — findings cited from daemon.md's OPEN backlog. Evidence tier:
code-reading of the OLD daemon/elisp tree (reference material).

########## a3a6cbe0456bcea00
Read-only; nothing modified.

## Headline

The merge **engine** is Emacs-agnostic and MULTI_REPO_ROOT-agnostic by design. Both distinctions live at the edges: ingress, post-merge editor consequences, and account/wrap-up-verb routing.

---

## 1. Emacs vs non-Emacs workspaces in merge handling

**Ingress differs**

1. Emacs merges arrive as a frontend UDS `mergeWorkspace` command; non-Emacs merges arrive as a `merge` verb in a workspace command file.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:88` vs `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/create/inbox.go:264`

2. A merge is deliberately **never** a host action, even when Emacs is attached — the round-trip was removed.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/mergedispatch.go:24` — "It exists because that verb no longer round-trips through Emacs."
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/create/create_test.go:1698`

3. An unrecorded workspace fails differently: the file route rejects/quarantines before any merge state is recorded, the Emacs route stamps `merge_enqueuing` then `merge_failed`.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/mergedispatch.go:97` vs `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/frontendcmd.go:1303`

4. Conflict resume (`conflict_resolved_continue`) exists only on the Emacs route — the command-file `MergeCommand` has no such field.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/frontendcmd.go:1286`; `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/create/inbox.go:38`

5. The Emacs route gets a `CommandAck` it defers/settles a host action on; the file route is acknowledgement-less.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:99`

**No-live-session / no-host branches (the closest thing to an "Emacs-ness" test in Go)**

6. The merge lease's turn interrupt is a no-op with no live session controller, so no displaced turn is recorded or resumed.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/sessioncontroller/mergelease.go:278`

7. Post-merge teardown (hibernate + SIGTERM the shim) is skipped entirely when the workspace has no live session controller.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/sessioncontroller/mergedteardown.go:31` — "merged from a sibling worktree without ever having been opened here."

8. Teardown is also skipped when no `MergedTeardown` is bound to the SSM — the merged workspace's session keeps running.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/ssm/merged.go:146`

9. Boot merge-geometry backfill **yields** to an Emacs host connect for 5s, then runs anyway — merges must not depend on Emacs being up.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/cmd/claude-repld/bootbackfillgate.go:13,25`

10. A queued (not-yet-head) merge is never abandoned when its workspace closes; only a parked conflict is.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/merge/coordinator.go:680`

11. CLI-created, Emacs-less workspaces are fully mergeable because the shared `WorktreeStage` records geometry for both callers.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/create/worktreestage.go:8-25`; `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/cmd/claude-repld/workspace_create_cli.go:161` (`session=none emacs=none`)

**Emacs-only post-merge consequences (no non-Emacs equivalent)**

12. Emacs closes the workspace's tab on `:merged`, preserving the data-only entry.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:372-404`

13. Emacs re-establishes and front-orders a tab on a pushed `:merge-failed`.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/frontend-state.el:740`, `:772`

14. Emacs refuses to tear down a workspace whose merge is not terminal.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/workspace.el:1117`

15. Worktree removal after merge is Emacs-only (`finish`); the daemon only removes its own temporary rebase worktree.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:1838` vs `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/merge/rebasecleanup.go:95`

16. Merge narration (minibuffer echo) and merged/greyed sidebar-and-tab-bar filtering are Emacs-only.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:244`; `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/workspace.el:952`

**Retired — do not carry over**

17. Emacs-side geometry, handler registry, per-repo overrides, pre/post-merge actions, and the magit conflict popup are all deleted.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:14-29`; `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:1718`

---

## 2. MULTI_REPO_ROOT workspaces

**What it is**

1. An environment variable naming a directory whose subtree belongs to a second Claude account.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/session/accountroute.go:28`; mirror at `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/session.el:42`

2. Its value is set only in the user's shell profile, not in any repo file or launchd plist.
   - `/Users/dodgecoates/.zshrc:236` — `export MULTI_REPO_ROOT=~/workspace/ChessCom`
   - The daemon sees it only because Emacs (login-shell env) spawns it.

**Behavioral differences**

3. Under the root, the workspace's CLI runs under `~/.claude-chesscom`; outside, `""` meaning the CLI default `~/.claude`.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/session/accountroute.go:93-99`; Emacs at `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/session.el:613`

4. Transcript discovery probes the multi-repo root first for under-root workspaces, the default root first otherwise.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/cmd/claude-repld/workspace_transcripts_cli.go:125`

5. A parent's *resolved* account never propagates to a child — only a human's explicit selection does, precisely because sitting under the root "has chosen nothing".
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/accountresolve.go:102`

6. **The one merge-path difference: under-root repos land via PR + merge queue and then `close`; outside-root repos land via local cherry-pick `merge`.**
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:645-651` — "`close` rather than `merge`: a repo under `agent-repl-multi-repo-root-env` lands its change through the PR and merge queue, so cherry-picking … would duplicate the commits the CICD merge already owns."
   - **Caveat:** in elisp this is *hard-pinned by directory constants*, not computed from the env var — `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:558`, `:565`, dispatched at `:733-734`. The env var is only the stated rationale.
   - The out-of-repo skill (`~/.claude/skills/create-or-update-workspace/run.sh:379-405,590-594`) *does* compute it live, defaulting to merge-only when unset.

7. Emacs has an extra way to count as "under the root" that the daemon cannot evaluate: `agent-repl-doom-multi-repo-mode` folds the doom tree in.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/session.el:548`, `:581`
   - Acknowledged as unevaluable in Go at `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/cmd/claude-repld/workspace_transcripts_cli.go:44-46`.

**Confirmed NOT different**

8. Merge queue keying is purely `git rev-parse --git-common-dir` + `EvalSymlinks`, with no account or path-family input.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/merge/repokey.go:52-77`

9. Merge geometry, pipeline, conflict/test-failure resolvers, suite selection, and post-merge after-action carry no MULTI_REPO_ROOT dependence.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/geometry/derive.go`; `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/postmerge/postmerge.go:44`

---

## 3. Self-merge → self-reload (and its interaction)

1. A `merged` outcome fires `merge.PostMergeHook` exactly once, off the drain goroutine, after the queue entry is dropped.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/merge/posthook.go:18-36`

2. `reload.Trigger` compares the merge target's git identity against the daemon's own checkout, resolved from the binary's location via `bin/deploy-all.sh` as marker.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/reload/reload.go:128` — `if ident.CommonDir != t.self.CommonDir { return nil }`
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/reload/selfrepo.go:15`, `:73`

3. Same repo but a **sibling worktree** does not redeploy — the running code was not built from there.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/reload/reload.go:131-139`

4. The landed range is re-derived from `cherry picked from commit <sha>` annotations reachable from the source branch, then classified against `modules/app/agent-repl/` prefixes; an empty classification skips the deploy.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/reload/landed.go:55`; `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/reload/classify.go:50-57`, `:80`

5. **Emacs-ness interaction:** the deploy script defers the daemon bounce *and* the elisp hot-load when no Emacs server answers, and the reload latch is re-armed for exactly that case.
   - `/Users/dodgecoates/.config/doom/modules/app/agent-repl/bin/deploy-all.sh:317-330`; `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/reload/reload.go:105-114`, `:165`
   - The restart itself is performed *by Emacs* via `emacsclient` (`bin/deploy-all.sh:377`).

6. **MULTI_REPO_ROOT interaction: none.** Nothing under `daemon/internal/reload/` reads the env var; `~/.config/doom` reaches this hook only because, being outside the root, its one-shot uses the local `merge` verb rather than the PR flow.

---

## Unresolved

- Whether the launchd-run `shim-store` / `shim-claude-sidecar` inherit `MULTI_REPO_ROOT` (neither plist sets it, neither service reads it — moot but unverified).
- Whether merge queue ordering / lease / before-after actions behave differently with no attached frontend beyond "the phases reach nobody" — no host-presence conditional found in the pipeline.
- The out-of-repo `create-or-update-workspace/merge.md` skill's `--pr-was-merged` fast-forward variant has **zero** occurrences anywhere in this repo, so the merge engine has no PR-merged branch at all.
- The "parent notification on child merge" described in the skill has no daemon-side implementation and no Emacs caller; whether it still happens is unresolved.
########## a448d29ee5f87e6f3
## Headline

**The daemon's merge core does not branch on Emacs at all.** There is no `HasEmacs`, `EmacsPresent`, `emacs_registered`, `materialized`, `headless` or `frontend-attached` flag consulted anywhere in `daemon/internal/workspace/merge/`, `daemon/internal/ssm/merge*.go`, `daemon/internal/sessioncontroller/merge*.go`, or `daemon/internal/server/merge*.go`. This is deliberate and documented (the merge verb was moved off Emacs). The real differences are (a) **which ingress** the merge arrives on, (b) what happens when the workspace has **no live session controller / no attached host**, and (c) **post-merge editor work only Emacs does**.

## Branch points — ingress (Emacs-issued vs daemon-dispatched)

- **Ingress selection.** Emacs-backed merge goes over the frontend UDS as `mergeWorkspace`; a non-Emacs (skill/command-file) merge goes through the inbox `merge` verb. `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:88` — `(agent-repl--uds-send-command "mergeWorkspace" … (agent-repl--frontend-ws-command-key ws) …)` vs `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/create/inbox.go:264` `func (i *Inbox) routeMerge(...)`.
- **Unrecorded workspace: rejected-and-quarantined (file route) vs nack-plus-`merge_failed` state (Emacs route).** The file route pre-checks geometry and refuses *before* any merge phase is recorded: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/mergedispatch.go:97` — `if !found { … reason=no-recorded-workspace }`. The Emacs route marks `merge_enqueuing` FIRST and then records `merge_failed`: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/frontendcmd.go:1303` — `if err := h.markMergeEnqueuing(workspace, req.Name, requestID); err != nil`, then `:1306`/`failMergeAttempt`. So an unmergeable non-Emacs workspace leaves *no* merge state on the axis; an Emacs one leaves a visible `merge_enqueuing → merge_failed`.
- **Missing merge route is fatal at bring-up but per-file for a one-shot drain** (the non-Emacs path only). `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/create/inbox.go:124` — `if i.Merges == nil { return errNoMergeDispatcher }` vs `:284` — same test inside `routeMerge`, which quarantines that one file and continues.
- **Key normalization/validation differs.** File route requires an absolute path and `filepath.Clean`s it: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/mergedispatch.go:83` — `if projectDir == "" || !filepath.IsAbs(projectDir)`. Emacs route derives the key from the registered `:project-dir` and signals client-side if absent: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/frontend-client.el:129` and `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:69` — `(unless (and (stringp ws) (not (string-empty-p ws))) … (user-error "Cannot merge: no workspace name given"))`.
- **Conflict resume (`conflict_resolved_continue`) exists ONLY on the Emacs route.** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/frontendcmd.go:1286` — `if cmd.GetConflictResolvedContinue() {` … `return h.merges.Resume(ctx, req)`. The command-file `MergeCommand` has no such field (`/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/create/inbox.go:38-45`: only `workspace`, `project_dir`, `ID`), so a non-Emacs client cannot resume a parked conflict — only `SPC TAB M`'s sibling `agent-repl-workspace-merge-continue-after-resolve` (`/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:2952`) can.
- **Outcome feedback: acked/deferred host action vs nothing.** Emacs defers the host action and settles it on the `CommandAck`: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:99` — `:on-registered (lambda (id) … (agent-repl--host-action-defer id))`. The file route is explicitly acknowledgement-less: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/e2e/mergedispatch_e2e_test.go:26` — "There is no ack to nack, no connection to close, and no human watching."
- **Merge is deliberately NOT a host action any more**, i.e. the daemon never hands a merge to Emacs even when Emacs is attached: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/create/create_test.go:1698` — "A merge is NOT a host action any more. Emacs must never be handed one again"; Emacs-side confirmation at `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/workspace-create-client.el:777` — "`agent-repl--legacy-host-action-handlers` deliberately dropped its \"merge\" entry, so `agent-repl--merge-dispatch-over-uds` is reached only from `SPC TAB M`".

## Branch points — no live session / no attached host

- **Merge lease interrupt is a no-op when there is no live session controller** (the headless / never-opened / already-hibernated workspace). `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/sessioncontroller/mergelease.go:278` — `d, err := m.existing(workspace)` … "found no live session controller (%v) — nothing of the user's is in flight, so the lease's precondition already holds"; returns `nil, nil`, so **no displaced turn is recorded and none is resumed after the merge**.
- **Post-merge teardown is skipped when no session controller exists.** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/sessioncontroller/mergedteardown.go:31` — `d, err := m.existing(workspace)` … "decision=none … the merge landed over a workspace with no live session controller, so there is nothing to stand down". With a live one it hibernates + SIGTERMs the shim (`:39` `m.Hibernate(workspace, StopCauseMergedTeardown())`).
- **Teardown is also skipped when no `MergedTeardown` is bound to the SSM.** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/ssm/merged.go:146` — `if teardown == nil { … "no MergedTeardown is bound to this SSM, so the merged workspace's session keeps running" }`.
- **Boot geometry backfill runs regardless of whether an Emacs ever connects** — it yields to a host connect, but only for a grace. `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/cmd/claude-repld/bootbackfillgate.go:25` — "no host connected within the grace. The repair runs anyway; refusing to would make merges depend on an Emacs being up." Merge commands then block on that gate: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/cmd/claude-repld/bootgeometry.go:44` `func (d *deferredGeometryBackfill) Lookup(...)` — `select { case <-d.done: … }`.
- **Awaiting-Emacs creation jobs are abandoned (and thus never become mergeable) when their worktree is gone.** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/create/types.go:246` — `if job.State == StateAwaitingEmacs && worktreeExists != nil && !worktreeExists(job.WorktreePath)`. Geometry, the precondition for any merge, is recorded at materialization: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/cmd/claude-repld/workspace_create.go:700` — "Required: a workspace materialized without it can never be merged." The headless CLI create records the same geometry (`/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/cmd/claude-repld/workspace_create_cli.go:145` `Geometry: daemonGeometryRecorder{...}`, log at `:161` `session=none emacs=none`), so **CLI-created, Emacs-less workspaces are fully mergeable**.
- **Queued merges survive a closed/Emacs-less workspace; only parked conflicts are abandoned.** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/merge/coordinator.go:680` — "A workspace whose merge is QUEUED but not yet at the head is not abandoned … the work lives in git and does not need the closed workspace's frontend." The abandon is driven only by the frontend close command: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/server/frontendcmd.go:1368` — `abandoned, err := h.merges.Abandon(ctx, workspace)`.

## Branch points — Emacs-only merge consequences

- **Teardown guard: only Emacs refuses to kill a workspace mid-merge.** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/workspace.el:1117` — `(when (agent-repl--ws-merge-unfinished-p ws) … (user-error "Cannot tear down workspace '%s': its merge is still %s"))`, enforced at `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/workspace.el:1168`.
- **Merge-completed teardown preserves the hashmap entry** (Emacs-only lifecycle distinction). `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/workspace.el:1144-1146` — "the ws plist survives so the workspace's merged state stays visible … This is the merge-completed teardown path; standard kill/close callers pass nil".
- **Worktree removal after merge is Emacs-only** (`finish`, not the daemon). `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:1838` — `(when (and worktree-p project-dir (file-directory-p project-dir)) (agent-repl--remove-git-worktree project-dir))`. The daemon's merge only removes its own temporary rebase worktree (`/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/merge/rebasecleanup.go:95`).
- **Merged workspaces are dropped from the tab bar / greyed in the sidebar — Emacs only.** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/workspace.el:952` — `(cl-remove-if #'agent-repl--merged-ws-p names)`; `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/sidebar.el:269-270` — `(let ((closed (or merged (eq repl-state :inactive))))`.
- **Merge notification/narration exists only in Emacs.** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:244` `agent-repl--merge-echo-pushed-state`, gated on `agent-repl--merge-echo-states` (`:141`). No non-Emacs surface receives any merge notification; a dispatched merge produces log lines and pushed state only.
- **Emacs holds a durable `:merge-completed` / `:merge-failed` restore across restart** that the daemon does not model: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/session.el:260` — `(when (and saved (eq (plist-get saved :merge-completed) t)) …)`, with the failure re-classification probe at `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:2691` `agent-repl--detect-merge-actually-landed-p`.

## Explicitly retired (so: no branch, despite older docs)

- Emacs-side geometry, handler registry, per-repo overrides, pre/post-merge actions, magit conflict popup, and the merge host action are all deleted: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/merge-handlers.el:14-29`, `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:1718-1738` and `:2304-2320`. `merge.Result`'s "tell Emacs to open magit on a conflict" comment (`/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/workspace/merge/merge.go:272`) is stale — no such call exists.
- The interactive and skill paths are asserted to be the *same* request: `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:2941-2943` — "the interactive and headless paths are the SAME request rather than two strategies that can drift"; `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/worktree.el:1844-1846` — "there is no behavioral difference between the two callers."

## Could not determine

- Whether the merge **queue ordering, lease, before/after action, or conflict-resolution** logic ever behaves differently for a workspace with no attached frontend beyond "the phases reach nobody" — `merge/coordinator.go:96` only asserts a `Status` sink is required; I found no conditional on host presence in the pipeline.
- What the `create-or-update-workspace` **`merge.md`** skill actually emits (whether a merge is followed by a `close` entry, and the `--pr-was-merged` / one-shot variants). That file liv
########## a8f150c878624fb80
Findings below. All paths absolute.

## 1. What `MULTI_REPO_ROOT` is

- **Definition (name only, no default in-tree):** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/daemon/internal/session/accountroute.go:28` — `MultiRepoRootEnv = "MULTI_REPO_ROOT"`, described as "names the directory whose subtree belongs to the multi-repo account. It mirrors Emacs' agent-repl-multi-repo-root-env."
- **Emacs mirror:** `/Users/dodgecoates/.config/doom/modules/app/agent-repl/lisp/session.el:42` `(defcustom agent-repl-multi-repo-root-env "MULTI_REPO_ROOT" ...)`.
- **Where the VALUE is actually set:** only in the user's shell profile — `/Users/dodgecoates/.zshrc:236`: `export MULTI_REPO_ROOT=~/workspace/ChessCom`. Live value `/Users/dodgecoates/workspace/ChessCom`. It is **not** set in either launchd plist (`launchd/com.agentrepl.shim-store.plist`, `launchd/com.agentrepl.shim-claude-sidecar.plist`), not in `bin/`, not in `scripts/`. The daemon therefore only sees it because Emacs (which inherits the login shell env) spawns it.
- **Meaning:** subtree → Claude account. Under it: `~/.claude-chesscom` (`accountroute.go:30-33`, `MultiRepoConfigDirEnv = "AGENT_REPL_MULTI_REPO_CONFIG_DIR"`, `DefaultMultiRepoConfigDir = "~/.claude-chesscom"`); outside it: `""` = the CLI default `~/.claude`.
- **Unset ⇒ everything is "outside":** `accountroute.go:63-65` `if root == "" || path == "" { return false }`, so `AccountConfigDirFor` returns `""`.
- **Emacs adds one extra way to be "inside":** `lisp/session.el:548` `agent-repl-doom-multi-repo-mode` (global, default off) makes the doom-config tree count as under the root. `lisp/session.el:581`: `(doom-match-p (and (not root-match-p) agent-repl-doom-multi-repo-mode (agent-repl--doom-config-tree-p project-dir)))`. The Go side deliberately cannot evaluate this (`daemon/cmd/claude-repld/workspace_transcripts_cli.go:44-46`: "an Emacs-session toggle with no on-disk representation, so this command cannot evaluate it").

## 2. Behavioral differences, under vs outside `$MULTI_REPO_ROOT`

- **Account / CLAUDE_CONFIG_DIR (Go).** `daemon/internal/session/accountroute.go:93-99`: `if !UnderDir(root, resolved) { return "", nil }` … `return MultiRepoConfigDir()`. Under → `~/.claude-chesscom`; outside → `""` (CLI default).
- **Account / CLAUDE_CONFIG_DIR (Emacs), incl. the doom-mode extension.** `lisp/session.el:613-615`: `(dir (if under-multi-repo agent-repl-multi-repo-config-dir agent-repl-default-config-dir))`. Consumed at session launch `lisp/session.el:663` and prefixed as `CLAUDE_CONFIG_DIR=` in `agent-repl--assemble-cmd`.
- **Workspace-create account resolution.** `daemon/internal/server/accountresolve.go:89` `routed, err := session.AccountConfigDirFor(path)` — used for every create; a request-named account is overwritten (`daemon/cmd/claude-repld/workspace_create.go:540-543` "request account OVERWRITTEN … the account follows the selection, else the path").
- **Inheritance asymmetry on create.** A parent's *resolved* account never travels; only a human's webapp *selection* does. `daemon/internal/server/accountresolve.go:102-104`: "a parent that merely sits under `$MULTI_REPO_ROOT` has chosen nothing, and letting its resolved account ride along would move a child whose own path answers differently." Mirrored at `daemon/cmd/claude-repld/workspace_create.go:496-498`.
- **Transcript lookup root (daemon `list-transcripts`).** `daemon/cmd/claude-repld/workspace_transcripts_cli.go:125-133`: `routed, err := session.AccountConfigDirFor(workspace)` … `primary, other := fallback, multi; if routed != "" { primary, other = routed, fallback }`. Under-root workspaces are probed under `~/.claude-chesscom` first, outside-root under `~/.claude` first; the other root is still probed and reported.
- **Transcript path for resume viability.** `daemon/internal/session/transcript.go:36-46` `ClaudeConfigDir(dir)` — "a session whose CLI writes into ~/.claude-chesscom has no transcript under ~/.claude, so resolving against the daemon's own env would … silently downgrade a resume into a fresh conversation."
- **AI-title jsonl root (Emacs).** `lisp/ai-title.el:86-89`: `(if config-dir (expand-file-name "projects" config-dir) agent-repl-ai-title-projects-dir)`.
- **Frontend session posture sent to the daemon.** `lisp/frontend-client.el:186` `(config-dir (agent-repl--compute-config-dir cwd))`.
- **Superseding / single-writer conflict detection.** Two accounts make the same claude uuid two different files: `daemon/internal/server/supersede.go:58-61` "the same claude session uuid legitimately exists under two accounts (one uuid lives under both ~/.claude and ~/.claude-chesscom on this machine)".
- **One-shot wrap-up verb — the MERGE-vs-PR fork (Emacs).** Under-root repo → PR + merge queue, then `close`; outside → `merge` (host cherry-pick). Deciding constant `lisp/worktree.el:643-651`: "`close` rather than `merge`: a repo under `agent-repl-multi-repo-root-env` lands its change through the PR and merge queue, so cherry-picking the branch onto the local default branch (what `merge` does) would duplicate the commits the CICD merge already owns." Doom flavor uses `agent-repl--oneshot-merge-suffix` (`lisp/worktree.el:1543`), explanation-engine uses `agent-repl--oneshot-create-pr-suffix` (`lisp/worktree.el:1585`). Tests: `lisp/test-worktree.el:3425` and `:3578`.
  - **Caveat, important:** in Emacs this fork is **not computed from `$MULTI_REPO_ROOT` at all**. It is hard-pinned by directory constants — `lisp/worktree.el:558` `agent-repl--doom-config-dir` and `lisp/worktree.el:565-567` `agent-repl--explanation-engine-dir` (`~/workspace/ChessCom/explanation-engine`), dispatched by `lisp/worktree.el:732-733`: `((equal norm agent-repl--doom-config-dir) :doom) ((equal norm agent-repl--explanation-engine-dir) :explanation-engine)`. `$MULTI_REPO_ROOT` is only the *rationale* quoted in the docstring.
- **One-shot wrap-up verb — the skill side, which IS `$MULTI_REPO_ROOT`-computed.** `/Users/dodgecoates/.claude/skills/create-or-update-workspace/run.sh:590-594`: `if entry_under_multi_repo_root(entry): append_prompt_instruction(entry, ONE_SHOT_INSTRUCTION) else: append_prompt_instruction(entry, MERGE_ONLY_INSTRUCTION)`; the predicate at `run.sh:379-405` reads `MULTI_REPO_ROOT`, prefers `git_root`, falls back to `source_ws.path`, and `if not multi_repo_root: return False` — i.e. **unset ⇒ merge-only** (documented at `/Users/dodgecoates/.claude/skills/create-or-update-workspace/create.md:10`).
- **Account roster advertised to the daemon.** `lisp/daemon.el:938-940`: `(format "personal=,work=%s" (expand-file-name agent-repl-multi-repo-config-dir))` — exactly two roots, mirroring the two answers.

### Things that are explicitly NOT different (checked, no `$MULTI_REPO_ROOT` dependence)

- **Merge queue keying:** purely `git rev-parse --git-common-dir` + `EvalSymlinks` — `daemon/internal/workspace/merge/repokey.go:52-77`. No account/path-family input.
- **Merge geometry / merge pipeline / conflict + test-failure resolvers / suite selection / rebase sweep:** grep for `MultiRepo|multi-repo|ConfigDir|Account` across `daemon/internal/workspace/merge/*.go` and `daemon/internal/workspace/geometry/*.go` returns nothing. Suite selection is by changed-path prefix only (`daemon/internal/workspace/merge/suiteselect.go`).
- **Post-merge after-action:** keyed by worktree path only — `daemon/internal/workspace/postmerge/postmerge.go:44-45` `s.postprocessing.PostprocessingPrompt(req.SourceDir)`.
- **`--pr-was-merged` (fast-forward instead of cherry-pick):** exists **only** in the skill (`/Users/dodgecoates/.claude/skills/create-or-update-workspace/merge.md:10`). Zero occurrences anywhere in the agent-repl repo (elisp, Go, proto, webapp). So the daemon/Emacs merge engine has no PR-merged branch at all.
- **Notifications and teardown:** no config-dir/multi-repo conditional found in `lisp/notifications.el` or `lisp/worktree.el` teardown paths.

### Could not determine

- Whether the launchd-run `shim-store` / `shim-claude-sidecar` processes see `MULTI_REPO_ROOT` (neither plist sets it, and neither service reads it — so it is moot, but the inherited-env question itself is unanswered from the repo).
- Any behavior of the *installed* `/create-or-update-pr` and `/check-cicd` skills beyond their names; I did not audit them for `$MULTI_REPO_ROOT` (grep of `~/.claude/skills` shows `MULTI_REPO_ROOT` used only in `ceac-processor-script/run.sh`, `ceac-sandbox/run.sh`, and `create-or-update-workspace/run.sh`).

## 3. Self-merge → daemon self-reload/restart

Chain, in order:

1. **Arming (once, at boot).** `daemon/internal/server/postmergehook.go:70-104` `buildSelfReloadTrigger`. It resolves the checkout the running binary came from: `reload.ResolveSelf(context.Background(), exe)`; if `!deployed` it logs "self-merge redeploy DISABLED for this process: the running binary (%s) is not inside an agent-repl checkout" and returns `(nil,false,nil)`. Otherwise "self-merge redeploy ARMED {checkout=%s script=%s}".
2. **Identity of "self".** `daemon/internal/reload/selfrepo.go:14` `DeployScriptRelPath = stackPrefix + "bin/deploy-all.sh"` is the marker; `checkoutContaining` walks up from the binary. `SelfRepo{Root, CommonDir}` — Root = worktree, CommonDir = `rev-parse --git-common-dir` canonicalized (`IdentifyRepo`, `selfrepo.go:117-160`).
3. **Firing.** `daemon/internal/reload/reload.go` `Trigger.AfterMerged`, a `merge.PostMergeHook` run exactly once per `merged` terminal outcome, off the drain goroutine (contract at `daemon/internal/workspace/merge/posthook.go:16-36`). Deciding conditions, verbatim:
   - `if ident.CommonDir != t.self.CommonDir { return nil }` — different repository, one `git rev-parse` and out.
   - `if ident.Toplevel != t.self.Root {` → logs "merge landed in a sibling worktree of this daemon's own repository, so no redeploy"; a child workspace merging into a *parent worktree* of ~/.config/doom does **not** redeploy.
   - `if !t.launched.CompareAndSwap(false, true)` → "self-merge SKIPPED, a redeploy launched earlier in this daemon's lifetime is still the live one". In-memory latch only; re-armed by `Trigger.rearm` when the deploy exits without bouncing.
4. **Plan.** `Trigger.plan` derives the landed commit range by walking back from target HEAD matching `(cherry picked from commit <40-hex>)` and checking reachability from the source branch (`daemon/internal/reload/landed.go:10-30`, cap `maxLandedScan = 200`). Zero commits → "self-merge added no commits … so no redeploy". Then `Classify(paths)` maps repo-relative paths onto stack components under `stackPrefix = "modules/app/agent-repl/"` (`daemon/internal/reload/classify.go:37`, rules at `:50-57`). Empty set → "self-merge touched nothing the running stack executes, so no redeploy".
5. **Launch.** `daemon/internal/reload/launch.go` `DetachedScript.Launch` spawns `bin/deploy-all.sh` (with `--elisp <range>` when elisp is in the set) detached: `cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}`, `exec.Command` not `CommandContext`, output to a log file under the state root (never inside the checkout), stdin nil. A reaper goroutine calls `onExitWithoutBounce` if the deploy returns while the daemon still lives.
6. **The actual restart is done BY EMACS.** `bin/deploy-all.sh:377` `RESTART_OUT="$("$EMACSCLIENT" --eval "$RESTART_FORM" 2>&1)"` with `RESTART_FORM='(agent-repl-frontend-daemon-restart-await)'` (`:369`, always the shim-preserving form).
7. **Restart announcement** (separate, generic): `daemon/cmd/claude-repld/restartannouncement.go:96-120` `announceIntentionalRestart` pushes a `RestartPendingView` to the GUI broadcast and the Emacs UDS host so a deliberate bounce isn't painted as a crash; zero connected clients is not a failure, a nil publisher is. `daemon/cmd/claude-repld/bootphase.go` is generic boot instrumentation with no self-merge coupling.

**Is the self-reload path affected by `$MULTI_REPO_ROOT`-ness?** No. Nothing in `daemon/internal/reload/` reads `MultiRepoRootEnv` or any c
