# Implementation plan: agent-repld API contract

Main implementation document for the contract recorded in `agent-repld-api.md`
(branch `contract/agent-repld-api`, worktree
`~/.config/doom-worktrees/contract-agent-repld-api`). The owner asked for the
whole change to be carried to the finish line without stopping (2026-10-17).
Each item is checked off as it lands, with its commit.

## Daemon (Go, `daemon/`)

- [ ] D1 `internal/workspace`: CreateSpec gains `Requester`, `OwnerLands`,
      `OwnerNotice`; `ForkFrom` is independent of `Parent`; `parentWorkspace()`
      never falls back to `ForkFrom`; `ForkIntoPrimary` becomes "forked from a
      workspace other than the branch source" (`ForkElsewhere`) and its note
      applies to children forked from a non-owner too; public `BaseRef`,
      `settleParentAndBase`, `requireOnPrimary` retired (the merge workspace
      keeps its internal base); a closed owner is reopened first.
- [ ] D2 `internal/wsm`: persist `owner_lands`, `context_from` and `requester`
      on the workspace row (migration), read by MergeWorkspace refusals,
      WorkspaceStatus and the derived-workspace notices.
- [ ] D3 Notices: owner notice (PROMPT_ORIGIN_OWNER_NOTICE) on create;
      derived-workspace notices (PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE) to
      the branch-source and context-source workspaces on create, land, and
      close-without-landing, combined when one workspace is both, never to
      the requester.
- [ ] D4 `internal/merge`: async merge as a create form. DetachedRun gains
      the maker, the target (primary or maker) and front placement; the
      maker's context is forked; HAND-OFF (fork + pick) MOVES TO ADMISSION so
      the merge workspace forks when the merge starts; merge arms renamed to
      `async_merge_*`; a branch owned by a registered workspace is refused;
      the subagent maker is resolved from `<session>/subagents/agent-<id>.jsonl`.
- [ ] D5 MergeWorkspace: requester rules (`owner_lands`,
      `requester_may_not_land`), required placement.
- [ ] D6 CloseWorkspace: requester; a self-close is accepted as
      `closes_when_turn_ends` and performed at the turn-end edge.
- [ ] D7 CompactWorkspace rpc; SubmitPrompt refuses a slash command of origin
      AGENT_SENT.
- [ ] D8 Status: GetWorkspaceStatus, GetMergeQueueStatus.
- [ ] D9 Subscriptions: SubscribeWorkspace/UnsubscribeWorkspace (durable,
      notices as PROMPT_ORIGIN_WORKSPACE_NOTICE); SubscribeMergeQueue/
      UnsubscribeMergeQueue moved off UpdateMergeQueue, answering a snapshot.
- [ ] D10 UpdateRollouts; release deploys when the checkout is ahead.
- [ ] D11 EvaluateInEditor / AnswerEditorEvaluation / host push `evaluate`,
      token correlation with a measured deadline.
- [ ] D12 `cmd/agent-repld`: nouns `workspace`, `merge-queue`, `editor`,
      `runtime`; `merge-queue` verb ported onto RPCs; requester from cwd.
- [ ] D13 Retire `internal/commandfile` and the output-dir ingress.
- [ ] D14 Integration harness and e2e callers ported.

## Editor (Emacs lisp, `lisp/`)

- [ ] E1 Port CreateWorkspace / CloseWorkspace / MergeWorkspace callers
      (`verbs.el`, `wire-verbs.el`) to topology + context + requester, behavior
      unchanged; fake daemon (`lisp/testsupport/fakedaemon`) follows.
- [ ] E2 Answer the `evaluate` host push with AnswerEditorEvaluation.
- [ ] E3 Remove `SPC o h` and the help workspace.
- [ ] E4 Stop writing command files (core.el output dir).

## Webapp (`webapp/`)

- [ ] W1 Port `sidebar/create.ts`, `sidebar/roster.ts`, `feed/bubble-more.ts`.

## Skills, manifests, docs

- [ ] S1 `/agent-repl-workspaces` (create, prompt, set incl. hidden, merge,
      open, close, kill, compact, status, subscribe), replacing CEE's
      `/create-or-update-workspace`.
      It states that `workspace kill` and every cold-gate answer (`workspace
      gate pay|clear|compact|compact-and-resume`, with model and scope) run
      only when the user explicitly asked for or approved them.
- [ ] D15 `workspace gate` CLI over AnswerColdGate; WorkspaceStatus reports
      `cold_gate` with the gate's FeedId.
- [ ] S2 `/agent-repl-merge-queue` from `/merge-queue`.
- [ ] S3 `/agent-repl-runtime` from `/manage-agent-repl-runtime`, absorbing
      `/runtime-eval-code` as a runbook over `agent-repld editor eval`.
- [ ] S4 `/agent-repl-debug`, `/agent-repl-profile`, `/agent-repl-tests`,
      `/agent-repl-install` renames; `/agent-repl-help` removed.
- [ ] S5 Tests: every command a skill names exists; every agent command is
      reachable from a skill.
- [ ] S6 requirements.json skill entries, metaprompt merge rule, guard hook,
      AGENTS.md, USER-GUIDE.
- [ ] S7 CEE pull request removing `/create-or-update-workspace` (confirm
      with the owner before opening).

## Verification

- [ ] V1 Full unit + integration suites of daemon, lisp, webapp, shim.
- [ ] V2 e2e suite (systemic change).
- [ ] V3 Land through the merge queue.
