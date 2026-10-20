# Implementation plan: agent-repld API contract

Main implementation document for the contract recorded in `agent-repld-api.md`
(branch `contract/agent-repld-api`, worktree
`~/.config/doom-worktrees/contract-agent-repld-api`). The owner asked for the
whole change to be carried to the finish line without stopping (2026-10-17).
Each item is checked off as it lands, with its commit.

## Daemon (Go, `daemon/`)

- [x] D1 (885857bcfe) `internal/workspace`: CreateSpec gains `Requester`, `OwnerLands`,
      `OwnerNotice`; `ForkFrom` is independent of `Parent`; `parentWorkspace()`
      never falls back to `ForkFrom`; `ForkIntoPrimary` becomes "forked from a
      workspace other than the branch source" (`ForkElsewhere`) and its note
      applies to children forked from a non-owner too; public `BaseRef`,
      `settleParentAndBase`, `requireOnPrimary` retired (the merge workspace
      keeps its internal base); a closed owner is reopened first.
- [x] D2 (ab49cac61c) `internal/wsm`: persist `owner_lands`, `context_from` and `requester`
      on the workspace row (migration), read by MergeWorkspace refusals,
      WorkspaceStatus and the derived-workspace notices.
- [x] D3 Notices: owner notice (PROMPT_ORIGIN_OWNER_NOTICE) on create;
      derived-workspace notices (PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE) to
      the branch-source and context-source workspaces on create, land, and
      close-without-landing, combined when one workspace is both, never to
      the requester.
- [x] D4 `internal/merge`: async merge as a create form. DetachedRun gains
      the maker, the target (primary or maker) and front placement; the
      maker's context is forked; HAND-OFF (fork + pick) MOVES TO ADMISSION so
      the merge workspace forks when the merge starts; merge arms renamed to
      `async_merge_*`; a branch owned by a registered workspace is refused;
      the subagent maker is resolved from `<session>/subagents/agent-<id>.jsonl`.
- [x] D5 MergeWorkspace: requester rules (`owner_lands`,
      `requester_may_not_land`), required placement.
- [x] D6 CloseWorkspace: requester; a self-close is accepted as
      `closes_when_turn_ends` and performed at the turn-end edge.
- [x] D7 CompactWorkspace rpc; SubmitPrompt refuses a slash command of origin
      AGENT_SENT.
- [x] D8 Status: GetWorkspaceStatus, GetMergeQueueStatus.
- [x] D9 Subscriptions: SubscribeWorkspace/UnsubscribeWorkspace (durable,
      notices as PROMPT_ORIGIN_WORKSPACE_NOTICE); SubscribeMergeQueue/
      UnsubscribeMergeQueue moved off UpdateMergeQueue, answering a snapshot.
- [x] D10 UpdateRollouts; release deploys when the checkout is ahead.
- [x] D11 EvaluateInEditor / AnswerEditorEvaluation / host push `evaluate`,
      token correlation with a measured deadline.
- [x] D12 `cmd/agent-repld`: nouns `workspace`, `merge-queue`, `editor`,
      `runtime`; `merge-queue` verb ported onto RPCs; requester from cwd.
- [x] D16 One-shot standing (owner, 2026-10-17): an rpc that marks or
      unmarks a workspace a one-shot after its creation, refused for a child
      its owner lands; WorkspaceStatus reports it; the workspace skill tells
      every agent to mark itself a one-shot when it commits to one-shotting
      the rest of its work.
- [x] D17 Awaiting correspondence (owner, 2026-10-17): SetWorkspaceCorrespondence
      marks a workspace awaiting another and clears it; the mark is durable,
      ends with the workspace's close or the awaited workspace's forget, and
      stands the footer's background arm (yellow) while no turn runs;
      WorkspaceStatus reports it; the workspace skill tells every agent to
      mark itself when it ends a turn to wait on another workspace and to
      clear it when the answer arrives.
- [x] D13 Retire `internal/commandfile` and the output-dir ingress.
- [x] D14 Integration harness and e2e callers ported.

## Editor (Emacs lisp, `lisp/`)

- [x] E1 (90394009c2) Port CreateWorkspace / CloseWorkspace / MergeWorkspace callers
      (`verbs.el`, `wire-verbs.el`) to topology + context + requester, behavior
      unchanged; fake daemon (`lisp/testsupport/fakedaemon`) follows.
- [x] E2 (e372e4ef38) Answer the `evaluate` host push with AnswerEditorEvaluation.
- [x] E3 (dafcac3c6a) Remove `SPC o h` and the help workspace.
- [x] E4 (12e42e0363) Stop writing command files (core.el output dir).

## Webapp (`webapp/`)

- [x] W1 (e511f6830d) Port `sidebar/create.ts`, `sidebar/roster.ts`, `feed/bubble-more.ts`.

## Skills, manifests, docs

- [x] S1 (17b7d51303) `/agent-repl-workspaces` (create, prompt, set incl. hidden, merge,
      open, close, kill, compact, status, subscribe), replacing CEE's
      `/create-or-update-workspace`.
      It states that `workspace kill` and every cold-gate answer (`workspace
      gate pay|clear|compact|compact-and-resume`, with model and scope) run
      only when the user explicitly asked for or approved them.
- [x] D15 `workspace gate` CLI over AnswerColdGate; WorkspaceStatus reports
      `cold_gate` with the gate's FeedId.
- [x] S2 (17b7d51303) `/agent-repl-merge-queue` from `/agent-repl-merge-queue`.
- [x] S3 (17b7d51303) `/agent-repl-runtime` from `/agent-repl-runtime`, absorbing
      `/agent-repl-runtime` as a runbook over `agent-repld editor eval`.
- [x] S4 (17b7d51303) `/agent-repl-debug`, `/agent-repl-profile`, `/agent-repl-tests`,
      `/agent-repl-install` renames; `/agent-repl-help` removed.
- [x] S5 (17b7d51303) Tests: every command a skill names exists; every agent command is
      reachable from a skill.
- [x] S6 (17b7d51303) requirements.json skill entries, metaprompt merge rule, guard hook,
      AGENTS.md, USER-GUIDE.
- [ ] S7 CEE pull request removing `/create-or-update-workspace` (confirm
      with the owner before opening).

## Verification

- [x] V1 Full unit + integration suites of daemon, lisp, webapp, shim.
- [x] V2 e2e suite (systemic change).
- [ ] V3 Land through the merge queue.
