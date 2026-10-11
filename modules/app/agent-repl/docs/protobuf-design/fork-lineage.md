# Fork lineage

## Problem

On 2026-10-11 the workspace `ad-hoc-deploys` was forked from `emacs-bindings-support`. The fork's first request read only 27,752 tokens from the prompt cache, the prefix every fresh session shares, and wrote 138,933. The owner expects a fork to get cache hits only, because the conversation it resumes is the parent's.

The cause is the daemon's own port of the transcript. `claude-repld.internal.account.resolver.PortTranscript()` runs every record through `claude-repld.internal.remint.Mapper`, which replaces every tool_use id and re-encodes each record through a Go map, sorting its keys, tool inputs included. The vendor recorded a `thinking_drop` attachment on the fork's first request (`req_011CfvnaMW`): `reason: prefix_mismatch`, 55 of 56 thinking blocks dropped. The one block kept is the only one before the first rewritten tool_use.

A second symptom has its own cause in the same design. The fork's feed draws every inherited response folded to one line, because the store holds no turn-end rows for inherited turns in the fork's book. `claude-repld.internal.workspace.verbs.forkConversation()` copies only prompt rows, and turn-end rows never appear in the vendor transcript. The parent's book holds 20 prompt and 20 turn-end rows; the fork's holds 2 and 2, for its own turns only.

The owner chose the remedy: the vendor's `forkSession` writes the fork's transcript verbatim, and the fork reads its pre-fork history from the parent's book through a lineage pointer instead of a copy.

## Scope (settled 2026-10-11)

- **Objective**: a fork's first request is a full cache hit, and its feed draws inherited history exactly as the parent's feed does.
- **What is built**: the shim forks through the vendor's `forkSession`; the fork's book records a pointer to the parent's book, fixed at the fork point; the fork's feed pages from its own book into the parent's book, stopping at that fork point.
- **Frontend**: no new component. The webapp keeps drawing whatever rows the daemon sends, so figma→idl adds nothing here.
- **Systems**: daemon (create-workspace fork path, feed history paging, removal of the inherited-row classifier); shim (`shim.v1.StartSessionRequest` gains a fork source; it persists the fork's book together with its lineage); shim-sidecar (skips records carrying `forkedFrom`); shim-store (holds the lineage pointer, and pages across it or serves both books to the daemon); editor lisp only if `:fork-session-id` depends on the old copy.
- **Non-additive changes**: `claude-repld.internal.account.resolver.PortTranscript()` and the transcript use of `claude-repld.internal.remint` are retired; `claude-repld.internal.workspace.verbs.forkConversation()` and the `wsm` ported-prompt table are retired, since inherited prompts come from the parent's records; the inherited-row classifier in `daemon/internal/resolve/feed/lineage.go` is retired; `agentrepl.v1.CreateWorkspaceFork` keeps its meaning while the shim does the fork.
- **Rejected alternative**: copying the parent's store rows (prompts and turn-end rows) into the fork's book. It was first recommended on the claim that a copy keeps the fork readable once the parent is closed; that claim was wrong, because nothing deletes store rows. The pointer leaves nothing to half-copy and needs no classifier to tell inherited rows from the fork's own.

## Core design principles

### A fork's inherited entries read exactly as they do in the parent's feed

- **In the owner's terms**: "the workspace's forked entries in the feed should look exactly as they do in the original workspace's feed." Everything reads as normal, and an inherited answer folded to one line is a defect.
- **Consequences**: inherited rows are drawn by the same resolution as the parent's rows: same final-answer marking, same caps, same interim/answer distinction. That resolution needs the parent's prompt rows and turn-end rows, which is why the fork reads the parent's book rather than a copy of the transcript. No inherited-only drawing stance (the `inheritedStance` in `daemon/internal/resolve/feed/lineage.go`) may change how a row looks.
- **Reopens**: nothing landed yet. It confirms the retirement of the inherited-row classifier in the scope.
- **Does not claim**: that inherited rows behave like the fork's own in every respect. Rollback into them and file-edit checkpoints are not supported (see Owner rulings), and they arrive as loaded pages rather than live rows.

## Owner rulings (2026-10-11)

- The vendor's default `<parent title> (fork)` title on the forked transcript is acceptable.
- File-edit checkpoints of pre-fork turns are not supported in a fork. The vendor's `forkSession` does not copy file-history snapshots.
- Rolling back into inherited entries is not supported. This matches the existing rule in `daemon/internal/resolve/feed/rollback.go:89`.
- Inherited history loads as pages on demand ("load more"), never pushed live. The owner called this "perfect".

## Verified facts

- The vendor's `forkSession` (`@anthropic-ai/claude-agent-sdk` 0.3.280, `sdk.mjs`) copies each record with `{...record}`, so message content, tool_use ids and key order are untouched. It remaps only `uuid`, `parentUuid`, `logicalParentUuid` and `sessionId`, plus `deferred_tools_record` announcements and an attachment's `source_uuid`. It stamps every copied record with `forkedFrom: {sessionId, messageUuid}`, and does not copy file-history snapshots. Evidence tier: the shipped implementation, read directly.
- The comment in `agent-shim/claude/shim/src/engine/identity.ts:29` saying no transcript field links a fork to its parent is out of date for this SDK version, since `forkedFrom` now does.
- A bounded read of another book already exists on the wire but has no daemon caller. `shim.v1.ReadHistoryRequest.target` accepts any book, and `store.v1.ReadAgentPageRequest.through` (`conversation.v1.ConversationThrough`, a millisecond instant) reads a book as it stood at that instant; both comments name a fork reading its parent as the use. The store (`shim-store/internal/server/server.go:620`) and the shim implement it, and no daemon code calls it. Evidence tier: existing code, by grep.
- No code deletes rows from the store's `entry` or `agent` tables, so a parent's book outlives closing, nuking or forgetting the parent workspace.

## Landed changes
