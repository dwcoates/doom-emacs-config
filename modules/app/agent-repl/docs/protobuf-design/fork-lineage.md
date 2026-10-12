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
- A fork taken mid-turn ends just before the turn in flight started, which is that turn's opening prompt. The fork therefore holds only finished turns, which the parent never rewrites, so the fork's inherited rows cannot change after the fork. The owner then directed the change to be carried end to end and merged in one shot, with the orchestrator's recommendations standing as the design: a fork point that names a record rather than an instant, and the pointer held by the daemon.

## Design (settled 2026-10-12)

- **The fork runs under the parent's account root.** The child's session row takes the parent session's config dir instead of `ConfigDirFor(child dir)`. The vendor's `forkSession` reads the parent transcript only from the root the calling process runs under, and the prompt cache is account-scoped, so a fork under another account could never hit the cache. The owner ruled that the edge case (a parent whose account was switched away from its directory's default) does not matter: by default both are the account of the main branch.
- **The fork point is the end of the parent's newest concluded turn, as the parent's own running shim knows it.** Only that shim has seen every message of every turn in order, including turns the vendor started on its own, so it alone can name the final record of the newest concluded turn without a race. A new rpc, `shim.v1.GetForkPoint`, answers it, and the daemon asks the parent's shim while it holds the parent's delivery lock (the exclusion `promptqueue.queue.RollBack` uses). A parent with no running shim cannot be mid-turn, so its whole transcript is forked.
- **The vendor fork runs on the child's shim as its own rpc, `shim.v1.ForkSession`, before StartSession.** It calls the vendor's `forkSession()` cut at the fork point and moves the new file from the parent's project directory into the child's. It is separate from StartSession so the daemon persists the new vendor session id before resuming: a resume refused as cold is retried as a resume of the same fork, never a second fork.
- **The fork's feed reads the parent's book up to the fork point.** The daemon records the parent's book, the inherited turns, and the instant the fork point's turn concluded; it reads the parent's book through the existing `shim.v1.ReadHistoryRequest.target` plus `through`, and drops any row stamped with a turn the fork did not inherit.
- **Accepted residual (owner ruling, 2026-10-12, on the orchestrator's recommendation):** a row with no turn stamp, belonging to the parent's next turn, written in the same millisecond the inherited turn concluded, would be drawn in the fork. Excluding it structurally would need the store to order rows by turn, which reorders every book to fix a case needing three coincidences.

## Verified facts

- The vendor's `forkSession` (`@anthropic-ai/claude-agent-sdk` 0.3.280, `sdk.mjs`) copies each record with `{...record}`, so message content, tool_use ids and key order are untouched. It remaps only `uuid`, `parentUuid`, `logicalParentUuid` and `sessionId`, plus `deferred_tools_record` announcements and an attachment's `source_uuid`. It stamps every copied record with `forkedFrom: {sessionId, messageUuid}`, and does not copy file-history snapshots. Evidence tier: the shipped implementation, read directly.
- The comment in `agent-shim/claude/shim/src/engine/identity.ts:29` saying no transcript field links a fork to its parent is out of date for this SDK version, since `forkedFrom` now does.
- A bounded read of another book already exists on the wire but has no daemon caller. `shim.v1.ReadHistoryRequest.target` accepts any book, and `store.v1.ReadAgentPageRequest.through` (`conversation.v1.ConversationThrough`, a millisecond instant) reads a book as it stood at that instant; both comments name a fork reading its parent as the use. The store (`shim-store/internal/server/server.go:620`) and the shim implement it, and no daemon code calls it. Evidence tier: existing code, by grep.
- No code deletes rows from the store's `entry` or `agent` tables, so a parent's book outlives closing, nuking or forgetting the parent workspace. The store's prune deletes only `write_ledger` rows (`shim-store/internal/db/prune.go:205`).
- A parent's rows before the fork point can still change after the fork in two ways. The hook sweep (`shim-store/internal/db/hooksweep.go`) rewrites old hook rows store-wide, which changes the parent's feed identically, so the fork still matches it. An upsert rewrites a row whose unit was still open at the fork: in a mid-turn fork, the parent's in-flight units settle after the fork, so the fork would show content the parent produced after the fork. Finished turns are never rewritten by later writes of the same conversation. Evidence tier: existing code.
- A parent's rollback is recorded in the daemon's own database for that workspace, not written into the store, so it does not change the rows a fork reads. Inferred from `daemon/internal/resolve/feed/rollback.go`. The fork showing those turns is correct, because its conversation still contains them.

## Landed changes
