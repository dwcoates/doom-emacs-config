# Fork lineage

## Problem

On 2026-10-11 the workspace `ad-hoc-deploys` was forked from `emacs-bindings-support`. The fork's first request read only 27,752 tokens from the prompt cache, the prefix every fresh session shares, and wrote 138,933. The owner expects a fork to get cache hits only, because the conversation it resumes is the parent's.

The cause is the daemon's own port of the transcript. `claude-repld.internal.account.resolver.PortTranscript()` runs every record through `claude-repld.internal.remint.Mapper`, which replaces every tool_use id and re-encodes each record through a Go map, sorting its keys, tool inputs included. The vendor recorded a `thinking_drop` attachment on the fork's first request (`req_011CfvnaMW`): `reason: prefix_mismatch`, 55 of 56 thinking blocks dropped. The one block kept is the only one before the first rewritten tool_use.

A second symptom has its own cause in the same design. The fork's feed draws every inherited response folded to one line, because the store holds no turn-end rows for inherited turns in the fork's book. `claude-repld.internal.workspace.verbs.forkConversation()` copies only prompt rows, and turn-end rows never appear in the vendor transcript. The parent's book holds 20 prompt and 20 turn-end rows; the fork's holds 2 and 2, for its own turns only.

The owner chose the remedy: the vendor's `forkSession` writes the fork's transcript verbatim, and the fork reads its pre-fork history from the parent's book through a lineage pointer instead of a copy.

## Verified facts

- The vendor's `forkSession` (`@anthropic-ai/claude-agent-sdk` 0.3.280, `sdk.mjs`) copies each record with `{...record}`, so message content, tool_use ids and key order are untouched. It remaps only `uuid`, `parentUuid`, `logicalParentUuid` and `sessionId`, plus `deferred_tools_record` announcements and an attachment's `source_uuid`. It stamps every copied record with `forkedFrom: {sessionId, messageUuid}`, and does not copy file-history snapshots. Evidence tier: the shipped implementation, read directly.
- The comment in `agent-shim/claude/shim/src/engine/identity.ts:29` saying no transcript field links a fork to its parent is out of date for this SDK version, since `forkedFrom` now does.
- No code deletes rows from the store's `entry` or `agent` tables, so a parent's book outlives closing, nuking or forgetting the parent workspace.

## Landed changes
