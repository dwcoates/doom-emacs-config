# Vetting register: prompt rollback

Investigations owed once the contract is implemented. Each rests on the
Claude Agent SDK 0.3.280 (`agent-shim/claude/shim/node_modules/@anthropic-ai/claude-agent-sdk/sdk.d.ts`),
whose type surface states what an option is, not that it behaves so in our
configuration.

1. **The client uuid of a streaming-input send is the transcript record's uuid.**
   - Affected: `shim.v1.RollBackSession` finding the prompt record by the
     derived uuid; `prompt_not_recorded` would fire for every rollback.
   - Verify: send a prompt under a known uuid through the real SDK and read
     the vendor JSONL for a `type: "user"` record with that `uuid`.
   - Status: OPEN.
2. **A `resumeDropsTurn` refusal surfaces at boot, before any prompt.**
   - Affected: `RollBackSessionVendorRefused` being answered synchronously; if
     it only arrives with the next prompt, the shim cannot report it from the
     rpc and the feed would already have dropped the turns.
   - Verify: resume with a deliberately bad `resumeDropsTurn` and observe
     whether the `error_during_execution` result arrives without a pushed
     prompt.
   - Status: OPEN.
3. **`rewindFiles(prompt uuid)` restores files to their state when that
   prompt was sent, and a dry run predicts it.**
   - Affected: `files_not_restorable`, `RollBackSessionFilesRestored`.
   - Verify: with checkpointing on, edit a file in a turn, call
     `rewindFiles(uuid, {dryRun: true})` then without dry run, compare.
   - Status: OPEN.
4. **The first prompt of a conversation has a null `parentUuid`, and a
   prompt after a compaction has the compaction's boundary entry as parent.**
   - Affected: `first_prompt` refusal scope.
   - Verify: read a fresh and a compacted vendor JSONL.
   - Status: OPEN.
