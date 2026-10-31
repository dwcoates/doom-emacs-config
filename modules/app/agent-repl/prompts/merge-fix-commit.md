<!-- used by: daemon internal/merge/process.go (run.requireCommitted); placeholders: {{source_branch}}, {{worktree_dir}}, {{target_branch}}, {{prompt}}, {{max_prompts}} -->
Your fixing turn for the merge of {{source_branch}} into {{target_branch}} ended, but the worktree {{worktree_dir}} still has uncommitted changes. The merge queue rebases only committed work, so it has NOT gone on: the merge keeps its place in the queue and waits for this turn.

Commit your fix to {{source_branch}} in {{worktree_dir}} now, then end your turn. Commit only the fix you made for the failing tests; if something there is not part of the fix, remove it rather than commit it. Do not start new work in this turn.

This is prompt {{prompt}} of {{max_prompts}} to commit. If the worktree is still not clean after the last one, the merge fails and the workspace is handed back to the user.
