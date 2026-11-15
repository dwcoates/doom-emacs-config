<!-- used by: daemon internal/merge/process.go (run.untilCommitted); placeholders: {{source_branch}}, {{worktree_dir}}, {{target_branch}}, {{prompt}}, {{max_prompts}} -->
The merge queue is rebasing {{source_branch}} onto {{target_branch}}, but the worktree {{worktree_dir}} has uncommitted changes. The merge queue rebases only committed work, so the rebase has NOT started: the merge keeps its place in the queue and waits for this turn.

Commit the changes in {{worktree_dir}} to {{source_branch}} now if they belong to the branch's work, or remove them if they do not (for example generated files a build rewrote that the branch does not change), then end your turn. Do not start new work in this turn.

This is prompt {{prompt}} of {{max_prompts}} to commit. If the worktree is still not clean after the last one, the merge fails and the workspace is handed back to the user.
