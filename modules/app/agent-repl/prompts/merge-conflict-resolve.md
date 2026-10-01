<!-- used by: daemon internal/merge/process.go (run.conflict); placeholders: {{conflict_commit}}, {{source_branch}}, {{worktree_dir}}, {{target_branch}}, {{conflicted_files}} -->
The merge queue is landing {{source_branch}} on {{target_branch}}. It is rebasing {{source_branch}} onto {{target_branch}}'s tip in the worktree {{worktree_dir}}, one commit at a time, and replaying the commit {{conflict_commit}} CONFLICTED in: {{conflicted_files}}.

The rebase is stopped in progress in {{worktree_dir}}. Resolve the conflict there: edit each conflicted file to the right result, then `git add` it. Do NOT run `git rebase --continue`, `--skip` or `--abort`, and do not commit, reset or check anything out: when your turn ends the queue checks that nothing is still conflicted and continues the rebase itself.

Do NOT change the merge machinery while this merge is running: nothing under `modules/app/agent-repl/daemon/internal/merge/` and not `modules/app/agent-repl/bin/test-all.sh`. A resolution that does fails the merge. A fix to the merge or its gate lands through a branch of its own.

If the conflict cannot be resolved, say so plainly and leave the files as they are. The merge then fails, and the rebase is left in progress exactly where it stopped, for the user to carry on.
