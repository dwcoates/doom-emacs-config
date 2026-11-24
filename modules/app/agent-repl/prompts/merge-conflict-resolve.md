<!-- used by: daemon internal/merge/process.go (run.conflict); placeholders: {{conflict_commit}}, {{source_branch}}, {{worktree_dir}}, {{target_branch}}, {{conflicted_files}}, {{after_give_up}} -->
The merge queue is landing {{source_branch}} on {{target_branch}}. It is rebasing {{source_branch}} onto {{target_branch}}'s tip in the worktree {{worktree_dir}}, one commit at a time, and replaying the commit {{conflict_commit}} CONFLICTED in: {{conflicted_files}}.

The rebase is stopped in progress in {{worktree_dir}}. Resolve the conflict there: edit each conflicted file to the right result, then `git add` it. Make the result build and pass as you would leave any commit: when the resolution needs a change in another file to build (a renamed symbol, a changed signature), make that change too and `git add` it. Do NOT run `git rebase --continue`, `--skip` or `--abort`, and do not commit, reset or check anything out: when your turn ends the queue checks that nothing is still conflicted and continues the rebase itself.

Never leave a resolution broken, unresolved or half done on purpose to make the merge fail: that spends the queue's test run on a merge that cannot land. If you cannot resolve the conflict correctly, give the merge up instead, then end your turn with a reply saying why:

`agent-repld merge-queue give-up -reason "<why the conflict cannot be resolved here, in a sentence or two>"`

The merge leaves the queue at once, and the rebase is aborted so the branch keeps its base. {{after_give_up}}
