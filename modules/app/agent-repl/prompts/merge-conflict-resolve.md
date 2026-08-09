<!-- used by: daemon internal/workspace/merge/conflictresolver.go (ConflictResolution.Prompt); placeholders: {{conflict_commit}}, {{source_branch}}, {{target_dir}} -->
A rebase of commit {{conflict_commit}} from branch {{source_branch}} onto the merge target is CONFLICTED in the worktree at {{target_dir}}.

That worktree is a TEMPORARY REBASE WORKTREE, not the merge target and not your own workspace. The merge target has not been modified at all and will not be until every commit of this rebase has landed and passed the test suite, so nothing you do here can break the tree anybody else is working from.

Resolve every conflict in that worktree and stage each resolution with `git add`.

Then STOP. Do NOT run `git cherry-pick --continue` or `git rebase --continue`, do NOT commit, do NOT amend, and do NOT run `git cherry-pick --abort`, `git rebase --abort` or `git reset`. The daemon continues the rebase itself as soon as your turn ends, and it can only do that against a replay that is still paused.

If the conflicts cannot be resolved, say so plainly and leave the tree as you found it — a human takes it from there.
