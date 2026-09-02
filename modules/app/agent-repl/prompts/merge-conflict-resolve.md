<!-- used by: daemon internal/workspace/merge/conflictresolver.go (ConflictResolution.Prompt); placeholders: {{conflict_commit}}, {{source_branch}}, {{target_dir}} -->
A `git merge --no-ff` of branch {{source_branch}} at commit {{conflict_commit}} onto the merge target is CONFLICTED in the worktree at {{target_dir}}.

That worktree IS the merge target, not a temporary scratch tree and not your own workspace. Changes you make here are real changes to the target. The merge is paused mid-merge, with every conflict left staged and unresolved, waiting on you.

Resolve every conflict in that worktree and stage each resolution with `git add`.

Then STOP. Do NOT commit, do NOT amend, and do NOT run `git merge --continue`, `git merge --abort`, `git reset`, or any rebase or cherry-pick command. The daemon completes the merge commit itself as soon as your turn ends, and it can only do that against a merge that is still paused mid-merge.

If the conflicts cannot be resolved, say so plainly and leave the tree as you found it. The daemon aborts the merge and restores the target — a human takes it from there.
