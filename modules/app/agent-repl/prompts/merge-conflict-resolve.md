<!-- used by: daemon internal/workspace/merge/conflictresolver.go (ConflictResolution.Prompt); placeholders: {{conflict_commit}}, {{source_branch}}, {{target_dir}} -->
A cherry-pick of commit {{conflict_commit}} from branch {{source_branch}} is CONFLICTED in the worktree at {{target_dir}}.

Resolve every conflict in that worktree and stage each resolution with `git add`.

Then STOP. Do NOT run `git cherry-pick --continue`, do NOT commit, do NOT amend, and do NOT run `git cherry-pick --abort` or `git reset`. The daemon resumes the pick itself as soon as your turn ends, and it can only do that against a cherry-pick that is still paused.

If the conflicts cannot be resolved, say so plainly and leave the tree as you found it — a human takes it from there.
