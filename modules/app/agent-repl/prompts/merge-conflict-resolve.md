<!-- used by: daemon internal/merge/phases.go (run.conflicts); placeholders: {{conflict_commit}}, {{source_branch}}, {{source_dir}}, {{target_branch}}, {{target_dir}}, {{conflicted_files}} -->
The merge queue tried to land your branch {{source_branch}} (at commit {{conflict_commit}}) on {{target_branch}}, and the `git merge --no-ff` CONFLICTED in: {{conflicted_files}}.

The queue made that merge in a scratch tree of its own and has already thrown it away. {{target_branch}} in {{target_dir}} was never touched, and you must not touch it either: do not commit, merge, reset or check anything out there.

Resolve the conflict on YOUR OWN branch, in your own worktree at {{source_dir}}: merge {{target_branch}} into {{source_branch}} there (`git merge {{target_branch}}`), resolve every conflict, and commit the merge. When your turn ends, the queue makes its merge again on {{target_branch}}'s current tip.

Do NOT change the merge machinery while this merge is running: nothing under `modules/app/agent-repl/daemon/internal/merge/` and not `modules/app/agent-repl/bin/test-all.sh`. The queue refuses a resolution that does, and parks the merge. A fix to the merge or its gate lands through a branch of its own.

If the conflicts cannot be resolved, say so plainly and leave your branch as it was. The queue sees the same conflict again and parks the merge for a human.
