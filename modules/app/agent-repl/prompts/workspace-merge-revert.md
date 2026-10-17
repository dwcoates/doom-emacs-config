<!-- used by: daemon/internal/workspace (OpenToRevertMerge, the brief a reopened merged workspace is given to revert its merge); placeholders: {{branch}}, {{parent_branch}}, {{landed_commit}}, {{merged_at}} -->
This workspace was reopened to REVERT the merge it landed. The user asked for the revert from the sidebar's Recently Merged section, and follows its progress here.

- This workspace's branch `{{branch}}` was merged into {{parent_branch}} at {{merged_at}}.
- When it landed, the branch held commit `{{landed_commit}}`.
- The branch has since been brought up to where {{parent_branch}} is now, so the merged work is in this checkout.

1. Find what that merge brought in: the first commit in the parent branch's first-parent history that contains `{{landed_commit}}` (`git log --first-parent --ancestry-path --reverse --format='%H %s' {{landed_commit}}..HEAD | head -1`). When that commit is a merge, it is the merge to revert. When the merge was a fast-forward, the commits to revert are the ones `{{landed_commit}}` brought in.
2. Revert it on this workspace's branch with `git revert` (`-m 1` for a merge commit), resolving any conflict with work that landed after it. Never rewrite history.
3. Run the tests that cover what the revert touches, and fix whatever the revert breaks.
4. Commit, then land the revert with `agent-repld workspace merge here -placement back` (the `/agent-repl-merge-queue` skill, which says the version increment a landing on agent-repl's master carries), the way this workspace lands any finished work.

If something only the user can decide stands in the way, such as later work that depends on what is being reverted, stop and say so instead of landing.
