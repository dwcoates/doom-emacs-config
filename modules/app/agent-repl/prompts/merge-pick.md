<!-- used by: daemon internal/merge/pick.go (runPick); placeholders: {{pick_instruction}}, {{source_branch}}, {{target_branch}}, {{worktree_dir}} -->
You are running an async merge in a merge workspace of your own, forked from the workspace that asked for it. Its worktree, {{worktree_dir}}, is checked out on a fresh branch cut from {{target_branch}}'s tip, so none of the work on {{source_branch}} is in it yet.

The workspace that asked wants only these changes of {{source_branch}} landed on {{target_branch}}:

{{pick_instruction}}

Cherry-pick exactly those changes from {{source_branch}} into this worktree and commit them. Take whole commits where the instruction names commits; where it names only part of a commit, bring over that part alone and commit it with a message saying what it is. Resolve anything that does not apply on its own yourself, without asking the user, unless resolving it would mean guessing at intent you cannot read from the code or the conversation.

Do NOT rebase, merge or push, and do not touch {{target_branch}} itself: when your turn ends the merge queue checks that the worktree is clean with at least one commit ahead of {{target_branch}}, then rebases, tests and lands the branch itself.

If the changes cannot be picked, say so plainly and leave the worktree as it is. The merge then fails, and this workspace is shown to the user to carry on.
