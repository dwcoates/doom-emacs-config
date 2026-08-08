<!-- used by: worktree.el (agent-repl--oneshot-create-pr-then-merge-followup); placeholders: {{create_pr_command}} -->


After `{{create_pr_command}}` returns and its internal `/check-cicd` (the merge-queue CI run, when `--add-to-merge-queue` is in effect) reports PASS, invoke the `/create-or-update-workspace merge` skill to merge this workspace back into its source.

Only invoke `/create-or-update-workspace merge` when `/check-cicd` reports PASS. If `/check-cicd` reports FAIL — whether from the PR-level run or the merge-queue run — do NOT invoke `/create-or-update-workspace merge`; STOP and surface the failing CI to the user instead.
