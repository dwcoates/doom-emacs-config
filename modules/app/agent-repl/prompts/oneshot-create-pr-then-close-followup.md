<!-- used by: worktree.el (agent-repl--oneshot-create-pr-then-close-followup); placeholders: {{create_pr_command}}, {{wrapup_command}} -->


After `{{create_pr_command}}` returns and its internal `/check-cicd` (the merge-queue CI run, when `--add-to-merge-queue` is in effect) reports PASS, invoke the `{{wrapup_command}}` skill to close this workspace. Do NOT merge this workspace back into its source: the change lands through CICD, and merging would duplicate it onto the local default branch.

Only invoke `{{wrapup_command}}` when `/check-cicd` reports PASS. If `/check-cicd` reports FAIL — whether from the PR-level run or the merge-queue run — do NOT invoke `{{wrapup_command}}`; STOP and surface the failing CI to the user instead.
