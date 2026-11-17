<!-- used by: daemon internal/merge/dispatch.go (remediationBrief); placeholders: {{repo_dir}}, {{deploy_started}}, {{problems}} -->
The deploy intake found that the deploy that began at {{deploy_started}} caused the problems below. This workspace is the standing fixer of {{repo_dir}}, which takes every dispatch and every escalation, so your earlier work in this conversation may bear on these problems. Its worktree was brought to the main branch's tip, which holds every fix you landed; if it holds uncommitted work or commits that never landed, it stayed where it was, so start by checking `git status` and `git log` against the main branch. Each problem is stated as the intake found it, with the records that show it, verbatim:

{{problems}}

Fix them as before, with tests, and commit; then finish again: `agent-repld workspace finished here -version-part patch -changelog-line "<what the fix changes>"`. If you find a problem you cannot or should not fix (its cause is unknown, or the fix asked for would only hide it), do not finish: say why in your reply and end the turn. A turn that ends without a finish puts the check before the user, who decides it with you here.
