<!-- used by: daemon internal/merge/dispatch.go (remediationBrief); placeholders: {{repo_dir}}, {{deploy_started}}, {{problems}} -->
You dispatched to yourself the problems below, which the deploy of {{repo_dir}} that began at {{deploy_started}} caused, and a restart cut your turn before it reached you. Each is stated as you found it, with the records that show it, verbatim:

{{problems}}

Fix them as your session's rules say, with tests, and commit; then finish: `agent-repld workspace finished here -version-part patch -changelog-line "<what the fix changes>"`. If you find a problem you cannot or should not fix, do not finish: say why in your reply and end the turn, which puts the check before the user.
