<!-- used by: daemon internal/merge/dispatch.go (remediationBrief); placeholders: {{repo_dir}}, {{deploy_started}}, {{problems}} -->
The deploy intake found that the deploy that began at {{deploy_started}} caused the problems below. This workspace is the one standing deploy remediation workspace of {{repo_dir}}, which takes every dispatch, so your earlier fixes in this conversation may bear on these problems; it was reopened on the main branch's tip, which holds every fix you landed, or on your branch if your last fix never landed. Each problem is stated as the intake found it, with the records that show it, verbatim:

{{problems}}

Fix them as before, with tests, and commit; then finish again: `agent-repld workspace finished here -version-part patch -changelog-line "<what the fix changes>"`.
