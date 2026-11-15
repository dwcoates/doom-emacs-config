<!-- used by: daemon internal/merge/dispatch.go (remediationBrief); placeholders: {{repo_dir}}, {{deploy_started}}, {{problems}} -->
Your fix landed and deployed, and the deploy intake found that the deploy that began at {{deploy_started}} caused the problems below, in or caused by your fix. This workspace, the deploy remediation workspace of {{repo_dir}}, was reopened on the main branch's tip, which holds your landed fix. Each problem is stated as the intake found it, with the records that show it, verbatim:

{{problems}}

Fix them as before, with tests, and commit; then finish again: `agent-repld workspace finished here -version-part patch -changelog-line "<what the fix changes>"`.
