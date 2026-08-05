# Diagnosis: running webapp omits the breathing-bubble commit

Severity: Medium. Confidence: Certain.

The readiness report marked the webapp unready. The deployed commit was `2e75ec...`, the source commit was `6a784907...`, and the artifact was one commit and approximately 170 minutes behind during the audit.

The missing commit was `6a784907 feat(agent-repl/webapp): make the prompt bubble breathe`. Its changes were CSS and tests for the feature under active work in `agent-repl-bubble-breath`.

Root cause: source advanced without the canonical webapp deployment producing and activating a matching artifact.

Impact: the running UI cannot exhibit the feature being tested in the named workspace, so visual verification against current source is invalid. This is deployment drift, not evidence that the CSS implementation itself is wrong.
