# Remediation: deploy and prove artifact identity

1. Run the canonical webapp build and deployment from the intended source commit.
2. Verify the artifact embeds or reports the exact source revision.
3. Restart only the webapp component through the owned deployment path and wait for readiness.
4. Verify the breathing behavior and its reduced-motion behavior in the live surface.
5. Make the deployment gate fail when source and deployed revisions differ, with both revisions in structured output.

Success criteria: readiness is true, deployed and source commits match, and live behavior passes the feature's visual and automated checks.

Protobuf decision: no change.
