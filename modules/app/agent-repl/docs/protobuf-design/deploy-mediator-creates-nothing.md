# The deploy mediator creates no workspace

The owner ruled (2026-11-18) that the deploy mediator does all of its work in its own session and never creates a workspace of any kind; the daemon refuses such a create with a descriptive refusal that reminds the mediator of the rule.

## Landed changes

### CreateWorkspaceError gains requested_by_deploy_mediator (tag 63)

- WHAT: a new arm of `agentrepl.v1.CreateWorkspaceError`, carrying the mediator's workspace ref and a sentence saying why the create is refused and that the work belongs in the mediator's own session.
- WHY: the mediator handed a large redesign to a child workspace, which took work it owns out of its session and out of the deploy check that follows its fix; the owner ruled it must never do so, enforced by the daemon rather than by instruction alone.
- CONSEQUENCES: the daemon reads the requester's deploy-mediator standing (`deploy_mediators`, surfaced as the workspace record's `DeployMediator`) before admitting any create, the async merge form included. A create made `-as-user` names the user as requester and is not refused. Tag 62 is the async-merge off-queue arm, so this arm takes 63. Assumption: the requester always names the calling workspace for a workspace-made create (the CLI fills it with `here`); verified in `cmd/agent-repld/agentcli_test.go`'s requester assertions.
