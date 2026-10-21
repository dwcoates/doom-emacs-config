<!-- used by: daemon/internal/workspacenotice (a child that does not carry this workspace's context was created on this workspace's branch); placeholders: {{child}}, {{requester}}, {{notice}} -->
A new workspace, `{{child}}`, was cut from this workspace's branch by {{requester}}, and it lands into this workspace. It was not forked from this conversation, so it does not know why this workspace's work is shaped as it is. Its requester says:

> {{notice}}

LANDING `{{child}}` INTO THIS WORKSPACE IS YOUR RESPONSIBILITY: it never lands itself. When its work is done, review it against this workspace's own work, and land it with `agent-repld workspace finished {{child}} -placement back` (see `/agent-repl-workspaces`), or close it without landing if it should not land. You can talk to it with `agent-repld workspace prompt`.
