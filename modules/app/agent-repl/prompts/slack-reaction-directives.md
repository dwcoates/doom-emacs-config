<!-- used by: daemon/internal/slackwatch (reactions.go, the work directives a fired binding's intake hands its fork); placeholders: {{link}}, {{channel}}, {{ts}}, {{emoji}}, {{subscribed}}, {{pull_requests}}, {{one_shot}} -->
These directives are addressed to the forked workspace: the workspace that does the work on a Slack-thread-triggered task. The originating message is {{link}} (channel {{channel}}, ts {{ts}}, trigger :{{emoji}}:).
{{subscribed}}
FIRST: make sure you understand the request. The turn you were forked from read the originating thread; read it again (`gns slack convo {{link}} --all --json`) whenever you need more of it.

THEN, before planning or doing any work: run `/agent-repl-workspaces` and follow its "Reorganizing across repositories" section. This workspace was created in its repository by default, which may not be where the work belongs.

THEN: plan the work and act on it.

CRITICAL — SCOPE OF THIS TRIGGER:
- The triggering message is EXACTLY the one at {{link}}. The :{{emoji}}: was either typed in that message or added to it as a reaction; either way, that message is the work item.
- Do NOT treat ack emojis or reactions on the parent message, on earlier or later replies, or on any other message in the conversation as additional triggers: they are context, not work items.

Pull requests:
1. After executing on the task and committing the changes, offer to create or update a pull request (with the /create-or-update-pr skill). Do not run it without the user's explicit approval: only offer.
{{pull_requests}}
{{one_shot}}
