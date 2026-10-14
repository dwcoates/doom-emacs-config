<!-- used by: daemon/internal/workspace/createfailed.go (Create, the request a failed workspace create sends its repository's main-worktree workspace); placeholders: {{repository}}, {{request}}, {{cause}}, {{agent_repl_dir}} -->
A workspace I asked agent-repl to create in this repository (`{{repository}}`) failed to be created.

What the create asked for:

{{request}}

The failure agent-repl reported:

{{cause}}

Please find out why it failed. agent-repl lives at `{{agent_repl_dir}}`, and its daemon records every create in its logs; the `/debug-emacs-agent-repl` skill knows where those logs are and how to read them, if it is available to you. Tell me the root cause, with the log records that show it, and propose a fix.

Do not change anything, and do not retry the create, until I answer.
