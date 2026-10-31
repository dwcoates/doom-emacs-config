<!-- used by: daemon/internal/workspace/createfailed.go (Create, the request a failed workspace create sends its repository's main-worktree workspace); placeholders: {{repository}}, {{request}}, {{cause}}, {{agent_repl_dir}} -->
A workspace I asked agent-repl to create in this repository (`{{repository}}`) failed to be created.

What the create asked for:

{{request}}

The failure agent-repl reported:

{{cause}}

Please find out why it failed. agent-repl lives at `{{agent_repl_dir}}`, and its daemon records every create in its logs; the `/agent-repl-debug` skill knows where those logs are and how to read them, if it is available to you. Then fix it as the metaprompt's "Handed failures" section says: name the root cause with the log records that show it, dispatch the fix as a one-shot child workspace that lands into master, and report the cause, the records, and the child you dispatched.

Do not retry the create yourself.
