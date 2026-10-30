<!-- used by: daemon/internal/workspace/links.go (OpenFeedLink, the question an unresolved feed link sends); placeholders: {{href}}, {{candidates}}, {{resolver}}, {{agent_repl_dir}} -->
I clicked the link `{{href}}` in the message quoted above, and agent-repl could not open it: no file exists at any place it looked. It looked, in order, at:

{{candidates}}

Please tell me which file you meant, with its full path.

Then change agent-repl so a link like this one opens next time. The change is either:

1. a way for you to write links that are never ambiguous, or
2. a further resolution fallback in agent-repl's link resolver, which is `{{resolver}}`.

agent-repl lives at `{{agent_repl_dir}}`. Make the change there as the metaprompt's "Handed failures" section says, even if the repository you are working in is an unrelated one: dispatch it as a one-shot child workspace that lands into master, and report why the link missed and the child you dispatched.
