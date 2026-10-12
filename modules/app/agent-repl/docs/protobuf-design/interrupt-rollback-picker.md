# Interrupt, then roll back: the second `C-c C-k`

## Problem

`C-c C-k` in the composer interrupts the running turn. Pressed a second time,
after it interrupted a prompt, it should roll that prompt back exactly as a
rollback does today: the conversation is cut to just before the prompt and the
prompt's words land in the composer for editing. When a bubble has already
landed in the feed after that prompt, the second press must NOT roll back;
it opens an Emacs selection list (the completion backend in use) of the
prompts a rollback can reach, newest first, each with a column for the time
since it was sent and a column for the context size of the last response
before it.

Emacs holds none of that list today: the daemon owns which prompts a rollback
can reach (`daemon.internal.resolve.feed.resolver.RollbackPrompts()`), and
`agentrepl.v1.PlanRollbackRequest` can only target the daemon's selection or
the latest prompt. So the feature needs a contract change in `agentrepl.v1`.

## Landed changes

(none yet)
