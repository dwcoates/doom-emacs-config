<!-- used by: daemon/internal/slackwatch (reactions.go, the one-shot directive of a fork a one-shot binding created); placeholders: {{emoji}} -->
ONE-SHOT MODE:
- This run was triggered by a one-shot binding (:{{emoji}}:), so this workspace is an agent-repl one-shot: when the task is done, it finishes itself as agent-repl directs, and its repository's deploy opens the pull request and lands it.
- This OVERRIDES the "offer first" guidance of pull request item 1 above: in one-shot mode the pull request is created by the deploy, not offered.
