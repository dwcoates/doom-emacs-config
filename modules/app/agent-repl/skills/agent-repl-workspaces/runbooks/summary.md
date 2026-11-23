# Setting the topbar summary

- **SUMMARY.** `workspace summary WS -text SUMMARY` sets the one-sentence
  summary of WS's high-level task that its topbar draws (the purple heading)
  (sends `agentrepl.v1.SetWorkspaceSummary`; WS is usually `here`). The
  workspace's own agent owns its summary; the daemon no longer derives one.
  - When to set it is the metaprompt's rule ("Workspace summary"). While no
    summary stands (a new workspace, or after a /clear, which drops it) the
    daemon closes each user or agent prompt with a note asking for one.
  - The format is the daemon brief `prompts/workspace-summary-owed.md`'s. A
    summary that breaks it is refused `invalid_summary` with the reason:
    reword and retry.
  - `workspace status WS` answers `summary` (unset while one is owed).
