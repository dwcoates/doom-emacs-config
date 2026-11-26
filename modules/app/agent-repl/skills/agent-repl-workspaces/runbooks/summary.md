# Setting the topbar summary

- **SUMMARY.** `workspace summary WS -long LONG -short SHORT` sets the
  two-part summary of WS's high-level task (sends
  `agentrepl.v1.SetWorkspaceSummary`; WS is usually `here`). The workspace's
  own agent owns its summary; the daemon no longer derives one.
  - Write the long summary first, then distill the short one from it. Long:
    one paragraph (no line break), two to four sentences, at most eighty
    words. Short: one plain imperative sentence, at most fifteen words.
  - The topbar draws the short summary as its purple title and shows the
    long summary in a popup while the pointer rests on the title.
  - When to set it is the metaprompt's rule ("Workspace summary"). While no
    summary stands (a new workspace, or after a /clear, which drops it) the
    daemon closes each user or agent prompt with a note asking for one. A
    summary set before the two-part form has no long part and is owed again
    until both parts are set.
  - The format is the daemon brief `prompts/workspace-summary-owed.md`'s. A
    part that breaks it is refused `invalid_summary`, naming the part
    (`long_summary` or `short_summary`) and the reason: reword and retry.
  - `workspace status WS` answers `shortSummary` and `longSummary` (unset
    while one is owed).
