# Finding workspaces, open or closed

- `workspace find [-repository here|DIR] [-since WHEN] [-until WHEN]
  [-standing open|closed|landed] [-text TEXT] [-created-by WS] [-limit N|-all]` lists the
  workspaces the daemon's registry holds, OPEN OR CLOSED (including ones
  closed long ago that the roster no longer shows), each with its name,
  branch, repository, its summary (the one-sentence task summary the
  webapp's topbar draws as the workspace's title), when it was created and
  last active, whether it is open, closed or landed, its `createdBy` and,
  for a child, its `parent`, most recently active first. Each record also
  carries `roster` (section and live status `arm`) when the sidebar draws the
  workspace, and `calling` true for the caller's own record. WHEN is an age (`90m`, `36h`, `7d`), a date (`2026-10-20`) or
  an RFC 3339 instant: `-since` keeps workspaces active at or after WHEN,
  `-until` keeps workspaces created at or before WHEN. `-text` matches name,
  branch or summary case-insensitively. `-created-by WS` (`-created-by here`)
  keeps only the workspaces WS's agent asked for: the way a workspace
  accounts for every workspace it made, open or closed, when its conversation
  was compacted and no longer remembers them. It answers 50 by default; `-limit N`
  or `-all` change that, and a cut answer is a `partial` extent naming how
  many were omitted. Zettel chat threads and async-merge workspaces are not
  answered. Use it whenever the agent needs to find the workspace that did
  some piece of work, or what ran in a repository or a timeframe, including
  closed workspaces; `workspace list` answers the same record for just the
  workspaces the sidebar draws now.
