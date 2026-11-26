# Finding workspaces, open or closed

- `workspace find [-repository here|DIR] [-since WHEN] [-until WHEN]
  [-turned-since WHEN] [-turned-until WHEN] [-standing open|closed|landed]
  [-text TEXT] [-created-by WS] [-parent WS] [-ended-since WHEN]
  [-login here|WS | -login-root ROOT]
  [-limit N|-all]` lists the
  workspaces the daemon's registry holds, OPEN OR CLOSED (including ones
  closed long ago that the roster no longer shows), each with its name,
  branch, repository, its short and long summaries (the short one-sentence summary the
  webapp's topbar draws as the workspace's title), when it was created and
  last active, whether it is open, closed or landed, its `createdBy` and,
  for a child, its `parent`, most recently active first. Each record also
  carries `roster` (section and live status `arm`) when the sidebar draws the
  workspace, `calling` true for the caller's own record, `configDir` and
  `lastTurnAtMs` (both read as `workspace list` documents them). WHEN is an age (`90m`, `36h`, `7d`), a date (`2026-10-20`) or
  an RFC 3339 instant: `-since` keeps workspaces active at or after WHEN,
  `-until` keeps workspaces created at or before WHEN. `-since` matches any
  activity (a close, a landing, a turn's end). `-turned-since WHEN` and
  `-turned-until WHEN` instead keep only workspaces with at least one AGENT
  TURN that started at or after / at or before WHEN (both inclusive, either
  alone is open-ended on the other side, WHEN spelled as for `-since`). An
  agent turn is a prompt the user sent (editor, webapp or claude.ai), a prompt
  another workspace's agent sent, a daemon brief or notice, or a turn the
  vendor started on its own. A compaction or `/clear` (typed or the daemon's),
  any other session command the daemon acts on itself, and a restart's
  re-drive of an interrupted turn are not, and neither is a gate answer,
  rename or status change. `-login` and `-login-root` narrow to one login as
  `workspace list` documents them; a `-login-root` the daemon does not know is
  refused (exit refused) with a plain-English line on stderr naming the roots
  it knows, never an empty answer. Every filter combines with the others (all
  must hold), e.g. `-login-root ~/.claude-chesscom -turned-since 24h` finds
  the work login's workspaces that actually worked in the last day. `-text` matches name,
  branch or summary case-insensitively. `-created-by WS` (`-created-by here`)
  keeps only the workspaces WS's agent asked for: the way a workspace
  accounts for every workspace it made, open or closed, when its conversation
  was compacted and no longer remembers them. `-parent WS` (`-parent here`)
  keeps only WS's children. `-ended-since WHEN` keeps only workspaces open now
  or that landed or closed at or after WHEN, so `-parent here -ended-since
  2h` lists this workspace's children still at work plus those that ended in
  the last two hours. A closed workspace's `standing.closed.how` says how it
  ended: `closed`, `killed`, `folded` (into the child its `into` names) or
  `unrecorded` (closed before the daemon recorded how), or `nuked` (a nuke
  whose teardown has not yet forgotten the record). It answers 50 by default; `-limit N`
  or `-all` change that, and a cut answer is a `partial` extent naming how
  many were omitted. Zettel chat threads and async-merge workspaces are not
  answered. Use it whenever the agent needs to find the workspace that did
  some piece of work, or what ran in a repository or a timeframe, including
  closed workspaces; `workspace list` answers the same record for just the
  workspaces the sidebar draws now.
