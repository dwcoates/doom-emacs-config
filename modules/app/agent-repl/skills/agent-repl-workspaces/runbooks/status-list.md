# Reading status and listing workspaces

- `workspace status WS` reads one workspace: its activity, its merge target
  (`primary` or `workspace{workspace}`), its repository, its `relationship`
  (child{parent} or peer), its `owner` (the owning workspace; absent
  for a peer), `createdBy`, its open `children`, whether it is a one-shot,
  whether it awaits another workspace, and a cold gate it is parked at.
- `workspace list` prints `workspace find`'s record, one compact JSON object a
  line, for every workspace the sidebar draws (open, hidden and recently merged)
  in every repository, most recently active first. Its one narrowing is a
  login, named either way: `-login here|WS` is the login (Claude config dir)
  WS's repository is routed to, and `-login-root ROOT` is the login whose
  config dir is ROOT (e.g. `~/.claude-chesscom`; a leading `~/` is expanded).
  Either keeps the workspaces whose repository is routed to that login, plus
  the direct children of those workspaces in any repository. Narrow, filter and sort everything else
  yourself with `jq`, `head`, `sort`, `grep` and other shell utilities. Each line is one
  `agentrepl.v1.FoundWorkspace` in protojson field names, and a field that is
  unset or false is absent:
  - `name`, `branch`: strings.
  - `workspace.id`, `workspace.dir`, `repository.id`, `repository.dir`: strings.
  - `summary`: a string, the purple one-sentence task summary the topbar draws;
    absent when none was ever distilled.
  - `createdAtMs`, `lastActiveAtMs`: unix milliseconds encoded as JSON STRINGS
    (protojson `int64`), so compare them with `tonumber`.
  - `standing`: exactly one key of `open` (`{}`), `closed` (`{}` or
    `{closedAtMs}`) or `landed` (`{landedAtMs}`).
  - `createdBy`, `parent`: `{id, dir}` when set.
  - `roster.section`: exactly one key of `open`, `hidden` or `merged` (each `{}`).
  - `roster.status.arm`: the live status name, e.g. `thinking`, `submitting`,
    `permission`, `question`, `done`, `interrupted`, `turn_failed`, `ready`,
    `idle_async`, `cold_gate`, `init`.
  - `calling`: `true` only on the record of the workspace the command runs from.
  - `configDir`: the Claude config dir the workspace's session last came up
    under; absent when it never came up. It is the session's own, not what
    the login narrowing matches.
  - `lastTurnAtMs`: when its last AGENT TURN started (`workspace find`'s
    `-turned-since` says which turns count), as a JSON string; absent when it
    never took one.
  - Recipes, each one command:
    - the 5 most recently active: `agent-repld workspace list | head -n 5`
    - by live status: `agent-repld workspace list | jq -c 'select(.roster.status.arm == "thinking")'`
    - by section: `agent-repld workspace list | jq -c 'select(.roster.section | has("hidden"))'`
    - regex over the summary, case-insensitive: `agent-repld workspace list | jq -c 'select((.summary // "") | test("footer"; "i"))'`
    - one repository: `agent-repld workspace list | jq -c 'select(.repository.dir == "/path/to/main/checkout")'`
    - active in the last hour: `agent-repld workspace list | jq -c --argjson since "$(( ($(date +%s) - 3600) * 1000 ))" 'select((.lastActiveAtMs | tonumber) >= $since)'`
    - a readable table: `agent-repld workspace list | jq -r '[.name, .roster.status.arm, .summary // ""] | @tsv'`
    - sorted by name: `agent-repld workspace list | jq -s -c 'sort_by(.name)[]'`
