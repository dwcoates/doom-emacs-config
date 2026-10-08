# The roster projects the footer's status

The footer strip and the roster row (the webapp rail, which the Emacs tab bar
paints) each walk the status ladder over their own facts. A fact fed to only
one of them moves that surface alone, so the two can make different coarse
claims about one workspace. The owner's report (2026-10-08): the doom
workspace's footer read `agent repl fault` (blue) while its rail and tab read
`thinking` (red), because a standing `watch_open_refused` fault reaches only
the footer. The owner's requirement is that the footer and the
sidebar/tab-bar status are structurally the same, never the same by
coincidence.

The fix makes the footer the one walker of the ladder and the roster a
projection of the footer's resolved status. The roster keeps only its own
detail within the claim the footer resolved. Some footer statuses have no
roster arm to project onto, so the roster's status `oneof` needs new arms.

## Landed changes

### 1. Three roster status arms: `closing`, `daemon_impaired`, `waiting`

- **What changed:** `RosterRow.status` gains `closing` (53),
  `daemon_impaired` (54) and `waiting` (55), each an empty arm message.
- **Why:** the owner ruled the footer and the sidebar/tab-bar status must
  always be the same, structurally. With the roster projecting the footer's
  resolved status, every footer status needs a roster arm making the same
  coarse claim. Three had none: a refused close (the `closing` rung), the
  daemon itself impaired (`agent_repl_fault · daemon_impaired`) and a wait
  that is not a tool permission (`waiting` in its `question`, `cold_gate`
  and `interrupting` substatuses). Agreed by the owner on 2026-10-08 ("go for
  it").
- **Consequences:**
  - `render-colors.json`'s `roster_status` table gains three rows: `closing`
    blue, `daemon_impaired` blue, `waiting` green. These are the colors of
    the footer arms they project.
  - `ladder.RosterArmClaim` places them: `closing` on Closing,
    `daemon_impaired` on AgentReplFault, `waiting` on Waiting.
  - Every roster consumer that switches on the arm (the webapp's
    `vocab.ts`, the editor's `wire-roster.el`) gains the three arms.
  - The roster resolver's status is no longer resolved from its own facts:
    the footer is the one walker of the ladder, and the roster projects the
    footer's status. Within the idle claim the roster keeps its own detail
    (an unread turn result outranks background work), because that ruling
    is the roster's.
