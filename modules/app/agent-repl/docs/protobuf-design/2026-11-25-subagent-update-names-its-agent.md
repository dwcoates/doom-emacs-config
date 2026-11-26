# A spawn's running update names the agent it created

`conversation.v1.AgentSubagentUpdate` restated the spawn's prompt but not the
agent it created. The shim writes each running beat of a subagent
(`src/convert/detached.ts`, `subagentProgressEntries`) under the unit's own
upsert key, the key its start was written under. So every beat replaced the
start in the store, and a run still going was stored as an update naming no
agent.

Observed on 2026-11-25 in the workspace `pre-queue-rule-tests`. After the
0.573.1 handover the daemon's watches resumed from their pointers, so the
catch-up pages carried six running spawns' updates and no start. The daemon
logged `daemon.feed.subagent_without_start` 17 times at 13:10:43, 13:12:37 and
13:25:45, each with occasion "the history page ended", and the bubbles it drew
addressed no sub-feed.

Owner rulings: contract changes are decided and carried through by the
implementing agent (2026-10-13).

## Landed changes

### `AgentSubagentUpdate.created_agent_id`

- WHAT: `conversation.v1.AgentSubagentUpdate.created_agent_id = 6`, the same
  identity `conversation.v1.AgentSubagentStart.created_agent_id` names.
- WHY: an update upserts its start's row, so the frame a replay serves for a
  running spawn is the update alone. Restating the agent makes that frame name
  its sub-feed, as the settle arms already do.

### Contract revision 4

- WHAT: `conversation.v1.AgentActivityContract` gains
  `SUBAGENT_UPDATE_NAMES_ITS_AGENT = 4`.
- WHY A NEW REVISION: rows already stored hold updates that name no agent, and
  their starts are gone, so no producer can rewrite them. The daemon grades an
  update by the producer's stamped revision. An update stamped below 4 that
  names no agent, held with no start beside it, is expected old data (INFO).
  One stamped 4 is a producer defect (WARN, as before).
- Consequences: the shim fills the field from the task's agent or the minting
  rule, as its terminals do. The shim and the sidecar both stamp revision 4.
  The daemon's spawn fold takes an update's created agent as naming the agent.
