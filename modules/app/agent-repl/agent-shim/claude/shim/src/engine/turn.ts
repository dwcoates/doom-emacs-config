/**
 * engine/turn.ts — the turn: delivery, refusal, and what a kill reaches.
 *
 * OWNER: the session-engine agent.
 *
 * RESPONSIBILITY. `StartTurn` (deliver the daemon's prompt and adopt its
 * TurnId), `UpdateAgent` (speak to an agent that already exists), and
 * `KillTurn` (end the turn and everything it spawned, transitively).
 *
 * ONE TURN IN FLIGHT. The DAEMON is the only queue, so a second `StartTurn`
 * while one is open is the daemon's bug and is refused with
 * `StartTurnTurnAlreadyOpen` rather than queued. Queuing it here would create a
 * second queue nobody can see, and the daemon's model of what is pending would
 * silently stop being true.
 *
 * SPAWN PROVENANCE IS THE ONE BOUNDED STATE EXCEPTION. `task → spawning call →
 * turn`, kept solely so `KillTurn` can NAME its transitive refusal set. It
 * never appears on the wire except inside a refusal, and it is bounded by the
 * number of live tasks.
 *
 * R15: THE PROMPT ROW IS WRITTEN AND ACKED FIRST. `StartTurn` writes its
 * `AgentPrompt` row and has the store's DURABLE ACK before the turn's first
 * activity frame is written — otherwise a crash in between leaves activity
 * hanging under a prompt that was never recorded.
 */
export {};
