/**
 * engine/permission-gate.ts — `canUseTool`, and everything it owes.
 *
 * OWNER: the permission agent.
 *
 * RESPONSIBILITY. Turn the vendor's `canUseTool` callback into an
 * `AgentPermission` (a gate on a tool call) or an `AgentQuestion` (an
 * AskUserQuestion), carry the consumer's decision back, and resolve the pending
 * promise.
 *
 * CALLBACK LIVENESS IS A HARD OBLIGATION. Every teardown path — interrupt,
 * shutdown, SDK abort — resolves ALL pending permission callbacks (as denied)
 * BEFORE proceeding. An unresolved `canUseTool` promise wedges the vendor
 * process: it is waiting for an answer that will never come, and nothing else
 * can proceed past it.
 *
 * THE ANSWER BOUNDARY, UNDONE. The question tool keys answers by question TEXT
 * and comma-joins multi-selects. The shim undoes both here using the ECHOED
 * VALUES, validated against the pending callback it already holds — no new
 * state is introduced to do it. FREE-TEXT RESIDUE (ruled 2026-08-29): whatever
 * remains of the joined answer string after every validated label is removed IS
 * the typed free text. That residue is `free_text`'s producer definition, not
 * an approximation of it.
 *
 * AN ECHO THAT DOES NOT MATCH IS `UpdateAgentAnswerMismatch`, never a guess: a
 * mismatched echo means the consumer answered a question the shim is not
 * holding, and choosing an option on the user's behalf is the one outcome a
 * permission gate must never produce.
 */
export {};
