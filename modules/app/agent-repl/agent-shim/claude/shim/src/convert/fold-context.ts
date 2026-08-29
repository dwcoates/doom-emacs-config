/**
 * convert/fold-context.ts — WHAT THE ENGINE KNOWS AND THE FOLD DOES NOT.
 *
 * The fold accumulates nothing beyond constant-size joins, which means every
 * fact that belongs to the SESSION rather than to the message being converted
 * has to be handed in. That is what this is: the engine's knowledge, passed per
 * message, so the fold stays a pure function of (message, context).
 *
 * DECLARED IN ITS OWN FILE because both sides import it — the engine builds one
 * per message, the converters read one — and a shared type that lives in one
 * side's file makes the other side's import look like a dependency it is not.
 *
 * OWNER: the record-plane agent. The engine imports `FoldContext` from HERE.
 */
import type { conversationv1 } from "../proto.js";

/**
 * What kind of ask the engine's permission gate is holding open for a call.
 *
 * The fold consults this so it never DOUBLE-PRODUCES a blocking unit: the gate
 * owns the `start` and `success` frames of any ask it is holding (it has the
 * callback), and the fold produces only the shapes that never had an open ask —
 * the vendor's own `permission_denied` record.
 */
export interface PendingAsk {
  readonly kind: "permission" | "question";
}

/**
 * A live vendor task, and what it is: the join `task_started` establishes
 * between the vendor's task id and the work underneath it.
 *
 * ONE LOOKUP, never a scan: the engine holds the table (it must, to serve
 * WatchBash and to name a KillTurn's refusal set) and the fold reads it.
 */
export interface LiveTask {
  /** The call that spawned the task — the unit the work detached FROM. */
  readonly toolUseId: string;
  /** The agent the task is running, when the task is an agent rather than a shell. */
  readonly agentId?: string;
}

/**
 * The session facts one message is converted against.
 *
 * Rebuilt per message by the engine. Nothing here is remembered by the fold.
 */
export interface FoldContext {
  /** The conversation's main agent — the book a session-scoped row belongs to. */
  readonly mainAgentId: conversationv1.AgentId;
  /**
   * The turn this message belongs to, when one is open. UNSET between turns:
   * a vendor record can arrive with no turn in flight (a detached agent's late
   * notification), and inventing a turn id for it would attribute work to a
   * turn that did not cause it.
   */
  readonly turnId?: conversationv1.TurnId;
  /**
   * Whether the open turn is a KEEP-ALIVE. Every row the message produces then
   * lands unserved, so no page ever returns keep-alive traffic.
   */
  readonly keepalive: boolean;
  /** The clock, for the instants the shim stamps itself (start and settle). */
  nowMs(): number;
  /** What ask, if any, the permission gate is holding open for this call. */
  pendingAsk(toolUseId: string): PendingAsk | undefined;
  /** What the vendor's task id names, from the `task_started` join. */
  liveTask(taskId: string): LiveTask | undefined;
  /**
   * The last write or edit unit this agent produced.
   *
   * The vendor's IDE-diagnostics record carries NO tool-call id, so the join to
   * the change it describes is by ADJACENCY — one remembered value, stated here
   * because nothing in the schema implies it. UNSET when no write or edit has
   * happened yet, in which case a diagnostics record is residue rather than a
   * consequence arm on a unit that does not exist.
   */
  readonly lastWriteOrEditUnit?: conversationv1.AgentActivityId;
}
