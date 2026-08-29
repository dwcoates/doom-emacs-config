/**
 * convert/fold.ts — the seam every converter plugs into.
 *
 * # What the fold IS
 *
 * The vendor's stream is a FLAT LOG of records. `conversation.v1` is a model of
 * UNITS WITH IDENTITY that are upserted whole. The fold is the mapping between
 * them, and it is the shim's central act: one SDK message in, zero or more
 * self-describing frames out.
 *
 * It ACCUMULATES NOTHING beyond constant-size joins. That is not an efficiency
 * preference, it is the statelessness the whole architecture rests on: a shim
 * that grew state per turn would be a second, divergent copy of the record the
 * store already owns, and a bounce would lose it. The joins it is allowed are
 * each ONE lookup of a remembered value — a tool result to its call by
 * `tool_use_id`, a skill document to its call by `sourceToolUseID`, a spawned
 * agent to its spawn — never a scan and never a history.
 *
 * # The rules every converter obeys
 *
 *   - Units upsert BY IDENTITY, and every frame of a unit is self-describing.
 *   - `update` frames are DELTAS, never cumulative; terminals carry wholes.
 *   - Usage and effort ride the FIRST content block's unit of each API
 *     response, and no other.
 *   - The EXEMPT SET is dropped silently and never becomes `AgentUnmodeled`;
 *     `AgentUnmodeled` means a genuinely unknown tool, and a recognizable
 *     built-in arriving there is a producer defect.
 *   - Anything unconvertible becomes RESIDUE rather than being dropped or
 *     crashing. Residue is the reason the fold can be eager: nothing is lost by
 *     failing to understand it.
 *   - Every converter LOGS its branch, so a wrong conversion is diagnosable
 *     from the record of what was chosen rather than by re-deriving it.
 *
 * # Ownership
 *
 * OWNER: the fold agent (`convert/`). This file declares the seam and the
 * output shape and deliberately implements NO conversion: the dispatcher, the
 * stream-event converters, the per-tool files, the terminals and the residue
 * are that agent's, and they attach here.
 */
import type { conversationv1, storev1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";

/**
 * Everything ONE SDK message produced.
 *
 * Three lists rather than one union, because the three go to three different
 * places and a consumer must not have to re-derive which is which:
 *
 *   - `frames` are the agent's own record. They route by
 *     `AgentFrame.agent_id` and land as page lines (an `update`), as a page
 *     line PLUS the agent's terminal state (a `success`/`failure`), or as the
 *     lifecycle record for their kind (a `detached_work`).
 *   - `sessionUpdates` are facts about the SESSION, not about any agent. They
 *     ride WatchSession, and the vendor-sourced ones are written to the store.
 *     The shim-SYNTHESIZED ones (`diagnostics`, `context_usage`) are pushed and
 *     never written — they are the shim's report about itself, not vendor
 *     conversation.
 *   - `residue` is what could not be converted. Never dropped, never a crash.
 *
 * All three are empty for a message that produced nothing, which is a normal
 * and frequent outcome — every exempt tool call yields exactly this.
 */
export interface FoldOutput {
  /**
   * Conversation frames, in the order the vendor stated them.
   *
   * `AgentUpdate` carries five page-line arms the shim can produce: `activity`,
   * `question`, `permission`, `context_cut` (a /clear, a compaction, or a
   * compaction that failed) and `api_error` (a failed API request as MID-TURN
   * evidence — a turn TERMINAL is still `AgentFailure.api_request_failed`, and
   * the two must not be confused: one says the turn is over, the other says it
   * is not).
   */
  readonly frames: readonly conversationv1.AgentFrame[];
  /** Session-level facts this message stated. */
  readonly sessionUpdates: readonly conversationv1.SessionUpdate[];
  /** What this message carried that no converter could model. */
  readonly residue: readonly storev1.StoreUnservedItem[];
}

/** An output that produced nothing — the exempt set's answer, and the common case. */
export const EMPTY_FOLD_OUTPUT: FoldOutput = { frames: [], sessionUpdates: [], residue: [] };

/**
 * The fold, as the engine drives it.
 *
 * ONE method, called once per SDK message in arrival order. It is synchronous
 * on purpose: a fold that could await would be able to interleave two messages
 * and break the ordering every upsert depends on. Anything that must be awaited
 * (a store write) is the caller's, after the fold has answered.
 */
export interface Fold {
  /**
   * Convert one SDK message.
   *
   * NEVER THROWS for an unrecognized record: that is what `residue` is for. It
   * may throw for a BROKEN one — a record missing a field the contract says is
   * always present — because that is a producer defect the session must surface
   * as a `SessionFault` rather than quietly convert around.
   */
  onSdkMessage(message: SdkMessage): FoldOutput;
}
