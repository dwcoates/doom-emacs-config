/**
 * refusalSentence — the words the FOUR CROSS-CUTTING refusal causes say,
 * wherever they turn up.
 *
 * Every per-workspace agentrepl verb can answer with the same four causes
 * (landing 4): the workspace is unknown, the echoed dir disagrees with the
 * registry, this daemon is handing the workspace to a successor, a joining
 * daemon has not adopted it yet. They arrive under four differently-named but
 * structurally identical message types PER RPC, so written at each call site
 * they would be four sentences per verb and a dozen chances for one fact to be
 * worded three ways.
 *
 * ANYTHING ELSE IS THE CALL SITE'S. A per-rpc arm (`no_session`,
 * `not_in_catalog`, `no_login_open`) is a fact only that verb's caller can
 * word, so this answers `undefined` for it rather than inventing a generic
 * sentence — and an `undefined` the caller has no table entry for is a
 * MalformedView at the caller, never a shrug drawn at the control.
 */
import { CROSS_CUTTING_SENTENCES } from "../feed/refusal-text.js";

/** One arm of a `<Rpc>Error.cause` oneof, as protobuf-es hands it over. */
export interface RefusalCause {
  case: string;
  value?: unknown;
}

/**
 * The sentence for CAUSE, or `undefined` when the arm is RPCNAME's own.
 *
 * RPCNAME rides the signature so a future cross-cutting arm whose wording
 * differs by verb has somewhere to differ; today the four read identically
 * everywhere, which is the property this module exists to hold.
 */
export function refusalSentence(rpcName: string, cause: RefusalCause): string | undefined {
  const say = CROSS_CUTTING_SENTENCES[cause.case];
  if (say === undefined) return undefined;
  void rpcName;
  return say(cause.value as never);
}
