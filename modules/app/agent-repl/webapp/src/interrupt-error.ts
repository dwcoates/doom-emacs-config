/**
 * interrupt-error — the ONE wording of an `InterruptError` arm, shared by every
 * control that can issue a stop.
 *
 * THE VERB HAS TWO CALL SITES AND ONE VOCABULARY. The footer's turn and
 * fan-wide stops and a detached bubble's own stop all call `Interrupt`, all
 * receive the same eight refusal arms, and all draw the answer AT THE CONTROL
 * that was clicked — so the sentence each arm reads as lives here once rather
 * than in two tables that drift apart. Each site still builds its own refusal
 * element, because the chrome around the sentence is the site's.
 *
 * THE CROSS-CUTTING FOUR ARE NOT WORDED HERE. An unknown workspace, a ref
 * whose dir disagrees with the registry's, a workspace handed to a successor
 * daemon and one a joining daemon has not adopted yet say the same thing on
 * every per-workspace rpc, and they go through `crossCuttingSentence` — THE ONE
 * REFUSAL HOOK (src/rpc/refuse.ts) — like every other call site's do.
 *
 * THAT DELEGATION IS LOAD-BEARING, NOT TIDINESS. This file used to spell those
 * four out itself, in the same words, which made it the second implementation
 * the one-hook rule exists to prevent: the hook is also what RAISES the
 * page-wide "workspace moved" notice on `transferring_away`, so a stop refused
 * with that arm drew a sentence beside the stop button and left the rest of the
 * page looking live while nothing it showed could still be true. Whether the
 * reader learned their workspace had moved depended on which button they
 * happened to press. (Audit 1 item 18, ruled: any rpc's `transferringAway`
 * raises the moved notice, quiesces, and cancels every stream.)
 *
 * `confirm_required` IS NOT A DEAD END, but only at the turn stop: the challenge
 * exists because interrupting a TURN also ends live detached agents, and the
 * answer is the same request re-sent. Every other target that receives it has
 * been answered with an arm that cannot apply to it, so it reads here as the
 * plain refusal it is and the call site logs the oddity.
 */
import { InterruptErrorSchema, type InterruptError } from "../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { log } from "./log.js";
import { crossCuttingSentence } from "./rpc/refuse.js";
import { unreachableArm } from "./rpc/strict.js";

/**
 * Every `InterruptError.kind` arm, in the generated spelling, read off the
 * SCHEMA so an arm added to the proto reaches both call sites' suites without
 * anybody remembering to extend a list.
 */
export const INTERRUPT_ERROR_ARMS: readonly string[] = (
  InterruptErrorSchema.oneofs.find((oneof) => oneof.name === "kind")?.fields ?? []
).map((field) => field.localName);

/** A set arm of the refusal oneof. */
export type InterruptErrorKind = NonNullable<InterruptError["kind"]> & { case: string };

/**
 * What the arm SAYS, as one short sentence.
 *
 * Exhaustive over the oneof: an arm a newer daemon set is a malformed view, not
 * a refusal drawn with no wording — the reader would be told their stop failed
 * and nothing about why.
 *
 * THE SHARED HOOK GETS THE FIRST LOOK, and answering it also raises the
 * page-wide move notice on `transferring_away`; what remains below is the
 * stop's OWN vocabulary, which only this verb can phrase.
 */
export function interruptErrorSentence(kind: InterruptErrorKind, path: string): string {
  const shared = crossCuttingSentence(path, kind);
  if (shared !== undefined) return shared;
  switch (kind.case) {
    case "confirmRequired": {
      const count = kind.value.liveAgentCount;
      return count === 1n
        ? "1 live agent would also stop"
        : `${count} live agents would also stop`;
    }
    case "notDetachedWork":
      return "this row names no detached work to stop";
    case "noSession":
      return "this workspace has no session to interrupt";
    case "shimRefused":
      return `the agent runner refused the stop: ${kind.value.detail}`;
    default: {
      const other: { case: string } = kind;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * Record the refusal once, at warn, with the arm and its own wording.
 *
 * A refused stop is a click that did not land, which is worth a line in the log
 * whichever control made it; the call sites differ only in their operation
 * name, so that is the one thing they pass.
 */
export function logInterruptRefusal(
  kind: InterruptErrorKind,
  sentence: string,
  operation: string,
): void {
  log("warn", `the stop was refused: ${kind.case}`, {
    operation,
    context: { arm: kind.case, sentence },
  });
}
