/**
 * refusal-text — the sentence a typed `<Rpc>Error` arm says at the control that
 * provoked it.
 *
 * WHY ONE MODULE. Every agentrepl verb's error carries the SAME FOUR
 * cross-cutting causes — the workspace is unknown, the echoed dir disagrees with
 * the registry, the daemon is handing this workspace to a successor, a joining
 * daemon has not adopted it yet — under four differently-named-but-identical
 * message types, one set per rpc. Written at each call site, those four would be
 * four sentences per verb and a dozen chances for the same fact to be worded
 * three ways. Written here once, a reader who sees "workspace ref mismatch" at a
 * permission button sees exactly the same words at a cold-gate button.
 *
 * THE ARM IS THE MESSAGE. Nothing here reads a payload it was not given, and
 * nothing invents a cause: an arm this build has no sentence for is a MALFORMED
 * VIEW (a newer daemon added a cause and this build must say so loudly, not
 * draw a shrug), and an UNSET cause is malformed too — an error that names no
 * reason is not an error the wire is allowed to send.
 *
 * THE SENTENCES ARE SHORT because they sit beside a button in a card, and they
 * carry the payload where the payload is what the reader needs to act: the
 * successor's address to dial, the registry's dir to reconcile against, the
 * shim's own account of its refusal.
 */
import { requireCase, unreachableArm } from "../rpc/strict.js";

/** One arm's sentence, given whatever that arm carried. */
export type ArmSentence = (value: never) => string;

/** How a caller states the arms that are its own verb's alone. */
export type SentenceTable = Record<string, (value: never) => string>;

/**
 * THE FOUR CROSS-CUTTING CAUSES, worded once.
 *
 * Every agentrepl verb can answer with these, so every call site in this app
 * says the same four sentences for them.
 */
export const CROSS_CUTTING_SENTENCES = {
  unknownWorkspace: () => "unknown workspace",
  workspaceRefMismatch: (value: { registryDir: string }) =>
    `workspace ref mismatch — registry says ${value.registryDir}`,
  transferringAway: (value: { address: string }) =>
    `this workspace is transferring to ${value.address}`,
  notYetAdopted: () => "the new daemon has not adopted this workspace yet",
} as unknown as SentenceTable;

/**
 * The arm and its sentence, for an error whose cause oneof is named FIELD.
 *
 * The oneof is required through `requireCase`, so an error carrying no cause
 * throws rather than drawing an unexplained refusal — the arm's NAME is what the
 * `.refusal[data-arm]` hook carries, and there is no honest value for it when
 * the wire named none.
 */
export function refusalOf(
  cause: { case?: string | undefined; value?: unknown },
  own: SentenceTable,
  path: string,
): { arm: string; text: string } {
  const arm = requireCase(cause, path);
  const say = own[arm.case] ?? CROSS_CUTTING_SENTENCES[arm.case];
  if (say === undefined) {
    // A cause a NEWER daemon added. Drawing a generic sentence would tell the
    // reader their click failed for a reason this build simply did not read.
    return unreachableArm(path, arm.case);
  }
  return { arm: arm.case, text: say(arm.value as never) };
}
