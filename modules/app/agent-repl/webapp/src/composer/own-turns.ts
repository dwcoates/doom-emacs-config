/**
 * own-turns — the turns THIS page submitted.
 *
 * A prompt can reach a workspace from several places at once: this webview's
 * dev composer, a bubble composer, and the host's own (Emacs) composer. The
 * feed draws every one of them identically, because the daemon serves them
 * identically — but the person looking at the page did type one of them, and a
 * row that is the answer to their own keystrokes reads differently from one
 * that appeared while they watched.
 *
 * SO THE PAGE REMEMBERS ONLY WHAT IT DID ITSELF. `SubmitPrompt` answers with
 * the `TurnId` it minted; that id is kept here, verbatim and unparsed, and the
 * feed marks a row whose turn is one of them. Nothing is derived from the text,
 * the time or the order — an echoed identifier is the only evidence that holds
 * when two clients submit the same words at once.
 *
 * IT IS PAGE-SCOPED AND NOT PERSISTED (R14): a reload has no claim on a turn it
 * did not submit, and remembering across one would state authorship this page
 * cannot vouch for.
 */
import { log } from "../log.js";
import type { TurnId } from "../../../proto/gen/ts/conversation/v1/turn_pb";

/** The turn ids this page submitted, by their verbatim value. */
const own = new Set<string>();

/** Remember a turn `SubmitPrompt` minted for this page. */
export function rememberOwnTurn(turn: TurnId): void {
  own.add(turn.value);
  log.debug(`this page minted turn ${turn.value}`, {
    operation: "composer.own-turn",
    context: { turn: turn.value, held: own.size },
  });
}

/** Whether TURN is one this page submitted. */
export function isOwnTurn(turn: TurnId): boolean {
  return own.has(turn.value);
}

/** Forget every claim. For a harness booting a second page in one document. */
export function forgetOwnTurns(): void {
  own.clear();
}
