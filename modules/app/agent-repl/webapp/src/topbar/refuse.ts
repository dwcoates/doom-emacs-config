/**
 * How a topbar or login control states the refusal its own click provoked.
 *
 * AT THE CALL SITE, NEVER AS PUSHED STATE. A `<Rpc>Error` is the answer to ONE
 * click, so it marks the control that was clicked and disappears the moment the
 * reader clicks again. `data-arm` carries the wire's own arm name.
 *
 * THE FOUR CROSS-CUTTING CAUSES ARE WORDED ONCE, in `src/rpc/refusal.ts`; a
 * verb's own arms are worded by the verb's caller, which is the only place that
 * knows what `no_login_open` means for THAT control. An arm neither of them has
 * a sentence for is a MALFORMED VIEW — a newer daemon added a cause and this
 * build must say so loudly rather than draw a shrug.
 *
 * `transferring_away` ALSO RAISES THE PAGE-WIDE NOTICE. It is the same fact the
 * `transferred` push carries, arriving as an answer instead: this daemon has
 * released the workspace. Drawing it only as a small refusal beside one button
 * would leave the rest of the page looking live while nothing it shows can
 * still be true.
 */
import { refusal, clearRefusals, drawMalformedRefusal } from "../feed/cards/controls.js";
import { workspaceMoved } from "../lifecycle/lifecycle.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { refusalSentence } from "../rpc/refusal.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";

/** How a caller words the arms that are its own verb's alone. */
export type SentenceTable = Record<string, (value: never) => string>;

/** The cause oneof, as protobuf-es hands it over. */
export type Cause = { case?: string | undefined; value?: unknown };

/**
 * State a typed refusal at HOST.
 *
 * Returns the arm drawn, so a caller can log or assert it. Throws MalformedView
 * for an unset cause or an arm this build has no sentence for — the caller
 * catches it through `drawMalformedRefusal`, which is the click-time analog of
 * the feed's malformed-frame path.
 */
export function drawTypedRefusal(
  host: HTMLElement,
  path: string,
  rpcName: string,
  cause: Cause,
  own: SentenceTable,
): string {
  const arm = requireCase(cause, path);
  const say = own[arm.case];
  const text = say !== undefined ? say(arm.value as never) : refusalSentence(rpcName, arm);
  if (text === undefined) {
    // A cause a NEWER daemon added. A generic sentence here would tell the
    // reader their click failed for a reason this build simply did not read.
    return unreachableArm(path, arm.case);
  }
  clearRefusals(host);
  host.append(refusal(arm.case, text));
  if (arm.case === "transferringAway") {
    const address = (arm.value as { address: string }).address;
    log("info", `a refusal says this workspace moved to ${address}`, {
      operation: "topbar.refusal-transferring-away",
      context: { rpc: rpcName, address },
    });
    workspaceMoved(address);
  }
  return arm.case;
}

/** State a transport failure at HOST; `callUnary` already logged it once. */
export function drawTransportRefusal(host: HTMLElement): void {
  clearRefusals(host);
  host.append(refusal("transport", "the daemon could not be reached"));
}

/**
 * Draw a refusal this build could not read, and report it once.
 *
 * Answers whether ERR was a malformed view; anything else is the caller's to
 * rethrow.
 */
export function drawUnreadableRefusal(
  ctx: AppContext,
  host: HTMLElement,
  operation: string,
  err: unknown,
): boolean {
  return drawMalformedRefusal(ctx, host, operation, err);
}
