/**
 * refuse — THE ONE refusal hook. Every `<Rpc>Error` this app draws goes through
 * exactly this module, wherever it was clicked.
 *
 * WHY ONE. Until the wiring pass there were two: a text-only `refusalOf` the
 * feed's cards used, and a drawing `drawTypedRefusal` the topbar, the login
 * overlay and the lifecycle used. They wrote the SAME four cross-cutting causes
 * in two different sets of words, and only one of them raised the page-wide
 * "the workspace moved" notice — so whether the reader learned their workspace
 * had been handed to a successor daemon depended on which button they happened
 * to press. That is the defect a single hook removes.
 *
 * WHAT THE HOOK DOES, once, for every call site:
 *   - REQUIRES the cause oneof. An error naming no reason is a MALFORMED VIEW;
 *     the arm's NAME is what `.refusal[data-arm]` carries and there is no
 *     honest value for it when the wire named none.
 *   - WORDS the arm: the caller's own table first (only the call site knows
 *     what `no_login_open` means for THAT control), then the cross-cutting four
 *     from `refusalSentence` in `./refusal.ts`. An arm neither knows is a
 *     malformed view — a newer daemon added a cause and this build says so
 *     loudly rather than drawing a shrug.
 *   - RAISES THE MOVE NOTICE on `transferring_away`, through `./moved.ts`. It is
 *     the same fact the `transferred` push carries, arriving as an answer
 *     instead: drawing it only as a small sentence beside one button would leave
 *     the rest of the page looking live while nothing it shows can still be true.
 *
 * IT LIVES IN `src/rpc/` because it belongs to the rpc contract, not to any one
 * component, and because a component-owned hook is exactly how the second
 * implementation grew last time.
 */
import { Code, ConnectError } from "@connectrpc/connect";
import { frameUndecodable } from "../failure/sink.js";
import { log } from "../log.js";
import type { AppContext } from "./context.js";
import { isMalformedView } from "./malformed.js";
import { workspaceMoved } from "./moved.js";
import { refusalSentence } from "./refusal.js";
import { requireCase, unreachableArm } from "./strict.js";

/** How a caller words the arms that are its own verb's alone. */
export type SentenceTable = Record<string, (value: never) => string>;

/** The cause oneof, as protobuf-es hands it over. */
export type Cause = { case?: string | undefined; value?: unknown };

/**
 * THE REFUSAL ELEMENT, drawn at the call site.
 *
 * `data-arm` carries the error's own arm name (or `transport`, or `malformed`),
 * never a word this end invented for it.
 */
export function refusal(arm: string, text: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "refusal";
  el.setAttribute("data-arm", arm);
  el.textContent = text;
  return el;
}

/** The one wording for a call that never landed. */
const UNREACHABLE = "the daemon could not be reached";

/**
 * The arm and sentence for a call that THREW, rather than answering.
 *
 * "the daemon could not be reached" IS ONLY TRUE WHEN IT WAS NOT. Grounded
 * 2026-09-13: the owner answered a cold gate, the daemon answered `internal`
 * with the shim's own account of why the start was refused, and the card drew
 * the unreachable line over it — the same lie the malformed-view rule already
 * refuses to tell about an answer this build cannot read. So the three codes
 * that mean the call never landed keep that line, and every other code carries
 * the daemon's OWN message to the control the reader clicked.
 *
 * The arm is `transport` or `failed`, never a cause name: no cause was named.
 */
export function callFailure(err: unknown): { arm: string; text: string } {
  const connect = ConnectError.from(err);
  switch (connect.code) {
    case Code.Unavailable:
    case Code.DeadlineExceeded:
    case Code.Canceled:
      return { arm: "transport", text: UNREACHABLE };
    default: {
      const said = connect.rawMessage.trim();
      return {
        arm: "failed",
        text: said === "" ? "the daemon refused the call and said nothing" : said,
      };
    }
  }
}

/**
 * Drop whatever a previous click left inside HOST.
 *
 * Called before every call rather than after: a stale refusal standing beside a
 * control the user just clicked again reads as an answer to the NEW click.
 */
export function clearRefusals(host: HTMLElement): void {
  for (const stale of host.querySelectorAll(".refusal")) stale.remove();
}

/**
 * The arm and its sentence for one `<Rpc>Error` cause — the whole hook, minus
 * the DOM.
 *
 * PATH is the message tree path of the cause oneof
 * (`"AnswerPermissionError.cause"`), which is both what a `MalformedView`
 * quotes and where the endpoint's name is read from for the cross-cutting
 * wording's own diagnostics.
 *
 * THE MOVE NOTICE IS RAISED HERE, not by the drawing wrapper, so a call site
 * that words its own refusal (a card that puts the sentence inside its body)
 * cannot accidentally opt out of it.
 */
export function refusalOf(
  cause: Cause,
  own: SentenceTable,
  path: string,
): { arm: string; text: string } {
  const arm = requireCase(cause, path);
  const say = own[arm.case];
  const text = say !== undefined ? say(arm.value as never) : crossCuttingSentence(path, arm);
  if (text === undefined) {
    // A cause a NEWER daemon added. A generic sentence here would tell the
    // reader their click failed for a reason this build simply did not read.
    return unreachableArm(path, arm.case);
  }
  return { arm: arm.case, text };
}

/**
 * The sentence for one of the CROSS-CUTTING FOUR, or `undefined` when the arm
 * is the endpoint's own — and the page-wide move notice as a side effect.
 *
 * THIS IS THE HOOK EVERY CALL SITE GOES THROUGH, whether it draws the sentence
 * itself (the sidebar's verbs, the tray, the composer, `link.ts`) or lets
 * `drawTypedRefusal` draw it. Calling `refusalSentence` from `./refusal.ts`
 * directly is what let `transferring_away` be drawn as a small sentence beside
 * one button while the rest of the page went on looking live; this wrapper is
 * the reason no site has to remember to raise the notice itself.
 */
export function crossCuttingSentence(path: string, cause: Cause): string | undefined {
  const arm = requireCase(cause, path);
  const text = refusalSentence(rpcNameOf(path), arm);
  if (text === undefined) return undefined;
  if (arm.case === "transferringAway") raiseMoved(path, arm.value);
  return text;
}

/**
 * State a typed refusal at HOST, clearing whatever stood there before.
 *
 * Returns the arm drawn, so a caller can log or assert it. Throws
 * `MalformedView` for an unset cause or an arm this build has no sentence for;
 * the caller catches it through `drawUnreadableRefusal`, the click-time analog
 * of the feed's malformed-frame path.
 *
 * RPCNAME is the endpoint's name for the log record only — it never appears in
 * the sentence, because the control the refusal sits beside already says which
 * verb was clicked.
 */
export function drawTypedRefusal(
  host: HTMLElement,
  path: string,
  rpcName: string,
  cause: Cause,
  own: SentenceTable,
): string {
  const said = refusalOf(cause, own, path);
  clearRefusals(host);
  host.append(refusal(said.arm, said.text));
  log.debug(`a ${rpcName} click was refused: ${said.arm}`, {
    operation: "rpc.refused",
    context: { rpc: rpcName, arm: said.arm },
  });
  return said.arm;
}

/**
 * State a failed call at HOST; `callUnary` already logged it once.
 *
 * Pass the ERROR wherever the call site has it: the wording then comes from
 * {@link callFailure}, which tells a daemon that could not be reached from one
 * that answered a failure. Without it the unreachable line is the only thing
 * this can say, which is the older, blunter contract and not a second wording.
 */
export function drawTransportRefusal(host: HTMLElement, err?: unknown): void {
  clearRefusals(host);
  const said = err === undefined ? { arm: "transport", text: UNREACHABLE } : callFailure(err);
  host.append(refusal(said.arm, said.text));
}

/**
 * REPORT a refusal this build could not read, exactly once.
 *
 * A `<Rpc>Error` whose cause oneof is unset — or whose arm a newer daemon added
 * — is a malformed view arriving on a CLICK rather than on a draw, so the feed
 * core's own malformed path never sees it. Left to propagate it would become an
 * unhandled rejection inside a click handler: the failure would be real and
 * logged nowhere the user can see. So it is logged at error and reported
 * through the failure sink, which is where an unreadable frame is told.
 *
 * NO REFUSAL IS DRAWN AT THE CONTROL. Since landing 4 every error carries a
 * typed cause, so an unset one is not "a refusal with no words" — it is a frame
 * this build cannot read, and inventing a sentence for it at the control would
 * state a refusal the daemon never made. The topbar's warning chip is the
 * surface for a frame nobody could read, and it names this one.
 *
 * Answers whether ERR was a malformed view; anything else is not this
 * function's to interpret and the caller must rethrow it.
 */
export function drawMalformedRefusal(
  ctx: Pick<AppContext, "failures">,
  /** The control that was clicked. Kept for the call sites' one shape. */
  _host: HTMLElement,
  operation: string,
  err: unknown,
): boolean {
  if (!isMalformedView(err)) return false;
  log.error(`a refusal could not be read: ${err.message}`, {
    operation,
    context: { path: err.path, detail: err.detail },
  });
  ctx.failures.report(frameUndecodable(err.detail, err.path));
  return true;
}

/** The topbar's and login's name for `drawMalformedRefusal`. */
export const drawUnreadableRefusal = drawMalformedRefusal;

/**
 * The endpoint's name, read back off the cause path.
 *
 * `refusalSentence` names the endpoint in the `MalformedView` it throws, and
 * every call site already passes the cause's own path (`"InterruptError.kind"`,
 * `"AnswerPermissionError.cause"`), so the name is derived rather than asked
 * for twice — two spellings of the same fact is how they drift apart.
 */
function rpcNameOf(path: string): string {
  return path.replace(/Error\.[A-Za-z0-9_]+$/, "");
}

/**
 * Raise the page-wide move notice off a `transferring_away` arm.
 *
 * The address is read strictly: `refusalSentence` already refused an arm that
 * carries no string, so reaching here without one would be a contradiction —
 * and a notice naming `undefined` is worse than none.
 */
function raiseMoved(path: string, value: unknown): void {
  const address = (value as { address?: unknown } | undefined)?.address;
  if (typeof address !== "string") return;
  log.info(`a refusal says this workspace moved to ${address}`, {
    operation: "rpc.refusal-transferring-away",
    context: { path, address },
  });
  workspaceMoved(address);
}
