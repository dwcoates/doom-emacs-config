/**
 * stop-control — the "stop" control a feed element offers on work it can stop:
 * a live detached subagent's head, a live detached shell's head, and a live
 * agentic merge tab.
 *
 * STOPPING IS ALWAYS AN RPC — never a stream close — and its target is the
 * element's own row id, exactly as the feed served it; the SITE says which
 * `Interrupt` target arm that row is sent as. The answer is drawn AT THE
 * CONTROL: every `InterruptSuccess` arm is a legitimate outcome (including
 * `nothing_running`, which is an answer and not a failure), and the
 * `InterruptError` arm is this click's own refusal, so it marks this button
 * rather than appearing anywhere else.
 *
 * NO CONFIRM STEP LIVES HERE. `confirm_required` is the daemon's challenge to
 * the TURN target — interrupting a turn while detached agents are live — and
 * neither target here can receive it legitimately. It draws as the ordinary
 * refusal it is rather than offering a second click that would resend the
 * identical request and be refused again.
 *
 * THE BUTTON IS LATCHED INERT WHILE ITS STOP IS IN FLIGHT (`whileInFlight`): a
 * second click would be a second stop. It comes back after a refusal or a
 * click that never landed, which the user may retry; after a landed stop it
 * stays inert until the row's own re-push replaces it.
 *
 * AN ANSWER THIS BUILD CANNOT READ is reported once through the failure sink
 * (`drawMalformedRefusal`) and no word is invented at the control.
 */
import { createControl, type Control } from "../control.js";
import { log } from "../log.js";
import { callUnary } from "../rpc/unary.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import {
  InterruptResponseSchema,
  type InterruptRequest,
  type InterruptResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { interruptErrorSentence, logInterruptRefusal, type InterruptErrorKind } from "../interrupt-error.js";
import { clearRefusals, drawMalformedRefusal, refusal } from "../rpc/refuse.js";
import { release, whileInFlight } from "./cards/controls.js";
import { armName } from "./renderers.js";
import type { RowContext } from "./renderers.js";
import { stopTicking, tick } from "./ticking.js";
import { queryAll } from "../dom.js";

/** How long a stop's own answer stays on the control before it clears. */
export const INTERRUPT_OUTCOME_MS = 4000;

/** One kind of stop control: its class prefix, its log records, and its request. */
export interface StopSite {
  /** The wrapper's class; the button and the outcome add `-button` and `-outcome`. */
  readonly prefix: string;
  /** The operation a click's record carries. */
  readonly operation: string;
  /** The operation a refusal's record carries. */
  readonly refusedOperation: string;
  /** What a click does, as the click's record says it. */
  readonly describe: string;
  /** The operation an unreadable answer's record carries. */
  readonly malformedOperation: string;
  /** The `Interrupt` request that stops TARGET in WORKSPACE. */
  readonly request: (workspace: WorkspaceRef, target: FeedId) => InterruptRequest;
}

/** The stop control for TARGET, as SITE stops it. */
export function drawStopControl(rc: RowContext, site: StopSite, target: FeedId): HTMLElement {
  const wrap = document.createElement("span");
  wrap.className = site.prefix;

  const button = createControl();
  button.className = `${site.prefix}-button`;
  button.setAttribute("data-interrupt", target.value);
  button.textContent = "stop";
  wrap.append(button);

  button.addEventListener("click", () => {
    void stop(rc, site, target, wrap, button);
  });
  return wrap;
}

/** Issue the stop and draw whatever came back beside the control. */
async function stop(rc: RowContext, site: StopSite, target: FeedId, wrap: HTMLElement, button: Control): Promise<void> {
  clearOutcome(wrap, site);
  log.info(site.describe, {
    operation: site.operation,
    context: { row: target.value },
  });
  const answered = await whileInFlight([button], () =>
    callUnary(
      rc.ctx,
      "Interrupt",
      (client) => client.interrupt(site.request(rc.ctx.workspace, target)),
      InterruptResponseSchema,
    ),
  );
  if ("failed" in answered) {
    // callUnary already logged the transport failure once, as its owner; what
    // is left is telling the reader their click did not land.
    wrap.append(refusal("transport", "the daemon could not be reached"));
    return;
  }
  try {
    drawAnswer(answered.value, rc, site, wrap, button);
  } catch (err) {
    // An answer this build cannot read is still a failure the reader owns; it
    // is reported once rather than becoming an unhandled rejection inside a
    // click handler.
    if (!drawMalformedRefusal(rc.ctx, wrap, site.malformedOperation, err)) throw err;
  }
}

/** The stop's answer: a success arm's word, or this click's own refusal. */
function drawAnswer(response: InterruptResponse, rc: RowContext, site: StopSite, wrap: HTMLElement, button: Control): void {
  const result = requireCase(response.result, "InterruptResponse.result");
  switch (result.case) {
    case "success":
      wrap.append(successNote(result.value, rc, site));
      return;
    case "error":
      wrap.append(errorNote(result.value, site));
      release([button]);
      return;
    default:
      unreachableArm("InterruptResponse.result", armName(result));
  }
}

/** The stop's own answer, stated by arm and cleared after a few seconds. */
function successNote(
  success: { outcome: { case?: string; value?: unknown } },
  rc: RowContext,
  site: StopSite,
): HTMLElement {
  const outcome = requireCase(success.outcome, "InterruptSuccess.outcome");
  const el = document.createElement("span");
  el.className = `${site.prefix}-outcome`;
  el.setAttribute("data-outcome", outcome.case);
  switch (outcome.case) {
    case "interruptedTurn":
      el.textContent = "turn interrupted";
      break;
    case "interruptedDetached":
      el.textContent = `stopped ${(outcome.value as { count: bigint }).count.toString()}`;
      break;
    case "nothingRunning":
      el.textContent = "nothing was running";
      break;
    default:
      unreachableArm("InterruptSuccess.outcome", armName(outcome));
  }
  expire(el, rc);
  return el;
}

/**
 * The refusal, at the control that made the call. EVERY ARM DRAWS, and the
 * wording is the shared one (`interrupt-error.ts`), so a stop here and the
 * footer's stops read identically about the same fact.
 */
function errorNote(error: { kind: { case?: string; value?: unknown } }, site: StopSite): HTMLElement {
  const kind = requireCase(error.kind, "InterruptError.kind") as InterruptErrorKind;
  const sentence = interruptErrorSentence(kind, "InterruptError.kind");
  logInterruptRefusal(kind, sentence, site.refusedOperation);
  return refusal(kind.case, sentence);
}

/** Drop whatever the previous click left beside the control. */
function clearOutcome(wrap: HTMLElement, site: StopSite): void {
  for (const stale of queryAll(wrap, `.${site.prefix}-outcome`)) stale.remove();
  clearRefusals(wrap);
}

/**
 * Take EL off screen once the outcome has been on it long enough to read.
 *
 * Through the shared ticker rather than a timer of this module's own: there is
 * exactly one clock on the page, and a component that starts its own is the
 * thing the ticker exists to prevent.
 */
function expire(el: HTMLElement, rc: RowContext): void {
  const deadline = rc.ctx.ticker.now() + INTERRUPT_OUTCOME_MS;
  tick(el, rc.ctx.ticker, (nowMs) => {
    if (nowMs < deadline) return;
    stopTicking(el);
    el.remove();
  });
}
