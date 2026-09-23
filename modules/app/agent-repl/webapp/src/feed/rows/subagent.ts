/**
 * subagent — THE BUBBLE HEAD: a subagent's collapsed line, sync and detached
 * alike.
 *
 * THE BUBBLE IS A FEED, and this message is only its head: the rows are never
 * carried here, they are opened as a sub-feed addressed by this row's own
 * `FeedId` (see bubble.ts). The head is what the PARENT feed's one connection
 * paints, which is why a settled bubble from deep history draws completely
 * without ever opening anything.
 *
 * ONE DRAWING FOR BOTH PLACEMENTS. `FeedDetachedSubagent` wraps the SAME
 * `FeedSubagent` a synchronous spawn carries — the wrapper states placement,
 * never a second drawing — so there is one head renderer and the wrapper's own
 * function is a one-line delegation. What the placement DOES change is the stop
 * control: only detached work outlives its turn, so only a LIVE DETACHED bubble
 * offers "stop" (stopping a synchronous spawn is stopping the turn, which is
 * the footer's affordance, not this row's).
 *
 * THE CLOCKS ARE THE CLIENT'S. `started_at_ms` is the ORIGINAL start — detaching
 * does not reset it — so a live head counts up from it and a settled one shows
 * the span to `ended_at_ms` and stops. `last_progress` ticks "quiet for N s"
 * locally: a heartbeat feedback, never a liveness verdict, and absent before the
 * first beat rather than shown as zero.
 *
 * `lost` NEVER READS AS FAILURE. The arm exists because "we stopped being able
 * to see it" is not "it failed", and drawing the two the same way would state
 * something the daemon deliberately refused to state.
 */
import { formatElapsed, formatTickedElapsed } from "../../duration.js";
import { log } from "../../log.js";
import { callUnary } from "../../rpc/unary.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import {
  InterruptResponseSchema,
  type InterruptResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import type {
  FeedDetachedSubagent,
  FeedSubagent,
  FeedSubagentLastProgress,
  FeedSubagentLost,
  FeedSubagentRuntime,
  FeedSubagentSettled,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  interruptErrorSentence,
  logInterruptRefusal,
  type InterruptErrorKind,
} from "../../interrupt-error.js";
import { buildInterruptDetachedRequest } from "../requests.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { stopTicking, tick } from "../ticking.js";
import { foldTitle } from "../title-fold.js";

const PATH = "FeedSubagent";

/** How long a stop's own answer stays on the control before it clears. */
export const INTERRUPT_OUTCOME_MS = 4000;

/** The dot class each settled outcome wears. `lost` is its own, deliberately. */
const SETTLED_DOTS = {
  succeeded: "agent-done",
  failed: "agent-error",
  cancelled: "agent-error",
  lost: "agent-lost",
} as const satisfies Record<string, string>;

/** The word each settled outcome says. */
const SETTLED_WORDS = {
  succeeded: "done",
  failed: "failed",
  cancelled: "stopped",
  lost: "lost sight of",
} as const satisfies Record<string, string>;

/**
 * The badge each settled outcome wears — the SAME `badge` shape and colours the
 * tool-call verdicts draw (tool-call.ts: "done" is `badge ok`, "error" is
 * `badge err`), so a settled subagent reads as consistently as a settled tool
 * call. `done` is green, a failure is red; a stop and a lost sight are NEITHER
 * (a person's stop is not a fault, and losing sight is not failure), so they
 * take the muted badge rather than the error one — the dot already carries each
 * one's distinct signal.
 */
const SETTLED_BADGES = {
  succeeded: "badge ok",
  failed: "badge err",
  cancelled: "badge muted",
  lost: "badge muted",
} as const satisfies Record<string, string>;

/**
 * The cause each `lost` arm names — the sidecar's staleness ruling, said in
 * words rather than left as the bare "lost sight of". Still not a failure: the
 * clause says WHICH lost it was, never that the work went wrong.
 */
const LOST_CAUSE_WORDS = {
  fileVanished: "file vanished",
  wentSilent: "went silent",
  sweptUp: "swept up at boot",
} as const satisfies Record<string, string>;

/** Every lost cause this build words, for the suite to hold to the schema. */
export const SUBAGENT_LOST_CAUSE_ARMS: readonly string[] = Object.keys(LOST_CAUSE_WORDS);

/**
 * The clause the lost cause adds to the outcome word.
 *
 * An UNSET `how` is an older daemon that never ruled, not a malformed row: the
 * outcome then stays the plain word it has always been.
 */
function lostCauseClause(lost: FeedSubagentLost): string {
  const how = lost.how;
  if (how.case === undefined) return "";
  return `: ${LOST_CAUSE_WORDS[how.case]}`;
}

/** Every settled outcome this build draws, for the suite to hold to the schema. */
export const SUBAGENT_SETTLED_ARMS: readonly string[] = Object.keys(SETTLED_WORDS);

/** The detached placement wrapper: the same head, drawn once. */
export function drawFeedDetachedSubagent(
  wrapper: FeedDetachedSubagent,
  rc: RowContext,
): HTMLElement {
  return drawFeedSubagent(
    requireMessage(wrapper.subagent, "FeedDetachedSubagent.subagent"),
    rc,
  );
}

/** The head line. */
export function drawFeedSubagent(msg: FeedSubagent, rc: RowContext): HTMLElement {
  const state = requireCase(msg.state, `${PATH}.state`);
  log.info(`drawing a subagent head as ${state.case}`, {
    operation: "feed.draw-subagent",
    context: { arm: state.case, detached: isDetachedRow(rc) },
  });

  const el = document.createElement("div");
  el.className = "subagent-head";

  const dot = document.createElement("span");
  dot.className = "agent-dot";
  dot.setAttribute("aria-hidden", "true");
  dot.textContent = "●";
  el.append(dot);

  const label = document.createElement("span");
  label.className = "subagent-label";
  label.textContent = requireMessage(msg.label, `${PATH}.label`).text;
  el.append(label);

  let description: HTMLElement | null = null;
  if (msg.description !== undefined) {
    description = document.createElement("span");
    description.className = "subagent-description";
    description.textContent = msg.description.text;
    el.append(description);
  }

  if (msg.tokens !== undefined) {
    const tokens = document.createElement("span");
    // `token-count` is the ONE token hue (yellow for now); `subagent-tokens`
    // sizes it to the head's other figures. Every token figure wears
    // `token-count` so a count reads as a count wherever it is drawn.
    tokens.className = "subagent-tokens token-count";
    tokens.textContent = msg.tokens.text;
    el.append(tokens);
  }

  const runtime = requireMessage(msg.runtime, `${PATH}.runtime`);
  switch (state.case) {
    case "live": {
      el.setAttribute("data-state", "live");
      dot.classList.add("agent-running");
      el.append(drawLiveClock(runtime, rc));
      if (state.value.lastProgress !== undefined) {
        el.append(drawFeedSubagentLastProgress(state.value.lastProgress, rc));
      }
      if (isDetachedRow(rc)) el.append(drawStopControl(rc));
      break;
    }
    case "settled": {
      const outcome = requireCase(state.value.outcome, `${PATH}.settled.outcome`);
      el.setAttribute("data-state", outcome.case);
      dot.classList.add(SETTLED_DOTS[outcome.case]);
      el.append(drawSettledClock(runtime, state.value));
      const word = document.createElement("span");
      word.className = `subagent-outcome ${SETTLED_BADGES[outcome.case]}`;
      word.textContent =
        outcome.case === "lost"
          ? `${SETTLED_WORDS.lost}${lostCauseClause(outcome.value)}`
          : SETTLED_WORDS[outcome.case];
      el.append(word);
      // A CARD'S TIMER STOPS THE MOMENT ITS UNIT SETTLES. The settled clock is
      // frozen at the span the MESSAGE reports, and this element carries no
      // live subscription onward from a terminal draw.
      stopTicking(el);
      break;
    }
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
  // THE DESCRIPTION IS THE BUBBLE'S TITLE (owner ruling, 2026-09-23): the one
  // two-line title fold, owned by the bubble's fold (bubble.ts). Folded AFTER a
  // settled draw's stop, which would otherwise tear down the fold's measurer.
  if (description !== null) foldTitle(description, "card");
  return el;
}

/** Whether the row placing this head is the DETACHED wrapper. */
function isDetachedRow(rc: RowContext): boolean {
  return rc.row.row.case === "detachedSubagent";
}

/** The live clock, counting up from the original start. */
function drawLiveClock(runtime: FeedSubagentRuntime, rc: RowContext): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.runtime.started_at_ms`);
  const el = document.createElement("span");
  el.className = "subagent-clock";
  tick(el, rc.ctx.ticker, (nowMs) => {
    el.textContent = formatTickedElapsed(nowMs - startedMs);
  });
  return el;
}

/** The settled clock: the span that ran, stopped where the run stopped. */
function drawSettledClock(
  runtime: FeedSubagentRuntime,
  settled: FeedSubagentSettled,
): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.runtime.started_at_ms`);
  const endedMs = msOf(settled.endedAtMs, `${PATH}.settled.ended_at_ms`);
  const el = document.createElement("span");
  el.className = "subagent-clock";
  el.textContent = formatElapsed(endedMs - startedMs);
  return el;
}

/**
 * "quiet for N s", ticking from the last beat the daemon observed.
 *
 * A QUIETNESS AFFORDANCE, NOT A VERDICT: the client sets no threshold and
 * decides nothing from it — a silent agent is still live until an arm says
 * otherwise.
 */
export function drawFeedSubagentLastProgress(
  lastProgress: FeedSubagentLastProgress,
  rc: RowContext,
): HTMLElement {
  const atMs = msOf(lastProgress.atMs, `${PATH}.live.last_progress.at_ms`);
  const el = document.createElement("span");
  el.className = "subagent-quiet";
  tick(el, rc.ctx.ticker, (nowMs) => {
    el.textContent = `quiet for ${formatTickedElapsed(nowMs - atMs)}`;
  });
  return el;
}

/**
 * The stop control on a live detached bubble.
 *
 * STOPPING IS ALWAYS AN RPC — never a stream close — and the target arm is this
 * bubble's own row id, exactly as the feed served it. The answer is drawn AT
 * THE CONTROL: every `InterruptSuccess` arm is a legitimate outcome (including
 * `nothing_running`, which is an answer and not a failure), and the
 * `InterruptError` arm is this click's own refusal, so it marks this button
 * rather than appearing anywhere else.
 */
function drawStopControl(rc: RowContext): HTMLElement {
  const wrap = document.createElement("span");
  wrap.className = "subagent-stop";

  const button = document.createElement("button");
  button.type = "button";
  button.className = "subagent-stop-button";
  button.setAttribute("data-interrupt", requireMessage(rc.row.id, "FeedRow.id").value);
  button.textContent = "stop";
  wrap.append(button);

  button.addEventListener("click", () => {
    void stop(rc, wrap);
  });
  return wrap;
}

/** Issue the stop and draw whatever came back beside the control. */
async function stop(rc: RowContext, wrap: HTMLElement): Promise<void> {
  const id = requireMessage(rc.row.id, "FeedRow.id");
  clearOutcome(wrap);
  log.info("stopping a detached subagent", {
    operation: "feed.subagent-stop",
    context: { row: id.value },
  });
  let response: InterruptResponse;
  try {
    response = await callUnary(
      rc.ctx,
      "Interrupt",
      (client) => client.interrupt(buildInterruptDetachedRequest(rc.ctx.workspace, id)),
      InterruptResponseSchema,
    );
  } catch {
    // callUnary already logged the transport failure once, as its owner; what
    // is left is telling the reader their click did not land.
    wrap.append(refusal("transport", "the daemon could not be reached"));
    return;
  }
  const result = requireCase(response.result, "InterruptResponse.result");
  switch (result.case) {
    case "success":
      wrap.append(successNote(result.value, rc));
      return;
    case "error":
      wrap.append(errorNote(result.value));
      return;
    default:
      unreachableArm("InterruptResponse.result", armName(result));
  }
}

/** The stop's own answer, stated by arm and cleared after a few seconds. */
function successNote(
  success: { outcome: { case?: string; value?: unknown } },
  rc: RowContext,
): HTMLElement {
  const outcome = requireCase(success.outcome, "InterruptSuccess.outcome");
  const el = document.createElement("span");
  el.className = "subagent-stop-outcome";
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
 * The refusal, at the control that made the call.
 *
 * EVERY ARM DRAWS, and the wording is the shared one (`interrupt-error.ts`), so
 * a detached stop and the footer's stops read identically about the same fact.
 *
 * NO CONFIRM STEP LIVES HERE. `confirm_required` is the daemon's challenge to
 * the TURN target — interrupting a turn while detached agents are live — and
 * `confirm_agents` is meaningless on a detached target, so a detached stop that
 * receives it has been answered with an arm that cannot apply to it. It draws
 * as the ordinary refusal it is rather than offering a second click that would
 * resend the identical request and be refused again.
 */
function errorNote(error: { kind: { case?: string; value?: unknown } }): HTMLElement {
  const kind = requireCase(error.kind, "InterruptError.kind") as InterruptErrorKind;
  const sentence = interruptErrorSentence(kind, "InterruptError.kind");
  logInterruptRefusal(kind, sentence, "feed.subagent-stop-refused");
  return refusal(kind.case, sentence);
}

/** The shared refusal element every call site draws its own error into. */
function refusal(arm: string, text: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "refusal";
  el.setAttribute("data-arm", arm);
  el.textContent = text;
  return el;
}

/** Drop whatever the previous click left beside the control. */
function clearOutcome(wrap: HTMLElement): void {
  for (const stale of wrap.querySelectorAll(".subagent-stop-outcome, .refusal")) stale.remove();
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
