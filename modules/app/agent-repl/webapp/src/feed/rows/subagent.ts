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
import { reservedLiveElapsedClock, reservedQuietClock, reservedSettledElapsedClock } from "../../elapsed-clock.js";
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { feedSubagentDotColor } from "../../vocab.js";
import { drawWorkDot } from "../work-dot.js";
import { asHeadClock } from "../cards/tool-head.js";
import type {
  FeedDetachedSubagent,
  FeedSubagent,
  FeedSubagentLastProgress,
  FeedSubagentLost,
  FeedSubagentModel,
  FeedSubagentRuntime,
  FeedSubagentSettled,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawStopControl, type StopSite } from "../stop-control.js";
import { buildInterruptDetachedRequest } from "../requests.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { stopTicking } from "../ticking.js";
import { foldTitle } from "../title-fold.js";
import { drawDetachedWorkId } from "../work-id.js";

const PATH = "FeedSubagent";

/** The word each settled outcome says. */
const SETTLED_WORDS = {
  succeeded: "done",
  failed: "failed",
  cancelled: "stopped",
  lost: "lost sight of",
  restarted: "ended by a restart",
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
  restarted: "badge muted",
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
  processEnded: "process ended",
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

/**
 * The model the subagent's latest response named, small after the label:
 * "Explore (claude-sonnet-5)" (owner request, 2026-10-22). Drawn only once a
 * response has named one; never a guessed model.
 */
export function drawFeedSubagentModel(model: FeedSubagentModel): HTMLElement {
  const el = document.createElement("span");
  el.className = "subagent-model";
  el.textContent = `(${model.name})`;
  return el;
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

  // THE STATE DOT: the shared vocabulary's color for `live` or the settled
  // outcome; hollow when it spends none (work-dot.ts).
  const dotState = state.case === "settled" ? requireCase(state.value.outcome, `${PATH}.settled.outcome`).case : state.case;
  el.append(drawWorkDot(feedSubagentDotColor(dotState), state.case === "live"));

  const label = document.createElement("span");
  label.className = "subagent-label";
  label.textContent = requireMessage(msg.label, `${PATH}.label`).text;
  el.append(label);
  if (msg.model !== undefined) el.append(drawFeedSubagentModel(msg.model));

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
      el.append(drawLiveClock(runtime, rc));
      if (state.value.lastProgress !== undefined) {
        el.append(drawFeedSubagentLastProgress(state.value.lastProgress, rc));
      }
      if (isDetachedRow(rc)) el.append(drawStopControl(rc, SUBAGENT_STOP, requireMessage(rc.row.id, "FeedRow.id")));
      break;
    }
    case "settled": {
      const outcome = requireCase(state.value.outcome, `${PATH}.settled.outcome`);
      el.setAttribute("data-state", outcome.case);
      const word = document.createElement("span");
      word.className = `subagent-outcome ${SETTLED_BADGES[outcome.case]}`;
      word.textContent =
        outcome.case === "lost"
          ? `${SETTLED_WORDS.lost}${lostCauseClause(outcome.value)}`
          : SETTLED_WORDS[outcome.case];
      // THE BADGE COMES BEFORE THE CLOCK (owner ruling, 2026-10-11): the head
      // reads title, outcome, then how long it ran.
      el.append(word, drawSettledClock(runtime, state.value));
      // A CARD'S TIMER STOPS THE MOMENT ITS UNIT SETTLES. The settled clock is
      // frozen at the span the MESSAGE reports, and this element carries no
      // live subscription onward from a terminal draw.
      stopTicking(el);
      break;
    }
    case "settling": {
      // STOPPED, OUTCOME ON ITS WAY (conversation.v1 SessionLiveWork): the
      // clock stops where the run left the vendor's live set, and the head
      // turns settled when the outcome lands.
      el.setAttribute("data-state", "settling");
      const startedMs = msOf(runtime.startedAtMs, `${PATH}.runtime.started_at_ms`);
      const stoppedMs = msOf(state.value.stoppedAtMs, `${PATH}.settling.stopped_at_ms`);
      const word = document.createElement("span");
      word.className = "subagent-outcome badge muted";
      word.textContent = "settling";
      el.append(word, asHeadClock(reservedSettledElapsedClock("subagent-clock", stoppedMs - startedMs)));
      stopTicking(el);
      break;
    }
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
  // THE DETACHED-WORK ID closes the head, drawn verbatim when the daemon set
  // one (a detached bubble); a synchronous spawn names none.
  const workId = drawDetachedWorkId(msg.workId);
  if (workId !== null) el.append(workId);
  // THE DESCRIPTION IS THE BUBBLE'S TITLE (owner ruling, 2026-09-23): the
  // one-line title fold, owned by the bubble's fold (bubble.ts). Folded AFTER a
  // settled draw's stop, which would otherwise tear down the fold's measurer.
  if (description !== null) foldTitle(description, "card", rc);
  return el;
}

/** Whether the row placing this head is the DETACHED wrapper. */
function isDetachedRow(rc: RowContext): boolean {
  return rc.row.row.case === "detachedSubagent";
}

/** The live clock, counting up from the original start. */
function drawLiveClock(runtime: FeedSubagentRuntime, rc: RowContext): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.runtime.started_at_ms`);
  return asHeadClock(reservedLiveElapsedClock(rc.ctx.ticker, "subagent-clock", startedMs));
}

/** The settled clock: the span that ran, stopped where the run stopped. */
function drawSettledClock(
  runtime: FeedSubagentRuntime,
  settled: FeedSubagentSettled,
): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.runtime.started_at_ms`);
  const endedMs = msOf(settled.endedAtMs, `${PATH}.settled.ended_at_ms`);
  return asHeadClock(reservedSettledElapsedClock("subagent-clock", endedMs - startedMs));
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
  return reservedQuietClock(rc.ctx.ticker, "subagent-quiet", atMs);
}

/** The stop a live detached bubble offers: its own row, stopped as detached work. */
const SUBAGENT_STOP: StopSite = {
  prefix: "subagent-stop",
  operation: "feed.subagent-stop",
  refusedOperation: "feed.subagent-stop-refused",
  describe: "stopping a detached subagent",
  malformedOperation: "feed.subagent-stop-malformed",
  request: buildInterruptDetachedRequest,
};
