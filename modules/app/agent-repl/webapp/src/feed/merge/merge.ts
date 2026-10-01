/**
 * merge — THE MERGE BUBBLE'S HEAD: one line for a whole workspace merge.
 *
 * THIS ROW IS THE HEAD ONLY. The bubble is a SUB-FEED addressed by this row's
 * own `FeedId`, opened through exactly the subagent bubble's plumbing
 * (OpenFeed → WatchFeed, collapse → cancel); the tabs and their content are
 * rows of that feed, never fields here. A merge-specific loader would be a
 * defect, and the head is what makes the laziness pay: a settled merge from
 * deep history draws completely without opening anything.
 *
 * THE ARM IS THE STATE OF THE MERGE AS A WHOLE, not of whatever step is
 * running: "what it is doing right now" is the last tab of the sub-feed. So the
 * head badge says merging / merged / failed / abandoned and no more — no counts
 * (R5), no phase word derived from a table the client would own.
 *
 * THE CLOCK IS THE CLIENT'S: the wire ships `started_at_ms` and, on the
 * terminal arms only, `ended_at_ms` — because an end instant means nothing
 * while the run is live. A live head counts up through the shared ticker; a
 * settled one shows the span that ran and stops.
 *
 * THE GLYPH NAME IS VOCABULARY, NOT DATA. `glyph.icon` is a name from the
 * shared render vocabulary, and a name this build has not caught up with draws
 * the generic merge glyph with a warning rather than failing the row: the head
 * is the only thing standing between the reader and a merge they cannot see.
 */
import { liveElapsedClock, settledElapsedClock } from "../../elapsed-clock.js";
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { FEED_MERGE_HEAD_GLYPH } from "../../vocab.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { stopTicking } from "../ticking.js";
import type {
  FeedMerge,
  FeedMergeError,
  FeedMergeGlyph,
  FeedMergeHead,
  FeedMergeLabel,
  FeedMergeRuntime,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";

const PATH = "FeedMerge";

/** The character each vocabulary glyph name draws as. */
const GLYPHS: Readonly<Record<string, string>> = { merge: "⇄" };

/** What the generic (unknown-name) glyph draws as. */
const GENERIC_GLYPH = "⇄";

/** The head line. */
export function drawFeedMerge(msg: FeedMerge, rc: RowContext): HTMLElement {
  const result = requireCase(msg.result, `${PATH}.result`);
  const head = requireMessage(msg.head, `${PATH}.head`);
  log.debug(`drawing a merge head as ${result.case}`, {
    operation: "merge.draw-head",
    context: { arm: result.case },
  });

  const el = document.createElement("div");
  el.className = "merge-head";
  el.append(drawFeedMergeHead(head));

  const runtime = requireMessage(head.runtime, `${PATH}.head.runtime`);
  switch (result.case) {
    case "update":
      el.setAttribute("data-state", "update");
      el.append(drawLiveClock(runtime, rc));
      el.append(badge("merging", "is-live"));
      return el;
    case "success": {
      el.setAttribute("data-state", "success");
      el.append(
        settledClock(runtime, msOf(result.value.endedAtMs, `${PATH}.success.ended_at_ms`)),
      );
      el.append(badge("merged", "is-succeeded"));
      const commit = document.createElement("span");
      commit.className = "merge-commit";
      commit.textContent = result.value.commit;
      el.append(commit);
      // A CARD'S TIMER STOPS THE MOMENT ITS UNIT SETTLES: the clock beside a
      // landed merge is the span the MESSAGE reports, and nothing on this
      // element ticks past it.
      stopTicking(el);
      return el;
    }
    case "error":
      el.append(drawFeedMergeError(result.value, runtime));
      el.setAttribute(
        "data-state",
        requireCase(result.value.reason, `${PATH}.error.reason`).case,
      );
      stopTicking(el);
      return el;
    default:
      return unreachableArm(`${PATH}.result`, armName(result));
  }
}

/** The constant props of the head: the kind glyph and the branch line. */
export function drawFeedMergeHead(head: FeedMergeHead): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-head-props";
  el.append(drawFeedMergeGlyph(requireMessage(head.glyph, `${PATH}.head.glyph`)));
  el.append(drawFeedMergeLabel(requireMessage(head.label, `${PATH}.head.label`)));
  return el;
}

/**
 * The kind glyph.
 *
 * An unknown name is DRAWN GENERICALLY AND LOGGED, never thrown: the name comes
 * from a shared vocabulary both sides evolve, and losing a whole merge row over
 * an icon this build has not learned would trade the substance for decoration.
 */
export function drawFeedMergeGlyph(glyph: FeedMergeGlyph): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-glyph";
  el.setAttribute("aria-hidden", "true");
  el.setAttribute("data-glyph", glyph.icon);
  const known = GLYPHS[glyph.icon];
  if (known === undefined) {
    log.warn(`merge glyph '${glyph.icon}' is not one this build draws; using the generic one`, {
      operation: "merge.unknown-glyph",
      context: { icon: glyph.icon, known: FEED_MERGE_HEAD_GLYPH },
      dedupKey: `merge.glyph:${glyph.icon}`,
    });
    el.textContent = GENERIC_GLYPH;
    return el;
  }
  el.textContent = known;
  return el;
}

/** The branch line, resolved by the daemon, drawn verbatim. */
export function drawFeedMergeLabel(label: FeedMergeLabel): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-label";
  el.textContent = label.text;
  return el;
}

/** The terminal failure arms: why it did not land. */
export function drawFeedMergeError(
  error: FeedMergeError,
  runtime: FeedMergeRuntime,
): HTMLElement {
  const reason = requireCase(error.reason, `${PATH}.error.reason`);
  const el = document.createElement("span");
  el.className = "merge-error";
  el.append(settledClock(runtime, msOf(error.endedAtMs, `${PATH}.error.ended_at_ms`)));
  switch (reason.case) {
    case "failed":
      el.append(badge("failed", "is-failed"), summaryOf(reason.value.summary));
      return el;
    case "abandoned":
      // Landing 7: `abandoned` carries the daemon's resolved sentence too (the
      // cause — a user drop, a closed workspace, a shutdown — otherwise lives
      // only in the daemon's log), and it is drawn exactly as `failed`'s is.
      el.append(badge("abandoned", "is-abandoned"), summaryOf(reason.value.summary));
      return el;
    default:
      return unreachableArm(`${PATH}.error.reason`, armName(reason));
  }
}

/** The daemon's resolved sentence for a settled merge, drawn verbatim. */
function summaryOf(summary: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-summary";
  el.textContent = summary;
  return el;
}

/** The head's badge: the word for the merge as a whole. No counts (R5). */
function badge(word: string, tone: string): HTMLElement {
  const el = document.createElement("span");
  el.className = `merge-badge ${tone}`;
  el.textContent = word;
  return el;
}

/** The live clock, counting up from the enqueue instant. */
function drawLiveClock(runtime: FeedMergeRuntime, rc: RowContext): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.head.runtime.started_at_ms`);
  return liveElapsedClock(rc.ctx.ticker, "merge-clock", startedMs);
}

/** The settled clock: the span that ran, stopped where it stopped. */
function settledClock(runtime: FeedMergeRuntime, endedMs: number): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.head.runtime.started_at_ms`);
  return settledElapsedClock("merge-clock", endedMs - startedMs);
}
