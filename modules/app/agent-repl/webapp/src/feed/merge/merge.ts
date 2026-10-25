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
 * head badge says waiting / merging / merged / failed / abandoned and no more —
 * no counts (R5), no phase word derived from a table the client would own. A
 * live merge's word is the daemon's stage (`frontend.v1.FeedMergeUpdate.stage`):
 * `waiting` while it waits in line, `merging` once it is worked on.
 *
 * A PAUSED QUEUE SHOWS ON THE HEAD (owner request, 2026-10-23): while the
 * live arm carries `frontend.v1.FeedMergeUpdate.paused`, a "paused" pill in the
 * paused amber follows the stage badge and the head is marked `data-paused`,
 * which the stylesheet tints the whole bubble by, folded and expanded alike.
 *
 * THE CLOCK CLOSES THE HEAD'S RIGHT END, after the landed commit and the
 * switch (`HEAD_CLOCK_CLASS`, owner request 2026-10-18), at the badge's size.
 *
 * THE CLOCK IS THE CLIENT'S: the wire ships `started_at_ms` (the enqueue
 * instant while queued, the admission instant once popped, as the daemon
 * resolves it) and, on the terminal arms only, `ended_at_ms` — because an
 * end instant means nothing while the run is live. A live head counts up through the shared ticker; a
 * settled one shows the span that ran and stops.
 *
 * THE GLYPH NAME IS VOCABULARY, NOT DATA. `glyph.icon` is a name from the
 * shared render vocabulary, and a name this build has not caught up with draws
 * the generic merge glyph with a warning rather than failing the row: the head
 * is the only thing standing between the reader and a merge they cannot see.
 */
import { reservedLiveElapsedClock, reservedSettledElapsedClock } from "../../elapsed-clock.js";
import { renderMergeCommitLink } from "../../link.js";
import { log } from "../../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { FEED_MERGE_HEAD_GLYPH } from "../../vocab.js";
import { MERGE_GLYPH, MERGE_GLYPH_NAMES } from "../../merge-glyph.js";
import { asHeadClock } from "../cards/tool-head.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { stopTicking } from "../ticking.js";
import { drawPausedMark } from "./paused.js";
import { drawFeedMergeVersion } from "./version.js";
import { drawMergeWorkspaceSwitch, switchFacts } from "./workspace-switch.js";
import type {
  FeedMerge,
  FeedMergeError,
  FeedMergeGlyph,
  FeedMergeHead,
  FeedMergeLabel,
  FeedMergeRuntime,
  FeedMergeSuccess,
  FeedMergeUpdate,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";

const PATH = "FeedMerge";

/** A live merge's badge word while it waits in line. */
export const WAITING_STAGE = "waiting";

/** The pill a live merge on a paused queue draws beside its badge. */
export const PAUSED_WORD = "paused";


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
  // THE HEAD'S RIGHT END: the landed commit, the workspace switch, then the
  // clock. One box, pushed right by the stylesheet, so they never split the
  // line's free space between them. Each arm closes the head with it, so the
  // drawn order is the order read.
  const end = document.createElement("span");
  end.className = "merge-head-end";
  // A DETACHED MERGE'S BUBBLE CARRIES ITS SWITCH for its whole life: queued,
  // running and settled alike (workspace-switch.ts). Only the async mode has
  // one; a head with no mode was recorded before heads carried theirs.
  const mode = head.mode?.mode;
  if (mode?.case === "async" && mode.value.workspaceSwitch !== undefined) {
    end.append(
      drawMergeWorkspaceSwitch(
        switchFacts(mode.value.workspaceSwitch, `${PATH}.head.mode.async.workspace_switch`),
        rc,
      ),
    );
  }

  const runtime = requireMessage(head.runtime, `${PATH}.head.runtime`);
  switch (result.case) {
    case "update":
      el.setAttribute("data-state", "update");
      // THE BADGE COMES BEFORE THE CLOCK (owner ruling, 2026-10-11).
      el.append(badge(liveWord(result.value), "is-live"));
      if (result.value.paused !== undefined) {
        el.setAttribute("data-paused", "");
        el.append(drawPausedMark("merge-badge is-paused", PAUSED_WORD));
      }
      end.append(drawLiveClock(runtime, rc));
      el.append(end);
      return el;
    case "success": {
      el.setAttribute("data-state", "success");
      el.append(badge("merged", "is-succeeded"));
      end.append(settledClock(runtime, msOf(result.value.endedAtMs, `${PATH}.success.ended_at_ms`)));
      // THE VERSION A MERGE ONTO AGENT-REPL'S MASTER LANDED AS, when the
      // daemon resolved one (version.ts).
      if (head.version !== undefined) {
        el.append(drawFeedMergeVersion(head.version));
      }
      // THE COMMIT SHOWS ONLY WHILE THE BUBBLE IS EXPANDED (the stylesheet
      // hides it on a folded bubble's head), and opens its branch's log.
      end.prepend(drawFeedMergeCommit(result.value, rc));
      el.append(end);
      // A CARD'S TIMER STOPS THE MOMENT ITS UNIT SETTLES: the clock beside a
      // landed merge is the span the MESSAGE reports, and nothing on this
      // element ticks past it.
      stopTicking(el);
      return el;
    }
    case "error":
      el.append(drawFeedMergeError(result.value));
      end.append(settledClock(runtime, msOf(result.value.endedAtMs, `${PATH}.error.ended_at_ms`)));
      el.setAttribute(
        "data-state",
        requireCase(result.value.reason, `${PATH}.error.reason`).case,
      );
      el.append(end);
      stopTicking(el);
      return el;
    default:
      return unreachableArm(`${PATH}.result`, armName(result));
  }
}

/**
 * The landed commit: a link to its branch's log when the head says where it
 * landed. A head recorded before heads carried `landed_on` draws the commit as
 * plain text, since there is no log to name.
 */
export function drawFeedMergeCommit(success: FeedMergeSuccess, rc: RowContext): HTMLElement {
  if (success.landedOn !== undefined) {
    return renderMergeCommitLink(rc.ctx, success.commit, success.landedOn);
  }
  return drawMergeCommitText(success.commit);
}

/** A commit drawn as plain text, in the merge bubble's commit face. */
export function drawMergeCommitText(commit: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "merge-commit";
  el.textContent = commit;
  return el;
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
  const known = MERGE_GLYPH_NAMES[glyph.icon];
  if (known === undefined) {
    log.warn(`merge glyph '${glyph.icon}' is not one this build draws; using the generic one`, {
      operation: "merge.unknown-glyph",
      context: { icon: glyph.icon, known: FEED_MERGE_HEAD_GLYPH },
      dedupKey: `merge.glyph:${glyph.icon}`,
    });
    el.textContent = MERGE_GLYPH;
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

/**
 * The terminal failure arms: why it did not land. This draws the bubble's
 * HEAD, always visible whether folded or expanded, so it carries only the
 * badge — the daemon's full resolved sentence is long and belongs in the
 * expanded body alone (each tab's own outcome summary, drawSummary in
 * tab-set.ts/tab-row.ts/tab-panels.ts), never duplicated in the collapsed row.
 */
export function drawFeedMergeError(error: FeedMergeError): HTMLElement {
  const reason = requireCase(error.reason, `${PATH}.error.reason`);
  const el = document.createElement("span");
  el.className = "merge-error";
  switch (reason.case) {
    case "failed":
      el.append(badge("failed", "is-failed"));
      return el;
    case "abandoned":
      el.append(badge("abandoned", "is-abandoned"));
      return el;
    default:
      return unreachableArm(`${PATH}.error.reason`, armName(reason));
  }
}

/**
 * A live merge's badge word: where the daemon says it stands in its queue.
 * The stage is required, so a live head the daemon sent without one is a
 * malformed view rather than a guessed word.
 */
export function liveWord(update: FeedMergeUpdate): string {
  const stage = requireCase(update.stage, `${PATH}.update.stage`);
  switch (stage.case) {
    case "waiting":
      return WAITING_STAGE;
    case "merging":
      return "merging";
    default:
      return unreachableArm(`${PATH}.update.stage`, armName(stage));
  }
}

/** The head's badge: the word for the merge as a whole. No counts (R5). */
function badge(word: string, tone: string): HTMLElement {
  const el = document.createElement("span");
  el.className = `merge-badge ${tone}`;
  el.textContent = word;
  return el;
}

/**
 * The live clock, counting up from the daemon's resolved origin: the enqueue
 * instant while the merge waits, its admission once it is being worked on.
 */
function drawLiveClock(runtime: FeedMergeRuntime, rc: RowContext): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.head.runtime.started_at_ms`);
  return asHeadClock(reservedLiveElapsedClock(rc.ctx.ticker, "merge-clock", startedMs));
}

/** The settled clock: the span that ran, stopped where it stopped. */
function settledClock(runtime: FeedMergeRuntime, endedMs: number): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.head.runtime.started_at_ms`);
  return asHeadClock(reservedSettledElapsedClock("merge-clock", endedMs - startedMs));
}
