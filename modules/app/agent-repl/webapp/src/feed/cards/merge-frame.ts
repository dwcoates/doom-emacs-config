/**
 * merge-frame — THE MERGE BUBBLE'S FRAME WITHOUT ITS TAB STRIP: the head line a
 * card drawn in a deploy mediator's feed shares with a merge bubble (glyph,
 * label, the card's own props, badge, clock), over the card's own body.
 *
 * Every such card (a fix at its gate, a deploy's hot reload) is drawn through
 * here, so their heads read alike and as a merge head does: a live card ticks
 * its clock from `live.started_at_ms`, a settled one shows the span that ran,
 * stops ticking, and a failed one carries the daemon's one-line account.
 */
import { msOf, requireCase, unreachableArm } from "../../rpc/strict.js";
import { MERGE_GLYPH } from "../../merge-glyph.js";
import { liveMergeClock, mergeBadge, settledMergeClock } from "../merge/merge.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { stopTicking } from "../ticking.js";
import type { FeedMergeTabLive, FeedMergeTabSettled } from "../../../../proto/gen/ts/frontend/v1/feed_pb";

/** A framed card's state: the oneof every framed card's badge reads. */
export type MergeFrameState =
  | { case: "live"; value: FeedMergeTabLive }
  | { case: "settled"; value: FeedMergeTabSettled }
  | { case: undefined; value?: undefined };

/** The badge words a framed card says in each state. */
export interface MergeFrameWords {
  readonly live: string;
  readonly succeeded: string;
  readonly failed: string;
}

/** What a framed card's head draws. */
export interface MergeFrameHead {
  /** The card's kind, its class beside `tool-card` ("fix-gate"). */
  readonly kind: string;
  /** The daemon's label, drawn verbatim. */
  readonly label: string;
  /** The card's own props, drawn after the label. */
  readonly props: readonly HTMLElement[];
  readonly words: MergeFrameWords;
}

/**
 * A framed card: `tool-card <kind>`, stamped with its state as `data-state`,
 * holding its head line and nothing else yet. PATH names the state's oneof
 * in a malformed-row refusal.
 */
export function drawMergeFrame(head: MergeFrameHead, state: MergeFrameState, rc: RowContext, path: string): HTMLElement {
  const st = requireCase(state, path);
  const card = document.createElement("div");
  card.className = `tool-card ${head.kind}`;
  const line = document.createElement("div");
  line.className = `merge-head ${head.kind}-head`;
  line.append(drawProps(head));
  const end = document.createElement("span");
  end.className = "merge-head-end";

  switch (st.case) {
    case "live": {
      card.setAttribute("data-state", "live");
      line.append(mergeBadge(head.words.live, "is-live"));
      end.append(liveMergeClock(msOf(st.value.startedAtMs, `${path}.live.started_at_ms`), rc));
      break;
    }
    case "settled": {
      const outcome = requireCase(st.value.outcome, `${path}.settled.outcome`);
      const startedMs = msOf(st.value.startedAtMs, `${path}.settled.started_at_ms`);
      const endedMs = msOf(st.value.endedAtMs, `${path}.settled.ended_at_ms`);
      switch (outcome.case) {
        case "succeeded":
          card.setAttribute("data-state", "succeeded");
          line.append(mergeBadge(head.words.succeeded, "is-succeeded"));
          break;
        case "failed":
          card.setAttribute("data-state", "failed");
          line.append(mergeBadge(head.words.failed, "is-failed"));
          if (outcome.value.summary !== "") line.append(failureSummary(head.kind, outcome.value.summary));
          break;
        default:
          return unreachableArm(`${path}.settled.outcome`, armName(outcome));
      }
      end.append(settledMergeClock(endedMs - startedMs));
      stopTicking(card);
      break;
    }
    default:
      return unreachableArm(path, armName(st));
  }
  line.append(end);
  card.append(line);
  return card;
}

/** The head's glyph, label and the card's own props. */
function drawProps(head: MergeFrameHead): HTMLElement {
  const props = document.createElement("span");
  props.className = "merge-head-props";
  const glyph = document.createElement("span");
  glyph.className = "merge-glyph";
  glyph.textContent = MERGE_GLYPH;
  const label = document.createElement("span");
  label.className = "merge-label";
  label.textContent = head.label;
  props.append(glyph, label, ...head.props);
  return props;
}

/** A failed card's one-line account, after its badge. */
function failureSummary(kind: string, text: string): HTMLElement {
  const el = document.createElement("span");
  el.className = `${kind}-summary`;
  el.textContent = text;
  return el;
}
