/**
 * separation — THE ONE DIVIDER RENDERER.
 *
 * feed.proto states this as a structural invariant rather than a preference:
 * ONE subroutine draws EVERY arm — the same rule geometry with the label under
 * it — and an arm selects only its ACCENT COLOR and its label/payload. A per-arm
 * divider renderer is a defect, not a variation. So there is exactly one
 * element-building path below, and the arm feeds it two things: an accent class
 * and a payload element (or none).
 *
 * THE GEOMETRY IS THE EXISTING ONE. The context arms keep the treatment the
 * `/clear` and `/compact` dividers already had — a 4px bar across the central
 * column with a centered muted label beneath it — and the worktree arms are the
 * same bar in BLUE. That is why the rule's own class is shared and only the
 * accent differs.
 *
 * A COMPACTION'S SUMMARY HAS NO FOLD: its bubble is always drawn under the
 * bar (owner ruling, 2026-10-02), in its collapsed bubble form.
 */
import { log } from "../../log.js";
import { markdownSlot } from "../../bubble/body.js";
import { drawBubble } from "../../bubble/draw.js";
import { renderEditorLink } from "../../link.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedRow,
  FeedContextCutCleared,
  FeedContextCutColdRead,
  FeedContextCutCompacted,
  FeedContextCutCompactionFailed,
  FeedContextCutTokens,
  FeedSessionSeparation,
  FeedSessionSeparationLabel,
  FeedWorktreeEntered,
  FeedWorktreeLeft,
  FeedWorktreePath,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";

const PATH = "FeedSessionSeparation";

/** The accent class each arm's rule wears. Colour is the ONLY thing it picks. */
const ACCENTS = {
  cleared: "sep-accent-cleared",
  compacted: "sep-accent-compacted",
  worktreeEntered: "sep-accent-worktree",
  worktreeLeft: "sep-accent-worktree",
  compactionFailed: "sep-accent-compaction-failed",
} as const satisfies Record<string, string>;

/** Every separation arm this build draws, for the suite to hold to the schema. */
export const SEPARATION_ARMS: readonly string[] = Object.keys(ACCENTS);

/**
 * Whether this row is a separation that CUT CONTEXT — and so is where the feed
 * now begins.
 *
 * A compaction or a clear is the session saying that what came before it is no
 * longer the conversation: after a compaction the surviving account is the
 * summary THIS row carries, and after a clear there is no surviving account at
 * all. The daemon stops delivering the rows above such a divider, and a client
 * that already has them on screen drops them, so the two agree on where the
 * feed starts without the daemon having to retire anything.
 *
 * `compactionFailed` cut NOTHING — that is the whole of what it says — and the
 * worktree arms change no context, so neither may hide the conversation behind
 * it.
 */
export function separationBoundsFeed(row: FeedRow): boolean {
  if (row.row.case !== "separation") return false;
  const kind = row.row.value.kind.case;
  return kind === "cleared" || kind === "compacted";
}

/**
 * The divider.
 *
 * `tokens` is set on the CONTEXT arms only — every cut has one, even a clear
 * (which reloads the system prompt, skills and memory, so the after side is
 * small rather than zero) — and unset on the worktree arms, which change no
 * context. Absence draws no figure; the client does no arithmetic on either
 * side, both being already formatted by the daemon.
 */
export function drawFeedSessionSeparation(
  msg: FeedSessionSeparation,
  rc: RowContext,
): HTMLElement {
  const kind = requireCase(msg.kind, `${PATH}.kind`);
  log.info(`drawing a separation row as ${kind.case}`, {
    operation: "feed.draw-separation",
    context: { arm: kind.case, has_tokens: msg.tokens !== undefined },
  });

  const el = document.createElement("div");
  el.className = "separation";
  el.setAttribute("data-arm", kind.case);
  el.setAttribute("data-state", kind.case);

  const rule = document.createElement("div");
  rule.className = `sep-rule ${ACCENTS[kind.case]}`;
  el.append(rule);

  const label = drawFeedSessionSeparationLabel(
    requireMessage(msg.label, `${PATH}.label`),
    msg.tokens,
  );
  el.append(label);

  const payload = ((): HTMLElement | null => {
    switch (kind.case) {
      case "cleared":
        return drawFeedContextCutCleared(kind.value);
      case "compacted":
        return drawFeedContextCutCompacted(kind.value);
      case "worktreeEntered":
        return drawFeedWorktreeEntered(kind.value, rc);
      case "worktreeLeft":
        return drawFeedWorktreeLeft(kind.value, rc);
      case "compactionFailed":
        return drawFeedContextCutCompactionFailed(kind.value);
      default:
        return unreachableArm(`${PATH}.kind`, armName(kind));
    }
  })();
  if (payload !== null) el.append(payload);
  return el;
}

/** The label line, with the size change beside it when the arm carries one. */
export function drawFeedSessionSeparationLabel(
  labelMsg: FeedSessionSeparationLabel,
  tokens: FeedContextCutTokens | undefined,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "sep-label";
  el.textContent = labelMsg.text;
  if (tokens !== undefined) el.append(drawFeedContextCutTokens(tokens));
  return el;
}

/**
 * The size change, as the daemon already formatted BOTH sides of it.
 *
 * No arithmetic and no unit rounding happens here: the wire carries two
 * display strings precisely so two frontends cannot round the same cut
 * differently.
 */
export function drawFeedContextCutTokens(tokens: FeedContextCutTokens): HTMLElement {
  const el = document.createElement("span");
  el.className = "sep-tokens";
  el.textContent = ` ${tokens.beforeText} → ${tokens.afterText}`;
  return el;
}

/**
 * The outright discard has NOTHING to expand — the history is gone and there is
 * no summary standing in its place — so the divider is the whole of it.
 */
export function drawFeedContextCutCleared(_cleared: FeedContextCutCleared): null {
  return null;
}

/**
 * The compaction's surviving account, always shown, plus the cold-read notice
 * when the compaction paid full price for the read it exists to avoid.
 *
 * THERE IS NO FOLD (owner ruling, 2026-10-02): the summary bubble stands under
 * the divider bar as soon as the row is drawn, with no "summary" disclosure
 * and no chevron. It is still the one capped bubble, so it starts in its
 * collapsed bubble form and opens through the shared toggle like any other.
 * The wire's `fold` is still required (an unset one is a `MalformedView`, as
 * for every non-optional message field), but its value no longer selects
 * anything here.
 *
 * The notice is a WARNING ON A COMPACTION THAT HAPPENED, not a failure of it,
 * which is why it sits beside the summary rather than replacing it.
 */
export function drawFeedContextCutCompacted(compacted: FeedContextCutCompacted): HTMLElement {
  const el = document.createElement("div");
  el.className = "sep-compacted";

  const fold = requireMessage(compacted.fold, `${PATH}.compacted.fold`);
  const summary = requireMessage(compacted.summary, `${PATH}.compacted.summary`);
  log.debug("drawing a compaction summary open, ignoring the wire's fold", {
    operation: "feed.separation-summary-drawn",
    context: { wire_folded: fold.folded, cold_read: compacted.coldRead !== undefined },
  });

  // THE SUMMARY IS A RESPONSE BUBBLE, drawn by the one bubble: the response
  // fill and rail, the shared cap, the one expand toggle and has-more, and a
  // tree in it wrapped at its cap. Its border is the divider bar's own color
  // (the compaction variant, styles.css).
  const body = drawBubble({
    role: "response",
    variant: "compaction",
    hooks: ["assistant", "compact-summary", "sep-summary"],
    content: [markdownSlot("compact-summary-prose", summary.markdown)],
    capLines: "feed",
  }).bubble;

  el.append(body);
  if (compacted.coldRead !== undefined) {
    el.append(drawFeedContextCutColdRead(compacted.coldRead));
  }
  return el;
}

/** What the cold read cost, stated rather than alluded to. */
export function drawFeedContextCutColdRead(coldRead: FeedContextCutColdRead): HTMLElement {
  const evidence = requireMessage(coldRead.evidence, `${PATH}.compacted.cold_read.evidence`);
  const el = document.createElement("div");
  el.className = "sep-cold-read";
  el.setAttribute("data-cold-read", "true");
  el.textContent = `read cold: ${evidence.uncachedInputTokens.toString()} uncached input tokens`;
  log.warn("a compaction re-read the whole conversation at the uncached rate", {
    operation: "feed.separation-cold-read",
    context: { uncached_input_tokens: evidence.uncachedInputTokens.toString() },
  });
  return el;
}

/**
 * The compaction that was OFFERED AND DID NOT HAPPEN, drawn in the slot the
 * compacted divider would have taken.
 *
 * Nothing was cut, so there is no size change to draw beside the label (the
 * wire leaves `tokens` unset) and no summary to fold open. The producer's own
 * account of the failure is the only evidence anyone has, so it is STATED
 * verbatim rather than reworded here.
 */
export function drawFeedContextCutCompactionFailed(
  failed: FeedContextCutCompactionFailed,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "sep-compaction-failed";
  el.setAttribute("data-compaction-failed", "true");
  el.textContent = failed.error;
  log.warn("a compaction was offered and did not happen", {
    operation: "feed.separation-compaction-failed",
    context: { error: failed.error },
  });
  return el;
}

/** The tree the session moved INTO: its path as a jump target, and the branch. */
export function drawFeedWorktreeEntered(
  entered: FeedWorktreeEntered,
  rc: RowContext,
): HTMLElement {
  const el = document.createElement("div");
  el.className = "sep-worktree";
  el.append(drawFeedWorktreePath(requireMessage(entered.path, `${PATH}.worktree_entered.path`), rc));
  if (entered.branch !== undefined) {
    const branch = document.createElement("span");
    branch.className = "sep-branch";
    branch.textContent = ` on ${entered.branch.text}`;
    el.append(branch);
  }
  return el;
}

/**
 * What became of the tree the session left.
 *
 * The two arms draw differently because they mean different things to a reader:
 * a KEPT tree is somewhere to go (so its path is the same jump target the
 * entered divider drew), and a REMOVED one may have taken work with it (so the
 * discard line is loud when the vendor stated one, and absent when it did not).
 */
export function drawFeedWorktreeLeft(left: FeedWorktreeLeft, rc: RowContext): HTMLElement {
  const outcome = requireCase(left.outcome, `${PATH}.worktree_left.outcome`);
  const el = document.createElement("div");
  el.className = "sep-worktree";
  el.setAttribute("data-left", outcome.case);
  switch (outcome.case) {
    case "kept":
      el.append(
        drawFeedWorktreePath(
          requireMessage(outcome.value.path, `${PATH}.worktree_left.kept.path`),
          rc,
        ),
      );
      return el;
    case "removed": {
      if (outcome.value.discarded !== undefined) {
        const loud = document.createElement("span");
        loud.className = "sep-discarded";
        loud.textContent = outcome.value.discarded.text;
        el.append(loud);
      }
      return el;
    }
    default:
      return unreachableArm(`${PATH}.worktree_left.outcome`, armName(outcome));
  }
}

/**
 * A worktree path: drawn, and handed to the editor verbatim on click, through
 * THE ONE shared jump-to-file component the plan button and the findings
 * locations use.
 */
export function drawFeedWorktreePath(path: FeedWorktreePath, rc: RowContext): HTMLElement {
  return renderEditorLink(rc.ctx, { text: path.text, path: path.text });
}
