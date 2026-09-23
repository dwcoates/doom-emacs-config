/**
 * hook — a failing hook's card: the tool-card shell, hook-flavored.
 *
 * ONLY FAILURES DRAW. A hook that let its gated action through is not a card at
 * all, so there is no success arm to render and no quiet state to draw: every
 * card of this kind is either a refusal or a hook that itself broke, and the
 * ARM PICKS THE TONE.
 *
 * THE HEADLINE IS COMPOSED DAEMON-SIDE ("hook blocked: protect-master
 * (PreToolUse)") — the client never assembles the hook's name, its event or its
 * verb, and never infers the tone from the words.
 *
 * THE GATED CALL IS A ROW REFERENCE, NOT A COPY. When the firing gated a call,
 * the card carries that call's `FeedId` and draws a "gated:" link that reveals
 * the card already in the feed. It never re-draws the call: two drawings of one
 * fact would be two things to keep in step, and the reader wants to land on the
 * real card with its output.
 */
import type {
  FeedHook,
  FeedId,
  FeedHookBlocked,
  FeedHookFailed,
  FeedHookGatedCall,
  FeedHookHeadline,
  FeedHookOutput,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../../log.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type { RowContext } from "./context.js";
import { foldTitle } from "../title-fold.js";

/**
 * The hook card.
 *
 * `data-state` carries the outcome arm, and the blocked arm additionally wears
 * `.tool-hook-blocked` — the loud treatment — so the refusal reads as a refusal
 * at a glance rather than as one more grey card in the feed.
 */
export function drawFeedHook(u: FeedHook, rc: RowContext): HTMLElement {
  const path = "FeedHook";
  const outcome = requireCase(u.outcome, `${path}.outcome`);
  log.debug("drawing a hook card", {
    operation: "feed.cards.hook",
    context: { outcome: outcome.case },
  });

  const card = document.createElement("div");
  card.className =
    outcome.case === "blocked" ? "tool-card tool-hook tool-hook-blocked" : "tool-card tool-hook";
  card.setAttribute("data-state", outcome.case);

  const head = document.createElement("div");
  head.className = "tool-head";
  // THE HEADLINE IS THE CARD'S TITLE (owner ruling, 2026-09-23): the one
  // two-line title fold. A hook card has no card-level fold, so the headline is
  // its own (title-fold.ts). No draw here stops the card's ticking, so folding
  // it at once keeps its measurer.
  head.appendChild(
    foldTitle(
      drawFeedHookHeadline(requireMessage(u.headline, `${path}.headline`), `${path}.headline`),
      "standalone",
    ),
  );
  card.appendChild(head);

  if (u.gatedCall !== undefined) {
    card.appendChild(drawFeedHookGatedCall(u.gatedCall, rc, `${path}.gated_call`));
  }

  switch (outcome.case) {
    case "blocked":
      card.appendChild(drawFeedHookBlocked(outcome.value, `${path}.blocked`));
      break;
    case "failed": {
      // The chip belongs beside the headline it qualifies; the output belongs
      // below the card's divider, exactly as a tool call's does.
      const failed = drawFeedHookFailed(outcome.value, `${path}.failed`);
      head.appendChild(failed.chip);
      if (failed.output !== null) card.appendChild(failed.output);
      break;
    }
    default: {
      // The narrowed value is `never` here, which is the compile-time half of
      // the guarantee; the run-time half still needs the arm's NAME, and an arm
      // a NEWER daemon set is exactly the case that reaches this line.
      const other: { case: string } = outcome;
      return unreachableArm(`${path}.outcome`, other.case);
    }
  }
  return card;
}

/** The composed head line, verbatim. */
export function drawFeedHookHeadline(u: FeedHookHeadline, path: string): HTMLElement {
  log.debug("drawing a hook headline", {
    operation: "feed.cards.hook.headline",
    context: { path },
  });
  const headline = document.createElement("span");
  headline.className = "tool-name";
  headline.textContent = u.text;
  return headline;
}

/**
 * The "gated:" link to the call this firing gated.
 *
 * THE ROW ID IS ECHOED, NEVER PARSED: the click hands it straight back to the
 * feed's own reveal, which is the one thing that knows how to get there (and
 * which answers `false` for a row it could not reach, e.g. one inside a
 * collapsed bubble — reported at the link rather than swallowed).
 */
export function drawFeedHookGatedCall(
  u: FeedHookGatedCall,
  rc: RowContext,
  path: string,
): HTMLElement {
  const row = requireMessage(u.row, `${path}.row`);
  log.debug("drawing a hook gated-call link", {
    operation: "feed.cards.hook.gated-call",
    context: { path, row: row.value },
  });
  const link = document.createElement("a");
  link.className = "hook-gated";
  link.setAttribute("role", "button");
  link.setAttribute("data-gated-row", row.value);
  link.tabIndex = 0;
  const caret = document.createElement("span");
  caret.className = "hook-gated-caret";
  caret.setAttribute("aria-hidden", "true");
  caret.textContent = "▸";
  link.appendChild(caret);
  link.appendChild(document.createTextNode(" gated:"));
  link.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    event.stopPropagation();
    void reveal(rc, row, link);
  });
  return link;
}

/**
 * Reveal the gated row, saying so at the link when it cannot be reached.
 *
 * THE SERVED ID IS HANDED BACK WHOLE — the message the row carried, not a value
 * pulled out of it and re-wrapped, which would make this a construction site
 * for an identity only the daemon may mint.
 */
async function reveal(rc: RowContext, row: FeedId, link: HTMLElement): Promise<void> {
  link.removeAttribute("data-unreachable");
  const reached = await rc.revealRow(row);
  if (reached) return;
  link.setAttribute("data-unreachable", "true");
  log.warn("the gated call's row could not be revealed", {
    operation: "feed.cards.hook.gated-call-unreachable",
    context: { row: row.value },
  });
}

/**
 * The blocked outcome: the LOUD treatment, with the hook's own stated reason.
 *
 * The reason is the hook's text, verbatim — this end neither summarizes it nor
 * prefixes it with a judgment of its own.
 */
export function drawFeedHookBlocked(u: FeedHookBlocked, path: string): HTMLElement {
  log.debug("drawing a blocked hook reason", {
    operation: "feed.cards.hook.blocked",
    context: { path },
  });
  const reason = document.createElement("div");
  // `.hook-output` is the class the hook card's box is click-to-expand BY
  // (CAPPED_CLASSES in expand.ts). It keeps the hook card's own per-section
  // fold after the tool-call/skill cards moved to a card-level fold — the box
  // still caps, clips and scrolls through its `.tool-output`/`.bash-output`
  // rules exactly as it did before.
  reason.className = "tool-output bash-output hook-reason hook-output";
  reason.textContent = u.reason;
  return reason;
}

/**
 * The failed outcome: the ordinary card, with the exit chip and the output.
 *
 * THE CHIP'S TONE IS THE CODE'S. A zero exit on a hook that is reported as
 * failed is still a fact worth drawing plainly rather than in the error hue —
 * the failure was in the hook's own machinery, and the code says what the
 * process reported.
 */
export function drawFeedHookFailed(
  u: FeedHookFailed,
  path: string,
): { chip: HTMLElement; output: HTMLElement | null } {
  log.debug("drawing a failed hook", {
    operation: "feed.cards.hook.failed",
    context: { path, exit_code: u.exitCode, output: u.output !== undefined },
  });
  const chip = document.createElement("span");
  chip.className = u.exitCode === 0 ? "badge hook-exit" : "badge err hook-exit";
  chip.setAttribute("data-exit-code", String(u.exitCode));
  chip.textContent = `exit ${u.exitCode}`;
  return {
    chip,
    output: u.output === undefined ? null : drawFeedHookOutput(u.output, `${path}.output`),
  };
}

/** The capped output text, verbatim. */
export function drawFeedHookOutput(u: FeedHookOutput, path: string): HTMLElement {
  log.debug("drawing a hook output", {
    operation: "feed.cards.hook.output",
    context: { path },
  });
  const output = document.createElement("pre");
  // `.hook-output`: the hook card keeps its own per-section click-to-expand
  // (CAPPED_CLASSES in expand.ts), unchanged by the card-level fold the
  // tool-call/skill cards adopted.
  output.className = "tool-output bash-output hook-output";
  output.textContent = u.text;
  return output;
}
