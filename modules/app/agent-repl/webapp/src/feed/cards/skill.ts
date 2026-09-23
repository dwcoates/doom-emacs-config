/**
 * skill — THE TEAL SKILL CARD: what the agent loaded, and what the document it
 * brought said.
 *
 * TEAL IS THE CARD'S OWN VOCABULARY, not a state color. A skill is a whole
 * conversation of its own rather than one more file read — the same claim the
 * subagent cards make — so it wears the shared async teal (`.tool-card
 * .tool-skill`), and the ARM decides only the badge and what sits under the
 * head.
 *
 * THE DOCUMENT IS THE SUBSTANCE. `running` and `denied` are empty messages
 * because there is nothing yet (or nothing ever) to show: the badge IS the
 * whole statement, and drawing a placeholder box for an absent document would
 * be a broken-looking card standing in for a fact the arm already states.
 *
 * THE ALLOWANCES SENTENCE IS COMPOSED DAEMON-SIDE ("allows: Bash, Write") and
 * absent when the skill declared none. Absence draws NO LINE — never an empty
 * one, and never a synthesized "allows: nothing", which would state something
 * the skill did not.
 *
 * WORK NESTED UNDER A SKILL IS THE FEED CORE'S SLOT, NOT THIS CARD'S. The
 * daemon may place subsequent activity under this row by `parent`; the core
 * creates that row's `[data-nest]` slot as a direct child of the row chrome and
 * fills it. This card therefore draws no nest element of its own — a second one
 * inside the body would never be found by the core's `:scope > [data-nest]`
 * lookup and would sit empty forever beside the real one. What the card DOES
 * own is the register that slot renders in, which the stylesheet keys off the
 * row's `data-unit="skill"` so a skill's nested work keeps the teal card's
 * always-open, shared-cap, scrolled-not-clipped guarantees.
 */
import type {
  FeedSkill,
  FeedSkillAllowances,
  FeedSkillDocument,
  FeedSkillFailed,
  FeedSkillInvocation,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../../log.js";
import { renderMarkdown } from "../../markdown.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { foldTitle } from "../title-fold.js";

const PATH = "FeedSkill";

/** The badge each outcome wears, and the word it says. */
const OUTCOME_BADGES = {
  running: { className: "badge run", text: "loading" },
  loaded: { className: "badge ok", text: "loaded" },
  failed: { className: "badge err", text: "failed" },
  denied: { className: "badge err", text: "denied" },
} as const satisfies Record<string, { className: string; text: string }>;

/** Every outcome this build draws, for the suite to hold to the schema. */
export const SKILL_OUTCOME_ARMS: readonly string[] = Object.keys(OUTCOME_BADGES);

/** The skill card. */
export function drawFeedSkill(u: FeedSkill, _rc: RowContext): HTMLElement {
  const outcome = requireCase(u.outcome, `${PATH}.outcome`);
  log.debug("drawing a skill card", {
    operation: "feed.cards.skill",
    context: { outcome: outcome.case },
  });

  const card = document.createElement("div");
  card.className = "tool-card tool-skill";
  card.setAttribute("data-state", outcome.case);

  const head = document.createElement("div");
  head.className = "tool-head";
  // THE INVOCATION IS THE CARD'S TITLE (owner ruling, 2026-09-23): the one
  // two-line title fold. A LOADED card is a `.tool-fold` (below) and owns it; a
  // card in any other state has no fold of its own, so the title is its own
  // (title-fold.ts). No draw here stops the card's ticking, so folding it at
  // once keeps its measurer.
  head.append(
    foldTitle(
      drawFeedSkillInvocation(requireMessage(u.invocation, `${PATH}.invocation`), `${PATH}.invocation`),
      outcome.case === "loaded" ? "card" : "standalone",
    ),
  );
  card.append(head);

  switch (outcome.case) {
    case "running":
      head.append(badge("running"));
      return card;
    case "loaded": {
      head.append(badge("loaded"));
      const loaded = outcome.value;
      // CARD-LEVEL FOLD (owner ruling, 2026-09-15): the whole card is the
      // toggle (`.tool-fold`, CAPPED_CLASSES in expand.ts), so the SKILL.md
      // document is HIDDEN — no preview — until the reader clicks the card open,
      // then revealed scrolling at 50vh. A SKILL.md is a long document and the
      // reader asked for a skill to run rather than to be read to, so the card
      // starts collapsed (its default: `.tool-fold` without `.expanded`).
      card.classList.add("tool-fold");
      card.append(
        drawFeedSkillDocument(
          requireMessage(loaded.document, `${PATH}.loaded.document`),
          `${PATH}.loaded.document`,
        ),
      );
      if (loaded.allowances !== undefined) {
        card.append(
          drawFeedSkillAllowances(loaded.allowances, `${PATH}.loaded.allowances`),
        );
      }
      return card;
    }
    case "failed":
      head.append(badge("failed"));
      card.append(drawFeedSkillFailed(outcome.value, `${PATH}.failed`));
      return card;
    case "denied":
      head.append(badge("denied"));
      return card;
    default:
      return unreachableArm(`${PATH}.outcome`, armName(outcome));
  }
}

/** The invocation line — the line a user would have typed, drawn verbatim. */
export function drawFeedSkillInvocation(
  u: FeedSkillInvocation,
  path: string,
): HTMLElement {
  log.debug("drawing a skill invocation line", {
    operation: "feed.cards.skill.invocation",
    context: { path },
  });
  const el = document.createElement("span");
  el.className = "tool-name";
  el.textContent = u.text;
  return el;
}

/**
 * The skill's SKILL.md, rendered as the markdown document it is.
 *
 * IT WEARS `.tool-output skill-content`, which is what the card-level fold
 * hides while the card is collapsed and reveals — scrolling at 50vh — once the
 * card is `.expanded` (the `.tool-fold` rules in styles.css). The reader never
 * sees a preview of it on a collapsed card.
 */
export function drawFeedSkillDocument(u: FeedSkillDocument, path: string): HTMLElement {
  log.debug("drawing a skill document", {
    operation: "feed.cards.skill.document",
    context: { path, length: u.markdown.length },
  });
  const el = document.createElement("div");
  el.className = "tool-output skill-content skill-content-md";
  el.innerHTML = renderMarkdown(u.markdown);
  return el;
}

/** The consent line, composed daemon-side, drawn verbatim. */
export function drawFeedSkillAllowances(
  u: FeedSkillAllowances,
  path: string,
): HTMLElement {
  log.debug("drawing a skill's allowances", {
    operation: "feed.cards.skill.allowances",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "skill-allowances";
  el.textContent = u.text;
  return el;
}

/** The failed state's composed reason, drawn verbatim. */
export function drawFeedSkillFailed(u: FeedSkillFailed, path: string): HTMLElement {
  log.debug("drawing a failed skill", {
    operation: "feed.cards.skill.failed",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "skill-failed";
  el.textContent = u.text;
  return el;
}

/** One of the four badges, by arm. */
function badge(arm: keyof typeof OUTCOME_BADGES): HTMLElement {
  const spec = OUTCOME_BADGES[arm];
  const el = document.createElement("span");
  el.className = spec.className;
  el.textContent = spec.text;
  return el;
}
