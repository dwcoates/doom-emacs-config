/**
 * title-fold — THE ONE two-line fold every tool card's TITLE wears.
 *
 * OWNER RULING, 2026-09-23: a tool-call bubble's TITLE is the command being run
 * ("$ cd /some/path && cat some_file.txt"). Collapsed, it is truncated after
 * TWO lines and — only when it actually overflows them — wears the same bottom
 * fade a response bubble wears for "more to expand" (the fade only, never a
 * chevron: owner ruling, 2026-09-23).
 * Expanded, the whole title shows, alongside the card's output and details.
 *
 * Every title site goes through `foldTitle`, and nothing else caps a title: the
 * shell bubble's command (shell.ts), a tool call's input line (tool-call.ts), a
 * skill's invocation (skill.ts), a hook's headline (hook.ts) and a subagent's
 * description (subagent.ts). A grouped run (tool-group.ts) moves those same
 * cards into its tabs, so its titles are theirs. title-fold.test.ts reads every
 * card source and fails on a title site that does not come through here.
 *
 * THE FOLD IS THE CARD'S, NOT THE TITLE'S. This module adds no toggle. A title
 * in a card that has a fold defers to it: a `.tool-fold` card is opened by the
 * feed-wide click (expand.ts), a `.bubble-fold` by its head (bubble.ts). A
 * title in a card with NO fold of its own (a hook card, a skill card that is
 * not loaded) is made its own fold by one more CAPPED_CLASSES entry
 * (`title-fold-standalone`), so the same feed-wide click opens it. Which fold
 * owns a title is `TITLE_FOLD_OPEN_SELECTOR` (bubble-more.ts), read by both the
 * measurer and the stylesheet.
 *
 * THE FADE IS THE RESPONSE BUBBLE'S RULE, generalized to
 * `.title-fold.has-more` (styles.css); the measurer is bubble-more.ts's, which
 * already served the response bubble. So there is one cap, one fade and one
 * measurer, and this module only marks the element and arms
 * the measurement on it.
 *
 * CALL IT LAST. A card whose draw ends terminal calls `stopTicking` on itself,
 * which also tears down every `onDiscard` hook under it — this fold's
 * `ResizeObserver` included. A card therefore folds its title AFTER that stop,
 * as the final step of its draw.
 */
import { log } from "../log.js";
import {
  TITLE_FOLD_CLASS,
  TITLE_FOLD_OPEN_SELECTOR,
  TITLE_FOLD_STANDALONE_CLASS,
  installHasMore,
  refreshHasMore,
} from "./bubble-more.js";

export { TITLE_FOLD_CLASS, TITLE_FOLD_OPEN_SELECTOR, TITLE_FOLD_STANDALONE_CLASS };

/**
 * Who opens a title: the card's own fold (`.tool-fold` or `.bubble-fold`), or
 * the title itself, for a card that has no fold.
 */
export type TitleFoldOwner = "card" | "standalone";

/** The folds a card-owned title can defer to. */
export const CARD_FOLD_SELECTOR = ".tool-fold, .bubble-fold";

/** Titles already reported as orphaned, so a resize storm logs one record. */
const reportedOrphans = new WeakSet<Element>();

/**
 * Mark TITLE as a two-line title fold owned by OWNER, arm the overflow
 * measurement that puts `has-more` on it, and hand it back.
 */
export function foldTitle(title: HTMLElement, owner: TitleFoldOwner): HTMLElement {
  title.classList.add(TITLE_FOLD_CLASS);
  if (owner === "standalone") title.classList.add(TITLE_FOLD_STANDALONE_CLASS);
  log.debug("folding a card title to two lines", {
    operation: "feed.title-fold",
    context: { owner, element: title.className },
  });
  const view = title.ownerDocument.defaultView;
  if (view === null || typeof view.ResizeObserver !== "function") {
    // Without an observer nothing ever measures this title, so an overflowing
    // one would be clipped at two lines with no fade to say so.
    log.error("a card title cannot measure its overflow: the page has no ResizeObserver", {
      operation: "feed.title-fold.unmeasured",
      context: { owner, element: title.className },
    });
    return title;
  }
  installHasMore(title, refreshTitle);
  return title;
}

/**
 * Re-measure every title fold at or under ROOT. The fold owners call it on a
 * toggle, because an expand/collapse is the one moment the owner's state moves
 * and the title's own box may not resize.
 */
export function refreshTitleFolds(root: Element): void {
  const titles = [...root.querySelectorAll<HTMLElement>(`.${TITLE_FOLD_CLASS}`)];
  if (root instanceof HTMLElement && root.classList.contains(TITLE_FOLD_CLASS)) titles.unshift(root);
  for (const title of titles) refreshTitle(title);
}

/**
 * One title's re-measure: `has-more` follows its overflow and its owner's fold,
 * and a card-owned title that sits in no card fold is reported — it would be
 * capped at two lines with nothing on the page able to open it.
 */
function refreshTitle(title: HTMLElement): void {
  checkOwned(title);
  refreshHasMore(title);
}

/** Report a connected, card-owned title that no card fold holds. */
function checkOwned(title: HTMLElement): void {
  if (!title.isConnected || title.classList.contains(TITLE_FOLD_STANDALONE_CLASS)) return;
  if (title.closest(CARD_FOLD_SELECTOR) !== null) return;
  if (reportedOrphans.has(title)) return;
  reportedOrphans.add(title);
  log.error("a card title is folded to two lines but no card fold can open it", {
    operation: "feed.title-fold.orphan",
    context: { element: title.className, open: title.matches(TITLE_FOLD_OPEN_SELECTOR) },
  });
}
