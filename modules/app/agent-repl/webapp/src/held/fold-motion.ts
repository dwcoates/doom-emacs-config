/**
 * fold-motion — A FOLD IS SEEN HAPPENING (owner request, 2026-10-15).
 *
 * When a held prompt is folded into the entry ahead of it (the user's "fold
 * above", or the classifier's coalesce), the held-section change that removes
 * it names the fold (WatchFeedHeldChange.folds). Two things show it:
 *
 *   - the entry folded INTO flashes once (`flashFoldedInto`), so the eye finds
 *     where the words went;
 *   - the folded prompt's card, when this client was drawing it, stays up for
 *     a moment wearing a "folding above" badge (`showFolding`), then fades and
 *     is taken down. A folded prompt this client never drew is not drawn now.
 *
 * THE LEAVING CARD ENDS ON A WEB ANIMATION, not a timer and not a CSS
 * `animationend`: an animation the script started runs whatever the
 * stylesheet's motion rules say, so its `finished` always comes, and the
 * unshown-page pause (host/unshown-animations.ts) holds it while the page is
 * hidden, so a fold made while the reader looked away is still seen.
 */
import { log } from "../log.js";
import { stopTicking } from "../feed/ticking.js";
import { HELD_BADGE_CLASS } from "./held-prompt.js";

/** How long a folded card stays up, its badge reading "folding above", before it is gone. */
export const FOLD_LEAVE_MS = 1600;

/** The badge words a folded card wears while it leaves. */
export const FOLDING_BADGE_LABEL = "folding above";

/** The class the leaving card's badge wears beside the shared held badge's. */
export const FOLDING_BADGE_CLASS = "held-folding";

/** The class whose one-shot animation is the flash of the entry folded into. */
export const FOLD_FLASH_CLASS = "held-fold-flash";

/** The stylesheet's keyframes the flash runs (styles.css). */
export const FOLD_FLASH_ANIMATION = "held-fold-flash";

/**
 * Flash CARD once: the entry a held prompt was just folded into, its held item
 * ID. A flash still running is started over, so a second fold in quick
 * succession is seen too.
 */
export function flashFoldedInto(card: HTMLElement, id: string): void {
  log.debug("a held entry flashes: a prompt was folded into it", {
    operation: "held.fold.flash",
    context: { item: id },
  });
  card.classList.remove(FOLD_FLASH_CLASS);
  // Reading layout between the removal and the add restarts the animation.
  void card.offsetWidth;
  card.classList.add(FOLD_FLASH_CLASS);
  const done = (event: AnimationEvent): void => {
    if (event.target !== card || event.animationName !== FOLD_FLASH_ANIMATION) return;
    card.removeEventListener("animationend", done);
    card.classList.remove(FOLD_FLASH_CLASS);
  };
  card.addEventListener("animationend", done);
}

/**
 * Turn CARD, a held prompt's card whose prompt was just folded into the entry
 * ahead of it, into a leaving card: its status badge reads "folding above",
 * its controls take no input, its clocks stop, and it fades over
 * `FOLD_LEAVE_MS`. GONE is called once the fade has run; the caller takes the
 * card down there. ID is the folded item's id. The card's identity as a held
 * item is the section's to retire (section.ts).
 *
 * Every held card draws one status badge (held-prompt.ts), so a card with none
 * is a broken DOM, thrown rather than left leaving with its old status.
 */
export function showFolding(card: HTMLElement, id: string, gone: () => void): void {
  const status = card.querySelector<HTMLElement>(`.${HELD_BADGE_CLASS}`);
  if (status === null) {
    log.error("a folded held card has no status badge to say it is folding", {
      operation: "held.fold.no-badge",
      context: { item: id },
    });
    throw new Error(`held card ${id} has no .${HELD_BADGE_CLASS} to wear "${FOLDING_BADGE_LABEL}"`);
  }
  const badge = document.createElement("span");
  badge.className = `badge ${HELD_BADGE_CLASS} ${FOLDING_BADGE_CLASS}`;
  badge.setAttribute("data-held-status", "folding");
  badge.textContent = FOLDING_BADGE_LABEL;
  status.replaceWith(badge);

  card.setAttribute("inert", "");
  stopTicking(card);
  log.debug("a folded held card shows it is folding above, then leaves", {
    operation: "held.fold.leaving",
    context: { item: id, leave_ms: FOLD_LEAVE_MS },
  });

  const fade = card.animate([{ opacity: 1 }, { opacity: 1, offset: 0.7 }, { opacity: 0 }], {
    duration: FOLD_LEAVE_MS,
    fill: "forwards",
  });
  fade.finished.then(
    () => {
      log.debug("a folded held card left", { operation: "held.fold.left", context: { item: id } });
      gone();
    },
    (cause: unknown) => {
      // A fade is cancelled only when its card is already gone (the section
      // was disposed), so the card is taken down all the same.
      log.debug("a folded held card's fade was cancelled; it leaves at once", {
        operation: "held.fold.left",
        context: { item: id, cause: String(cause) },
      });
      gone();
    },
  );
}
