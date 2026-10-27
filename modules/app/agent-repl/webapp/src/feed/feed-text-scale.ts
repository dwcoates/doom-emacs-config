/**
 * feed-text-scale — apply the daemon-owned feed text zoom to the DOM.
 *
 * The daemon owns and persists a single global feed text scale and pushes it on
 * every feed watch (frontend.v1.FeedTextScale). This module is the ONE place
 * that lands that value in the DOM: it writes the `--feed-text-scale` custom
 * property on the document element, which every feed-scroll font-size multiplies
 * into as `calc(<base> * var(--feed-text-scale))` (see styles.css). The scale is
 * therefore applied in exactly one spot and reaches all feed text at once, while
 * nothing outside the feed references the property.
 *
 * The FIRST value never waits for the watch: the daemon serves the entry
 * document with the zoom in force already stamped onto the <html> element's
 * inline `--feed-text-scale` (daemon/internal/server/assets.go), so the first
 * paint is at the saved zoom. The watch's push then rewrites that same inline
 * property, with every later change.
 *
 * THE PAGE ON SCREEN RESCALES AT ONCE; A HIDDEN PAGE IN ITS TURN. Every open
 * workspace's page hears the same push, and they share one main thread, so a
 * nudge that every page applied at once restyled every page in one frame and
 * the page on screen waited behind them (host/restyle-turn.ts). A page the
 * host shows applies the scale the moment it arrives; one it does not show
 * applies it in its restyle turn, or the moment it is shown, whichever is
 * first. A scale the page already shows is not applied again: the root feed
 * and every sub-feed watch push the same value.
 *
 * A SCALE MOVES EVERY TREE'S DRAWN WIDTHS, so whatever drew to them is told
 * (`onFeedTextScaleApplied`) in the same task the scale lands, and redraws in
 * the same layout.
 */
import { log } from "../log.js";
import { perDocument } from "../per-document.js";
import { pagePresence, type Presence } from "../presence.js";
import { pageRestyleTurns, type RestyleTurns } from "../host/restyle-turn.js";
import { createListeners } from "../listeners.js";

/** The custom property every feed font-size multiplies into. */
export const FEED_TEXT_SCALE_PROPERTY = "--feed-text-scale";

/** The operation every scale this page lands is recorded under. */
export const FEED_TEXT_SCALE_APPLIED = "feed.text-scale.applied";

/** What a page's scale needs: its document, whether it is shown, its turns, and a clock. */
export interface FeedTextScaleEnv {
  readonly doc: Document;
  readonly presence: Pick<Presence, "shown" | "embedded" | "onChange">;
  readonly turns: RestyleTurns;
  /** A monotonic clock in ms, for how long a landing took. */
  readonly now: () => number;
}

/** A page's feed text scale. */
export interface FeedTextScale {
  /** Take SCALE, pushed by a feed watch. */
  apply(scale: number): void;
  /** Tell LISTENER each time a scale lands, after it lands; answers the unsubscribe. */
  onApplied(listener: () => void): () => void;
}

/** The page's scale over ENV. */
export function createFeedTextScale(env: FeedTextScaleEnv): FeedTextScale {
  const { doc, presence, turns, now } = env;
  const listeners = createListeners<[]>();
  const shownValue = (): string => doc.documentElement.style.getPropertyValue(FEED_TEXT_SCALE_PROPERTY);
  let pending: number | null = null;

  /** Land SCALE now, and record how long the page took to take it. */
  const land = (scale: number, when: "shown" | "turn" | "on-show"): void => {
    if (shownValue() === String(scale)) return;
    const started = now();
    doc.documentElement.style.setProperty(FEED_TEXT_SCALE_PROPERTY, String(scale));
    listeners.notify();
    // The layout the scale costs, taken here so it is paid in this task (a
    // hidden page's turn) rather than in a frame shared with other pages.
    void doc.documentElement.offsetHeight;
    const context = { scale, when, restyle_ms: Math.round(now() - started) };
    if (when === "turn") {
      log.debug("a hidden page applied the feed text scale in its turn", { operation: FEED_TEXT_SCALE_APPLIED, context });
    } else {
      log.info("applied the feed text scale", { operation: FEED_TEXT_SCALE_APPLIED, context });
    }
  };

  presence.onChange(() => {
    if (pending === null || !presence.shown()) return;
    const scale = pending;
    pending = null;
    land(scale, "on-show");
  });

  return {
    apply(scale) {
      if (!Number.isFinite(scale) || scale <= 0) {
        log.warn("ignored a non-finite or non-positive feed text scale", {
          operation: "feed.text-scale.invalid",
          context: { scale: String(scale) },
        });
        return;
      }
      if (!presence.embedded || presence.shown()) {
        pending = null;
        land(scale, "shown");
        return;
      }
      if (pending === null && shownValue() === String(scale)) return;
      pending = scale;
      turns.request(() => {
        if (pending === null) return;
        const next = pending;
        pending = null;
        land(next, "turn");
      });
    },
    onApplied(listener) {
      return listeners.add(listener);
    },
  };
}

/** The page's own scale: one per document, made on first use. */
const pageFeedTextScale = perDocument(
  (doc): FeedTextScale =>
    createFeedTextScale({ doc, presence: pagePresence(), turns: pageRestyleTurns(doc), now: () => performance.now() }),
);

/** Take SCALE, pushed by one of the page's feed watches. */
export function applyFeedTextScale(scale: number): void {
  pageFeedTextScale(document).apply(scale);
}

/** Tell LISTENER each time this page lands a new scale; answers the unsubscribe. */
export function onFeedTextScaleApplied(doc: Document, listener: () => void): () => void {
  return pageFeedTextScale(doc).onApplied(listener);
}
