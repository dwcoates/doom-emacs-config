/**
 * progress — the page-wide "a compaction is running, and this is where it is"
 * signal: the daemon's own composed line, published on every footer push.
 *
 * WHY IT IS ITS OWN MODULE. The compaction line is pushed on `WatchFooter` and
 * read by a FEED row — the cold gate, whose "compact and resume" starts a
 * compaction that can run for a minute with the card's buttons latched inert.
 * The page holds ONE connection and the footer's stream is already carrying the
 * line, so the card must not open a second subscription for the same fact; it
 * reads the one the footer already has. Registering here rather than handing
 * the card a footer handle keeps the feed from reaching into the footer's
 * mount, exactly as `src/rpc/moved.ts` keeps the refusal hook out of the
 * lifecycle.
 *
 * NOTHING HERE COMPOSES ANYTHING. The value is the daemon's `text`, verbatim,
 * or `null` when the pushed view carries no compaction activity at all — and a
 * `null` means the reader is shown nothing, never a placeholder this end
 * invented.
 *
 * There is at most one footer, so there is at most one publisher.
 */
import { log } from "../log.js";

/** The line the last push carried, or null when it carried no compaction. */
let standing: string | null = null;

/** Everyone drawing off the line; today, the cold-gate card while it waits. */
const listeners = new Set<(text: string | null) => void>();

/**
 * The footer pushed a view whose activity is (or is no longer) a compaction.
 *
 * Idempotent: an unchanged line publishes nothing, so a push that moved some
 * other cell does not churn every subscriber.
 */
export function publishCompactionProgress(text: string | null): void {
  if (standing === text) return;
  standing = text;
  log.debug(
    text === null
      ? "the footer's compaction line is gone"
      : `the footer's compaction line is now: ${text}`,
    { operation: "footer.compaction-progress", context: { text: text ?? undefined } },
  );
  // A copy: a listener may unsubscribe itself as it runs.
  for (const fn of [...listeners]) fn(standing);
}

/** The compaction line now standing, or null. */
export function compactionProgress(): string | null {
  return standing;
}

/**
 * Observe the line changing, and be told the current one at once.
 *
 * The immediate call is what lets a card that starts waiting MID-compaction
 * draw the phase already reached instead of sitting blank until the daemon
 * happens to push the next one.
 */
export function onCompactionProgress(fn: (text: string | null) => void): () => void {
  listeners.add(fn);
  fn(standing);
  return () => {
    listeners.delete(fn);
  };
}

/** Drop the standing line and every listener. For the suite's teardown only. */
export function resetCompactionProgress(): void {
  standing = null;
  listeners.clear();
}
