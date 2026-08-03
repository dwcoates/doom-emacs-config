/**
 * Lazy heavy rendering for feed items.
 *
 * THE PROBLEM. Opening a workspace whose agent has produced a lot of output
 * replays the whole history, and the feed builds DOM — markdown parse plus
 * syntax highlight — for EVERY item. The feed then parks at its tail, so
 * nearly all of that work paints nothing anybody looks at, and the cost of
 * opening a session grows with the length of its history. The live path is
 * no better: `FeedRenderer.render` recomputes every entry's HTML on every
 * frame to diff it against the mounted string, so a long history taxes each
 * frame of every streaming turn too.
 *
 * THE ANSWER. An item outside the tail viewport (plus a generous margin)
 * renders a PLACEHOLDER: the item's own text, escaped, at an approximate
 * height, with none of the markdown or highlighting. It UPGRADES to the real
 * render when the reader scrolls near it, which an `IntersectionObserver`
 * decides. The placeholder is derived straight from the conversation item, so
 * producing one never runs the rendering it exists to avoid.
 *
 * WHAT IS NEVER DEFERRED. Two rules, both about not lying to the reader:
 *
 *   - The tail renders in full at boot, synchronously. An observer answers a
 *     frame late at the earliest, and the tail is the one region the reader is
 *     guaranteed to be looking at — deferring it would flash a placeholder on
 *     every open.
 *   - Anything appended during a live turn renders in full, immediately. New
 *     output arrives AT the tail, which is where the reader is; the deferral
 *     is a property of replayed history alone.
 *
 * Interactive and cheap item kinds (`isHeavyItem`) are never deferred either:
 * a permission prompt's buttons are the item, and a result chip or a `/clear`
 * divider costs less to render than to approximate.
 *
 * SEARCH. The feed search (search.ts) re-derives its marks from the feed's DOM
 * text after every render, so a placeholder that dropped its item's text would
 * silently under-report matches — the one failure that feature must not have.
 * It does not drop it: the placeholder CARRIES the item's text. Beyond that,
 * starting a search upgrades the whole feed (`FeedRenderer.upgradeAll`), so
 * every match is located in exactly the DOM the search would have walked
 * before this module existed.
 */
import { escapeHtml } from "./highlight.js";
import { ConversationItem } from "./store.js";
import { userTurnText } from "./turn.js";
import { logVerbose } from "./wslog.js";

/** Class on the wrapper of an item rendered as a placeholder. */
export const PLACEHOLDER_CLASS = "feed-placeholder";

/**
 * Class on a `.feed-item` whose content is a placeholder. Carried on the
 * ITEM rather than only the inner wrapper so the stylesheet can size the
 * skipped box (`contain-intrinsic-size`) without reaching inside it.
 */
export const DEFERRED_CLASS = "feed-deferred";

/** Custom property holding a deferred item's estimated height. */
export const HEIGHT_VAR = "--lazy-h";

/**
 * How far outside the viewport an item upgrades. Deliberately generous: an
 * upgrade changes an item's height, and doing that inside the viewport would
 * shove the text the reader is on. A margin this wide lands the upgrade —
 * and the reflow — while the item is still off screen.
 */
export const UPGRADE_MARGIN_PX = 1500;

/**
 * Longest placeholder text actually written into the DOM. The HEIGHT is
 * still estimated from the untruncated text, so the box does not shrink;
 * this caps only the escape-and-insert cost of a pathologically long
 * item, which is the very thing being deferred.
 */
export const PLACEHOLDER_TEXT_CAP = 4000;

/** Characters a placeholder line is assumed to hold, for the height estimate. */
export const PLACEHOLDER_CHARS_PER_LINE = 90;

/** Height of one placeholder line, in px, matching `.feed-placeholder`. */
export const PLACEHOLDER_LINE_PX = 20;

/** Chrome (padding, bubble border) added to every placeholder estimate. */
export const PLACEHOLDER_CHROME_PX = 24;

/**
 * Ceiling on an estimate, so one enormous item cannot claim most of the
 * scroll range on a guess. There is no matching floor: every text counts as
 * at least one line, so a line plus the chrome IS the floor.
 */
export const PLACEHOLDER_MAX_PX = 1200;

/**
 * Whether ITEM is worth deferring: its render runs markdown, syntax
 * highlighting, or a whole tool card, and it carries no controls the reader
 * could need before scrolling to it.
 *
 * A permission prompt is excluded because its buttons ARE the item — a
 * placeholder would be a decision the reader cannot make. The dividers,
 * chips, notes and failure cards are excluded because they are fixed-shape
 * markup already cheaper than the estimate that would stand in for them.
 * A merge-origin user turn renders one constant card, so it is excluded too.
 */
export function isHeavyItem(item: ConversationItem): boolean {
  switch (item.kind) {
    case "text":
    case "thinking":
    case "tool":
      return true;
    case "user-turn":
      return item.origin !== "merge";
    default:
      return false;
  }
}

/**
 * ITEM's text, for a placeholder to show and for its height to be estimated
 * from. Read straight off the conversation item — never off rendered HTML,
 * which would mean doing the work this module exists to defer.
 *
 * A tool call has no prose, so its call line stands in: the tool's name and
 * its raw input, which is what the reader scrolling past a collapsed card
 * sees anyway, and what a search over a deferred card must still find.
 */
export function itemPlainText(item: ConversationItem): string {
  switch (item.kind) {
    case "text":
    case "thinking":
      return item.text;
    case "user-turn":
      return userTurnText(item);
    case "tool":
      return `${item.toolName} ${item.inputJson}`;
    default:
      return "";
  }
}

/**
 * About how tall TEXT will render, in px. An estimate, and only ever an
 * estimate: it sizes the skipped box so the scrollbar is roughly honest and
 * the upgrade's reflow is small. Capped so one enormous tool result cannot
 * claim most of the scroll range on a guess.
 */
export function estimateHeightPx(text: string): number {
  const lines = text
    .split("\n")
    .reduce((n, line) => n + Math.max(1, Math.ceil(line.length / PLACEHOLDER_CHARS_PER_LINE)), 0);
  const raw = lines * PLACEHOLDER_LINE_PX + PLACEHOLDER_CHROME_PX;
  return Math.min(PLACEHOLDER_MAX_PX, raw);
}

/**
 * ITEM's placeholder markup: its own text, escaped, and nothing else. No
 * markdown, no highlighting, no tool card — which is the entire point.
 *
 * The text is present in full up to `PLACEHOLDER_TEXT_CAP` so the feed search
 * still finds it (search.ts walks item DOM text), and the box is sized from
 * the untruncated text so the cap never shortens the item.
 */
export function placeholderHtml(item: ConversationItem): string {
  const text = itemPlainText(item);
  const shown = text.length > PLACEHOLDER_TEXT_CAP ? text.slice(0, PLACEHOLDER_TEXT_CAP) : text;
  return `<div class="${PLACEHOLDER_CLASS}">${escapeHtml(shown)}</div>`;
}

/**
 * Whether this environment can decide upgrades at all.
 *
 * Without `IntersectionObserver` there is no signal saying an item came near,
 * so a deferred item would never upgrade and its placeholder would be
 * permanent. The renderer therefore defers NOTHING here and renders exactly
 * as it did before this module — a narrower behavior, never a broken one.
 */
export function canDeferItems(): boolean {
  return typeof IntersectionObserver !== "undefined";
}

/**
 * Watches deferred feed items and reports each one that comes within
 * `UPGRADE_MARGIN_PX` of the viewport, by the `data-key` on its element.
 *
 * One callback per observer batch, not per element: the observer already
 * groups the elements that crossed together, and the renderer answers an
 * upgrade with a full re-render, which must happen once for the batch.
 */
export class LazyUpgrader {
  private observer: IntersectionObserver | null = null;
  private onNear: (keys: string[]) => void;

  constructor(root: HTMLElement, onNear: (keys: string[]) => void) {
    this.onNear = onNear;
    if (!canDeferItems()) {
      logVerbose("info", "lazy: no IntersectionObserver, deferral disabled", {
        operation: "render.lazy-unavailable",
      });
      return;
    }
    this.observer = new IntersectionObserver((entries) => this.handle(entries), {
      // The feed scrolls inside its own container, so the margin is quoted
      // against THAT box rather than the document viewport.
      root,
      rootMargin: `${UPGRADE_MARGIN_PX}px 0px`,
    });
  }

  /** Report the near ones, having stopped watching each (upgrade is final). */
  private handle(entries: readonly IntersectionObserverEntry[]): void {
    const keys: string[] = [];
    for (const entry of entries) {
      if (!entry.isIntersecting) continue;
      const el = entry.target as HTMLElement;
      this.observer?.unobserve(el);
      const key = el.dataset.key;
      if (key !== undefined && key !== "") keys.push(key);
    }
    if (keys.length === 0) return;
    logVerbose("info", `lazy: upgrading ${keys.length} feed item(s)`, {
      operation: "render.lazy-upgrade",
      context: { keys },
    });
    this.onNear(keys);
  }

  /** Watch EL, whose `data-key` names the deferred entry it holds. */
  watch(el: HTMLElement): void {
    this.observer?.observe(el);
  }

  /** Stop watching everything — a rebuild discards the elements wholesale. */
  reset(): void {
    this.observer?.disconnect();
  }
}
