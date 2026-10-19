/**
 * tabs — THE ONE FEED TAB: the tab strip and the tab every tabbed card in the
 * feed draws, so a tool group's tabs and a question card's tabs are the same
 * element with the same look (owner ruling, 2026-10-11).
 *
 * A TAB IS A CONTROL, so the feed-wide click-to-expand handler treats it as one
 * (CLICK_THROUGH_SELECTOR in expand.ts) and a tab pick never toggles a capped
 * section. It draws an icon glyph, its 1-based index, and, when the card has
 * one, a label after them; the active tab wears `is-active` and
 * `aria-selected`.
 *
 * THE STRIP OWNS NO POSITION. `.feed-tab-strip` is the strip's size and flow;
 * where a strip sits is its card's concern (the tool group lays its tabline
 * over the shown member, a question card draws its tabline in flow).
 *
 * A CARD WITH TABS CLOCKS ON ITS TABLINE. The tabline is the row holding a
 * card's tab strip, and a tabbed card's clock sits on it, after the tabs,
 * rather than on the card's head line; a card with no tabs keeps its clock on
 * its head (`tool-head.ts`). The clock slot is the tabline's, so every tabbed
 * card places its clock the same way.
 */
import { createControl, type Control } from "../control.js";
import { setInlineMarkdown } from "../markdown.js";
import { replaceTicking } from "./ticking.js";

/** The class every feed tab strip wears. */
export const FEED_TAB_STRIP_CLASS = "feed-tab-strip";

/** The class every feed tabline wears: the row holding a strip and its card's clock. */
export const FEED_TABLINE_CLASS = "feed-tabline";

/** The class a tabline's clock slot wears. */
export const FEED_TABLINE_CLOCK_CLASS = "feed-tabline-clock";

/** The class every feed tab wears. */
export const FEED_TAB_CLASS = "feed-group-tab";

/** What one tab draws and does. */
export interface FeedTabSpec {
  /** The glyph in front, drawn muted and hidden from assistive tech. */
  icon: string;
  /** The tab's 1-based position in its strip. */
  index: number;
  /** A label after the index, drawn as inline markdown, when the card has one to name the tab by. */
  label?: string;
  /** Whether this is the tab whose content is shown. */
  active: boolean;
  /** What a click on the tab does. */
  onPick: () => void;
}

/** A tab strip: a tablist wearing EXTRA classes beside the shared one. */
export function drawFeedTabStrip(...extra: string[]): HTMLElement {
  const strip = document.createElement("div");
  strip.classList.add(FEED_TAB_STRIP_CLASS, ...extra);
  strip.setAttribute("role", "tablist");
  return strip;
}

/**
 * A tabline: STRIP, then a slot holding CLOCK after the tabs, or holding
 * nothing when the card states no clock.
 */
export function drawFeedTabline(strip: HTMLElement, clock: HTMLElement | null): HTMLElement {
  const line = document.createElement("div");
  line.className = FEED_TABLINE_CLASS;
  const slot = document.createElement("span");
  slot.className = FEED_TABLINE_CLOCK_CLASS;
  line.append(strip, slot);
  setFeedTablineClock(line, clock);
  return line;
}

/**
 * Put CLOCK in TABLINE's clock slot, or empty the slot for null, stopping the
 * clock it drops.
 */
export function setFeedTablineClock(tabline: HTMLElement, clock: HTMLElement | null): void {
  const slot = tabline.querySelector(`:scope > .${FEED_TABLINE_CLOCK_CLASS}`);
  // Every tabline is drawn by `drawFeedTabline`, which always draws the slot.
  if (slot === null) throw new Error("a feed tabline has no clock slot");
  replaceTicking(slot, clock === null ? [] : [clock]);
}

/** One tab. */
export function drawFeedTab(spec: FeedTabSpec): Control {
  const tab = createControl();
  tab.className = FEED_TAB_CLASS;
  tab.setAttribute("role", "tab");
  tab.setAttribute("aria-selected", spec.active ? "true" : "false");
  tab.classList.toggle("is-active", spec.active);

  const icon = document.createElement("span");
  icon.className = "feed-group-tab-icon";
  icon.setAttribute("aria-hidden", "true");
  icon.textContent = spec.icon;

  const index = document.createElement("span");
  index.className = "feed-group-tab-index";
  index.textContent = String(spec.index);

  tab.append(icon, index);
  if (spec.label !== undefined) {
    const label = document.createElement("span");
    label.className = "feed-group-tab-label";
    setInlineMarkdown(label, spec.label);
    tab.append(label);
  }
  tab.addEventListener("click", spec.onPick);
  return tab;
}
