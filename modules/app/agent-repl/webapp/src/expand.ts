/**
 * Click-to-expand for the feed's capped sections.
 *
 * Every N-line preview in the feed (Read previews, Bash command/output,
 * diffs, tool input/output) is a height-capped box whose overflow is
 * reachable only by scrolling it (scroll.ts). Scrolling is the way to
 * READ past the cap in place; expanding is the way to be RID of the cap:
 * a click anywhere on a section lays it out at full length, and a second
 * click restores the capped preview.
 *
 * Expansion is one class on the section (EXPANDED_CLASS), which the
 * stylesheet turns into `max-height: none`. FeedRenderer re-applies it
 * across an item's re-render (expandedKeys + applyExpanded), so a
 * section the user opened stays open when its tool card later changes.
 *
 * installClickExpand is the only DOM-facing piece; every decision it
 * makes lives in the pure helpers above it.
 */
import { ancestorMatching } from "./dom.js";

/**
 * Classes that mark a height-capped section. One entry per cap rule in
 * styles.css — a section is expandable exactly when the stylesheet caps
 * it, and styles.test.ts holds the two lists to each other.
 */
export const CAPPED_CLASSES = [
  "tool-input",
  "tool-output",
  "tool-read-output",
  "bash-input",
  "bash-output",
  "diff-output",
  "skill-input",
  "skill-content",
] as const;

/** Selector matching every capped section (an element may carry several). */
export const CAPPED_SELECTOR = CAPPED_CLASSES.map((c) => `.${c}`).join(", ");

/** The class that lifts a capped section's height cap. */
export const EXPANDED_CLASS = "expanded";

/**
 * Class of the body a subagent card's activity panel drops (its child
 * items). A capped section inside one belongs to a NESTED child, not to
 * the card itself — the distinction `ownsSection` draws.
 */
export const PANEL_CLASS = "agent-panel";

/** Controls that own their own click, so a click on one never toggles. */
export const CLICK_THROUGH_SELECTOR = "a, button, summary";

/** The class membership test a section is recognized by. */
export interface ClassTest {
  contains(name: string): boolean;
}

/** The classes an expandable section carries, as the toggle drives them. */
export interface Classes extends ClassTest {
  add(name: string): void;
  remove(name: string): void;
}

/** A section as the toggle sees it: nothing but its classes. */
export interface Section {
  classList: Classes;
}

/** True when the classes mark a height-capped section. */
export function isCappedSection(classList: ClassTest): boolean {
  return CAPPED_CLASSES.some((c) => classList.contains(c));
}

/** Innermost capped section at or above `start`, stopping below `feed`. */
export function cappedSectionAt<
  T extends { parentElement: T | null; classList: ClassTest },
>(start: T | null, feed: T): T | null {
  return ancestorMatching(start, feed, (node) => isCappedSection(node.classList));
}

/**
 * The whole click decision: the section to toggle, or null to leave the
 * click alone. A click that ends a text highlight is a selection gesture
 * rather than a toggle, and a click on a link or a disclosure triangle
 * belongs to that control.
 */
export function expandAction<T>(opts: {
  section: T | null;
  interactive: boolean;
  selectedText: string;
}): T | null {
  if (opts.section === null || opts.interactive) return null;
  if (opts.selectedText.trim() !== "") return null;
  return opts.section;
}

/** True when the section is currently laid out at full length. */
export function isExpanded(section: Section): boolean {
  return section.classList.contains(EXPANDED_CLASS);
}

/**
 * Flip the section between its capped preview and full length,
 * answering the state it lands in.
 */
export function toggleExpanded(section: Section): boolean {
  if (isExpanded(section)) {
    section.classList.remove(EXPANDED_CLASS);
    return false;
  }
  section.classList.add(EXPANDED_CLASS);
  return true;
}

/** The class a section is keyed by: its first CAPPED_CLASSES entry. */
function primaryClass(section: Section): string {
  return CAPPED_CLASSES.find((c) => section.classList.contains(c)) ?? "";
}

/**
 * Walk an item's sections in order, handing each to VISIT with its
 * stable `class:occurrence` key — the one walk both sides of the
 * capture/re-apply round trip share, so their keys cannot drift apart.
 */
function eachSectionKey(
  sections: ArrayLike<Section>,
  visit: (section: Section, key: string) => void,
): void {
  const seen = new Map<string, number>();
  for (let i = 0; i < sections.length; i++) {
    const cls = primaryClass(sections[i]);
    const n = seen.get(cls) ?? 0;
    seen.set(cls, n + 1);
    visit(sections[i], `${cls}:${n}`);
  }
}

/**
 * Stable identities of the expanded sections among an item's capped
 * sections: `class:occurrence` rather than raw position, so a section the
 * user opened keeps its expansion when a re-render inserts or drops a
 * DIFFERENT kind of section around it (a result box landing beneath an
 * open command, a child panel growing above an open output).
 */
export function expandedKeys(sections: ArrayLike<Section>): string[] {
  const open: string[] = [];
  eachSectionKey(sections, (section, key) => {
    if (isExpanded(section)) open.push(key);
  });
  return open;
}

/** Re-expand the sections whose keys are in KEYS (from expandedKeys). */
export function applyExpanded(sections: ArrayLike<Section>, keys: readonly string[]): void {
  const open = new Set(keys);
  eachSectionKey(sections, (section, key) => {
    if (open.has(key)) section.classList.add(EXPANDED_CLASS);
  });
}

/** The capped sections inside one rendered feed item, in document order. */
export function sectionsIn(item: HTMLElement): HTMLElement[] {
  return [...item.querySelectorAll<HTMLElement>(CAPPED_SELECTOR)];
}

/**
 * True when CARD owns SECTION directly — the section is not a capped box
 * belonging to a child rendered inside an open activity panel. Reveal
 * (see `FeedRenderer.revealAgent`) lays a clicked agent's OWN input and
 * output out in full without also un-capping every nested child's box.
 */
export function ownsSection<
  T extends { parentElement: T | null; classList: ClassTest },
>(section: T, card: T): boolean {
  return ancestorMatching(section, card, (n) => n.classList.contains(PANEL_CLASS)) === null;
}

/**
 * Lay out at full length every capped section CARD owns directly (input,
 * progress, output), leaving the sections nested in an open activity
 * panel alone. A section the user later clicks toggles back to its cap.
 */
export function expandOwnSections(card: HTMLElement): void {
  for (const section of sectionsIn(card)) {
    if (ownsSection(section, card)) section.classList.add(EXPANDED_CLASS);
  }
}

/**
 * Arm click-to-expand on `feed`: a click on a capped section lifts its
 * height cap, and the next click on it restores the capped preview.
 */
export function installClickExpand(
  feed: HTMLElement,
  selection: () => string = () => window.getSelection()?.toString() ?? "",
): void {
  feed.addEventListener("click", (e: MouseEvent) => {
    const target = e.target instanceof HTMLElement ? e.target : null;
    const section = expandAction({
      section: cappedSectionAt(target, feed),
      interactive: target !== null && target.closest(CLICK_THROUGH_SELECTOR) !== null,
      selectedText: selection(),
    });
    if (section === null) return;
    toggleExpanded(section);
  });
}
