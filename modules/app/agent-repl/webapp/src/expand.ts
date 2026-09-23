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
import { collapseClicked } from "./scroll.js";

/**
 * Classes that mark a click-to-expand section — the thing the feed-wide click
 * handler toggles `.expanded` on. Each entry is the ONE class a whole
 * expandable unit is keyed by, and every element carries at most one of them,
 * so `primaryClass` never has a collision to resolve.
 *
 * CARD-LEVEL FOLD (owner ruling, 2026-09-15). A tool-call and a skill card are
 * ONE fold apiece: the whole `.tool-fold` card is the toggle, its header (title
 * + a 2-row-capped input line) is the collapsed face, and its output section is
 * HIDDEN until the card is `.expanded` — no capped preview. The inner section
 * classes (`.tool-output`, `.bash-output`, `.tool-input`, …) are therefore NOT
 * listed here: a click anywhere in the card resolves to `.tool-fold`, never to
 * an inner box, so the card opens and closes as a unit. The stylesheet still
 * caps and scrolls those inner boxes, but their EXPANDABILITY is the card's.
 *
 * THE STANDALONE CAPPED BOXES that keep the old per-section fold are the ones
 * that live OUTSIDE a `.tool-fold` card: the detached shell's live tail
 * (`.shell-tail`, in `.shell-bubble`) and the hook card's reason/output
 * (`.hook-output`), each preserved exactly as it worked before this change.
 *
 * `bubble-scroll` is the response/prompt (and peer) bubble's own scroll box,
 * click-to-expand since 2026-09-15 and deliberately UNTOUCHED here.
 */
export const CAPPED_CLASSES = [
  // The card-level fold: a whole tool-call / skill card, opened as one unit.
  "tool-fold",
  // The detached shell's live tail — its own per-section fold, unchanged.
  "shell-tail",
  // A hook card's reason/output box — its own per-section fold, unchanged.
  "hook-output",
  // A card TITLE that is its own fold (owner ruling, 2026-09-23): the two-line
  // title of a card with no card-level fold to defer to — a hook card, a skill
  // card that is not loaded (title-fold.ts). A title INSIDE a `.tool-fold` or a
  // `.bubble-fold` never wears it, so a click there still opens the whole card.
  "title-fold-standalone",
  // The response/prompt/peer bubble's own scroll box (bubble-scroll.ts). Owner
  // ruling, 2026-09-15: a collapsed bubble no longer scrolls; clicking it
  // expands it to at most 50vh, and only then is its scroll revealed. Last in
  // the list, and left exactly as it was.
  "bubble-scroll",
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

/**
 * Every capped section of ROOT, in document order, ROOT ITSELF FIRST when it is
 * one.
 *
 * A card-level fold (`.tool-fold` on a whole skill or tool-call card) IS the
 * element a renderer hands back, so a plain `querySelectorAll` — which never
 * matches its own root — would walk straight past the one section the reader is
 * most likely to have opened. Both sides of the capture/re-apply round trip go
 * through this function, so the two walks cannot disagree about whether the
 * root counts.
 */
export function cappedSectionsOf(root: HTMLElement): HTMLElement[] {
  const nested = [...root.querySelectorAll<HTMLElement>(CAPPED_SELECTOR)];
  return isCappedSection(root.classList) ? [root, ...nested] : nested;
}

/**
 * Carry the reader's expansions from the body a redraw REPLACES onto the body
 * that replaces it.
 *
 * R2, THE WIRE'S FOLD IS THE INITIAL FOLD: a push states how a section starts
 * on its FIRST draw and never again — a re-push may not un-toggle a section the
 * reader opened. The named folds (`foldSection`, `data-folded`) have always read
 * their state back off the previous element; the CAPPED sections are keyed by
 * class rather than by name, and this is where they get the same guarantee.
 */
export function carryExpanded(previous: HTMLElement, next: HTMLElement): void {
  const keys = expandedKeys(cappedSectionsOf(previous));
  if (keys.length === 0) return;
  applyExpanded(cappedSectionsOf(next), keys);
}

/** Re-expand the sections whose keys are in KEYS (from expandedKeys). */
export function applyExpanded(sections: ArrayLike<Section>, keys: readonly string[]): void {
  const open = new Set(keys);
  eachSectionKey(sections, (section, key) => {
    if (open.has(key)) section.classList.add(EXPANDED_CLASS);
  });
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
 * Arm click-to-expand on `feed`: a click on a capped section lifts its
 * height cap, and the next click on it restores the capped preview.
 *
 * `afterToggle` runs once per toggle with the section and the state it landed
 * in, so a caller can keep a class it draws (the bubble's "more below" fade,
 * bubble-more.ts) in step with an expand/collapse whose height did not change
 * and so fired no resize.
 */
export function installClickExpand(
  feed: HTMLElement,
  selection: () => string = () => window.getSelection()?.toString() ?? "",
  afterToggle?: (section: HTMLElement, expanded: boolean) => void,
): void {
  feed.addEventListener("click", (e: MouseEvent) => {
    const target = e.target instanceof HTMLElement ? e.target : null;
    const section = expandAction({
      section: cappedSectionAt(target, feed),
      interactive: target !== null && target.closest(CLICK_THROUGH_SELECTOR) !== null,
      selectedText: selection(),
    });
    if (section === null) return;
    const expanded = toggleExpanded(section);
    // FIX3 (owner ruling, 2026-09-15): a collapse shows the preview from the
    // top. The reader's own click is what moves the box, so the write lives in
    // scroll.ts with every other scroll write (`collapseClicked`).
    if (!expanded) collapseClicked(section);
    afterToggle?.(section, expanded);
  });
}

/**
 * The reader's expansions across a WHOLE PAGE, keyed by row id.
 *
 * `carryExpanded` carries one row's folds from the element a redraw replaces
 * onto its successor, which is everything an UPSERT needs: the row keeps its
 * place and only its drawing changes. A REPLACE (`applyPage("replace")` —
 * reconnect, reload, compaction replay) tears every row down and rebuilds it,
 * so there is no previous element to read back off; the rows' identities are
 * all that survives, and this is the snapshot taken across that gap.
 *
 * The key is the row's own `FeedId` value, which the daemon keeps stable across
 * a re-serve of the same conversation; the class:occurrence section keys inside
 * one row are the same ones `carryExpanded` uses, so a single walk keys both.
 */
export function snapshotExpanded(bodies: Iterable<[string, HTMLElement]>): Map<string, string[]> {
  const snapshot = new Map<string, string[]>();
  for (const [id, body] of bodies) {
    const keys = expandedKeys(cappedSectionsOf(body));
    if (keys.length > 0) snapshot.set(id, keys);
  }
  return snapshot;
}

/**
 * Drop every snapshot entry whose row is gone from the page that replaced it.
 *
 * A row the replace did not serve again is not coming back, and a key held for
 * it would be a leak that outlives the conversation it described.
 */
export function retainRows(snapshot: Map<string, string[]>, live: ReadonlySet<string>): void {
  for (const id of [...snapshot.keys()]) {
    if (!live.has(id)) snapshot.delete(id);
  }
}
