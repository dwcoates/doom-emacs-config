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
 * installClickExpand and the AutoCollapse owner it registers with are the
 * DOM-facing pieces: a click toggles a section, and the owner closes an open
 * one the reader scrolled away from or left (see `AutoCollapse`). Both go
 * through the one collapse, `collapseSection`.
 */
import { ancestorMatching, scrollbarWidthPx } from "./dom.js";
import { log } from "./log.js";
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
  // the list, and left exactly as it was. Only a CAPPED bubble's box wears it:
  // an uncapped one (`BUBBLE_UNCAPPED`, src/bubble/draw.ts) is no section.
  "bubble-scroll",
] as const;

/**
 * The DOM event a feed item's OWN expansion dispatches from the expanded
 * element, bubbling, when the READER expanded it (a sub-feed bubble's fold, a
 * compaction's summary fold). The root feed, which owns the scroll box,
 * centers the item's row on it (`itemExpanded`). The capped sections this
 * module toggles center through the click owner's own callback instead.
 */
export const ITEM_EXPANDED_EVENT = "feed-item-expanded";

/** Announce that the reader expanded the feed item EL belongs to. */
export function announceItemExpanded(el: HTMLElement): void {
  el.dispatchEvent(new CustomEvent(ITEM_EXPANDED_EVENT, { bubbles: true }));
}

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
 * The innermost OPEN (expanded) capped section at or above `start`, stopping
 * below `feed` — the section a wheel there must stay inside (scroll.ts's
 * `installIntentScroll` contains it).
 */
export function expandedSectionAt(start: HTMLElement, feed: HTMLElement): HTMLElement | null {
  return ancestorMatching(start, feed, (node) => isCappedSection(node.classList) && isExpanded(node));
}

/**
 * The class every element of a bubble's HEADER STRIP wears (src/bubble/draw.ts
 * stamps it). The strip is the bubble's collapsed face as much as its capped
 * box is — a peer message's collapsed face is nothing BUT its strip — so a
 * click on it toggles the bubble's one scroll box. Declared here, where the
 * click is resolved, and imported by the bubble.
 */
export const BUBBLE_STRIP_CLASS = "bubble-strip";

/**
 * The class every element of a bubble's EXPAND-ONLY region wears (src/bubble/
 * draw.ts stamps it): chrome after the scroll box that the stylesheet hides
 * until the toggle below marks that scroll box `.expanded`. It is the same one
 * toggle opening it, so it needs no click wiring of its own.
 */
export const BUBBLE_EXPAND_ONLY_CLASS = "bubble-expand-only";

/**
 * The section a click at START toggles, stopping below FEED: the innermost
 * capped section at or above it, or — when a bubble's header strip is met
 * first — that bubble's own scroll box. ONE toggle for every bubble kind: a
 * peer message's label, a prompt's address line and a held prompt's badges all
 * open the same box a click on the text itself does. An UNCAPPED bubble's box
 * (`BUBBLE_UNCAPPED`, src/bubble/draw.ts) wears no `.bubble-scroll`, so neither
 * its text nor its strip resolves to a section: a click on it toggles nothing.
 */
export function sectionAt(start: HTMLElement | null, feed: HTMLElement): HTMLElement | null {
  const hit = ancestorMatching(
    start,
    feed,
    (node) => isCappedSection(node.classList) || node.classList.contains(BUBBLE_STRIP_CLASS),
  );
  if (hit === null || !hit.classList.contains(BUBBLE_STRIP_CLASS)) return hit;
  return hit.parentElement?.querySelector<HTMLElement>(":scope > .bubble-scroll") ?? null;
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
 * What a host runs after every toggle of one of its sections, with the section
 * and the state it landed in (see `installClickExpand`).
 */
export type AfterToggle = (section: HTMLElement, expanded: boolean) => void;

/**
 * THE ONE COLLAPSE. Every way a section returns to its capped preview goes
 * through here, so every side effect of a collapse stays in step whatever
 * asked for it: the class comes off, the preview shows from the top, and the
 * host's `afterToggle` re-measures what it draws (the "more below" fade, the
 * title folds).
 *
 * FIX3 (owner ruling, 2026-09-15): a collapse shows the preview from the top.
 * The reader's own gesture is what moves the box, so the write lives in
 * scroll.ts with every other scroll write (`collapseClicked`).
 */
export function collapseSection(section: HTMLElement, afterToggle?: AfterToggle): void {
  section.classList.remove(EXPANDED_CLASS);
  collapseClicked(section);
  afterToggle?.(section, false);
}

/**
 * Flip SECTION through the one expand and the one collapse
 * (`collapseSection`), answering the state it lands in.
 */
export function toggleSection(section: HTMLElement, afterToggle?: AfterToggle): boolean {
  // Only a capped section has an open state. An uncapped bubble's box is not
  // one (src/bubble/draw.ts `BUBBLE_UNCAPPED`), and no caller may open it.
  if (!isCappedSection(section.classList)) {
    log.error("refused to toggle an element that is not a capped section", {
      operation: "expand.toggle-uncapped",
      context: { classes: section.className },
    });
    throw new Error("expand: only a capped section can be toggled");
  }
  if (isExpanded(section)) {
    collapseSection(section, afterToggle);
    return false;
  }
  section.classList.add(EXPANDED_CLASS);
  afterToggle?.(section, true);
  return true;
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
  afterToggle?: AfterToggle,
): () => void {
  const onClick = (e: MouseEvent): void => {
    const target = e.target instanceof HTMLElement ? e.target : null;
    const section = expandAction({
      section: sectionAt(target, feed),
      interactive: target !== null && target.closest(CLICK_THROUGH_SELECTOR) !== null,
      selectedText: selection(),
    });
    if (section === null) return;
    toggleSection(section, afterToggle);
  };
  feed.addEventListener("click", onClick);
  const unregister = autoCollapseFor(feed.ownerDocument).register(feed, afterToggle);
  return () => {
    feed.removeEventListener("click", onClick);
    unregister();
  };
}

/**
 * Why an open section closed on its own (see `AutoCollapse`): the reader
 * scrolled somewhere other than inside it, or focus left the page.
 */
export type CollapseTrigger = "scrollOutside" | "windowBlur" | "pageHidden";

/**
 * Every expanded capped section under HOST, in document order — the DOM's own
 * answer to "which sections are open", read at the moment it is asked, so a
 * section a redraw replaced (and `carryExpanded` re-opened) is found on the
 * element that is actually drawn.
 */
export function expandedSectionsOf(host: HTMLElement): HTMLElement[] {
  return cappedSectionsOf(host).filter((section) => isExpanded(section));
}

/**
 * True when a pointer at (CLIENT_X, CLIENT_Y) lands on EL's own classic
 * (layout-width) vertical scrollbar: right of its padding box and inside its
 * border box. An overlay scrollbar takes no layout width, so it never reads as
 * one here (see `AutoCollapse`).
 */
export function onVerticalScrollbar(el: Element, clientX: number): boolean {
  const barWidth = el instanceof HTMLElement ? scrollbarWidthPx(el, el.clientLeft * 2) : 0;
  if (barWidth <= 0) return false;
  const rect = el.getBoundingClientRect();
  const barLeft = rect.left + el.clientLeft + el.clientWidth;
  return clientX >= barLeft && clientX < barLeft + barWidth;
}

/**
 * THE AUTO-COLLAPSE OWNER: the one place that closes an open section the
 * reader has moved away from. One per page (`autoCollapseFor`); every host that
 * arms click-to-expand registers with it, and the owner closes that host's open
 * sections through the SAME `collapseSection` a click-to-collapse uses, with the
 * host's own `afterToggle`, so the fade, the title folds and the expand-only
 * regions all follow exactly as they do on a click.
 *
 * WHAT CLOSES AN OPEN SECTION.
 *
 *   - `scrollOutside`: a reader SCROLL GESTURE whose target is not inside it —
 *     a `wheel` anywhere else on the page (the feed, another box, the sidebar),
 *     or a `pointerdown` on another element's classic scrollbar — ARMS it, and
 *     it closes once NO PART OF IT IS VISIBLE in its host's scroll area (owner
 *     request, 2026-09-30: an item the reader can still see stays open while
 *     they scroll past it). A wheel inside the open section keeps it open,
 *     whichever box the wheel ends up moving.
 *   - `windowBlur`: the window lost focus. In Emacs's xwidget the webview is a
 *     WKWebView inside the frame's EmacsView; a click on any other Emacs window
 *     makes the EmacsView first responder (it accepts first responder, so
 *     AppKit hands it on mouse-down), the WKWebView resigns, and WebKit blurs
 *     the page's window. Switching Emacs frames or applications resigns key
 *     window, which blurs it too.
 *   - `pageHidden`: the page went hidden (`visibilitychange`), which the
 *     xwidget reports whenever Emacs is not the frontmost application.
 *
 * STRUCTURALLY BLIND TO ITS OWN LAYOUT. The owner never listens to `scroll`:
 * only the reader's INPUT events arm a section, and they precede the movement
 * they cause and no layout change can synthesize them. Visibility is read by
 * an IntersectionObserver, which decides WHEN an armed section closes, never
 * WHETHER: an expand's growth, the collapse's own shrink, or an implicit feed
 * move (a tail follow, a compensation) cannot close a section no reader
 * gesture armed. Keys are not a trigger: xwidget hands a key to the page only while an
 * input has focus, and forwards every other key to Emacs.
 */
export class AutoCollapse {
  private readonly hosts = new Map<HTMLElement, AfterToggle | undefined>();
  /** Each host's visibility watch, made when one of its sections is first armed. */
  private readonly watches = new Map<HTMLElement, VisibilityWatch>();
  /** The armed sections: a reader gesture left them, and they close once unseen. */
  private readonly armed = new Map<HTMLElement, HTMLElement>();

  constructor(
    private readonly doc: Document,
    private watchVisibility: VisibilityWatcher = intersectionWatcher,
  ) {}

  /**
   * Put HOST's open sections under this owner, closed with AFTER_TOGGLE; the
   * page-wide listeners are attached with the first host and detached with the
   * last. Answers the unregister.
   */
  register(host: HTMLElement, afterToggle?: AfterToggle): () => void {
    if (this.hosts.size === 0) this.attach();
    this.hosts.set(host, afterToggle);
    return () => {
      if (!this.hosts.delete(host)) return;
      this.watches.get(host)?.disconnect();
      this.watches.delete(host);
      for (const [section, owner] of this.armed) if (owner === host) this.armed.delete(section);
      if (this.hosts.size === 0) this.detach();
    };
  }

  /**
   * Watch visibility through WATCHER from now on. Refused while any section
   * is armed: its watch would be orphaned, and it would never close.
   */
  useWatcher(watcher: VisibilityWatcher): void {
    // A section removed from the page can neither show nor close: it is no
    // longer armed.
    for (const section of [...this.armed.keys()]) {
      if (!section.isConnected) this.armed.delete(section);
    }
    if (this.armed.size > 0) {
      throw new Error("expand: the visibility watch cannot change while a section is armed");
    }
    for (const watch of this.watches.values()) watch.disconnect();
    this.watches.clear();
    this.watchVisibility = watcher;
  }

  /**
   * ARM every open section of every registered host that does not contain
   * INSIDE: each closes once no part of it is visible in its host's scroll
   * area, which may be at once.
   */
  armOutside(inside: Node): void {
    for (const host of this.hosts.keys()) {
      for (const section of expandedSectionsOf(host)) {
        if (!section.isConnected || section.contains(inside) || this.armed.has(section)) continue;
        this.armed.set(section, host);
        this.watchOf(host).observe(section);
      }
    }
  }

  /** The visibility watch over HOST's scroll area, made on first use. */
  private watchOf(host: HTMLElement): VisibilityWatch {
    let watch = this.watches.get(host);
    if (watch === undefined) {
      watch = this.watchVisibility(scrollRootOf(host), (section, visible) => {
        this.onSectionVisibility(host, section, visible);
      });
      this.watches.set(host, watch);
    }
    return watch;
  }

  /** An armed section's visibility changed: it closes once none of it shows. */
  private onSectionVisibility(host: HTMLElement, section: HTMLElement, visible: boolean): void {
    if (this.armed.get(section) !== host) return;
    if (!section.isConnected || !isExpanded(section)) {
      // Closed some other way, or redrawn away: nothing is armed any more.
      this.armed.delete(section);
      this.watches.get(host)?.unobserve(section);
      return;
    }
    if (visible) return;
    this.armed.delete(section);
    this.watches.get(host)?.unobserve(section);
    log.debug(`auto-collapsing an open ${primaryClass(section)} scrolled out of view`, {
      operation: "expand.auto-collapse",
      context: { trigger: "scrollOutside", kind: primaryClass(section), role: roleOf(section) },
    });
    collapseSection(section, this.hosts.get(host));
  }

  /**
   * Close every open section of every registered host that does not contain
   * INSIDE (null: close them all), logging each at DEBUG with its trigger.
   */
  collapseOutside(trigger: CollapseTrigger, inside: Node | null): void {
    for (const [host, afterToggle] of this.hosts) {
      for (const section of expandedSectionsOf(host)) {
        if (inside !== null && section.contains(inside)) continue;
        log.debug(`auto-collapsing an open ${primaryClass(section)} on ${trigger}`, {
          operation: "expand.auto-collapse",
          context: { trigger, kind: primaryClass(section), role: roleOf(section) },
        });
        collapseSection(section, afterToggle);
      }
    }
  }

  private readonly onWheel = (e: WheelEvent): void => {
    if (!(e.target instanceof Node)) return;
    this.armOutside(e.target);
  };

  private readonly onPointerDown = (e: PointerEvent): void => {
    const target = e.target;
    if (!(target instanceof Element) || !onVerticalScrollbar(target, e.clientX)) return;
    this.armOutside(target);
  };

  private readonly onBlur = (e: FocusEvent): void => {
    // An element's `blur` whose target is a node of the page can reach the
    // window too; only the window's own blur (no node target) means focus left
    // the page.
    if (e.target instanceof Node) return;
    this.collapseOutside("windowBlur", null);
  };

  private readonly onVisibility = (): void => {
    if (this.doc.visibilityState !== "hidden") return;
    this.collapseOutside("pageHidden", null);
  };

  private attach(): void {
    const win = this.requireWindow();
    this.doc.addEventListener("wheel", this.onWheel, { capture: true, passive: true });
    this.doc.addEventListener("pointerdown", this.onPointerDown, { capture: true, passive: true });
    this.doc.addEventListener("visibilitychange", this.onVisibility);
    win.addEventListener("blur", this.onBlur);
  }

  private detach(): void {
    const win = this.requireWindow();
    this.doc.removeEventListener("wheel", this.onWheel, { capture: true });
    this.doc.removeEventListener("pointerdown", this.onPointerDown, { capture: true });
    this.doc.removeEventListener("visibilitychange", this.onVisibility);
    win.removeEventListener("blur", this.onBlur);
  }

  private requireWindow(): Window {
    const win = this.doc.defaultView;
    if (win === null) throw new Error("expand: auto-collapse needs a document with a window");
    return win;
  }
}

/** One watch over a scroll area: which observed sections show any part. */
export interface VisibilityWatch {
  observe(section: HTMLElement): void;
  unobserve(section: HTMLElement): void;
  disconnect(): void;
}

/**
 * Makes a visibility watch over ROOT (null: the viewport), reporting through
 * ON each observed section's visibility when first observed and whenever it
 * changes. VISIBLE is true while any part of the section shows.
 */
export type VisibilityWatcher = (
  root: HTMLElement | null,
  on: (section: HTMLElement, visible: boolean) => void,
) => VisibilityWatch;

/** The page's own visibility watch: an IntersectionObserver over ROOT. */
export const intersectionWatcher: VisibilityWatcher = (root, on) => {
  const observer = new IntersectionObserver(
    (entries) => {
      for (const entry of entries) {
        if (entry.target instanceof HTMLElement) on(entry.target, entry.isIntersecting);
      }
    },
    { root, threshold: 0 },
  );
  return {
    observe: (section) => observer.observe(section),
    unobserve: (section) => observer.unobserve(section),
    disconnect: () => observer.disconnect(),
  };
};

/**
 * The scroll area a host's sections are seen through: the nearest box, the
 * host included, that scrolls vertically; null (the viewport) when none does.
 */
export function scrollRootOf(host: HTMLElement): HTMLElement | null {
  for (let el: HTMLElement | null = host; el !== null; el = el.parentElement) {
    const overflow = getComputedStyle(el).overflowY;
    if (overflow === "auto" || overflow === "scroll") return el;
  }
  return null;
}

/** The bubble role of an open section that is a bubble's own scroll box. */
function roleOf(section: HTMLElement): string | undefined {
  return primaryClass(section) === "bubble-scroll" ? section.parentElement?.dataset.role : undefined;
}

/** The one owner per document. */
const owners = new WeakMap<Document, AutoCollapse>();

/**
 * Make DOC's one `AutoCollapse` watch visibility through WATCHER. A seam for
 * the suite, whose DOM lays nothing out and has no IntersectionObserver; it is
 * refused while any section is being watched, so no watch is ever orphaned.
 */
export function useVisibilityWatcher(doc: Document, watcher: VisibilityWatcher): void {
  autoCollapseFor(doc).useWatcher(watcher);
}

/** The page's one `AutoCollapse`, made on first use. */
export function autoCollapseFor(doc: Document): AutoCollapse {
  let owner = owners.get(doc);
  if (owner === undefined) {
    owner = new AutoCollapse(doc);
    owners.set(doc, owner);
  }
  return owner;
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
