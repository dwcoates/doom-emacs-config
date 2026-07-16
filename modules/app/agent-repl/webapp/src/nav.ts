/**
 * Cycling the feed's bubbles from the input box.
 *
 * The user never leaves the composer: a chord pressed while typing moves
 * the OUTPUT window and focus stays where it was, because nothing here
 * ever calls `focus()` on the feed. The composer keeps every key that is
 * not one of ours.
 *
 * The scheme is one mechanism, not a handler per bubble kind. The
 * renderer stamps semantic tokens on each feed-item wrapper (`data-nav`,
 * a space-separated list exactly like `class`), and a cycle is "walk the
 * wrappers carrying token X". A modifier picks the class, `j`/`k` picks
 * the direction (vim), so adding a class is a row in CLASS_MODIFIERS
 * rather than new traversal code. Tokens are stamped more finely than
 * the bound chords use them — every assistant bubble carries `response`
 * though only `final` is bound today — so widening the scheme later is a
 * registry row with no renderer change.
 *
 * Both input surfaces drive the same core. The webapp's own composer
 * (`installNavKeys`) is the browser case; the Emacs GUI hides that
 * composer (`composer=0`) and owns input itself, so it drives the
 * identical cycle out-of-band through `installNavHook` — the xwidget
 * cannot deliver keys into the page.
 *
 * The cursor is a `data-key` STRING, never an index or a node. That is
 * what makes an in-flight cycle survive the feed re-rendering under it:
 * the reconciler keys its wrappers by exactly that string, so a streaming
 * turn rewriting bubble bodies leaves the cursor resolvable. A cursor
 * whose item is gone (a `/clear` dropped it) simply stops resolving and
 * the next press re-seeds from the viewport, rather than pointing at a
 * detached node.
 *
 * The pure helpers hold every decision; `FeedNav` and the two installers
 * are the only DOM-facing pieces.
 */
import type { ConversationItem } from "./store.js";
import { revealNode } from "./scroll.js";

/** Attribute carrying a wrapper's semantic nav tokens. */
export const NAV_ATTR = "data-nav";

/** Class marking the bubble the cycle currently sits on. */
export const NAV_CURRENT_CLASS = "nav-current";

/**
 * Name of the global the Emacs host cycles the feed through.
 * `agent-repl-frontend-nav-hook' (output-nav.el) MUST match this string.
 */
export const NAV_HOOK = "agentReplNavigate";

/**
 * The bubble classes a cycle can walk, as the token each selects.
 *
 * `final` rather than `response` is what the response chord binds: those
 * are the purple final-response bubbles that answer a prompt, as against
 * the commentary the agent emits between tool calls.
 */
export const NAV_CLASSES = ["prompt", "final", "tool"] as const;

export type NavClass = (typeof NAV_CLASSES)[number];

/** Forward (down the feed) or backward (up it). */
export type NavDirection = 1 | -1;

/** True when NAME is a class this build can cycle. */
export function isNavClass(name: string): name is NavClass {
  return (NAV_CLASSES as readonly string[]).includes(name);
}

/**
 * Semantic nav tokens for ITEM, or "" when it is not a cycle stop.
 *
 * Finality is not a wire fact — it is derived per render (`finalResponses`
 * in render.ts) — so the caller passes it rather than this re-deriving it.
 * Thinking blocks, permission cards and results are deliberately no-stops:
 * they are not something the user navigates BY.
 */
export function navTokensForItem(item: ConversationItem, isFinal: boolean): string {
  switch (item.kind) {
    case "user-turn":
      return "prompt";
    case "text":
      return isFinal ? "response final" : "response";
    case "tool":
      return "tool";
    default:
      return "";
  }
}

/** True when ATTR's token list carries TOKEN. */
export function hasToken(attr: string, token: string): boolean {
  return attr.split(/\s+/).includes(token);
}

/** DOM-order indexes of the wrappers whose tokens carry CLS. */
export function matchingIndexes(attrs: readonly string[], cls: NavClass): number[] {
  const hits: number[] = [];
  attrs.forEach((attr, i) => {
    if (hasToken(attr, cls)) hits.push(i);
  });
  return hits;
}

/** Slack absorbing the sub-pixel gap between a wrapper's top and the viewport's. */
export const SEED_EPS_PX = 2;

/**
 * Where a cycle ENTERS when nothing is marked yet: the nearest match in
 * the direction asked for, counting from the viewport's top edge.
 *
 * The feed may be scrolled anywhere and there is no cursor in it, so the
 * only honest origin is what the user is LOOKING at — not the tail, which
 * would throw away the position they deliberately scrolled to.
 *
 * The first press enters the cycle rather than stepping through it, so a
 * match already at the top of the viewport is where a forward cycle
 * STARTS, not what it skips. That press still says something even though
 * it scrolls nowhere: the marker appears on it. Every press after that has
 * a cursor and steps (`cycleTarget`).
 *
 * Wraps like every other step: scrolled past the last match, a forward
 * entry comes back to the first.
 */
export function seedTarget(opts: {
  matches: readonly number[];
  tops: readonly number[];
  viewportTop: number;
  dir: NavDirection;
  epsilonPx?: number;
}): number | null {
  const { matches, tops, viewportTop, dir } = opts;
  const eps = opts.epsilonPx ?? SEED_EPS_PX;
  if (matches.length === 0) return null;
  if (dir === 1) {
    const below = matches.find((m) => tops[m] >= viewportTop - eps);
    return below ?? matches[0];
  }
  for (let i = matches.length - 1; i >= 0; i--) {
    if (tops[matches[i]] <= viewportTop + eps) return matches[i];
  }
  return matches[matches.length - 1];
}

/**
 * The next matching wrapper's index, wrapping at both ends.
 *
 * Wrap-around is uniform across every class: running off the last match
 * returns to the first, and off the first returns to the last. Null only
 * when the class has no matches at all, which is the one case with
 * nowhere to go.
 *
 * FROM is excluded in both directions, so a repeated press always moves.
 */
export function cycleTarget(opts: {
  matches: readonly number[];
  from: number;
  dir: NavDirection;
}): number | null {
  const { matches, from, dir } = opts;
  if (matches.length === 0) return null;
  if (dir === 1) {
    const ahead = matches.find((m) => m > from);
    return ahead ?? matches[0];
  }
  for (let i = matches.length - 1; i >= 0; i--) {
    if (matches[i] < from) return matches[i];
  }
  return matches[matches.length - 1];
}

/** The parts of a key event a nav chord is decided from. */
export interface NavKeyEvent {
  code: string;
  ctrlKey: boolean;
  altKey: boolean;
  shiftKey: boolean;
  metaKey: boolean;
}

/** A resolved chord: which class to walk, and which way. */
export interface NavChord {
  cls: NavClass;
  dir: NavDirection;
}

/**
 * One modifier combination per bubble class — the whole binding scheme.
 *
 * Adding a class means adding a row. Shift is on every chord (and so is
 * not listed) because the unshifted `C-j` / `C-k` are long spoken for by
 * window motion and `kill-visual-line`.
 */
export const CLASS_MODIFIERS: ReadonlyArray<{ cls: NavClass; ctrl: boolean; alt: boolean }> = [
  { cls: "prompt", ctrl: true, alt: false },
  { cls: "final", ctrl: false, alt: true },
  { cls: "tool", ctrl: true, alt: true },
];

/** `j` walks forward and `k` backward, per vim. */
const DIRECTION_CODES: Record<string, NavDirection> = { KeyJ: 1, KeyK: -1 };

/**
 * The chord E resolves to, or null when it is not ours and the composer
 * must keep it.
 *
 * Keyed off `code`, never `key`: on macOS the Option modifier rewrites
 * `key` into whatever glyph it composes (`M-j` arrives as `∆`), so `key`
 * would silently never match the alt-modified chords. Meta is never ours,
 * so Cmd-based system and browser chords pass straight through.
 */
export function navChord(e: NavKeyEvent): NavChord | null {
  if (!e.shiftKey || e.metaKey) return null;
  const dir = DIRECTION_CODES[e.code];
  if (dir === undefined) return null;
  const hit = CLASS_MODIFIERS.find((m) => m.ctrl === e.ctrlKey && m.alt === e.altKey);
  return hit ? { cls: hit.cls, dir } : null;
}

/**
 * Mark EL as the current thing among NODES, carrying CLS.
 *
 * The other half of the seam `revealNode` (scroll.ts) opens: a cycle's
 * current bubble and a search's current match are the same "you are here"
 * problem, and the marker must be exclusive or two of them accumulate.
 * The class is a parameter so a search can mark with its own.
 */
export function markCurrent(
  nodes: readonly HTMLElement[],
  el: HTMLElement | null,
  cls: string = NAV_CURRENT_CLASS,
): void {
  for (const node of nodes) node.classList.remove(cls);
  el?.classList.add(cls);
}

/** One feed wrapper, reduced to what a cycle decision reads off it. */
export interface NavItem {
  /** The reconciler's stable identity (`data-key`). */
  key: string | null;
  /** Its `data-nav` token list, "" when it is not a cycle stop. */
  tokens: string;
  /** Its offset from the feed's top, for viewport seeding. */
  top: number;
}

/**
 * The whole cycle decision: the index ITEMS moves to, or null when the
 * class has no stops at all.
 *
 * Pure, so every rule above is testable without a DOM: an unresolvable
 * cursor falls back to a viewport seed, and the seed itself is where the
 * user is looking rather than the tail.
 */
export function cycleDecision(opts: {
  items: readonly NavItem[];
  cursor: string | null;
  scrollTop: number;
  cls: NavClass;
  dir: NavDirection;
}): number | null {
  const { items, cursor, scrollTop, cls, dir } = opts;
  const matches = matchingIndexes(
    items.map((i) => i.tokens),
    cls,
  );
  // A cursor whose item left the feed resolves to -1 and re-seeds, rather
  // than anchoring the cycle to a key nothing carries any more.
  const marked = cursor === null ? -1 : items.findIndex((i) => i.key === cursor);
  if (marked >= 0) return cycleTarget({ matches, from: marked, dir });
  return seedTarget({
    matches,
    tops: items.map((i) => i.top),
    viewportTop: scrollTop,
    dir,
  });
}

/**
 * The cursor a render leaves behind, given the keys still in the feed.
 *
 * A new turn retires the cursor; otherwise it survives exactly as long as
 * its key does, which is what carries an in-flight cycle through the
 * re-render of a streaming turn.
 */
export function nextCursor(opts: {
  keys: readonly (string | null)[];
  cursor: string | null;
  turnChanged: boolean;
}): string | null {
  if (opts.turnChanged) return null;
  if (opts.cursor === null) return null;
  return opts.keys.includes(opts.cursor) ? opts.cursor : null;
}

/** The feed's top-level bubble wrappers, in DOM order. */
function feedItems(feed: HTMLElement): HTMLElement[] {
  return [...feed.querySelectorAll<HTMLElement>(":scope > .feed-item")];
}

/** Reduce a live wrapper to the shape a cycle decision reads. */
function navItem(el: HTMLElement): NavItem {
  return {
    key: el.dataset.key ?? null,
    tokens: el.getAttribute(NAV_ATTR) ?? "",
    top: el.offsetTop,
  };
}

/**
 * The feed's cycle cursor: which bubble is current, and how it moves.
 *
 * Holds the cursor as a reconcile-stable `data-key` and nothing else, so
 * there is no bookkeeping to invalidate when the feed re-renders.
 */
export class FeedNav {
  /** `data-key` of the marked bubble, or null when no cycle is in flight. */
  private cursor: string | null = null;
  /** The turn the cursor belongs to, so a NEW prompt can retire it. */
  private turnId: string | null = null;

  constructor(private readonly feed: HTMLElement) {}

  /** The marked bubble's key, for tests and for a caller restoring state. */
  get current(): string | null {
    return this.cursor;
  }

  /**
   * Step to the next bubble of CLS in direction DIR, marking it and
   * bringing it into view. Answers whether anything was there to move to.
   */
  cycle(cls: NavClass, dir: NavDirection): boolean {
    const wrappers = feedItems(this.feed);
    const target = cycleDecision({
      items: wrappers.map(navItem),
      cursor: this.cursor,
      scrollTop: this.feed.scrollTop,
      cls,
      dir,
    });
    if (target === null) return false;
    const el = wrappers[target];
    this.cursor = el.dataset.key ?? null;
    markCurrent(wrappers, el);
    revealNode(el);
    return true;
  }

  /** Drop the cycle: no bubble is current and the marker comes off. */
  clear(): void {
    this.cursor = null;
    markCurrent(feedItems(this.feed), null);
  }

  /**
   * Re-seat the cycle after a render, given the feed's newest turn id.
   *
   * A re-render must never SILENTLY lose the cursor, so this is what the
   * render path calls: the marker is re-applied to whichever wrapper now
   * carries the cursor's key, which covers the restored-session render
   * rebuilding every node from scratch.
   *
   * A new user turn retires the cursor instead. The feed re-pins to its
   * tail on a fresh prompt (`repinsToTail`, render.ts) because a sender
   * wants to watch the answer, and a marker left parked up in history
   * after the user has moved the conversation on is exactly the stale
   * state that makes the next press jump somewhere unaccountable.
   */
  reconcile(turnId: string | null): void {
    const turnChanged = turnId !== this.turnId;
    this.turnId = turnId;
    const wrappers = feedItems(this.feed);
    this.cursor = nextCursor({
      keys: wrappers.map((w) => w.dataset.key ?? null),
      cursor: this.cursor,
      turnChanged,
    });
    markCurrent(
      wrappers,
      this.cursor === null ? null : (wrappers.find((w) => w.dataset.key === this.cursor) ?? null),
    );
  }
}

/** What a hook is planted on: `window`, or a plain object under test. */
export type NavGlobal = Record<string, unknown>;

/**
 * Plant the cycle hook on TARGET for the Emacs host to call.
 *
 * The GUI's input box is an Emacs buffer, not this page's composer, so
 * its chords arrive here as an injected script rather than a key event.
 * An unknown class is a broken contract between the two halves of one
 * feature — not an expected runtime condition — so it raises rather than
 * quietly cycling nothing.
 */
export function installNavHook(target: NavGlobal, nav: FeedNav): void {
  target[NAV_HOOK] = (cls: string, dir: number): boolean => {
    if (!isNavClass(cls)) throw new Error(`${NAV_HOOK}: unknown nav class ${cls}`);
    return nav.cycle(cls, dir >= 0 ? 1 : -1);
  };
}

/**
 * Arm the cycle chords on INPUT (the webapp's own composer).
 *
 * Ours are swallowed so the textarea never sees them; everything else
 * reaches it untouched, which is what keeps typing working mid-cycle.
 */
export function installNavKeys(input: HTMLElement, nav: FeedNav): void {
  input.addEventListener("keydown", (event) => {
    const e = event as KeyboardEvent;
    const chord = navChord(e);
    if (!chord) return;
    e.preventDefault();
    nav.cycle(chord.cls, chord.dir);
  });
}
