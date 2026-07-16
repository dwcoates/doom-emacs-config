/**
 * Incremental search over the feed, driven from the composer.
 *
 * The model is Emacs' isearch, because the surrounding frame IS Emacs:
 * `C-s` starts and steps forward, `C-r` steps back, typing refines without
 * ever leaving the composer, `C-g` aborts back to where the search started,
 * and `RET` accepts and stays. Point never moves out of the input box — the
 * query lives in a status line, exactly as isearch keeps it in the echo
 * area rather than in the buffer being searched.
 *
 * WHAT IS SEARCHED. The scope is the feed's DOM text, walked structurally
 * (a TreeWalker ignores CSS), so it covers both the text on screen and the
 * text merely hidden:
 *
 *   - height-capped sections (`.tool-input`, `.bash-output`, …) whose
 *     overflow is only a scroll away (expand.ts);
 *   - the `display: none` folds — a subagent's whole prompt in
 *     `.agent-json`, a tool's raw input in `.folded-json`;
 *   - collapsed `<details class="thinking">` blocks, which is every
 *     FINISHED thinking block.
 *
 * A match in any of those is REVEALED rather than skipped, which is what
 * isearch does when it lands inside folded or invisible text. The reveal is
 * the search's OWN (`REVEAL_CLASS`, `REVEAL_ATTR`), never the user's
 * `EXPANDED_CLASS`: the renderer deliberately persists that one across a
 * re-render as "the user opened this", so a search borrowing it would have
 * its reveal laundered into a click it never made, and the abort could no
 * longer tell what it was entitled to close. With a marker of its own,
 * "what did the search open" is a question the DOM answers directly — no
 * bookkeeping to be invalidated by the next render. An abort re-closes
 * exactly those; an accept promotes them to the user's own expansion.
 *
 * WHAT IS NOT SEARCHED, and why it is COUNTED rather than ignored. Some
 * conversation text never reaches the DOM at all: a closed activity or
 * watcher fold renders its body through a thunk that is not called
 * (render.ts `Fold`), and a tab group emits only its active member's card.
 * A DOM walk cannot see those, and silently under-reporting matches is the
 * one failure this feature must not have — so the status line carries a
 * count of the regions the search could not reach, and the user can open
 * one and search again. Opening them here instead would mean rendering
 * every buffered child and issuing network fetches for watcher tails on
 * every keystroke, which is not a thing an incremental search can do.
 *
 * SURVIVING A RE-RENDER. The feed reconciles as the session streams, and
 * `FeedRenderer.render` rewrites an item's `innerHTML` wholesale whenever
 * its HTML changed — which destroys any `<mark>` inside it. So no search
 * state is ever kept in the DOM: the marks are derived output, re-applied
 * from the query after every render (`refresh`), and the current match is
 * held as a `key:ordinal` identity rather than a node or an index — the
 * same trick `expandedKeys` uses to carry an expansion across the re-render
 * of the item holding it. New output arriving mid-search therefore cannot
 * drop the highlight or move the user off the match they are on.
 *
 * `FeedSearch` is the only DOM-facing piece; every decision it makes lives
 * in the pure helpers above it.
 */
import { EXPANDED_CLASS, isCappedSection } from "./expand.js";

/**
 * Name of the global that drives a search from outside the page.
 * The composer is absent whenever Emacs owns input (`composer=0`,
 * frontend.el), and the NS xwidget cannot deliver keys into the page at
 * all, so the host's only route in is this hook — the same contract
 * `CHESS_NAV_HOOK` (chess-game.ts) uses, and it answers the status line so
 * the caller has something to echo.
 */
export const SEARCH_HOOK = "agentReplSearch";

/** Class on every match's `<mark>`. */
export const MARK_CLASS = "search-match";

/** Class distinguishing the current match from the others. */
export const CURRENT_CLASS = "search-current";

/**
 * Class lifting a capped section's height cap because the SEARCH needed to
 * see inside it. Deliberately not `EXPANDED_CLASS`: that one means "the user
 * opened this", and the renderer persists it across a re-render, so a search
 * borrowing it could never tell its own reveals from the user's clicks when
 * the time came to put them back.
 */
export const REVEAL_CLASS = "search-revealed";

/** The same marker for a `<details>`, whose open state is an attribute. */
export const REVEAL_ATTR = "data-search-revealed";

/** Mounted widgets own their subtree, so their text is not searched. */
export const SKIP_SELECTOR = ".chess-game, .chess-game-error";

/** A fold whose body was never rendered, so its text is not in the DOM. */
export const CLOSED_FOLD_SELECTOR = "[data-panel-toggle]:not(.open)";

/** A tab whose card was never rendered, so its text is not in the DOM. */
export const INACTIVE_TAB_SELECTOR = ".tab-chip:not(.active)";

/** The parts of a KeyboardEvent a search chord is decided from. */
export interface KeyChord {
  key: string;
  ctrlKey: boolean;
  metaKey: boolean;
  altKey: boolean;
  shiftKey: boolean;
}

/**
 * What a key means to the search.
 *
 * `accept` and `exit` both end the search keeping the current match; they
 * differ in who gets the key. `accept` is `RET`, which the search consumes
 * so the composer cannot also send the prompt. `exit` is any other key the
 * search does not speak (an arrow, say): isearch ends and the key then does
 * its ordinary job, so it is deliberately NOT consumed.
 */
export type SearchCommand = "start" | "next" | "prev" | "cancel" | "accept" | "exit" | "erase" | "append";

/**
 * A decided key. A union rather than an optional `char`, so the one command
 * that carries a character is the only one that HAS one: `append` without a
 * character is not a state to fall back from, it is a state that cannot be
 * constructed.
 */
export type SearchKey =
  | { command: Exclude<SearchCommand, "append"> }
  | { command: "append"; char: string };

/** Keys that are a modifier being held, not a keystroke to act on. */
const MODIFIER_KEYS = ["Shift", "Control", "Alt", "Meta", "CapsLock"];

/**
 * The whole key decision: null leaves the event to the composer.
 *
 * An inactive search answers only its start chord, so the composer types
 * normally. An active one owns every printable key — that is what makes the
 * search incremental, and it is why the composer's draft is untouched while
 * one runs.
 */
export function searchKeyAction(e: KeyChord, active: boolean): SearchKey | null {
  const chord = (key: string): boolean =>
    e.key.toLowerCase() === key && e.ctrlKey && !e.metaKey && !e.altKey;
  if (!active) return chord("s") ? { command: "start" } : null;
  if (chord("s")) return { command: "next" };
  if (chord("r")) return { command: "prev" };
  if (chord("g")) return { command: "cancel" };
  if (e.key === "Escape") return { command: "cancel" };
  if (e.key === "Enter") return { command: "accept" };
  if (e.key === "Backspace") return { command: "erase" };
  if (MODIFIER_KEYS.includes(e.key)) return null;
  if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
    return { command: "append", char: e.key };
  }
  return { command: "exit" };
}

/** A half-open span of a flattened text run. */
export interface Match {
  start: number;
  end: number;
}

/**
 * True when the query should match case-insensitively: isearch's own rule,
 * where an all-lowercase query folds case and any capital makes the search
 * literal, so the user asks for a case-sensitive match by typing one.
 */
export function foldsCase(query: string): boolean {
  return query === query.toLowerCase();
}

/**
 * Every occurrence of NEEDLE in HAYSTACK, in order, NON-OVERLAPPING: the
 * scan resumes past the whole match, so `aa` in `aaa` is one match and not
 * two.
 *
 * Overlap is not a preference here, it is unrepresentable. Each match is
 * drawn by wrapping its text in a `<mark>`, and two overlapping matches
 * would need two elements each containing part of the other — there is no
 * such tree. Every find-in-page counts this way for the same reason, and
 * Emacs' own lazy-highlight (the face this feature's non-current matches
 * stand in for) does too. `search-forward` resuming one character on is a
 * property of REPEATING a search from point, not of highlighting a set.
 */
export function findMatches(haystack: string, needle: string, foldCase: boolean): Match[] {
  if (needle === "") return [];
  const hay = foldCase ? haystack.toLowerCase() : haystack;
  const pin = foldCase ? needle.toLowerCase() : needle;
  const found: Match[] = [];
  for (let at = hay.indexOf(pin); at !== -1; at = hay.indexOf(pin, at + pin.length)) {
    found.push({ start: at, end: at + pin.length });
  }
  return found;
}

/** One node's contribution to a flattened text run. */
export interface Chunk<N> {
  node: N;
  text: string;
}

/** A flattened text run, plus the map back to the nodes that produced it. */
export interface TextIndex<N> {
  text: string;
  spans: Array<{ start: number; end: number; node: N }>;
}

/**
 * Flatten CHUNKS into one searchable string, remembering where each node's
 * text landed. Searching the concatenation rather than each node alone is
 * what lets a match cross an inline element: markdown renders `the config
 * file` with `config` in its own `<code>`, and a per-node search would miss
 * a query spanning the boundary.
 *
 * Generic over the node type (nothing but identity is touched), so it runs
 * against real text nodes and against plain objects under test alike.
 */
export function buildTextIndex<N>(chunks: readonly Chunk<N>[]): TextIndex<N> {
  const spans: Array<{ start: number; end: number; node: N }> = [];
  let text = "";
  for (const chunk of chunks) {
    if (chunk.text === "") continue;
    spans.push({ start: text.length, end: text.length + chunk.text.length, node: chunk.node });
    text += chunk.text;
  }
  return { text, spans };
}

/** A slice of one node's text, in that node's own offsets. */
export interface NodeRange<N> {
  node: N;
  start: number;
  end: number;
}

/**
 * The per-node slices MATCH covers, in document order. A match inside one
 * text node yields one range; one crossing an inline element yields a range
 * per node it touches.
 */
export function locateMatch<N>(index: TextIndex<N>, match: Match): Array<NodeRange<N>> {
  const ranges: Array<NodeRange<N>> = [];
  for (const span of index.spans) {
    if (span.end <= match.start || span.start >= match.end) continue;
    ranges.push({
      node: span.node,
      start: Math.max(match.start, span.start) - span.start,
      end: Math.min(match.end, span.end) - span.start,
    });
  }
  return ranges;
}

/**
 * A match's identity across a re-render: the feed item holding it, plus
 * which match within that item it is. Mirrors `expandedKeys`' `class:n`
 * (expand.ts) — an ordinal within a stable parent survives content
 * arriving above or below it, where a bare index into the whole feed would
 * silently slide onto a different match.
 */
export function matchKey(itemKey: string, ordinal: number): string {
  return `${itemKey}#${ordinal}`;
}

/** Step COUNT matches from CURRENT in DIR, wrapping like isearch does. */
export function stepIndex(count: number, current: number, dir: 1 | -1): number {
  if (count === 0) return -1;
  return (current + dir + count) % count;
}

/**
 * The match a fresh search starts on: the first one at or after the scroll
 * position the search began from, so `C-s` moves forward from where the
 * user is looking. Wraps to the first match when every match is above.
 */
export function firstAtOrAfter(tops: readonly number[], originTop: number): number {
  if (tops.length === 0) return -1;
  const at = tops.findIndex((top) => top >= originTop);
  return at === -1 ? 0 : at;
}

/**
 * The order ranges must be wrapped in: each node's own ranges back to
 * front, nodes otherwise left in the order they arrived.
 *
 * Wrapping splits a text node, which renumbers every offset after the
 * split — so a node carrying two matches (`aa` twice in one `<pre>` line)
 * must take its later match FIRST, or the earlier wrap invalidates the
 * offsets the later one was located at. Splitting one node never disturbs
 * another, so the grouping between nodes is free.
 */
export function wrapOrder<R extends { node: unknown; start: number }>(
  ranges: readonly R[],
): R[] {
  const byNode = new Map<unknown, R[]>();
  for (const range of ranges) {
    const seen = byNode.get(range.node);
    if (seen) seen.push(range);
    else byNode.set(range.node, [range]);
  }
  const ordered: R[] = [];
  for (const group of byNode.values()) {
    ordered.push(...[...group].sort((a, b) => b.start - a.start));
  }
  return ordered;
}

/**
 * The match to sit on: the one the user was already on when it is still
 * there, else the first at or after ORIGIN-TOP.
 *
 * The re-seat is what a query edit and a re-render share. A re-render can
 * take the current match away (its item re-rendered into different text),
 * and jumping to the feed's top would be the wrong answer to that — so both
 * fall back to where the user is looking rather than to the beginning.
 */
export function seatIndex(opts: {
  keys: readonly string[];
  currentKey: string | null;
  tops: readonly number[];
  originTop: number;
}): number {
  const at = opts.currentKey === null ? -1 : opts.keys.indexOf(opts.currentKey);
  return at === -1 ? firstAtOrAfter(opts.tops, opts.originTop) : at;
}

/**
 * The match a step lands on: one along from the current one, or the first
 * at or after ORIGIN-TOP when the current match is gone (a step is also how
 * a user resumes after new output replaced what they were on).
 */
export function stepFrom(opts: {
  keys: readonly string[];
  currentKey: string | null;
  tops: readonly number[];
  originTop: number;
  dir: 1 | -1;
}): number {
  const at = opts.currentKey === null ? -1 : opts.keys.indexOf(opts.currentKey);
  if (at === -1) return firstAtOrAfter(opts.tops, opts.originTop);
  return stepIndex(opts.keys.length, at, opts.dir);
}

/** The note naming regions the search could not reach, empty when none. */
export function unsearchedNotice(regions: number): string {
  if (regions <= 0) return "";
  return `  (${regions} unopened ${regions === 1 ? "fold" : "folds"} not searched)`;
}

/**
 * The status line: the query, where the user is among the matches, and what
 * the search could not see. Mirrors isearch's echo area, down to reporting
 * a failing search rather than going quiet.
 */
export function statusText(opts: {
  query: string;
  total: number;
  current: number;
  unsearched: number;
}): string {
  const tail = unsearchedNotice(opts.unsearched);
  if (opts.query === "") return `I-search:${tail}`;
  if (opts.total === 0) return `I-search: ${opts.query}  [no match]${tail}`;
  return `I-search: ${opts.query}  [${opts.current + 1}/${opts.total}]${tail}`;
}

/** A located match: its ranges, its stable key, and its owning item. */
interface LiveMatch {
  /** The match's own identity: `itemKey#ordinal` (see `matchKey`). */
  key: string;
  /** The owning item's reconcile key, which folds are identified within. */
  itemKey: string;
  ranges: Array<NodeRange<Text>>;
  item: HTMLElement;
}

/**
 * Incremental search bound to a feed. `handleKey` answers whether the key
 * was the search's (so the composer knows to stand down), and `refresh`
 * re-derives everything from the query, which is what a re-render calls.
 */
export class FeedSearch {
  private feed: HTMLElement;
  private status: (text: string) => void;
  private query = "";
  private searching = false;
  /** Where the feed sat when the search started, restored by an abort. */
  private originScrollTop = 0;
  /** The `<mark>` elements currently in the feed, all search-owned. */
  private marks: HTMLElement[] = [];
  /** Identity of the current match, carried across re-renders. */
  private currentKey: string | null = null;
  /** The last line handed to `status`, which `statusLine` answers with. */
  private lastStatus = "";

  constructor(feed: HTMLElement, status: (text: string) => void) {
    this.feed = feed;
    this.status = status;
  }

  /** The one write to the status line, so the echoed copy cannot drift. */
  private report(text: string): void {
    this.lastStatus = text;
    this.status(text);
  }

  /** True while a search is running. */
  active(): boolean {
    return this.searching;
  }

  /**
   * The status line as of the last thing the search did, for a host with its
   * own echo area to print. Answers what was computed rather than recomputing
   * it: every path that changes the search already ends in `status`, and a
   * fresh walk of the feed here would be a second full scan per host command
   * that could only agree with the first.
   */
  statusLine(): string {
    return this.lastStatus;
  }

  /**
   * Route KEY into the search, answering whether the search consumed it.
   * A consumed key is the search's alone — the composer must not also act
   * on it, which is what keeps `RET` from sending the draft prompt.
   */
  handleKey(e: KeyChord & { preventDefault(): void }): boolean {
    const action = searchKeyAction(e, this.searching);
    if (action === null) return false;
    switch (action.command) {
      case "start":
        this.start();
        break;
      case "next":
        this.step(1);
        break;
      case "prev":
        this.step(-1);
        break;
      case "cancel":
        this.cancel();
        break;
      case "accept":
        this.accept();
        break;
      case "erase":
        this.query = this.query.slice(0, -1);
        this.rerun(true);
        break;
      case "append":
        this.query += action.char;
        this.rerun(true);
        break;
      case "exit":
        // isearch hands the key on: the search ends here, and the key does
        // whatever it ordinarily would in the composer.
        this.accept();
        return false;
    }
    e.preventDefault();
    return true;
  }

  /**
   * Re-derive the highlight from the query. Called after every feed render,
   * because a render that rewrote an item's `innerHTML` took that item's
   * marks with it — the marks are output, never state.
   */
  refresh(): void {
    if (!this.searching) return;
    this.rerun(false);
  }

  /** Begin a search from the feed's current position. */
  private start(): void {
    this.searching = true;
    this.query = "";
    this.currentKey = null;
    this.originScrollTop = this.feed.scrollTop;
    this.rerun(false);
  }

  /** End the search keeping the current match on screen. */
  private accept(): void {
    this.searching = false;
    this.clearMarks();
    // The reveals stay, handed over as the user's own: they accepted a match
    // inside one, and re-closing it would hide the very thing they searched
    // for.
    this.keepRevealed();
    this.query = "";
    this.currentKey = null;
    this.report("");
  }

  /**
   * Abort: put the feed back exactly as the search found it. The scroll
   * position is the whole point — landing the user somewhere they never
   * chose is the failure this exists to prevent — and the folds the search
   * opened are re-closed for the same reason.
   */
  private cancel(): void {
    this.searching = false;
    this.clearMarks();
    this.closeRevealed();
    this.query = "";
    this.currentKey = null;
    // Last, after both mutations above have finished changing the layout the
    // restored position is quoted against.
    this.feed.scrollTop = this.originScrollTop;
    this.report("");
  }

  /**
   * Recompute matches, re-mark, and re-seat the current match.
   *
   * QUERY-CHANGED separates the two things that can invalidate a paint, and
   * they want opposite treatment. A keystroke changed the QUERY: re-seat from
   * the origin and take the view to whatever the refined query landed on. A
   * render changed the DOM underneath an unchanged query: keep the match the
   * user is on and leave their view alone.
   */
  private rerun(queryChanged: boolean): void {
    this.clearMarks();
    const matches = this.collect();
    this.paint(
      matches,
      seatIndex({
        keys: matches.map((m) => m.key),
        // A match's ordinal only means anything under a fixed query: `X#2` is
        // "the 3rd hit in item X", so carrying it across an edit would land
        // the user on the 3rd hit for the NEW query, which they have never
        // seen. An edit therefore drops the seat and searches forward from
        // the origin, the way isearch re-searches as you refine.
        currentKey: queryChanged ? null : this.currentKey,
        tops: this.itemTops(matches),
        // An edit walks forward from where the search STARTED. A re-render
        // has no new query to seat, and falls back to where the user is now,
        // since they may have scrolled since.
        originTop: queryChanged ? this.originScrollTop : this.feed.scrollTop,
      }),
      queryChanged,
    );
  }

  /** Step the current match by DIR and bring it into view. */
  private step(dir: 1 | -1): void {
    this.clearMarks();
    const matches = this.collect();
    this.paint(
      matches,
      stepFrom({
        keys: matches.map((m) => m.key),
        currentKey: this.currentKey,
        tops: this.itemTops(matches),
        originTop: this.feed.scrollTop,
        dir,
      }),
      true,
    );
  }

  /**
   * Mark MATCHES, make INDEX the current one, and reveal it.
   *
   * MOVE-VIEW is false for the re-paint a render triggers: the match has not
   * moved, so neither should the view. Re-centring it on every render would
   * fight the user for the scrollbar for as long as a turn keeps streaming,
   * and would undo any scrolling they did while reading a match.
   */
  private paint(matches: readonly LiveMatch[], index: number, moveView: boolean): void {
    this.currentKey = index === -1 ? null : matches[index].key;
    // Every range of every match, gathered before ANY of them is wrapped:
    // two matches can share one text node, and wrapping either one splits
    // that node out from under the other's offsets. wrapOrder is what makes
    // the whole set safe to apply.
    const ranges = matches.flatMap((match, i) =>
      match.ranges.map((range) => ({ ...range, current: i === index })),
    );
    const currentMarks: HTMLElement[] = [];
    for (const range of wrapOrder(ranges)) {
      const mark = wrapRange(
        range,
        range.current ? `${MARK_CLASS} ${CURRENT_CLASS}` : MARK_CLASS,
      );
      this.marks.push(mark);
      if (range.current) currentMarks.push(mark);
    }
    if (currentMarks.length > 0) {
      // The first in document order: a match crossing an inline element
      // wears several marks, and the view belongs at its start.
      const head = currentMarks.reduce((a, b) =>
        a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_PRECEDING ? b : a,
      );
      // Revealed on every paint, scrolled to only when the match moved: a
      // render can re-close a fold the search opened, and leaving the current
      // match buried inside it would be the silent miss this all guards.
      this.reveal(head, matches[index].item);
      if (moveView) head.scrollIntoView({ block: "center" });
    }
    this.report(
      statusText({
        query: this.query,
        total: matches.length,
        current: Math.max(0, index),
        unsearched: this.unsearchedRegions(),
      }),
    );
  }

  /**
   * Each match's top within the feed's SCROLLABLE CONTENT, which is the
   * frame `scrollTop` is quoted in and so the only one the two can be
   * compared in. Measured off the rects rather than read from `offsetTop`:
   * `#feed` is not a positioned element, so a feed item's `offsetParent` is
   * the body and its `offsetTop` carries the topbar's height as a silent
   * offset — which would seat every search a topbar too far down the feed.
   */
  private itemTops(matches: readonly LiveMatch[]): number[] {
    const base = this.feed.getBoundingClientRect().top - this.feed.scrollTop;
    return matches.map((m) => m.item.getBoundingClientRect().top - base);
  }

  /** Every match in the feed's DOM text, in document order. */
  private collect(): LiveMatch[] {
    if (this.query === "") return [];
    const fold = foldsCase(this.query);
    const found: LiveMatch[] = [];
    for (const item of this.feed.querySelectorAll<HTMLElement>(".feed-item")) {
      const key = item.dataset.key ?? "";
      const index = buildTextIndex(textChunksIn(item));
      findMatches(index.text, this.query, fold).forEach((match, ordinal) => {
        found.push({
          key: matchKey(key, ordinal),
          itemKey: key,
          ranges: locateMatch(index, match),
          item,
        });
      });
    }
    return found;
  }

  /** How many regions hold conversation text the DOM walk cannot reach. */
  private unsearchedRegions(): number {
    return (
      this.feed.querySelectorAll(CLOSED_FOLD_SELECTOR).length +
      this.feed.querySelectorAll(INACTIVE_TAB_SELECTOR).length
    );
  }

  /**
   * Bring MARK's text into view, marking every fold it opens as the SEARCH'S
   * rather than the user's: a capped ancestor takes `REVEAL_CLASS` (never
   * `EXPANDED_CLASS`) and a collapsed `<details>` ancestor takes `REVEAL_ATTR`
   * alongside its `open`.
   *
   * The separate marker is what makes the abort honest. `EXPANDED_CLASS` is
   * the USER's "I opened this", and the renderer persists it across a
   * re-render on purpose (`expandedKeys`/`applyExpanded`) — so a search that
   * reached for the same class would have its reveal laundered into user
   * state, indistinguishable from a click, and the abort could no longer tell
   * what it was entitled to close. With a marker of its own, "what did the
   * search open" is a question the DOM answers directly, needing no
   * bookkeeping that a re-render could invalidate.
   *
   * Runs on every paint, including the ones a re-render drives: a render
   * rebuilds the item from its HTML, dropping the reveal, and re-applying it
   * here is what keeps the current match visible under a streaming turn.
   */
  private reveal(mark: HTMLElement, item: HTMLElement): void {
    for (let el = mark.parentElement; el && el !== item; el = el.parentElement) {
      if (el instanceof HTMLDetailsElement) {
        if (el.open) continue;
        el.open = true;
        el.setAttribute(REVEAL_ATTR, "");
        continue;
      }
      // A section the user already expanded is left alone: it needs no reveal,
      // and marking it would make the abort re-cap something the search never
      // opened.
      if (isCappedSection(el.classList) && !el.classList.contains(EXPANDED_CLASS)) {
        el.classList.add(REVEAL_CLASS);
      }
    }
  }

  /**
   * Put back every fold the search opened, asking the feed as it stands NOW
   * rather than a list built when it opened them. Nothing to invalidate, so a
   * re-render in the middle of a search cannot strand a fold open.
   */
  private closeRevealed(): void {
    for (const el of this.feed.querySelectorAll(`.${REVEAL_CLASS}`)) {
      el.classList.remove(REVEAL_CLASS);
    }
    for (const el of this.feed.querySelectorAll(`details[${REVEAL_ATTR}]`)) {
      (el as HTMLDetailsElement).open = false;
      el.removeAttribute(REVEAL_ATTR);
    }
  }

  /**
   * Hand the search's reveals over to the user, who accepted a match inside
   * them: the capped ones become an ordinary `EXPANDED_CLASS` expansion, so
   * the renderer carries them like any the user clicked open, and the opened
   * `<details>` simply keep their `open` with the marker dropped.
   */
  private keepRevealed(): void {
    for (const el of this.feed.querySelectorAll(`.${REVEAL_CLASS}`)) {
      el.classList.remove(REVEAL_CLASS);
      el.classList.add(EXPANDED_CLASS);
    }
    for (const el of this.feed.querySelectorAll(`details[${REVEAL_ATTR}]`)) {
      el.removeAttribute(REVEAL_ATTR);
    }
  }

  /** Unwrap every mark, healing the text nodes it split. */
  private clearMarks(): void {
    for (const mark of this.marks) {
      const parent = mark.parentNode;
      // A mark whose item re-rendered is already gone with it, which is the
      // ordinary case rather than a broken invariant.
      if (!parent) continue;
      mark.replaceWith(...mark.childNodes);
      parent.normalize();
    }
    this.marks = [];
  }
}

/** Wrap RANGE's slice of its text node in a `<mark>`, answering the mark. */
function wrapRange(range: NodeRange<Text>, className: string): HTMLElement {
  const tail = range.node.splitText(range.start);
  tail.splitText(range.end - range.start);
  const mark = document.createElement("mark");
  mark.className = className;
  tail.replaceWith(mark);
  mark.appendChild(tail);
  return mark;
}

/**
 * ITEM's text nodes in document order. A TreeWalker is structural, so this
 * descends into `display: none` folds and collapsed `<details>` alike —
 * which is the point: that text is in the conversation, so it is searched.
 */
function textChunksIn(item: HTMLElement): Array<Chunk<Text>> {
  const walker = document.createTreeWalker(item, NodeFilter.SHOW_TEXT, {
    acceptNode: (node) => {
      const parent = node.parentElement;
      if (!parent || parent.closest(SKIP_SELECTOR)) return NodeFilter.FILTER_REJECT;
      return NodeFilter.FILTER_ACCEPT;
    },
  });
  const chunks: Array<Chunk<Text>> = [];
  for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
    const text = node as Text;
    chunks.push({ node: text, text: text.data });
  }
  return chunks;
}

/** What a hook is planted on: `window`, or a plain object under test. */
export type SearchHost = Record<string, unknown>;

/**
 * Plant the search hook on TARGET, driving SEARCH and answering the status
 * line. The host has no composer to type into and cannot deliver keys to
 * the page, so it hands whole commands in and echoes what comes back.
 */
export function installSearchHook(target: SearchHost, search: FeedSearch): void {
  target[SEARCH_HOOK] = (command: string, char?: string): string => {
    search.handleKey({
      key: command === "append" ? (char ?? "") : hookKeyFor(command),
      ctrlKey: HOOK_CTRL_COMMANDS.includes(command),
      metaKey: false,
      altKey: false,
      shiftKey: false,
      preventDefault: () => undefined,
    });
    return search.statusLine();
  };
}

/** Commands the hook spells as a control chord. */
const HOOK_CTRL_COMMANDS = ["start", "next", "prev", "cancel"];

/** The key a hook command arrives as, so one decision table serves both. */
function hookKeyFor(command: string): string {
  switch (command) {
    case "start":
    case "next":
      return "s";
    case "prev":
      return "r";
    case "cancel":
      return "g";
    case "accept":
      return "Enter";
    case "erase":
      return "Backspace";
    default:
      throw new Error(`unknown search command: ${command}`);
  }
}
