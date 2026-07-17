// @vitest-environment jsdom
//
// The pure helpers below need no DOM and would run under either environment.
// `FeedSearch` itself is the reason for jsdom: its whole job is to mark, to
// reveal, and to put the feed back, and none of that is assertable against a
// plain object. The rest of the suite stays on the node environment — this
// docblock is per-file on purpose.
import { afterEach, describe, expect, it } from "vitest";
import { EXPANDED_CLASS } from "../src/expand.js";
import {
  CURRENT_CLASS,
  FeedSearch,
  KeyChord,
  MARK_CLASS,
  REVEAL_ATTR,
  REVEAL_CLASS,
  SEARCH_HOOK,
  SearchHost,
  buildTextIndex,
  findMatches,
  firstAtOrAfter,
  foldsCase,
  installSearchHook,
  locateMatch,
  matchKey,
  searchKeyAction,
  seatIndex,
  statusText,
  stepFrom,
  stepIndex,
  unsearchedNotice,
  wrapOrder,
} from "../src/search.js";

/** A keystroke as the composer delivers it, modifiers off unless named. */
function chord(key: string, mods: Partial<KeyChord> = {}): KeyChord {
  return { key, ctrlKey: false, metaKey: false, altKey: false, shiftKey: false, ...mods };
}

describe("searchKeyAction", () => {
  it("starts a search on C-s when none is running", () => {
    expect(searchKeyAction(chord("s", { ctrlKey: true }), false)).toEqual({ command: "start" });
  });

  it("leaves a printable key to the composer when no search is running", () => {
    expect(searchKeyAction(chord("s"), false)).toBeNull();
  });

  it("does not start a search on the step-back chord", () => {
    expect(searchKeyAction(chord("r", { ctrlKey: true }), false)).toBeNull();
  });

  it("does not read Cmd-S as the start chord, since that is the embedder's save", () => {
    expect(searchKeyAction(chord("s", { metaKey: true }), false)).toBeNull();
  });

  it("steps to the next match on C-s while searching", () => {
    expect(searchKeyAction(chord("s", { ctrlKey: true }), true)).toEqual({ command: "next" });
  });

  it("steps to the previous match on C-r while searching", () => {
    expect(searchKeyAction(chord("r", { ctrlKey: true }), true)).toEqual({ command: "prev" });
  });

  it("cancels on C-g, the Emacs abort", () => {
    expect(searchKeyAction(chord("g", { ctrlKey: true }), true)).toEqual({ command: "cancel" });
  });

  it("cancels on Escape, the browser abort", () => {
    expect(searchKeyAction(chord("Escape"), true)).toEqual({ command: "cancel" });
  });

  it("accepts on Enter rather than letting the composer send the draft", () => {
    expect(searchKeyAction(chord("Enter"), true)).toEqual({ command: "accept" });
  });

  it("erases the last query character on Backspace", () => {
    expect(searchKeyAction(chord("Backspace"), true)).toEqual({ command: "erase" });
  });

  it("appends a printable key to the query instead of to the composer", () => {
    expect(searchKeyAction(chord("f"), true)).toEqual({ command: "append", char: "f" });
  });

  it("appends a capital as typed, so the query can ask for a literal match", () => {
    expect(searchKeyAction(chord("F", { shiftKey: true }), true)).toEqual({
      command: "append",
      char: "F",
    });
  });

  it("ignores a modifier being held, which is not a keystroke to act on", () => {
    expect(searchKeyAction(chord("Shift", { shiftKey: true }), true)).toBeNull();
  });

  it("exits on a key the search does not speak, handing the key on as isearch does", () => {
    expect(searchKeyAction(chord("ArrowLeft"), true)).toEqual({ command: "exit" });
  });
});

describe("foldsCase", () => {
  it("folds case for an all-lowercase query", () => {
    expect(foldsCase("config")).toBe(true);
  });

  it("goes literal once the query carries a capital", () => {
    expect(foldsCase("Config")).toBe(false);
  });
});

describe("findMatches", () => {
  it("finds every occurrence in order", () => {
    expect(findMatches("ab ab", "ab", false)).toEqual([
      { start: 0, end: 2 },
      { start: 3, end: 5 },
    ]);
  });

  it("finds nothing for an empty needle, so a fresh search highlights nothing", () => {
    expect(findMatches("anything", "", true)).toEqual([]);
  });

  it("matches across case when folding", () => {
    expect(findMatches("Config", "config", true)).toEqual([{ start: 0, end: 6 }]);
  });

  it("skips a case-mismatched run when not folding", () => {
    expect(findMatches("Config", "config", false)).toEqual([]);
  });

  it("never overlaps two matches, which no pair of marks could represent", () => {
    // Overlap is unrepresentable: each match is drawn by wrapping its text in
    // a <mark>, and two overlapping marks would each have to contain part of
    // the other. Emitting them crashed the wrap outright (splitText past the
    // shortened node), which leaked the keystroke into the composer.
    expect(findMatches("aaa", "aa", false)).toEqual([{ start: 0, end: 2 }]);
  });

  it("resumes the scan past a whole match rather than one character on", () => {
    expect(findMatches("banana", "ana", false)).toEqual([{ start: 1, end: 4 }]);
  });

  it("still finds runs that merely abut, which do not overlap", () => {
    expect(findMatches("aaaa", "aa", false)).toEqual([
      { start: 0, end: 2 },
      { start: 2, end: 4 },
    ]);
  });
});

describe("buildTextIndex", () => {
  it("concatenates the chunks into one searchable run", () => {
    const index = buildTextIndex([
      { node: "a", text: "the " },
      { node: "b", text: "config" },
    ]);
    expect(index.text).toBe("the config");
  });

  it("maps each chunk to the span of the run it produced", () => {
    const index = buildTextIndex([
      { node: "a", text: "the " },
      { node: "b", text: "config" },
    ]);
    expect(index.spans).toEqual([
      { start: 0, end: 4, node: "a" },
      { start: 4, end: 10, node: "b" },
    ]);
  });

  it("drops an empty chunk, which can hold no match", () => {
    const index = buildTextIndex([
      { node: "a", text: "x" },
      { node: "b", text: "" },
    ]);
    expect(index.spans).toEqual([{ start: 0, end: 1, node: "a" }]);
  });
});

describe("locateMatch", () => {
  it("yields one range for a match inside a single node", () => {
    const index = buildTextIndex([
      { node: "a", text: "the " },
      { node: "b", text: "config" },
    ]);
    expect(locateMatch(index, { start: 4, end: 10 })).toEqual([{ node: "b", start: 0, end: 6 }]);
  });

  it("yields a range per node for a match crossing an inline element", () => {
    // `the config file` with `config` in its own <code>: one text node each.
    const index = buildTextIndex([
      { node: "a", text: "the " },
      { node: "b", text: "config" },
      { node: "c", text: " file" },
    ]);
    expect(locateMatch(index, { start: 0, end: 15 })).toEqual([
      { node: "a", start: 0, end: 4 },
      { node: "b", start: 0, end: 6 },
      { node: "c", start: 0, end: 5 },
    ]);
  });

  it("leaves out a node the match does not touch", () => {
    const index = buildTextIndex([
      { node: "a", text: "the " },
      { node: "b", text: "config" },
    ]);
    expect(locateMatch(index, { start: 0, end: 3 })).toEqual([{ node: "a", start: 0, end: 3 }]);
  });

  it("clips a range to the part of the node the match covers", () => {
    const index = buildTextIndex([{ node: "a", text: "reconfigure" }]);
    expect(locateMatch(index, { start: 2, end: 8 })).toEqual([{ node: "a", start: 2, end: 8 }]);
  });
});

describe("matchKey", () => {
  it("keys a match by its item and its ordinal within that item", () => {
    expect(matchKey("user-turn:3", 1)).toBe("user-turn:3#1");
  });
});

describe("reveal markers", () => {
  it("never reveals under the class the user's own expansion uses", () => {
    // The renderer persists EXPANDED_CLASS across a re-render on purpose. A
    // search reaching for the same class would have its reveal laundered into
    // user state, and the abort could no longer tell what it may close.
    expect(REVEAL_CLASS).not.toBe(EXPANDED_CLASS);
  });
});

describe("wrapOrder", () => {
  it("wraps a node's own ranges back to front, so an earlier wrap cannot stale a later one", () => {
    // Two matches in one <pre> line: wrapping the first splits the node, and
    // the second's offsets were located against the unsplit text.
    const ordered = wrapOrder([
      { node: "a", start: 0 },
      { node: "a", start: 6 },
    ]);
    expect(ordered).toEqual([
      { node: "a", start: 6 },
      { node: "a", start: 0 },
    ]);
  });

  it("leaves ranges on different nodes in the order they arrived", () => {
    // Splitting one text node never disturbs another's offsets.
    const ordered = wrapOrder([
      { node: "a", start: 0 },
      { node: "b", start: 0 },
    ]);
    expect(ordered).toEqual([
      { node: "a", start: 0 },
      { node: "b", start: 0 },
    ]);
  });

  it("keeps every range, since each one is a mark that must be drawn", () => {
    const ordered = wrapOrder([
      { node: "a", start: 0 },
      { node: "b", start: 2 },
      { node: "a", start: 4 },
    ]);
    expect(ordered).toHaveLength(3);
  });
});

describe("seatIndex", () => {
  it("keeps the user on the match they were already on", () => {
    expect(
      seatIndex({ keys: ["i#0", "i#1"], currentKey: "i#1", tops: [0, 0], originTop: 0 }),
    ).toBe(1);
  });

  it("re-seats from where the user is looking when the match they were on is gone", () => {
    // The item holding it re-rendered into different text mid-search.
    expect(
      seatIndex({ keys: ["i#0", "i#1"], currentKey: "gone#3", tops: [10, 400], originTop: 300 }),
    ).toBe(1);
  });

  it("seats a fresh search with no current match from the origin", () => {
    expect(
      seatIndex({ keys: ["i#0", "i#1"], currentKey: null, tops: [10, 400], originTop: 300 }),
    ).toBe(1);
  });

  it("has nothing to seat on when the query matches nothing", () => {
    expect(seatIndex({ keys: [], currentKey: null, tops: [], originTop: 0 })).toBe(-1);
  });
});

describe("stepFrom", () => {
  it("steps one along from the current match", () => {
    expect(
      stepFrom({ keys: ["i#0", "i#1"], currentKey: "i#0", tops: [0, 0], originTop: 0, dir: 1 }),
    ).toBe(1);
  });

  it("steps back one from the current match", () => {
    expect(
      stepFrom({ keys: ["i#0", "i#1"], currentKey: "i#1", tops: [0, 0], originTop: 0, dir: -1 }),
    ).toBe(0);
  });

  it("wraps rather than sticking at the last match", () => {
    expect(
      stepFrom({ keys: ["i#0", "i#1"], currentKey: "i#1", tops: [0, 0], originTop: 0, dir: 1 }),
    ).toBe(0);
  });

  it("resumes from where the user is looking when new output replaced the current match", () => {
    expect(
      stepFrom({
        keys: ["i#0", "i#1"],
        currentKey: "gone#0",
        tops: [10, 400],
        originTop: 300,
        dir: 1,
      }),
    ).toBe(1);
  });
});

describe("stepIndex", () => {
  it("steps to the next match", () => {
    expect(stepIndex(3, 0, 1)).toBe(1);
  });

  it("wraps past the last match back to the first", () => {
    expect(stepIndex(3, 2, 1)).toBe(0);
  });

  it("wraps past the first match back to the last", () => {
    expect(stepIndex(3, 0, -1)).toBe(2);
  });

  it("has nowhere to step with no matches", () => {
    expect(stepIndex(0, -1, 1)).toBe(-1);
  });
});

describe("firstAtOrAfter", () => {
  it("starts on the first match at or after where the user is looking", () => {
    expect(firstAtOrAfter([10, 200, 400], 150)).toBe(1);
  });

  it("starts on a match sitting exactly at the origin", () => {
    expect(firstAtOrAfter([10, 200, 400], 200)).toBe(1);
  });

  it("wraps to the first match when every match is above the origin", () => {
    expect(firstAtOrAfter([10, 200], 900)).toBe(0);
  });

  it("has no match to start on when there are none", () => {
    expect(firstAtOrAfter([], 0)).toBe(-1);
  });
});

describe("unsearchedNotice", () => {
  it("says nothing when the search could reach everything", () => {
    expect(unsearchedNotice(0)).toBe("");
  });

  it("names a single unreachable fold in the singular", () => {
    expect(unsearchedNotice(1)).toContain("1 unopened fold ");
  });

  it("names several unreachable folds in the plural", () => {
    expect(unsearchedNotice(3)).toContain("3 unopened folds ");
  });
});

describe("statusText", () => {
  it("reports where the user is among the matches, counting from one", () => {
    expect(statusText({ query: "cfg", total: 7, current: 2, unsearched: 0 })).toBe(
      "I-search: cfg  [3/7]",
    );
  });

  it("says a search is failing rather than going quiet", () => {
    expect(statusText({ query: "zzz", total: 0, current: 0, unsearched: 0 })).toBe(
      "I-search: zzz  [no match]",
    );
  });

  it("shows the bare prompt before anything is typed", () => {
    expect(statusText({ query: "", total: 0, current: 0, unsearched: 0 })).toBe("I-search:");
  });

  it("carries the unreachable-fold count, so a partial search never reads as total", () => {
    expect(statusText({ query: "cfg", total: 1, current: 0, unsearched: 2 })).toBe(
      "I-search: cfg  [1/1]  (2 unopened folds not searched)",
    );
  });
});

// jsdom implements no layout, so it ships no `scrollIntoView` at all. The
// search calls it to bring the current match into view; a no-op stub is the
// jsdom gap being papered over, not any behavior of the search's.
Element.prototype.scrollIntoView = (): void => undefined;

afterEach(() => {
  document.body.innerHTML = "";
});

/** A feed holding one `.feed-item` per html, as FeedRenderer lays them out. */
function feedWith(...items: string[]): HTMLElement {
  const feed = document.createElement("main");
  feed.id = "feed";
  items.forEach((html, i) => {
    const el = document.createElement("div");
    el.className = "feed-item";
    el.dataset.key = `item:${i}`;
    el.innerHTML = html;
    feed.appendChild(el);
  });
  document.body.appendChild(feed);
  return feed;
}

/** A search over FEED, plus a reader for whatever it last put on the status. */
function searchOn(feed: HTMLElement): { search: FeedSearch; status: () => string } {
  let last = "";
  const search = new FeedSearch(feed, (text) => {
    last = text;
  });
  return { search, status: () => last };
}

/** Deliver one keystroke, answering whether the search consumed it. */
function press(search: FeedSearch, key: string, mods: Partial<KeyChord> = {}): boolean {
  return search.handleKey({ ...chord(key, mods), preventDefault: () => undefined });
}

/** Start a search and type QUERY into it, the way a user does. */
function searchFor(search: FeedSearch, query: string): void {
  press(search, "s", { ctrlKey: true });
  for (const ch of query) press(search, ch);
}

describe("FeedSearch marking", () => {
  it("wraps every match in a mark", () => {
    const feed = feedWith("<p>needle and needle</p>");
    searchFor(searchOn(feed).search, "needle");
    expect(feed.querySelectorAll(`.${MARK_CLASS}`)).toHaveLength(2);
  });

  it("marks the current match distinctly from the others", () => {
    const feed = feedWith("<p>needle and needle</p>");
    searchFor(searchOn(feed).search, "needle");
    expect(feed.querySelectorAll(`.${CURRENT_CLASS}`)).toHaveLength(1);
  });

  it("wraps a match crossing an inline element once per text node it touches", () => {
    // Markdown renders `the config file` with `config` in its own <code>.
    const feed = feedWith("<p>the <code>config</code> file</p>");
    searchFor(searchOn(feed).search, "the config file");
    expect(feed.querySelectorAll(`.${MARK_CLASS}`)).toHaveLength(3);
  });

  it("wraps two matches sharing one text node without stranding either", () => {
    // Wrapping splits the node, so the second match's offsets were located
    // against text that no longer exists unless the order is right.
    const feed = feedWith("<pre>foo bar foo</pre>");
    searchFor(searchOn(feed).search, "foo");
    expect(feed.querySelectorAll(`.${MARK_CLASS}`)).toHaveLength(2);
  });

  it("survives a query that self-overlaps rather than throwing mid-keystroke", () => {
    // `--` against `---` once crashed splitText and leaked the keystroke into
    // the composer. Reachable from any feed carrying a diff.
    const feed = feedWith("<pre>---</pre>");
    const { search } = searchOn(feed);
    expect(() => searchFor(search, "--")).not.toThrow();
    expect(feed.querySelectorAll(`.${MARK_CLASS}`)).toHaveLength(1);
  });

  it("finds text inside a display fold, which is in the DOM but not on screen", () => {
    const feed = feedWith('<div class="tool-input agent-input"><pre class="agent-json">needle</pre></div>');
    searchFor(searchOn(feed).search, "needle");
    expect(feed.querySelectorAll(`.${MARK_CLASS}`)).toHaveLength(1);
  });

  it("leaves a mounted widget's subtree alone, since the widget owns it", () => {
    const feed = feedWith('<div class="chess-game" data-game-file="/x.pgn">needle</div>');
    searchFor(searchOn(feed).search, "needle");
    expect(feed.querySelectorAll(`.${MARK_CLASS}`)).toHaveLength(0);
  });

  it("counts the regions it could not reach, rather than reporting only what it found", () => {
    const feed = feedWith(
      '<p>needle</p><div class="agent-activity" data-panel-toggle="a"><div class="ticker">x</div></div>',
    );
    const { search, status } = searchOn(feed);
    searchFor(search, "needle");
    expect(status()).toContain("1 unopened fold not searched");
  });
});

describe("FeedSearch aborting", () => {
  it("restores the scroll position the search started from", () => {
    const feed = feedWith("<p>alpha</p>", "<p>needle</p>");
    feed.scrollTop = 500;
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    feed.scrollTop = 1200; // the search took the view to its match
    press(search, "g", { ctrlKey: true });
    expect(feed.scrollTop).toBe(500);
  });

  it("re-closes a fold the search opened to show a match", () => {
    const feed = feedWith('<div class="tool-input"><pre>needle</pre></div>');
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    expect(feed.querySelectorAll(`.${REVEAL_CLASS}`)).toHaveLength(1);
    press(search, "g", { ctrlKey: true });
    expect(feed.querySelectorAll(`.${REVEAL_CLASS}`)).toHaveLength(0);
  });

  it("leaves alone a section the user had already expanded themselves", () => {
    const feed = feedWith(`<div class="tool-input ${EXPANDED_CLASS}"><pre>needle</pre></div>`);
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    press(search, "g", { ctrlKey: true });
    expect(feed.querySelector(".tool-input")!.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("leaves the feed's text exactly as it found it", () => {
    const feed = feedWith("<pre>foo bar foo</pre>");
    const before = feed.textContent;
    const { search } = searchOn(feed);
    searchFor(search, "foo");
    press(search, "g", { ctrlKey: true });
    expect(feed.textContent).toBe(before);
  });

  it("heals the text nodes it split, rather than leaving the run in pieces", () => {
    // normalize() merges the splits back; a <pre> left in fragments would
    // break any later search that must match across the seam.
    const feed = feedWith("<pre>foo bar foo</pre>");
    const { search } = searchOn(feed);
    searchFor(search, "foo");
    press(search, "g", { ctrlKey: true });
    expect(feed.querySelector("pre")!.childNodes).toHaveLength(1);
  });
});

describe("FeedSearch accepting", () => {
  it("leaves the view on the match it accepted rather than restoring", () => {
    const feed = feedWith("<p>needle</p>");
    feed.scrollTop = 500;
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    feed.scrollTop = 1200;
    press(search, "Enter");
    expect(feed.scrollTop).toBe(1200);
  });

  it("hands the search's reveal over as the user's own expansion", () => {
    // The renderer persists EXPANDED_CLASS across a re-render; the search's
    // own marker it knows nothing about.
    const feed = feedWith('<div class="tool-input"><pre>needle</pre></div>');
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    press(search, "Enter");
    const section = feed.querySelector(".tool-input")!;
    expect(section.classList.contains(EXPANDED_CLASS)).toBe(true);
    expect(section.classList.contains(REVEAL_CLASS)).toBe(false);
  });

  it("takes its marks back out of the feed", () => {
    const feed = feedWith("<p>needle</p>");
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    press(search, "Enter");
    expect(feed.querySelectorAll(`.${MARK_CLASS}`)).toHaveLength(0);
  });
});

describe("FeedSearch revealing", () => {
  it("reveals a capped section under its own class, never the user's", () => {
    const feed = feedWith('<div class="tool-input"><pre>needle</pre></div>');
    searchFor(searchOn(feed).search, "needle");
    const section = feed.querySelector(".tool-input")!;
    expect(section.classList.contains(REVEAL_CLASS)).toBe(true);
    expect(section.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("opens a collapsed details holding the match", () => {
    const feed = feedWith("<details class=thinking><summary>Thinking</summary><pre>needle</pre></details>");
    searchFor(searchOn(feed).search, "needle");
    expect(feed.querySelector("details")!.open).toBe(true);
  });

  it("marks a details it opened, so an abort knows the open was its doing", () => {
    const feed = feedWith("<details class=thinking><summary>Thinking</summary><pre>needle</pre></details>");
    searchFor(searchOn(feed).search, "needle");
    expect(feed.querySelector("details")!.hasAttribute(REVEAL_ATTR)).toBe(true);
  });

  it("leaves a details the user already opened unmarked, so an abort will not close it", () => {
    const feed = feedWith("<details open><summary>Thinking</summary><pre>needle</pre></details>");
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    press(search, "g", { ctrlKey: true });
    expect(feed.querySelector("details")!.open).toBe(true);
  });
});

describe("FeedSearch under a re-render", () => {
  /** What FeedRenderer.render does to an item whose HTML changed. */
  const rerenderItem = (feed: HTMLElement, key: string, html: string): void => {
    feed.querySelector<HTMLElement>(`.feed-item[data-key="${key}"]`)!.innerHTML = html;
  };

  it("re-applies the highlight a render destroyed", () => {
    const feed = feedWith("<p>needle</p>");
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    rerenderItem(feed, "item:0", "<p>needle</p><p>streamed in</p>");
    expect(feed.querySelectorAll(`.${MARK_CLASS}`)).toHaveLength(0);
    search.refresh(); // what Actions.onRendered calls
    expect(feed.querySelectorAll(`.${MARK_CLASS}`)).toHaveLength(1);
  });

  it("keeps the user on the match they were on when new output arrives", () => {
    const feed = feedWith("<p>needle one</p>", "<p>needle two</p>");
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    press(search, "s", { ctrlKey: true }); // step to the second match
    const moved = document.createElement("div");
    moved.className = "feed-item";
    moved.dataset.key = "item:2";
    moved.innerHTML = "<p>a fresh needle streamed in</p>";
    feed.appendChild(moved);
    search.refresh();
    expect(feed.querySelector(`.${CURRENT_CLASS}`)!.closest(".feed-item")).toHaveProperty(
      "dataset.key",
      "item:1",
    );
  });

  it("re-opens a fold a render closed under the current match", () => {
    // A done <details> rebuilds closed, which would bury the match the user
    // is standing on.
    const feed = feedWith("<details class=thinking><summary>T</summary><pre>needle</pre></details>");
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    rerenderItem(feed, "item:0", "<details class=thinking><summary>T</summary><pre>needle</pre></details>");
    expect(feed.querySelector("details")!.open).toBe(false);
    search.refresh();
    expect(feed.querySelector("details")!.open).toBe(true);
  });

  it("reports the match count the new output brought with it", () => {
    const feed = feedWith("<p>needle</p>");
    const { search, status } = searchOn(feed);
    searchFor(search, "needle");
    expect(status()).toContain("[1/1]");
    rerenderItem(feed, "item:0", "<p>needle</p><p>another needle</p>");
    search.refresh();
    expect(status()).toContain("[1/2]");
  });
});

describe("FeedSearch and the composer", () => {
  it("consumes RET, so accepting a match cannot also send the draft", () => {
    const feed = feedWith("<p>needle</p>");
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    expect(press(search, "Enter")).toBe(true);
  });

  it("leaves RET to the composer once no search is running", () => {
    const feed = feedWith("<p>needle</p>");
    const { search } = searchOn(feed);
    expect(press(search, "Enter")).toBe(false);
  });

  it("hands on a key it does not speak, ending the search as isearch does", () => {
    const feed = feedWith("<p>needle</p>");
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    expect(press(search, "ArrowLeft")).toBe(false);
    expect(search.active()).toBe(false);
  });

  it("stops answering the status line once the search is over", () => {
    // The Emacs host echoes statusLine(); a stale failing search there would
    // be the only surface the user sees, since composer=0 hides this page's.
    const feed = feedWith("<p>needle</p>");
    const { search } = searchOn(feed);
    searchFor(search, "needle");
    press(search, "g", { ctrlKey: true });
    expect(search.statusLine()).toBe("");
  });
});

/** A FeedSearch stand-in recording what the hook routed into it. */
function fakeSearch(): { search: FeedSearch; keys: KeyChord[] } {
  const keys: KeyChord[] = [];
  const search = {
    handleKey: (e: KeyChord) => {
      keys.push(e);
      return true;
    },
    statusLine: () => "I-search: x  [1/1]",
  };
  return { search: search as unknown as FeedSearch, keys };
}

describe("installSearchHook", () => {
  it("plants the hook under the name the Emacs host will call", () => {
    const target: SearchHost = {};
    installSearchHook(target, fakeSearch().search);
    expect(typeof target[SEARCH_HOOK]).toBe("function");
  });

  it("answers the status line, since the host has no echo area of this page's", () => {
    const target: SearchHost = {};
    installSearchHook(target, fakeSearch().search);
    expect((target[SEARCH_HOOK] as (c: string) => string)("next")).toBe("I-search: x  [1/1]");
  });

  it("routes a step command in as the control chord the composer would send", () => {
    const target: SearchHost = {};
    const { search, keys } = fakeSearch();
    installSearchHook(target, search);
    (target[SEARCH_HOOK] as (c: string) => string)("next");
    expect(keys[0]).toMatchObject({ key: "s", ctrlKey: true });
  });

  it("routes an appended character in as the plain keystroke it stands for", () => {
    const target: SearchHost = {};
    const { search, keys } = fakeSearch();
    installSearchHook(target, search);
    (target[SEARCH_HOOK] as (c: string, ch?: string) => string)("append", "f");
    expect(keys[0]).toMatchObject({ key: "f", ctrlKey: false });
  });

  it("raises on a command it does not speak rather than searching for something else", () => {
    const target: SearchHost = {};
    installSearchHook(target, fakeSearch().search);
    expect(() => (target[SEARCH_HOOK] as (c: string) => string)("sideways")).toThrow(
      /unknown search command/,
    );
  });
});
