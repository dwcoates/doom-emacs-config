import { describe, expect, it } from "vitest";
import { EXPANDED_CLASS } from "../src/expand.js";
import {
  FeedSearch,
  KeyChord,
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
