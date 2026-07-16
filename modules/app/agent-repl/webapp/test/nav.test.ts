/**
 * The feed's keyboard cycle.
 *
 * jsdom is not in the dep tree, so (as in scroll.test.ts) the DOM-facing
 * pieces are the thin assembly and every DECISION they make is a pure
 * helper asserted here against plain objects.
 */
import { describe, expect, it } from "vitest";
import {
  CLASS_MODIFIERS,
  NAV_CLASSES,
  NAV_CURRENT_CLASS,
  NAV_HOOK,
  type NavItem,
  type NavKeyEvent,
  cycleDecision,
  cycleTarget,
  hasToken,
  isNavClass,
  matchingIndexes,
  navChord,
  navTokensForItem,
  nextCursor,
  seedIndex,
} from "../src/nav.js";
import { TextItem, ThinkingItem, ToolItem, UserTurnItem } from "../src/store.js";

/** A feed wrapper as a cycle decision sees it. */
const item = (key: string, tokens: string, top: number): NavItem => ({ key, tokens, top });

/** A key event with no modifiers held, for chord tests to vary. */
const key = (over: Partial<NavKeyEvent> = {}): NavKeyEvent => ({
  code: "KeyJ",
  ctrlKey: false,
  altKey: false,
  shiftKey: false,
  metaKey: false,
  ...over,
});

const userTurn = (): UserTurnItem => ({
  kind: "user-turn",
  requestId: "r1",
  content: [{ type: "text", text: "hi" }],
  ts: "",
});

const text = (): TextItem => ({
  kind: "text",
  blockId: "b1",
  messageId: "m1",
  text: "hi",
  done: true,
  ts: "",
});

const thinking = (): ThinkingItem => ({
  kind: "thinking",
  blockId: "k1",
  messageId: "m1",
  text: "hmm",
  done: true,
});

const tool = (): ToolItem => ({
  kind: "tool",
  toolUseId: "t1",
  toolName: "Bash",
  messageId: "m1",
  inputJson: "{}",
  inputDone: true,
  ts: "",
});

describe("navTokensForItem", () => {
  it("marks a user turn as a prompt stop", () => {
    expect(navTokensForItem(userTurn(), false)).toBe("prompt");
  });

  it("marks an intermediate text block a response but not a final", () => {
    expect(navTokensForItem(text(), false)).toBe("response");
  });

  it("marks a turn-closing text block as both response and final", () => {
    expect(navTokensForItem(text(), true)).toBe("response final");
  });

  it("marks a tool card as a tool stop", () => {
    expect(navTokensForItem(tool(), false)).toBe("tool");
  });

  it("leaves a thinking block off every cycle", () => {
    expect(navTokensForItem(thinking(), false)).toBe("");
  });
});

describe("hasToken", () => {
  it("matches a token in a multi-token list", () => {
    expect(hasToken("response final", "final")).toBe(true);
  });

  it("does not match a token that is only a prefix of another", () => {
    expect(hasToken("finalize", "final")).toBe(false);
  });

  it("does not match against an unstamped wrapper", () => {
    expect(hasToken("", "prompt")).toBe(false);
  });
});

describe("matchingIndexes", () => {
  it("selects only the wrappers carrying the class", () => {
    const attrs = ["prompt", "response", "prompt", "tool"];
    expect(matchingIndexes(attrs, "prompt")).toEqual([0, 2]);
  });

  it("selects a final bubble through its multi-token list", () => {
    expect(matchingIndexes(["response", "response final"], "final")).toEqual([1]);
  });
});

describe("seedIndex", () => {
  it("seeds at the wrapper the viewport top sits on", () => {
    expect(seedIndex([0, 100, 200, 300], 210)).toBe(2);
  });

  it("seeds above every wrapper when the feed is scrolled to the top", () => {
    expect(seedIndex([0, 100], -5)).toBe(-1);
  });

  it("counts a wrapper sitting a sub-pixel above the viewport top as seeded", () => {
    expect(seedIndex([0, 100], 99)).toBe(1);
  });
});

describe("cycleTarget", () => {
  it("steps forward to the next match below the cursor", () => {
    expect(cycleTarget({ matches: [1, 4, 7], from: 4, dir: 1 })).toBe(7);
  });

  it("steps backward to the previous match above the cursor", () => {
    expect(cycleTarget({ matches: [1, 4, 7], from: 4, dir: -1 })).toBe(1);
  });

  it("wraps to the first match when stepping past the last", () => {
    expect(cycleTarget({ matches: [1, 4, 7], from: 7, dir: 1 })).toBe(1);
  });

  it("wraps to the last match when stepping back past the first", () => {
    expect(cycleTarget({ matches: [1, 4, 7], from: 1, dir: -1 })).toBe(7);
  });

  it("finds nothing to move to when the class has no stops", () => {
    expect(cycleTarget({ matches: [], from: 0, dir: 1 })).toBeNull();
  });

  it("stays on the sole match rather than reporting nothing", () => {
    expect(cycleTarget({ matches: [3], from: 3, dir: 1 })).toBe(3);
  });
});

describe("cycleDecision", () => {
  const feed: NavItem[] = [
    item("user-turn:0", "prompt", 0),
    item("text:a", "response", 100),
    item("tool:t1", "tool", 200),
    item("text:b", "response final", 300),
  ];

  it("seeds a first press from the viewport rather than the tail", () => {
    const target = cycleDecision({ items: feed, cursor: null, scrollTop: 150, cls: "prompt", dir: 1 });
    expect(target).toBe(0);
  });

  it("walks on from the marked bubble once a cycle is in flight", () => {
    const target = cycleDecision({
      items: feed,
      cursor: "text:a",
      scrollTop: 0,
      cls: "final",
      dir: 1,
    });
    expect(target).toBe(3);
  });

  it("re-seeds from the viewport when the cursor's item has left the feed", () => {
    const target = cycleDecision({
      items: feed,
      cursor: "text:gone",
      scrollTop: 250,
      cls: "tool",
      dir: -1,
    });
    expect(target).toBe(2);
  });

  it("reaches only the purple final bubble on the response cycle", () => {
    const target = cycleDecision({ items: feed, cursor: null, scrollTop: 0, cls: "final", dir: 1 });
    expect(target).toBe(3);
  });

  it("reports nothing to move to when no bubble carries the class", () => {
    const target = cycleDecision({
      items: [item("k", "prompt", 0)],
      cursor: null,
      scrollTop: 0,
      cls: "tool",
      dir: 1,
    });
    expect(target).toBeNull();
  });
});

describe("nextCursor", () => {
  it("carries the cursor through a re-render that kept its bubble", () => {
    expect(nextCursor({ keys: ["a", "b"], cursor: "b", turnChanged: false })).toBe("b");
  });

  it("drops a cursor whose bubble a /clear discarded", () => {
    expect(nextCursor({ keys: ["a"], cursor: "b", turnChanged: false })).toBeNull();
  });

  it("retires the cursor when the user sends a new prompt", () => {
    expect(nextCursor({ keys: ["a", "b"], cursor: "b", turnChanged: true })).toBeNull();
  });
});

describe("navChord", () => {
  it("cycles prompts forward on C-S-j", () => {
    expect(navChord(key({ code: "KeyJ", ctrlKey: true, shiftKey: true }))).toEqual({
      cls: "prompt",
      dir: 1,
    });
  });

  it("cycles prompts backward on C-S-k", () => {
    expect(navChord(key({ code: "KeyK", ctrlKey: true, shiftKey: true }))).toEqual({
      cls: "prompt",
      dir: -1,
    });
  });

  it("cycles the purple responses on M-S-j", () => {
    expect(navChord(key({ code: "KeyJ", altKey: true, shiftKey: true }))).toEqual({
      cls: "final",
      dir: 1,
    });
  });

  it("cycles tool cards on C-M-S-j", () => {
    expect(navChord(key({ code: "KeyJ", ctrlKey: true, altKey: true, shiftKey: true }))).toEqual({
      cls: "tool",
      dir: 1,
    });
  });

  it("resolves the alt chord off the key's code, which macOS's Option glyph does not disturb", () => {
    // Option-J arrives as key "∆"; only `code` still says KeyJ.
    expect(navChord(key({ code: "KeyJ", altKey: true, shiftKey: true, key: "∆" } as never))).toEqual({
      cls: "final",
      dir: 1,
    });
  });

  it("leaves an unshifted C-j to the window motion that owns it", () => {
    expect(navChord(key({ code: "KeyJ", ctrlKey: true }))).toBeNull();
  });

  it("leaves a plain letter to the composer", () => {
    expect(navChord(key({ code: "KeyJ" }))).toBeNull();
  });

  it("leaves a Cmd chord to the browser", () => {
    expect(navChord(key({ code: "KeyJ", ctrlKey: true, shiftKey: true, metaKey: true }))).toBeNull();
  });

  it("leaves a non-jk key alone however it is modified", () => {
    expect(navChord(key({ code: "KeyH", ctrlKey: true, shiftKey: true }))).toBeNull();
  });
});

describe("the class registry", () => {
  it("gives every cycleable class exactly one modifier combination", () => {
    expect(CLASS_MODIFIERS.map((m) => m.cls).sort()).toEqual([...NAV_CLASSES].sort());
  });

  it("gives no two classes the same modifier combination", () => {
    const combos = CLASS_MODIFIERS.map((m) => `${String(m.ctrl)}/${String(m.alt)}`);
    expect(new Set(combos).size).toBe(CLASS_MODIFIERS.length);
  });

  it("accepts a class name the elisp host may send", () => {
    expect(isNavClass("final")).toBe(true);
  });

  it("rejects a class name no cycle serves", () => {
    expect(isNavClass("thinking")).toBe(false);
  });
});

describe("the host contract", () => {
  it("names the hook output-nav.el calls", () => {
    expect(NAV_HOOK).toBe("agentReplNavigate");
  });

  it("names the marker class styles.css draws", () => {
    expect(NAV_CURRENT_CLASS).toBe("nav-current");
  });
});
