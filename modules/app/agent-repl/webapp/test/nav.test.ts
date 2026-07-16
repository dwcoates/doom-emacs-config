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
  NAV_ATTR,
  NAV_HOOK,
  FeedNav,
  installNavHook,
  installNavKeys,
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
  seedTarget,
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

describe("seedTarget", () => {
  // Wrappers at 0/100/200/300; the prompts are the outer two.
  const tops = [0, 100, 200, 300];
  const matches = [0, 3];

  it("enters at the match sitting at the viewport top rather than skipping it", () => {
    // The first press ENTERS the cycle: skipping the prompt the user is
    // looking at would make every cycle start one stop too far along.
    expect(seedTarget({ matches, tops, viewportTop: 0, dir: 1 })).toBe(0);
  });

  it("enters forward at the first match below the viewport", () => {
    expect(seedTarget({ matches, tops, viewportTop: 150, dir: 1 })).toBe(3);
  });

  it("enters backward at the last match above the viewport", () => {
    expect(seedTarget({ matches, tops, viewportTop: 150, dir: -1 })).toBe(0);
  });

  it("enters at a match a sub-pixel above the viewport top, which is still the one on screen", () => {
    expect(seedTarget({ matches, tops, viewportTop: 1, dir: 1 })).toBe(0);
  });

  it("wraps forward to the first match when scrolled past every one", () => {
    expect(seedTarget({ matches, tops, viewportTop: 900, dir: 1 })).toBe(0);
  });

  it("wraps backward to the last match when scrolled above every one", () => {
    expect(seedTarget({ matches, tops, viewportTop: -50, dir: -1 })).toBe(3);
  });

  it("finds nothing to enter when the class has no matches", () => {
    expect(seedTarget({ matches: [], tops, viewportTop: 0, dir: 1 })).toBeNull();
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
    item("user-turn:1", "prompt", 400),
  ];

  it("seeds a first press from the viewport, not from the tail", () => {
    // Scrolled between the two prompts: a viewport-seeded forward cycle
    // takes the one just below, where a tail-seeded one would wrap round
    // to the one at the top.
    const target = cycleDecision({ items: feed, cursor: null, scrollTop: 150, cls: "prompt", dir: 1 });
    expect(target).toBe(4);
  });

  it("seeds a backward first press from the viewport too", () => {
    const target = cycleDecision({ items: feed, cursor: null, scrollTop: 150, cls: "prompt", dir: -1 });
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

/**
 * The DOM assembly: FeedNav and the two installers.
 *
 * jsdom is not in the dep tree, so the feed is faked down to exactly the
 * surface FeedNav touches. Worth testing despite the fake because the
 * assembly is where the cursor actually persists and where "focus never
 * leaves the composer" is either honored or lost.
 */
describe("FeedNav", () => {
  /** A feed-item wrapper, faked to the surface FeedNav reads and writes. */
  const wrapper = (key: string, nav: string, top: number) => {
    const classes = new Set<string>();
    const reveals: string[] = [];
    return {
      dataset: { key },
      offsetTop: top,
      classes,
      reveals,
      getAttribute: (name: string) => (name === NAV_ATTR ? nav : null),
      classList: {
        add: (c: string) => classes.add(c),
        remove: (c: string) => classes.delete(c),
      },
      scrollIntoView: (arg: { block: string }) => reveals.push(arg.block),
    };
  };

  /** A feed holding WRAPPERS, scrolled to SCROLLTOP. */
  const feedOf = (wrappers: ReturnType<typeof wrapper>[], scrollTop = 0) => {
    const el = { scrollTop, querySelectorAll: () => wrappers };
    return { el: el as unknown as HTMLElement, wrappers };
  };

  /** A prompt / response / prompt feed, as a short conversation renders. */
  const conversation = () => [
    wrapper("user-turn:0", "prompt", 0),
    wrapper("text:a", "response final", 100),
    wrapper("user-turn:1", "prompt", 200),
  ];

  it("marks the bubble it lands on", () => {
    const { el, wrappers } = feedOf(conversation());
    new FeedNav(el).cycle("prompt", 1);
    expect(wrappers[0].classes.has(NAV_CURRENT_CLASS)).toBe(true);
  });

  it("brings the bubble it lands on into view", () => {
    const { el, wrappers } = feedOf(conversation());
    new FeedNav(el).cycle("prompt", 1);
    expect(wrappers[0].reveals).toEqual(["nearest"]);
  });

  it("moves off the marked bubble on the next press rather than re-landing on it", () => {
    const { el, wrappers } = feedOf(conversation());
    const nav = new FeedNav(el);
    nav.cycle("prompt", 1);
    nav.cycle("prompt", 1);
    expect(wrappers[2].classes.has(NAV_CURRENT_CLASS)).toBe(true);
  });

  it("leaves the marker on only one bubble at a time", () => {
    const { el, wrappers } = feedOf(conversation());
    const nav = new FeedNav(el);
    nav.cycle("prompt", 1);
    nav.cycle("prompt", 1);
    expect(wrappers.filter((w) => w.classes.has(NAV_CURRENT_CLASS))).toHaveLength(1);
  });

  it("wraps from the last prompt back to the first", () => {
    const { el, wrappers } = feedOf(conversation());
    const nav = new FeedNav(el);
    nav.cycle("prompt", 1);
    nav.cycle("prompt", 1);
    nav.cycle("prompt", 1);
    expect(wrappers[0].classes.has(NAV_CURRENT_CLASS)).toBe(true);
  });

  it("reports nothing to move to when the feed holds no bubble of the class", () => {
    const { el } = feedOf([wrapper("user-turn:0", "prompt", 0)]);
    expect(new FeedNav(el).cycle("tool", 1)).toBe(false);
  });

  it("keeps the cycle's place when a streaming turn re-renders the feed", () => {
    // Arrange: cycle onto a bubble, then re-render the same turn — the
    // wrappers are rebuilt but keep their keys, as the reconciler does.
    const first = feedOf(conversation());
    const nav = new FeedNav(first.el);
    nav.reconcile("r1");
    nav.cycle("prompt", 1);
    nav.cycle("prompt", 1);
    expect(nav.current).toBe("user-turn:1");
    // Act
    nav.reconcile("r1");
    // Assert
    expect(nav.current).toBe("user-turn:1");
  });

  it("re-marks the cursor's bubble after a render that rebuilt every node", () => {
    // Arrange: the restored-session render throws the nodes away, so the
    // marker must be re-applied rather than assumed to have survived.
    const { el, wrappers } = feedOf(conversation());
    const nav = new FeedNav(el);
    nav.reconcile("r1");
    nav.cycle("prompt", 1);
    wrappers[0].classes.clear();
    // Act
    nav.reconcile("r1");
    // Assert
    expect(wrappers[0].classes.has(NAV_CURRENT_CLASS)).toBe(true);
  });

  it("retires the cycle when the user sends a new prompt", () => {
    const { el } = feedOf(conversation());
    const nav = new FeedNav(el);
    nav.reconcile("r1");
    nav.cycle("prompt", 1);
    // Act
    nav.reconcile("r2");
    // Assert
    expect(nav.current).toBeNull();
  });

  it("takes the marker off the feed when the cycle is retired", () => {
    const { el, wrappers } = feedOf(conversation());
    const nav = new FeedNav(el);
    nav.reconcile("r1");
    nav.cycle("prompt", 1);
    nav.reconcile("r2");
    expect(wrappers.some((w) => w.classes.has(NAV_CURRENT_CLASS))).toBe(false);
  });
});

describe("installNavKeys", () => {
  /** A composer capturing its handler, plus the events it let through. */
  const composer = () => {
    let handler: ((e: unknown) => void) | null = null;
    return {
      el: {
        addEventListener: (_: string, h: (e: unknown) => void) => {
          handler = h;
        },
      } as unknown as HTMLElement,
      press: (over: Partial<NavKeyEvent> & { preventDefault: () => void }) => {
        handler?.({ ...key(), ...over });
      },
    };
  };

  const feed = () =>
    ({
      scrollTop: 0,
      querySelectorAll: () => [],
    }) as unknown as HTMLElement;

  it("swallows a cycle chord so the composer never types it", () => {
    const c = composer();
    let prevented = false;
    installNavKeys(c.el, new FeedNav(feed()));
    c.press({ code: "KeyJ", ctrlKey: true, shiftKey: true, preventDefault: () => (prevented = true) });
    expect(prevented).toBe(true);
  });

  it("leaves an ordinary keystroke to the composer, so typing keeps working", () => {
    const c = composer();
    let prevented = false;
    installNavKeys(c.el, new FeedNav(feed()));
    c.press({ code: "KeyJ", preventDefault: () => (prevented = true) });
    expect(prevented).toBe(false);
  });
});

describe("installNavHook", () => {
  const feed = () =>
    ({ scrollTop: 0, querySelectorAll: () => [] }) as unknown as HTMLElement;

  it("plants the hook under the name output-nav.el calls", () => {
    const target: Record<string, unknown> = {};
    installNavHook(target, new FeedNav(feed()));
    expect(typeof target[NAV_HOOK]).toBe("function");
  });

  it("raises on a class the two halves disagree about, rather than cycling nothing", () => {
    const target: Record<string, unknown> = {};
    installNavHook(target, new FeedNav(feed()));
    const hook = target[NAV_HOOK] as (c: string, d: number) => boolean;
    expect(() => hook("thinking", 1)).toThrow(/unknown nav class/);
  });
});
