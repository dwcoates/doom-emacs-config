import { describe, expect, it } from "vitest";
import {
  DEFAULT_REVEAL_OPTIONS,
  RevealClock,
  RevealOptions,
  SmoothReveal,
  revealSlice,
} from "../src/smooth.js";
import type { ConversationItem, StoreState, TextItem, ThinkingItem } from "../src/store.js";

/** A clock the test advances by hand, in milliseconds. */
function fakeClock(): RevealClock & { advance(ms: number): void } {
  let t = 0;
  return {
    now: () => t,
    advance(ms: number): void {
      t += ms;
    },
  };
}

function textItem(over: Partial<TextItem> = {}): TextItem {
  return {
    kind: "text",
    blockId: "b1",
    messageId: "m1",
    text: "",
    done: false,
    ts: "2026-05-24T10:00:00.000Z",
    ...over,
  };
}

function thinkingItem(over: Partial<ThinkingItem> = {}): ThinkingItem {
  return {
    kind: "thinking",
    blockId: "t1",
    messageId: "m1",
    text: "",
    done: false,
    ...over,
  };
}

function state(items: ConversationItem[]): StoreState {
  return {
    sessionId: "s1",
    daemonVersion: "0.1.0",
    model: "m",
    models: [],
    cwd: "/w",
    claudeSessionId: "",
    permissionMode: "default",
    statusSnapshot: null,
    items,
    queued: [],
    turnInFlight: true,
    turnStartedAt: "2026-05-24T10:00:00.000Z",
    lastFinalResponseAt: null,
    contextTokens: null,
    resultUsage: null,
    turnUsage: new Map(),
    modelUsage: null,
    compacting: false,
    interrupting: false,
    turnRetracted: false,
    costUsd: null,
    taskSummary: null,
    lastSeq: 0,
  };
}

/** Tight, fast options so a frame's advance lands on round character counts. */
const opts: RevealOptions = { minCps: 200, catchupSeconds: 0.3 };

/** The single text block of a smoothed feed state. */
function shownText(s: StoreState): TextItem {
  const item = s.items.find((i) => i.kind === "text");
  if (!item || item.kind !== "text") throw new Error("no text item");
  return item;
}

describe("revealSlice", () => {
  it("reveals nothing for a zero count", () => {
    // Arrange / Act / Assert
    expect(revealSlice("hello", 0)).toBe("");
  });

  it("reveals the whole string once the count reaches its length", () => {
    // Arrange / Act / Assert
    expect(revealSlice("hello", 5)).toBe("hello");
  });

  it("reveals a leading prefix for a mid-string count", () => {
    // Arrange / Act / Assert
    expect(revealSlice("hello", 3)).toBe("hel");
  });

  it("drops a high surrogate rather than cut a pair in half", () => {
    // Arrange — "a😀b": the emoji is a UTF-16 surrogate pair at units 1..2.
    // Act — cut right between the pair's halves.
    const shown = revealSlice("a😀b", 2);
    // Assert — the lone high surrogate is dropped, not shown as a glyph.
    expect(shown).toBe("a");
  });

  it("keeps a surrogate pair whole once its low half is included", () => {
    // Arrange / Act
    const shown = revealSlice("a😀b", 3);
    // Assert
    expect(shown).toBe("a😀");
  });
});

describe("SmoothReveal.reveal", () => {
  it("shows nothing on the first frame of a live block", () => {
    // Arrange — a block first seen this frame has no elapsed time to reveal.
    const smooth = new SmoothReveal(fakeClock(), opts);
    // Act
    const out = smooth.reveal(state([textItem({ text: "hello world" })]));
    // Assert
    expect(shownText(out.state).text).toBe("");
    expect(out.pending).toBe(true);
  });

  it("holds a still-revealing block's done flag false", () => {
    // Arrange — a whole short block that arrived done in one frame.
    const smooth = new SmoothReveal(fakeClock(), opts);
    // Act
    const out = smooth.reveal(state([textItem({ text: "hi", done: true })]));
    // Assert — it types out rather than snapping, so it reads as streaming.
    expect(shownText(out.state).done).toBe(false);
    expect(out.pending).toBe(true);
  });

  it("advances at the floor rate near the frontier", () => {
    // Arrange — a small backlog reveals at minCps (200 cps).
    const clock = fakeClock();
    const smooth = new SmoothReveal(clock, opts);
    smooth.reveal(state([textItem({ text: "0123456789" })]));
    // Act — one 16ms frame later.
    clock.advance(16);
    const out = smooth.reveal(state([textItem({ text: "0123456789" })]));
    // Assert — 200 cps * 0.016s = 3.2 → 3 characters.
    expect(shownText(out.state).text).toBe("012");
  });

  it("accelerates a large backlog toward the catchup window", () => {
    // Arrange — a 1000-char burst reveals at backlog/catchupSeconds, not the floor.
    const clock = fakeClock();
    const smooth = new SmoothReveal(clock, opts);
    const big = "x".repeat(1000);
    smooth.reveal(state([textItem({ text: big })]));
    // Act
    clock.advance(16);
    const out = smooth.reveal(state([textItem({ text: big })]));
    // Assert — 1000/0.3 = 3333 cps * 0.016s = 53.3 → 53 characters.
    expect(shownText(out.state).text.length).toBe(53);
  });

  it("eventually reveals the whole burst and stops asking for frames", () => {
    // Arrange — the reveal decelerates to the floor near the frontier, so a
    // big burst takes many frames to fully settle rather than one window.
    const clock = fakeClock();
    const smooth = new SmoothReveal(clock, opts);
    const big = "x".repeat(1000);
    let out = smooth.reveal(state([textItem({ text: big })]));
    // Act — run well past the drain (100 frames of 16ms is 1.6s).
    for (let i = 0; i < 100; i++) {
      clock.advance(16);
      out = smooth.reveal(state([textItem({ text: big })]));
    }
    // Assert — fully caught up, and no further frame requested.
    expect(shownText(out.state).text).toBe(big);
    expect(out.pending).toBe(false);
  });

  it("passes a caught-up block through untouched", () => {
    // Arrange — a block marked fully shown renders from the real store state.
    const smooth = new SmoothReveal(fakeClock(), opts);
    const s = state([textItem({ text: "done text", done: true })]);
    smooth.markShown(s);
    // Act
    const out = smooth.reveal(s);
    // Assert — same state object, real item identity, real done preserved.
    expect(out.state).toBe(s);
    expect(out.state.items[0]).toBe(s.items[0]);
    expect(out.pending).toBe(false);
  });

  it("reveals thinking blocks too", () => {
    // Arrange
    const clock = fakeClock();
    const smooth = new SmoothReveal(clock, opts);
    smooth.reveal(state([thinkingItem({ text: "0123456789" })]));
    // Act
    clock.advance(16);
    const out = smooth.reveal(state([thinkingItem({ text: "0123456789" })]));
    // Assert — thinking paces at the same floor rate as text.
    const think = out.state.items.find((i) => i.kind === "thinking");
    expect(think?.kind === "thinking" ? think.text : "").toBe("012");
  });

  it("leaves non-streaming items untouched", () => {
    // Arrange — a tool item carries no revealable text.
    const smooth = new SmoothReveal(fakeClock(), opts);
    const tool: ConversationItem = {
      kind: "tool",
      toolUseId: "u1",
      toolName: "Bash",
      messageId: "m1",
      ts: "2026-05-24T10:00:00.000Z",
      inputJson: "",
      inputDone: false,
    };
    const s = state([tool]);
    // Act
    const out = smooth.reveal(s);
    // Assert
    expect(out.state).toBe(s);
    expect(out.pending).toBe(false);
  });

  it("re-types a block whose id returns after leaving the feed", () => {
    // Arrange — reveal a block, then drop it from the feed.
    const clock = fakeClock();
    const smooth = new SmoothReveal(clock, opts);
    smooth.reveal(state([textItem({ text: "0123456789" })]));
    clock.advance(16);
    smooth.reveal(state([textItem({ text: "0123456789" })]));
    smooth.reveal(state([])); // block b1 leaves → its cursor is forgotten
    // Act — the same id reappears and must reveal from scratch.
    clock.advance(16);
    const out = smooth.reveal(state([textItem({ text: "0123456789" })]));
    // Assert — first frame back shows nothing, not the prior prefix.
    expect(shownText(out.state).text).toBe("");
  });
});

describe("SmoothReveal.markShown", () => {
  it("skips re-typing prose already on screen", () => {
    // Arrange — a reconnect's restored render drew this block in full.
    const smooth = new SmoothReveal(fakeClock(), opts);
    const s = state([textItem({ text: "restored prose" })]);
    smooth.markShown(s);
    // Act
    const out = smooth.reveal(s);
    // Assert — it is not truncated back to a typewriter start.
    expect(shownText(out.state).text).toBe("restored prose");
    expect(out.pending).toBe(false);
  });

  it("still animates growth that arrives after the join", () => {
    // Arrange — a partial block is restored, then a live delta grows it.
    const clock = fakeClock();
    const smooth = new SmoothReveal(clock, opts);
    smooth.markShown(state([textItem({ text: "restored" })]));
    // Act — new text appended; one frame later.
    clock.advance(16);
    const out = smooth.reveal(state([textItem({ text: "restored MORE" })]));
    // Assert — the restored prefix stays, only the growth types in slowly.
    expect(shownText(out.state).text.startsWith("restored")).toBe(true);
    expect(shownText(out.state).text.length).toBeLessThan("restored MORE".length);
  });
});

describe("SmoothReveal.reset", () => {
  it("re-types a block from scratch after a session rebind", () => {
    // Arrange — a block partially revealed, then the session is swapped.
    const clock = fakeClock();
    const smooth = new SmoothReveal(clock, opts);
    smooth.reveal(state([textItem({ text: "0123456789" })]));
    clock.advance(16);
    smooth.reveal(state([textItem({ text: "0123456789" })]));
    // Act
    smooth.reset();
    clock.advance(16);
    const out = smooth.reveal(state([textItem({ text: "0123456789" })]));
    // Assert — the successor's first frame reveals from zero.
    expect(shownText(out.state).text).toBe("");
  });
});

describe("DEFAULT_REVEAL_OPTIONS", () => {
  it("carries a positive floor and catchup window", () => {
    // Arrange / Act / Assert — the shipped pacing is a real, forward reveal.
    expect(DEFAULT_REVEAL_OPTIONS.minCps).toBeGreaterThan(0);
    expect(DEFAULT_REVEAL_OPTIONS.catchupSeconds).toBeGreaterThan(0);
  });
});
