// @vitest-environment jsdom
//
// The pure decisions in this module need no dom, but `observeScrollBox` is the
// one DOM-facing thing in it: it subscribes a real element's scroll events and
// a real `ResizeObserver` to the tail owner, and the subscription being wired
// to THAT element is half of what the footer-occlusion cases assert.
import scrollSource from "../src/scroll.ts?raw";
import selectionScrollSource from "../src/sidebar/selection-scroll.ts?raw";
import scrollKeepSource from "../src/sidebar/scroll-keep.ts?raw";
import bandFitSource from "../src/sidebar/band-fit.ts?raw";
import sidebarPopupsSource from "../src/host/sidebar-popups.ts?raw";
import { chatEndTop, revealChatExchange } from "../src/scroll.js";
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  SCROLL_CAUSES,
  TAIL_LANDING_CAUSES,
  tailGap,
  atTail as viewAtTail,
  armedWheelAction,
  bubbleNodeRevealed,
  collapseClicked,
  dragEdgeScroll,
  selectionRowScroll,
  scrollKept,
  installIntentScroll,
  createWalkHold,
  walkHeld,
  WALK_HOLD_ATTRIBUTE,
  WALK_HOLD_QUIET_FRAMES,
  innerScrollerAt,
  TailFollow,
  isScrollBox,
  movesToward,
  sectionTakesDelta,
  type ReanchorBox,
  type AnchorRows,
  feedAnchorRows,
  sectionFor,
  sectionTakesWheel,
  wheelDeltaPx,
  revealCenterDelta,
  nearestEdgeOffset,
  revealNearestDelta,
  revealGeometry,
  centerOffset,
  centerTarget,
  elementsSpan,
  type CenterTarget,
  observeScrollBox,
  holdViewOnClicks,
  scrollFloor,
  SCROLL_FLOOR_CLASS,
  SCROLL_FLOOR_HOST_CLASS,
  type ScrollFloor,
  latestEntryVisible,
  MoveWitness,
  type MoveWitnessReport,
  type RevealGeometry,
  heldRowName,
  entryOf,
  openAtNewest,
} from "../src/scroll.js";
import { captureLogRecords, forwardedRecord, type LogCapture } from "./log-capture.js";
import { tailTriggers, type TailTriggers } from "./tail-triggers.js";
import { fireResize } from "./resize-observer.js";
import { expandedSectionAt, installClickExpand } from "../src/expand.js";
import { frameQueue } from "./frame-queue.js";
import { boxRect } from "./dom-rect.js";

/** An `ExpandedSectionAt` for a feed with no open section. */
const noneOpen = (): HTMLElement | null => null;

/** Fake ancestor-chain node: the shape innerScrollerAt walks. */
interface FakeNode {
  name: string;
  parentElement: FakeNode | null;
  scrollHeight: number;
  clientHeight: number;
  overflowY: string;
  section: boolean;
}

function node(name: string, over: Partial<FakeNode> = {}): FakeNode {
  return {
    name,
    parentElement: null,
    scrollHeight: 100,
    clientHeight: 100,
    overflowY: "visible",
    section: false,
    ...over,
  };
}

const metrics = (n: FakeNode) => n;
const isSection = (n: FakeNode) => n.section;

describe("isScrollBox", () => {
  it("accepts an overflowing box with overflow-y auto", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 400, clientHeight: 160, overflowY: "auto" })).toBe(true);
  });

  it("accepts an overflowing box with overflow-y scroll", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 400, clientHeight: 160, overflowY: "scroll" })).toBe(true);
  });

  it("rejects an overflowing box that does not clip (overflow-y visible)", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 400, clientHeight: 160, overflowY: "visible" })).toBe(false);
  });

  it("rejects a collapsed box that clips without scrolling (overflow-y hidden)", () => {
    // Owner ruling, 2026-09-15: a collapsed bubble/section clips rather than
    // scrolls, so the intent-arm gate (installIntentScroll) must never arm it —
    // its wheel always redirects to the feed.
    expect(isScrollBox({ scrollHeight: 400, clientHeight: 160, overflowY: "hidden" })).toBe(false);
  });

  it("rejects a clipping box whose content fits", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 160, clientHeight: 160, overflowY: "auto" })).toBe(false);
  });

  it("rejects a sub-pixel overflow as rounding noise", () => {
    // Arrange + Act + Assert
    expect(isScrollBox({ scrollHeight: 160.5, clientHeight: 160, overflowY: "auto" })).toBe(false);
  });
});

describe("sectionTakesWheel", () => {
  it("keeps the wheel when the armed box is the one under it", () => {
    // Arrange — the reader deliberately entered this box, so it owns the wheel.
    const box = { name: "armed" };
    // Act + Assert
    expect(sectionTakesWheel(box, box)).toBe(true);
  });

  it("hands the wheel off when a DIFFERENT box is armed", () => {
    // Arrange — the wheel landed over a box the reader never entered.
    const armed = { name: "armed" };
    const under = { name: "other" };
    // Act + Assert
    expect(sectionTakesWheel(armed, under)).toBe(false);
  });

  it("hands the wheel off when nothing is armed", () => {
    // Arrange — a box the FEED scrolled under a still cursor is never armed.
    const under = { name: "scrolled-into" };
    // Act + Assert
    expect(sectionTakesWheel(null, under)).toBe(false);
  });

  it("keeps nothing when the wheel is over no box at all", () => {
    // Arrange — over bare feed there is no section to keep the wheel.
    const armed = { name: "armed" };
    // Act + Assert
    expect(sectionTakesWheel(armed, null)).toBe(false);
  });
});

describe("armedWheelAction", () => {
  const feed = { name: "feed" };
  const base = {
    armed: { name: "armed" } as { name: string } | null,
    wheelScroller: { name: "other" } as { name: string } | null,
    armedEncloses: false,
    armedHeight: 200,
    feed,
    feedScrollable: true,
    deltaY: 40,
    deltaMode: 0,
    feedHeight: 600,
  };

  it("redirects a wheel over a non-armed box to the feed", () => {
    // Arrange + Act + Assert
    expect(armedWheelAction(base)).toEqual({ to: feed, delta: 40 });
  });

  it("leaves a wheel over the armed box to the browser", () => {
    // Arrange — the armed box keeps its own wheel.
    const armed = { name: "armed" };
    // Act + Assert
    expect(armedWheelAction({ ...base, armed, wheelScroller: armed })).toBeNull();
  });

  it("leaves a wheel over no box to the browser", () => {
    // Arrange + Act + Assert
    expect(armedWheelAction({ ...base, wheelScroller: null })).toBeNull();
  });

  it("leaves a purely horizontal wheel to the browser", () => {
    // Arrange + Act + Assert
    expect(armedWheelAction({ ...base, deltaY: 0 })).toBeNull();
  });

  it("leaves the box alone when the feed itself cannot scroll", () => {
    // Arrange + Act + Assert
    expect(armedWheelAction({ ...base, feedScrollable: false })).toBeNull();
  });

  it("converts the delta to pixels before handing it to the feed", () => {
    // Arrange + Act + Assert — line-mode delta 2 at LINE_PX 16.
    expect(armedWheelAction({ ...base, deltaY: 2, deltaMode: 1 })).toEqual({ to: feed, delta: 32 });
  });

  it("redirects a wheel over a non-armed box inside the armed one to the armed box", () => {
    // Arrange — the armed box's own scrolling slid an inner box under the cursor.
    const armed = { name: "armed" };
    // Act + Assert
    expect(armedWheelAction({ ...base, armed, armedEncloses: true })).toEqual({ to: armed, delta: 40 });
  });

  it("redirects to the enclosing armed box even when the feed cannot scroll", () => {
    // Arrange
    const armed = { name: "armed" };
    // Act + Assert
    expect(armedWheelAction({ ...base, armed, armedEncloses: true, feedScrollable: false })).toEqual({ to: armed, delta: 40 });
  });

  it("measures a page-mode delta by the armed box's height when it redirects there", () => {
    // Arrange
    const armed = { name: "armed" };
    // Act + Assert — one page of a 200px box.
    expect(armedWheelAction({ ...base, armed, armedEncloses: true, deltaY: 1, deltaMode: 2 })).toEqual({ to: armed, delta: 200 });
  });

  it("redirects to the feed when nothing is armed, whatever encloses the box", () => {
    // Arrange + Act + Assert
    expect(armedWheelAction({ ...base, armed: null, armedEncloses: true })).toEqual({ to: feed, delta: 40 });
  });
});

describe("wheelDeltaPx", () => {
  it("passes a pixel-mode delta through unchanged", () => {
    // Arrange + Act + Assert
    expect(wheelDeltaPx({ deltaY: 53, deltaMode: 0 }, 600)).toBe(53);
  });

  it("scales a line-mode delta to pixels", () => {
    // Arrange + Act + Assert
    expect(wheelDeltaPx({ deltaY: 3, deltaMode: 1 }, 600)).toBe(48);
  });

  it("scales a page-mode delta by the viewport height", () => {
    // Arrange + Act + Assert
    expect(wheelDeltaPx({ deltaY: -1, deltaMode: 2 }, 600)).toBe(-600);
  });
});

describe("innerScrollerAt", () => {
  it("returns null when no ancestor below the feed scrolls", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const card = node("card", { parentElement: feed });
    const text = node("text", { parentElement: card });
    // Act + Assert
    expect(innerScrollerAt(text, feed, metrics)).toBeNull();
  });

  it("finds the scrolling section above the wheel target", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const section = node("section", {
      parentElement: feed,
      scrollHeight: 400,
      clientHeight: 160,
      overflowY: "auto",
    });
    const text = node("text", { parentElement: section });
    // Act + Assert
    expect(innerScrollerAt(text, feed, metrics)?.name).toBe("section");
  });

  it("returns the innermost of nested scrolling sections", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const outer = node("outer", {
      parentElement: feed,
      scrollHeight: 400,
      clientHeight: 160,
      overflowY: "auto",
    });
    const inner = node("inner", {
      parentElement: outer,
      scrollHeight: 300,
      clientHeight: 80,
      overflowY: "auto",
    });
    // Act + Assert
    expect(innerScrollerAt(inner, feed, metrics)?.name).toBe("inner");
  });

  it("never returns the feed itself", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    // Act + Assert
    expect(innerScrollerAt(feed, feed, metrics)).toBeNull();
  });

  it("skips a collapsed (clipping) bubble, so its wheel redirects to the feed", () => {
    // Arrange — a collapsed bubble scroll box (overflow-y hidden) whose content
    // exceeds it; the intent-arm gate must not treat it as a scroller (owner
    // ruling, 2026-09-15: collapsed boxes never scroll).
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const collapsed = node("collapsed", {
      parentElement: feed,
      scrollHeight: 400,
      clientHeight: 160,
      overflowY: "hidden",
    });
    const text = node("text", { parentElement: collapsed });
    // Act + Assert
    expect(innerScrollerAt(text, feed, metrics)).toBeNull();
  });

  it("arms an EXPANDED bubble that still overflows its 50vh cap", () => {
    // Arrange — an expanded bubble scroll box: overflow-y auto and content past
    // the 50vh cap, the only shape the intent-arm scroll applies to.
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    const expanded = node("expanded", {
      parentElement: feed,
      scrollHeight: 900,
      clientHeight: 400,
      overflowY: "auto",
    });
    const text = node("text", { parentElement: expanded });
    // Act + Assert
    expect(innerScrollerAt(text, feed, metrics)?.name).toBe("expanded");
  });

  it("returns null for a wheel with no target element", () => {
    // Arrange
    const feed = node("feed", { scrollHeight: 900, clientHeight: 300, overflowY: "auto" });
    // Act + Assert
    expect(innerScrollerAt(null, feed, metrics)).toBeNull();
  });
});

describe("sectionFor", () => {
  it("returns the card enclosing the scroll box, so the bars span the whole section", () => {
    // Arrange — a Bash card whose output box is one of several sub-boxes.
    const feed = node("feed");
    const card = node("card", { parentElement: feed, section: true });
    const output = node("bash-output", { parentElement: card });
    // Act + Assert
    expect(sectionFor(output, feed, isSection).name).toBe("card");
  });

  it("returns the innermost card when cards nest", () => {
    // Arrange
    const feed = node("feed");
    const outer = node("outer-card", { parentElement: feed, section: true });
    const inner = node("inner-card", { parentElement: outer, section: true });
    const output = node("bash-output", { parentElement: inner });
    // Act + Assert
    expect(sectionFor(output, feed, isSection).name).toBe("inner-card");
  });

  it("returns the scroll box itself when it is the card", () => {
    // Arrange
    const feed = node("feed");
    const card = node("card", { parentElement: feed, section: true });
    // Act + Assert
    expect(sectionFor(card, feed, isSection).name).toBe("card");
  });

  it("falls back to the scroll box when no card encloses it", () => {
    // Arrange
    const feed = node("feed");
    const output = node("bare-output", { parentElement: feed });
    // Act + Assert
    expect(sectionFor(output, feed, isSection).name).toBe("bare-output");
  });

  it("never returns the feed, even when the feed matches", () => {
    // Arrange — the feed is off-limits: lighting it would frame the viewport.
    const feed = node("feed", { section: true });
    const output = node("bare-output", { parentElement: feed });
    // Act + Assert
    expect(sectionFor(output, feed, isSection).name).toBe("bare-output");
  });
});

/** A box parked at its tail: 700 + 300 viewport = 1000. */
const atTail = (): ReanchorBox => ({ scrollTop: 700, scrollHeight: 1000, clientHeight: 300 });

/** A follow owner plus the three triggers it subscribed to (`tailTriggers`). */
function armed(box: ReanchorBox): TailTriggers & { tail: TailFollow } {
  const tail = new TailFollow(box);
  return { tail, ...tailTriggers(tail) };
}

/** A box a sent prompt parked at its tail, so a follow stands. */
function following(box: ReanchorBox = atTail()): ReturnType<typeof armed> & { box: ReanchorBox } {
  const a = armed(box);
  a.tail.promptSent();
  return { ...a, box };
}

/** The `scroll.feed-moved` records a capture holds, in order. */
async function moves(capture: LogCapture): Promise<Array<Record<string, unknown>>> {
  capture.logger.flush();
  await Promise.resolve();
  return capture.sent
    .filter((record) => record.operation === "scroll.feed-moved")
    .map((record) => {
      const { cause, from, to, follow } = record.context as Record<string, unknown>;
      return { cause, from, to, follow };
    });
}

/**
 * THE CLOSED SET OF IMPLICIT FEED-SCROLL CAUSES (owner rule, 2026-09-23: the
 * user owns the scroll). Each cause moves the feed as specified; nothing else
 * the owner offers can move it.
 */
describe("SCROLL_CAUSES", () => {
  it("names exactly the fifteen causes the owner rules allow", () => {
    // Arrange + Act + Assert
    expect([...SCROLL_CAUSES]).toEqual([
      "promptSent",
      "promptHeld",
      "selectionMoved",
      "detachedWorkSelected",
      "entryJumped",
      "composerEntryRevealed",
      "initialPlacement",
      "replaceRestore",
      "rolledBack",
      "workspaceSelected",
      "tailJumped",
      "prependCompensation",
      "latestVisible",
      "chatAnswerFollowed",
      "chatNodeRevealed",
    ]);
  });

  it("no longer names the last bubble's fold, which holds the view as any fold does", () => {
    // Arrange + Act + Assert — owner ruling, 2026-11-20.
    expect(SCROLL_CAUSES.filter((c: string) => c.startsWith("lastBubble"))).toEqual([]);
  });
});

/**
 * THE CAUSES THAT LAND THE FEED AT ITS TAIL (owner rulings, 2026-11-20):
 * exactly these seven, so a new one cannot slip in unnoticed.
 */
describe("tailGap", () => {
  it("is the content below the view's bottom", () => {
    // Arrange + Act + Assert
    expect(tailGap({ scrollTop: 600, scrollHeight: 1000, clientHeight: 300 })).toBe(100);
  });

  it("is zero at the tail", () => {
    // Arrange + Act + Assert
    expect(tailGap({ scrollTop: 700, scrollHeight: 1000, clientHeight: 300 })).toBe(0);
  });

  it("is the one gap arithmetic: no other source module computes the gap below the view by hand", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const sources = import.meta.glob(["../src/**/*.ts"], { query: "?raw", import: "default", eager: true }) as Record<string, string>;
    const gap = /scrollHeight\s*-\s*\w+\.(scrollTop\s*-\s*\w+\.clientHeight|clientHeight\s*-\s*\w+\.scrollTop)\b/;
    // Act
    const handRolled = Object.entries(sources)
      .filter(([, text]) => gap.test(text.replace(/export function tailGap[\s\S]*?\n}\n/, "")))
      .map(([path]) => path);
    // Assert
    expect(handRolled).toEqual([]);
  });
});

describe("atTail (imported as viewAtTail beside the fixture of that name)", () => {
  it("is true within PIN_PX of the tail", () => {
    // Arrange + Act + Assert
    expect(viewAtTail({ scrollTop: 661, scrollHeight: 1000, clientHeight: 300 })).toBe(true);
  });

  it("is false at PIN_PX or farther above the tail", () => {
    // Arrange + Act + Assert
    expect(viewAtTail({ scrollTop: 660, scrollHeight: 1000, clientHeight: 300 })).toBe(false);
  });
});

describe("TAIL_LANDING_CAUSES", () => {
  it("is exactly the seven explicit acts that land the feed at its tail", () => {
    // Arrange + Act + Assert
    expect([...TAIL_LANDING_CAUSES]).toEqual([
      "promptSent",
      "promptHeld",
      "workspaceSelected",
      "rolledBack",
      "tailJumped",
      "initialPlacement",
      "replaceRestore",
    ]);
  });
});

describe("TailFollow's parking causes", () => {
  const parks: Array<[string, (tail: TailFollow) => void, string]> = [
    ["a sent prompt", (tail) => tail.promptSent(), "promptSent"],
    ["a newly held prompt", (tail) => tail.promptHeld(), "promptHeld"],
    ["a first placement", (tail) => tail.initialPlacement(), "initialPlacement"],
    ["a replace restore", (tail) => tail.replaceRestore(), "replaceRestore"],
    ["a rolled-back row's removal", (tail) => tail.rolledBack(), "rolledBack"],
    ["a switch to this workspace", (tail) => tail.workspaceSelected(), "workspaceSelected"],
    ["the jump-to-tail control", (tail) => tail.jumpToTail(), "tailJumped"],
  ];

  it.each(parks)("%s lands a scrolled-up box at its tail", (_name, act) => {
    // Arrange
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    act(a.tail);
    // Assert
    expect(box.scrollTop).toBe(1000);
  });

  it.each(parks)("%s latches the follow", (_name, act) => {
    // Arrange
    const a = armed({ scrollTop: 100, scrollHeight: 1000, clientHeight: 300 });
    // Act
    act(a.tail);
    // Assert
    expect(a.tail.isFollowing()).toBe(true);
  });

  it.each(parks)("%s is recorded at DEBUG with its cause", async (_name, act, cause) => {
    // Arrange
    const capture = captureLogRecords("debug");
    const a = armed({ scrollTop: 100, scrollHeight: 1000, clientHeight: 300 });
    // Act
    act(a.tail);
    // Assert
    expect(await moves(capture)).toEqual([{ cause, from: 100, to: 1000, follow: false }]);
  });
});

describe("TailFollow.promptHeld", () => {
  it("keeps later content in view after parking a scrolled-up reader", () => {
    // Arrange
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    a.tail.promptHeld();
    box.scrollHeight = 1400;
    // Act
    a.tail.follow();
    // Assert
    expect(box.scrollTop).toBe(1400);
  });

  it("records the later follow move under promptHeld", async () => {
    // Arrange
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    a.tail.promptHeld();
    const capture = captureLogRecords("debug");
    box.scrollHeight = 1400;
    // Act
    a.tail.follow();
    // Assert
    expect(await moves(capture)).toEqual([{ cause: "promptHeld", from: 1000, to: 1400, follow: true }]);
  });
});

describe("TailFollow's follow", () => {
  it("follows nothing until a cause parks it, even on a box at its bottom", () => {
    // Arrange + Act — REMOVED TRIGGER: the owner used to START following any
    // box it was built on that sat at its bottom, with no cause behind it.
    const tail = new TailFollow(atTail());
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("keeps the tail on screen as content arrives under a standing follow", () => {
    // Arrange
    const f = following();
    f.box.scrollHeight = 1400;
    // Act
    f.tail.follow();
    // Assert
    expect(f.box.scrollTop).toBe(1400);
  });

  it("records a follow move under the cause that started the follow", async () => {
    // Arrange
    const f = following();
    const capture = captureLogRecords("debug");
    f.box.scrollHeight = 1400;
    // Act
    f.tail.follow();
    // Assert
    expect(await moves(capture)).toEqual([
      { cause: "promptSent", from: 1000, to: 1400, follow: true },
    ]);
  });

  it("records nothing for a follow that found the box already at its tail", async () => {
    // Arrange
    const f = following();
    const capture = captureLogRecords("debug");
    // Act
    f.tail.follow();
    // Assert
    expect(await moves(capture)).toEqual([]);
  });

  it("moves nothing when no cause started a follow", () => {
    // Arrange — REMOVED TRIGGER: rows arriving under a feed nobody parked.
    const box = { scrollTop: 200, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    box.scrollHeight = 1400;
    // Act
    a.tail.follow();
    // Assert
    expect(box.scrollTop).toBe(200);
  });

  it("stops following on a scroll UP of a few pixels", () => {
    // Arrange — a trackpad flick upward begins with a few px; a geometry
    // sample once called that "still pinned" and parked the feed back down.
    const f = following();
    // Act — the fake box does not clamp, so its reachable tail is 700.
    f.box.scrollTop = 690;
    f.gesture();
    // Assert
    expect(f.tail.isFollowing()).toBe(false);
  });

  it("stops following on a scroll DOWN that stops short of the tail", () => {
    // Arrange -- content grew under the follow before it was re-landed.
    const f = following();
    f.box.scrollHeight = 2000;
    // Act -- the reader scrolls down, but not to the new tail at 1700.
    f.box.scrollTop = 900;
    f.gesture();
    // Assert
    expect(f.tail.isFollowing()).toBe(false);
  });

  it("keeps following on a scroll DOWN that lands at the tail", () => {
    // Arrange
    const f = following();
    f.box.scrollHeight = 2000;
    // Act
    f.box.scrollTop = 1700;
    f.gesture();
    // Assert
    expect(f.tail.isFollowing()).toBe(true);
  });

  it("records the reader ending the follow", async () => {
    // Arrange
    const f = following();
    const capture = captureLogRecords("info");
    // Act
    f.box.scrollTop = 400;
    f.gesture();
    // Assert — at INFO, the default threshold, so the log writes it.
    const record = await forwardedRecord(capture, "scroll.follow-ended");
    expect([record.level.case, record.context]).toMatchObject(["info", { cause: "promptSent", from: 700, to: 400 }]);
  });

  it("does not restart the follow when the reader scrolls back to the tail", () => {
    // Arrange — REMOVED TRIGGER: arriving back at the tail used to resume the
    // follow, an implicit scroll no named cause asked for.
    const f = following();
    f.box.scrollTop = 200;
    f.gesture();
    // Act
    f.box.scrollTop = 1000;
    f.gesture();
    // Assert
    expect(f.tail.isFollowing()).toBe(false);
  });

  it("reports the reader's live position even before their scroll event lands", () => {
    // Arrange — the browser dispatches scroll asynchronously, so a render can
    // run between the gesture and its event.
    const f = following();
    // Act — the input landed and the box moved; no scroll event yet.
    f.input();
    f.box.scrollTop = 400;
    // Assert
    expect(f.tail.isFollowing()).toBe(false);
  });

  it("keeps a scrolled-away reader where they are across a burst of re-renders", () => {
    // Arrange
    const f = following();
    f.box.scrollTop = 400;
    f.gesture();
    // Act
    for (let i = 0; i < 20; i++) {
      f.box.scrollHeight += 120;
      f.tail.follow();
    }
    // Assert
    expect(f.box.scrollTop).toBe(400);
  });

  it("ignores the scroll event its own park emits", () => {
    // Arrange
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.promptSent();
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(true);
  });

  it("does not read the box's own clamp as the reader scrolling up", () => {
    // Arrange — content shrank and the browser clamped scrollTop down with it,
    // with no input behind the movement.
    const f = following();
    // Act
    f.box.scrollHeight = 700;
    f.box.scrollTop = 400;
    f.scroll();
    // Assert
    expect(f.tail.isFollowing()).toBe(true);
  });

  it("keeps the follow when a clamp is only seen after the content regrew", () => {
    // Arrange — THE MEASURED DEFECT (`scrollTop=52 scrollHeight=853
    // clientHeight=637`): shrink, clamp and regrowth with no reconcile between.
    const f = following({ scrollTop: 689, scrollHeight: 1326, clientHeight: 637 });
    // Act
    f.box.scrollHeight = 853;
    f.box.scrollTop = 52;
    f.scroll();
    // Assert
    expect(f.tail.isFollowing()).toBe(true);
  });

  it("decides nothing on an input that moved the box nowhere", () => {
    // Arrange
    const f = following();
    // Act
    f.input();
    f.scroll();
    // Assert
    expect(f.tail.isFollowing()).toBe(true);
  });

  it("stops reading the reader's earlier input once a park has re-landed", () => {
    // Arrange — the reader scrolled up, then a sent prompt parked the tail.
    const f = following();
    f.box.scrollTop = 200;
    f.gesture();
    f.tail.promptSent();
    // Act — the box's own clamp, after the park.
    f.box.scrollHeight = 700;
    f.box.scrollTop = 400;
    f.scroll();
    // Assert
    expect(f.tail.isFollowing()).toBe(true);
  });

  it("clamps the reconcile baseline at zero on a box shorter than its viewport", () => {
    // Arrange — scrollHeight minus clientHeight is NEGATIVE here.
    const f = following({ scrollTop: 0, scrollHeight: 120, clientHeight: 300 });
    f.box.scrollTop = 0;
    // Act — content arrives, still short of the viewport; nothing moved.
    f.box.scrollHeight = 200;
    // Assert
    expect(f.tail.isFollowing()).toBe(true);
  });
});

describe("TailFollow's resize", () => {
  it("re-lands a standing follow on the settled layout", () => {
    // Arrange
    const f = following({ scrollTop: 100, scrollHeight: 1000, clientHeight: 300 });
    // Act
    f.box.clientHeight = 200;
    f.box.scrollHeight = 2600;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(2600);
  });

  it("leaves a reader who scrolled away where they are", () => {
    // Arrange
    const f = following();
    f.box.scrollTop = 200;
    f.gesture();
    // Act
    f.box.scrollHeight = 1400;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(200);
  });

  it("leaves a feed no cause parked where it is", () => {
    // Arrange — REMOVED TRIGGER: a resize used to park any box the owner had
    // decided, from geometry alone, was at its bottom.
    const box = atTail();
    const a = armed(box);
    // Act
    box.scrollHeight = 1400;
    a.resize();
    // Assert
    expect(box.scrollTop).toBe(700);
  });

  it("keeps a first upward gesture when a resize lands before its scroll event", () => {
    // Arrange
    const f = following();
    // Act — the reader moves up; the resize, not the scroll event, arrives.
    f.input();
    f.box.scrollTop = 660;
    f.resize();
    // Assert
    expect([f.tail.isFollowing(), f.box.scrollTop]).toEqual([false, 660]);
  });

  it("re-lands the tail on the next size change after a clamp", () => {
    // Arrange
    const f = following({ scrollTop: 689, scrollHeight: 1326, clientHeight: 637 });
    f.box.scrollHeight = 853;
    f.box.scrollTop = 52;
    f.scroll();
    // Act
    f.box.scrollHeight = 1099;
    f.resize();
    // Assert — the fake does not clamp, so the write reads as scrollHeight.
    expect(f.box.scrollTop).toBe(1099);
  });
});

describe("TailFollow.prependCompensation", () => {
  it("shifts a reader who is not following by exactly the growth above them", () => {
    // Arrange
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.prependCompensation(250);
    // Assert
    expect(box.scrollTop).toBe(350);
  });

  it("shifts from where the reader now is, not from where it last wrote", () => {
    // Arrange — the reader moved during the render, event not yet dispatched.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    box.scrollTop = 500;
    // Act
    a.tail.prependCompensation(40);
    // Assert
    expect(box.scrollTop).toBe(540);
  });

  it("adds nothing on top of a standing follow", () => {
    // Arrange
    const f = following();
    // Act
    f.tail.prependCompensation(250);
    // Assert
    expect(f.box.scrollTop).toBe(1000);
  });

  it("does not start a follow when the shift lands the box on the tail", () => {
    // Arrange
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.prependCompensation(600);
    a.scroll();
    // Assert
    expect(a.tail.isFollowing()).toBe(false);
  });

  it("is recorded at DEBUG as prependCompensation", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const a = armed({ scrollTop: 100, scrollHeight: 1000, clientHeight: 300 });
    // Act
    a.tail.prependCompensation(250);
    // Assert
    expect(await moves(capture)).toEqual([
      { cause: "prependCompensation", from: 100, to: 350, follow: false },
    ]);
  });
});

/**
 * A centering jump's target that reads G when first read and, given BOX,
 * moves with the box's scroll afterwards, as content laid out in it does.
 */
function at(g: RevealGeometry, box?: { scrollTop: number }): CenterTarget {
  let from: number | null = null;
  return () => {
    if (box === undefined) return g;
    from ??= box.scrollTop;
    return { ...g, nodeTop: g.nodeTop - (box.scrollTop - from) };
  };
}

describe("TailFollow.selectionMoved", () => {
  /** A 300px viewport at the screen's top, and a 100px row 500px down the content, seen from scrollTop TOP. */
  const centered = (top: number) => at({ boxTop: 0, boxHeight: 300, nodeTop: 500 - top, nodeHeight: 100 });

  it("centers the selected row", () => {
    // Arrange
    const box = { scrollTop: 0, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.selectionMoved(centered(0));
    // Assert — (300 - 100) / 2 = 100 above the row: 400.
    expect(box.scrollTop).toBe(400);
  });

  it("ends a standing follow, so streaming rows cannot pull the reader off it", () => {
    // Arrange
    const f = following();
    // Act
    f.tail.selectionMoved(centered(1000));
    // Assert
    expect(f.tail.isFollowing()).toBe(false);
  });

  it("moves nothing when the feed has no row to center on", () => {
    // Arrange
    const box = { scrollTop: 200, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.selectionMoved(null);
    // Assert
    expect(box.scrollTop).toBe(200);
  });

  it("is recorded at DEBUG as selectionMoved", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const a = armed({ scrollTop: 0, scrollHeight: 1000, clientHeight: 300 });
    // Act
    a.tail.selectionMoved(centered(0));
    // Assert
    expect(await moves(capture)).toEqual([
      { cause: "selectionMoved", from: 0, to: 400, follow: false },
    ]);
  });
});

describe("TailFollow.detachedWorkSelected", () => {
  it("centers a card below the fold in the viewport", () => {
    // Arrange: a 200px card whose top is 250px down a 300px viewport.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.detachedWorkSelected(at({ boxTop: 0, boxHeight: 300, nodeTop: 250, nodeHeight: 200 }));
    // Assert: its midpoint (350) moves to the viewport's (150): 200px down.
    expect(box.scrollTop).toBe(300);
  });

  it("ends a standing follow", () => {
    // Arrange
    const f = following();
    // Act
    f.tail.detachedWorkSelected(at({ boxTop: 0, boxHeight: 300, nodeTop: 100, nodeHeight: 50 }));
    // Assert
    expect(f.tail.isFollowing()).toBe(false);
  });

  it("is recorded at DEBUG as detachedWorkSelected", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const a = armed({ scrollTop: 100, scrollHeight: 1000, clientHeight: 300 });
    // Act
    a.tail.detachedWorkSelected(at({ boxTop: 0, boxHeight: 300, nodeTop: 250, nodeHeight: 200 }));
    // Assert
    expect(await moves(capture)).toEqual([
      { cause: "detachedWorkSelected", from: 100, to: 300, follow: false },
    ]);
  });
});

describe("TailFollow.entryJumped", () => {
  it("centers a jumped-to entry below the fold in the viewport", () => {
    // Arrange: a 200px entry whose top is 250px down a 300px viewport.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.entryJumped(at({ boxTop: 0, boxHeight: 300, nodeTop: 250, nodeHeight: 200 }));
    // Assert: its midpoint (350) moves to the viewport's (150): 200px down.
    expect(box.scrollTop).toBe(300);
  });

  it("clamps at the feed's end, as the detached-work selection does", () => {
    // Arrange: an entry near the end; the box can reach 700 at most.
    const box = { scrollTop: 600, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    a.tail.entryJumped(at({ boxTop: 0, boxHeight: 300, nodeTop: 250, nodeHeight: 50 }));
    // Assert
    expect(box.scrollTop).toBe(700);
  });

  it("is recorded at DEBUG as entryJumped", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const a = armed({ scrollTop: 100, scrollHeight: 1000, clientHeight: 300 });
    // Act
    a.tail.entryJumped(at({ boxTop: 0, boxHeight: 300, nodeTop: 250, nodeHeight: 200 }));
    // Assert
    expect(await moves(capture)).toEqual([{ cause: "entryJumped", from: 100, to: 300, follow: false }]);
  });
});

/**
 * THE ONE CENTERING JUMP (owner rule, 2026-11-20): every jump centers the
 * thing it names, its own box, clamped at the feed's edges, and holds it there
 * as the layout settles until the reader's next input, a click, a park or
 * another reveal.
 */
describe("TailFollow's centering jumps", () => {
  /** A 300px viewport at the screen's top over a 3000px feed. */
  function feedBox(scrollTop = 0) {
    return { scrollTop, scrollHeight: 3000, clientHeight: 300 };
  }

  /** A target laid out at content TOP, HEIGHT tall (null: no box), read against BOX as it now scrolls. */
  function laidOut(box: { scrollTop: number }, node: { top: number; height: number } | null): {
    target: CenterTarget;
    move: (next: { top: number; height: number } | null) => void;
  } {
    let at = node;
    return {
      target: () => (at === null ? null : { boxTop: 0, boxHeight: 300, nodeTop: at.top - box.scrollTop, nodeHeight: at.height }),
      move: (next) => {
        at = next;
      },
    };
  }

  const jumps: Array<[string, (tail: TailFollow, target: CenterTarget) => void]> = [
    ["selectionMoved", (tail, target) => tail.selectionMoved(target)],
    ["detachedWorkSelected", (tail, target) => tail.detachedWorkSelected(target)],
    ["entryJumped", (tail, target) => tail.entryJumped(target)],
  ];

  it.each(jumps)("%s puts the target's center on the viewport's center", (_name, jump) => {
    // Arrange: a 100px node at 1000 in the content.
    const box = feedBox();
    const node = laidOut(box, { top: 1000, height: 100 });
    // Act
    jump(new TailFollow(box), node.target);
    // Assert: its center, 1050, on the viewport's 150.
    expect(box.scrollTop).toBe(900);
  });

  it.each(jumps)("%s clamps a target near the feed's end at the last position", (_name, jump) => {
    // Arrange
    const box = feedBox();
    const node = laidOut(box, { top: 2950, height: 40 });
    // Act
    jump(new TailFollow(box), node.target);
    // Assert
    expect(box.scrollTop).toBe(2700);
  });

  it.each(jumps)("%s clamps a target near the feed's start at the top", (_name, jump) => {
    // Arrange
    const box = feedBox(500);
    const node = laidOut(box, { top: 20, height: 40 });
    // Act
    jump(new TailFollow(box), node.target);
    // Assert
    expect(box.scrollTop).toBe(0);
  });

  it.each(jumps)("%s puts a target taller than the viewport top on top", (_name, jump) => {
    // Arrange
    const box = feedBox();
    const node = laidOut(box, { top: 1000, height: 600 });
    // Act
    jump(new TailFollow(box), node.target);
    // Assert
    expect(box.scrollTop).toBe(1000);
  });

  it.each(jumps)("%s centers the target again when the layout moves it after the jump", (_name, jump) => {
    // Arrange
    const box = feedBox();
    const node = laidOut(box, { top: 1000, height: 100 });
    const tail = new TailFollow(box);
    jump(tail, node.target);
    // Act: the target re-wraps 40px taller and lands 200px lower.
    node.move({ top: 1200, height: 140 });
    tail.onResize();
    // Assert: its center, 1270, on the viewport's 150.
    expect(box.scrollTop).toBe(1120);
  });

  it("moves nothing for a target with no box yet, and centers it once it is laid out", () => {
    // Arrange
    const box = feedBox(400);
    const node = laidOut(box, null);
    const tail = new TailFollow(box);
    tail.entryJumped(node.target);
    const before = box.scrollTop;
    // Act
    node.move({ top: 1000, height: 100 });
    tail.onResize();
    // Assert
    expect([before, box.scrollTop]).toEqual([400, 900]);
  });

  it("lets the reader's input end the hold", () => {
    // Arrange
    const box = feedBox();
    const node = laidOut(box, { top: 1000, height: 100 });
    const tail = new TailFollow(box);
    tail.entryJumped(node.target);
    // Act
    tail.onInput("wheel");
    node.move({ top: 1200, height: 100 });
    tail.onResize();
    // Assert
    expect(box.scrollTop).toBe(900);
  });

  it("lets a park end the hold", () => {
    // Arrange
    const box = feedBox();
    const node = laidOut(box, { top: 1000, height: 100 });
    const tail = new TailFollow(box);
    tail.entryJumped(node.target);
    // Act
    tail.promptSent();
    node.move({ top: 1200, height: 100 });
    tail.onResize();
    // Assert: the follow keeps the tail (this box keeps the write unclamped).
    expect(box.scrollTop).toBe(3000);
  });

  it("lets a click end the hold", () => {
    // Arrange
    const box = feedBox();
    const node = laidOut(box, { top: 1000, height: 100 });
    const tail = new TailFollow(box);
    tail.entryJumped(node.target);
    // Act
    tail.readerClicked(document.createElement("div"));
    node.move({ top: 1200, height: 100 });
    tail.onResize();
    // Assert
    expect(box.scrollTop).toBe(900);
  });

  it("drops the hold once the target loses its box, and records it", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const box = feedBox();
    const node = laidOut(box, { top: 1000, height: 100 });
    const tail = new TailFollow(box);
    tail.entryJumped(node.target);
    node.move(null);
    tail.onResize();
    // Act: the target comes back; the dropped hold no longer follows it.
    node.move({ top: 1200, height: 100 });
    tail.onResize();
    // Assert
    const record = await forwardedRecord(capture, "scroll.center-absent");
    expect([box.scrollTop, record.context]).toEqual([900, expect.objectContaining({ cause: "entryJumped", trigger: "resize", held: false })]);
  });

  it("records each centering of a held target at DEBUG", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const box = feedBox();
    const node = laidOut(box, { top: 1000, height: 100 });
    const tail = new TailFollow(box);
    tail.selectionMoved(node.target);
    // Act
    node.move({ top: 1200, height: 100 });
    tail.onResize();
    // Assert
    const record = await forwardedRecord(capture, "scroll.center-held");
    expect(record.context).toMatchObject({ cause: "selectionMoved", trigger: "resize", delta: 200 });
  });

  it("records the reader's input ending the hold at DEBUG", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const box = feedBox();
    const tail = new TailFollow(box);
    tail.detachedWorkSelected(laidOut(box, { top: 1000, height: 100 }).target);
    // Act
    tail.onInput("wheel");
    // Assert
    const record = await forwardedRecord(capture, "scroll.center-released");
    expect(record.context).toMatchObject({ cause: "detachedWorkSelected", by: "wheel" });
  });
});

/** The one measure of a centering jump's target. */
describe("centerTarget", () => {
  it("reads the span against the box's viewport, afresh on every read", () => {
    // Arrange
    let top = 400;
    const box = { getBoundingClientRect: () => boxRect({ top: 50, height: 300 }) };
    const target = centerTarget(box, () => ({ top, bottom: top + 20 }));
    // Act
    const first = target();
    top = 600;
    const second = target();
    // Assert
    expect([first, second]).toEqual([
      { boxTop: 50, boxHeight: 300, nodeTop: 400, nodeHeight: 20 },
      { boxTop: 50, boxHeight: 300, nodeTop: 600, nodeHeight: 20 },
    ]);
  });

  it("is null while the span has no box", () => {
    // Arrange
    const box = { getBoundingClientRect: () => boxRect({ top: 50, height: 300 }) };
    // Act
    const got = centerTarget(box, () => null)();
    // Assert
    expect(got).toBeNull();
  });
});

describe("elementsSpan", () => {
  /** An element laid out at TOP, HEIGHT tall. */
  function laid(top: number, height: number): HTMLElement {
    const el = document.createElement("div");
    el.getBoundingClientRect = () => boxRect({ top, height });
    return el;
  }

  it("spans the laid-out elements from the first's top to the last's bottom", () => {
    // Arrange, Act
    const got = elementsSpan([laid(280, 20), laid(300, 25)]);
    // Assert
    expect(got).toEqual({ top: 280, bottom: 325 });
  });

  it("leaves out an element that is not laid out", () => {
    // Arrange, Act
    const got = elementsSpan([laid(280, 20), laid(500, 0)]);
    // Assert
    expect(got).toEqual({ top: 280, bottom: 300 });
  });

  it("is null when no element is laid out", () => {
    // Arrange, Act
    const got = elementsSpan([laid(280, 0)]);
    // Assert
    expect(got).toBeNull();
  });
});

describe("TailFollow.composerEntryRevealed", () => {
  it("brings a subtree below the fold only as far as its bottom edge", () => {
    // Arrange: a 100px subtree whose top is 250px down a 300px viewport.
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    const moved = a.tail.composerEntryRevealed({ boxTop: 0, boxHeight: 300, nodeTop: 250, nodeHeight: 100 });
    // Assert: its bottom (350) moves to the viewport's (300): 50px down.
    expect([moved, box.scrollTop]).toEqual([true, 150]);
  });

  it("moves nothing for a subtree wholly in view", () => {
    // Arrange
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    // Act
    const moved = a.tail.composerEntryRevealed({ boxTop: 0, boxHeight: 300, nodeTop: 50, nodeHeight: 100 });
    // Assert
    expect([moved, box.scrollTop]).toEqual([false, 100]);
  });

  it("keeps a standing follow when the subtree is in view", () => {
    // Arrange
    const f = following();
    // Act
    f.tail.composerEntryRevealed({ boxTop: 0, boxHeight: 300, nodeTop: 100, nodeHeight: 50 });
    // Assert
    expect(f.tail.isFollowing()).toBe(true);
  });

  it("ends a standing follow when it moves the feed", () => {
    // Arrange
    const f = following();
    // Act
    f.tail.composerEntryRevealed({ boxTop: 0, boxHeight: 300, nodeTop: -400, nodeHeight: 50 });
    // Assert
    expect(f.tail.isFollowing()).toBe(false);
  });

  it("is recorded at DEBUG as composerEntryRevealed", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const a = armed({ scrollTop: 100, scrollHeight: 1000, clientHeight: 300 });
    // Act
    a.tail.composerEntryRevealed({ boxTop: 0, boxHeight: 300, nodeTop: 250, nodeHeight: 100 });
    // Assert
    expect(await moves(capture)).toEqual([{ cause: "composerEntryRevealed", from: 100, to: 150, follow: false }]);
  });

  it("records no move for a subtree wholly in view", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const a = armed({ scrollTop: 100, scrollHeight: 1000, clientHeight: 300 });
    // Act
    a.tail.composerEntryRevealed({ boxTop: 0, boxHeight: 300, nodeTop: 50, nodeHeight: 100 });
    // Assert
    expect(await moves(capture)).toEqual([]);
  });
});

describe("TailFollow's centering reveals land where their arithmetic says", () => {
  type Delta = (g: RevealGeometry, box: { scrollTop: number; scrollHeight: number; clientHeight: number }) => number;
  const reveals: Array<[string, (tail: TailFollow, g: RevealGeometry) => void, Delta]> = [
    ["detachedWorkSelected", (tail, g) => tail.detachedWorkSelected(at(g)), revealCenterDelta],
    ["entryJumped", (tail, g) => tail.entryJumped(at(g)), revealCenterDelta],
  ];
  const geometries: RevealGeometry[] = [
    { boxTop: 0, boxHeight: 300, nodeTop: 250, nodeHeight: 200 },
    { boxTop: 0, boxHeight: 300, nodeTop: 50, nodeHeight: 600 },
    { boxTop: 0, boxHeight: 300, nodeTop: -900, nodeHeight: 50 },
    { boxTop: 0, boxHeight: 300, nodeTop: 900, nodeHeight: 50 },
  ];

  it.each(reveals)("%s lands exactly where its delta says", (_name, reveal, delta) => {
    // Arrange
    const landed = geometries.map((g) => {
      const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
      const a = armed(box);
      // Act
      reveal(a.tail, g);
      return box.scrollTop;
    });
    // Assert
    const expected = geometries.map(
      (g) => 100 + delta(g, { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 }),
    );
    expect(landed).toEqual(expected);
  });
});

describe("latestEntryVisible", () => {
  // A 300px viewport whose top edge sits at 100.
  const at = (nodeTop: number, nodeHeight: number): RevealGeometry => ({
    boxTop: 100,
    boxHeight: 300,
    nodeTop,
    nodeHeight,
  });

  it("sees an entry wholly inside the viewport", () => {
    // Arrange + Act + Assert
    expect(latestEntryVisible(at(150, 100))).toBe(true);
  });

  it("sees an entry whose top shows above the fold and whose rest runs below it", () => {
    // Arrange + Act + Assert
    expect(latestEntryVisible(at(390, 200))).toBe(true);
  });

  it("sees an entry whose bottom shows below the viewport top and whose rest runs above it", () => {
    // Arrange + Act + Assert
    expect(latestEntryVisible(at(0, 110))).toBe(true);
  });

  it("sees an entry taller than the viewport that covers it end to end", () => {
    // Arrange + Act + Assert
    expect(latestEntryVisible(at(0, 1000))).toBe(true);
  });

  it("does not see an entry whose top sits exactly on the viewport bottom", () => {
    // Arrange + Act + Assert
    expect(latestEntryVisible(at(400, 100))).toBe(false);
  });

  it("does not see an entry whose bottom sits exactly on the viewport top", () => {
    // Arrange + Act + Assert
    expect(latestEntryVisible(at(0, 100))).toBe(false);
  });

  it("does not see an entry wholly below the fold", () => {
    // Arrange + Act + Assert
    expect(latestEntryVisible(at(900, 100))).toBe(false);
  });

  it("throws on a non-finite edge", () => {
    // Arrange + Act + Assert
    expect(() => latestEntryVisible(at(Number.NaN, 100))).toThrow(/not a real layout/);
  });

  it("throws on a negative height", () => {
    // Arrange + Act + Assert
    expect(() => latestEntryVisible(at(150, -1))).toThrow(/not a real layout/);
  });

  it("reports a geometry that is not a real layout at ERROR", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    expect(() => latestEntryVisible(at(150, Number.POSITIVE_INFINITY))).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "scroll.latest-geometry-invalid");
    expect(record.level.case).toBe("error");
  });
});

/**
 * THE AT-TAIL LATCH (owner ruling, 2026-11-20, superseding the latest-visible
 * latch of 2026-09-23): the follow starts only where the view already stands
 * at the tail, and never on the reader's own upward movement.
 *
 * The box is 1000px of content in a 300px viewport; the latest entry is the
 * last 100px of it (offset 900), read live off the box's position, so it is
 * visible exactly while `scrollTop > 600`. The tail is 700, and the view
 * stands at it while `scrollTop > 660` (`PIN_PX`).
 */
describe("TailFollow's at-tail latch", () => {
  interface Entry {
    offset: number;
    height: number;
  }

  function withLatest(scrollTop: number): ReturnType<typeof armed> & {
    box: ReanchorBox;
    entry: Entry;
    append: (height: number) => void;
  } {
    const box: ReanchorBox = { scrollTop, scrollHeight: 1000, clientHeight: 300 };
    const entry: Entry = { offset: 900, height: 100 };
    const tail = new TailFollow(box, () => ({
      boxTop: 0,
      boxHeight: box.clientHeight,
      nodeTop: entry.offset - box.scrollTop,
      nodeHeight: entry.height,
    }));
    return {
      tail,
      box,
      entry,
      ...tailTriggers(tail),
      // A new row drawn under the last one becomes the latest entry.
      append: (height: number) => {
        entry.offset = box.scrollHeight;
        entry.height = height;
        box.scrollHeight += height;
      },
    };
  }

  it("latches when the reader scrolls back down to the tail", () => {
    // Arrange
    const l = withLatest(100);
    // Act
    l.box.scrollTop = 670;
    l.gesture();
    // Assert
    expect(l.tail.isFollowing()).toBe(true);
  });

  it("does not move the view when it latches", () => {
    // Arrange
    const l = withLatest(100);
    // Act
    l.box.scrollTop = 670;
    l.gesture();
    // Assert
    expect(l.box.scrollTop).toBe(670);
  });

  it("keeps the tail in view when the next row arrives after the latch", () => {
    // Arrange
    const l = withLatest(100);
    l.box.scrollTop = 670;
    l.gesture();
    // Act
    l.append(100);
    l.tail.follow();
    // Assert
    expect(l.box.scrollTop).toBe(1100);
  });

  it("records the later follow move under latestVisible", async () => {
    // Arrange
    const l = withLatest(100);
    l.box.scrollTop = 670;
    l.gesture();
    const capture = captureLogRecords("debug");
    // Act
    l.append(100);
    l.tail.follow();
    // Assert
    expect(await moves(capture)).toEqual([{ cause: "latestVisible", from: 670, to: 1100, follow: true }]);
  });

  it("records the latch at INFO as a follow started by latestVisible", async () => {
    // Arrange — the default threshold, so the record is one the log writes.
    const l = withLatest(100);
    const capture = captureLogRecords("info");
    // Act
    l.box.scrollTop = 670;
    l.gesture();
    // Assert
    const record = await forwardedRecord(capture, "scroll.follow-started");
    expect([record.level.case, record.context]).toMatchObject(["info", { cause: "latestVisible", at: 670 }]);
  });

  it("does not latch while the latest entry is out of view", () => {
    // Arrange
    const l = withLatest(100);
    // Act
    l.box.scrollTop = 600;
    l.gesture();
    // Assert
    expect(l.tail.isFollowing()).toBe(false);
  });

  it("latches on a resize that brings the view to the tail", () => {
    // Arrange
    const l = withLatest(500);
    // Act
    l.box.clientHeight = 480;
    l.resize();
    // Assert
    expect(l.tail.isFollowing()).toBe(true);
  });

  it("latches on a row upsert that finds the view at the tail, without moving", () => {
    // Arrange
    const l = withLatest(670);
    // Act
    l.tail.follow();
    // Assert
    expect([l.tail.isFollowing(), l.box.scrollTop]).toEqual([true, 670]);
  });

  it("latches on a movement with no reader behind it that brings the view to the tail", () => {
    // Arrange
    const l = withLatest(100);
    // Act
    l.box.scrollTop = 670;
    l.scroll();
    // Assert
    expect(l.tail.isFollowing()).toBe(true);
  });

  it("ends following when the reader scrolls up while the latest entry stays partly visible", () => {
    // Arrange
    const l = withLatest(700);
    l.tail.promptSent();
    // Act
    l.box.scrollTop = 650;
    l.gesture();
    // Assert
    expect(l.tail.isFollowing()).toBe(false);
  });

  it("ends following when the reader scrolls the latest entry out of view", () => {
    // Arrange
    const l = withLatest(700);
    l.tail.promptSent();
    // Act
    l.box.scrollTop = 600;
    l.gesture();
    // Assert
    expect(l.tail.isFollowing()).toBe(false);
  });

  it("is held off while a reply selection is active", () => {
    // Arrange
    const l = withLatest(100);
    l.tail.selectionMoved(null);
    // Act
    l.box.scrollTop = 670;
    l.gesture();
    // Assert
    expect(l.tail.isFollowing()).toBe(false);
  });

  it("reports the held-off latch once per spell at the tail", async () => {
    // Arrange
    const l = withLatest(100);
    l.tail.selectionMoved(null);
    const capture = captureLogRecords("debug");
    // Act
    l.box.scrollTop = 670;
    l.gesture();
    l.box.scrollTop = 690;
    l.gesture();
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    const held = capture.sent.filter((r) => r.operation === "scroll.follow-held-by-selection");
    expect(held.length).toBe(1);
  });

  it("leaves a reader scrolled up where they are when the selection is dismissed", () => {
    // Arrange — owner ruling, 2026-11-20: a dismissal no longer returns to
    // the tail. 50px above it, the latest entry in sight.
    const l = withLatest(100);
    l.tail.selectionMoved(null);
    l.box.scrollTop = 650;
    l.gesture();
    // Act
    l.tail.selectionEnded();
    // Assert
    expect([l.tail.isFollowing(), l.box.scrollTop]).toEqual([false, 650]);
  });

  it("resumes the follow when the selection is dismissed at the tail", () => {
    // Arrange
    const l = withLatest(100);
    l.tail.selectionMoved(null);
    l.box.scrollTop = 690;
    l.gesture();
    // Act
    l.tail.selectionEnded();
    l.append(100);
    l.tail.follow();
    // Assert
    expect([l.tail.isFollowing(), l.box.scrollTop]).toEqual([true, 1100]);
  });

  it("applies again once the selection has cleared", () => {
    // Arrange
    const l = withLatest(100);
    l.tail.selectionMoved(null);
    l.tail.selectionEnded();
    l.box.scrollTop = 100;
    l.gesture();
    // Act
    l.box.scrollTop = 670;
    l.gesture();
    // Assert
    expect(l.tail.isFollowing()).toBe(true);
  });

  it("moves nothing when the selection ends in place", () => {
    // Arrange
    const l = withLatest(100);
    l.tail.selectionMoved(null);
    // Act — the selected row left the viewport (none.stay).
    l.tail.selectionEnded();
    // Assert
    expect([l.tail.isFollowing(), l.box.scrollTop]).toEqual([false, 100]);
  });

  it("latches once the reader reaches the tail after the selection ended in place", () => {
    // Arrange
    const l = withLatest(100);
    l.tail.selectionMoved(null);
    l.tail.selectionEnded();
    // Act
    l.box.scrollTop = 670;
    l.gesture();
    // Assert
    expect(l.tail.isFollowing()).toBe(true);
  });

  it("latches where the view stands when it is at the tail as the selection ends", () => {
    // Arrange
    const l = withLatest(670);
    l.tail.selectionMoved(null);
    // Act
    l.tail.selectionEnded();
    // Assert
    expect([l.tail.isFollowing(), l.box.scrollTop]).toEqual([true, 670]);
  });

  it("latches after a detached-work selection that lands at the tail", () => {
    // Arrange
    const l = withLatest(500);
    // Act — centering the card (its middle 200px below the viewport's) brings
    // the box to 700, its tail, where the latest entry shows.
    l.tail.detachedWorkSelected(at({ boxTop: 0, boxHeight: 300, nodeTop: 250, nodeHeight: 200 }));
    // Assert
    expect([l.tail.isFollowing(), l.box.scrollTop]).toEqual([true, 700]);
  });

  it("does not latch after a detached-work selection that lands above the tail", () => {
    // Arrange
    const l = withLatest(700);
    l.tail.promptSent();
    // Act — centering a card above the viewport top (its middle 725px above
    // the viewport's) brings the box from its parked 1000 up to 275.
    l.tail.detachedWorkSelected(at({ boxTop: 0, boxHeight: 300, nodeTop: -600, nodeHeight: 50 }));
    // Assert
    expect([l.tail.isFollowing(), l.box.scrollTop]).toEqual([false, 275]);
  });

  /** L with a counter of its `onTailReached` calls. */
  function counted(l: ReturnType<typeof withLatest>): { reached: number } {
    const counter = { reached: 0 };
    l.tail.onTailReached(() => {
      counter.reached += 1;
    });
    return counter;
  }

  it("tells onTailReached when the reader scrolls back to the tail", () => {
    // Arrange
    const l = withLatest(100);
    const c = counted(l);
    // Act
    l.box.scrollTop = 670;
    l.gesture();
    // Assert
    expect(c.reached).toBe(1);
  });

  it("tells onTailReached once per return, however the reader goes on scrolling at the tail", () => {
    // Arrange
    const l = withLatest(100);
    const c = counted(l);
    l.box.scrollTop = 670;
    l.gesture();
    // Act
    l.box.scrollTop = 700;
    l.gesture();
    // Assert
    expect(c.reached).toBe(1);
  });

  it("tells onTailReached when the reader jumps to the tail", () => {
    // Arrange
    const l = withLatest(100);
    const c = counted(l);
    // Act
    l.tail.jumpToTail();
    // Assert
    expect([c.reached, l.box.scrollTop]).toEqual([1, 1000]);
  });

  it("tells onTailReached once for a jump, not again for the jump's own scroll event", () => {
    // Arrange
    const l = withLatest(100);
    const c = counted(l);
    // Act
    l.tail.jumpToTail();
    l.scroll();
    // Assert
    expect(c.reached).toBe(1);
  });

  it("tells onTailReached again after the reader left the tail and came back", () => {
    // Arrange
    const l = withLatest(100);
    const c = counted(l);
    l.box.scrollTop = 670;
    l.gesture();
    l.box.scrollTop = 100;
    l.gesture();
    // Act
    l.box.scrollTop = 670;
    l.gesture();
    // Assert
    expect(c.reached).toBe(2);
  });

  it("tells onTailReached when the reader flicks up inside the tail's slack and back down", () => {
    // Arrange — parked at the tail; a small flick up ends the follow, and the
    // view, still within PIN_PX of the tail, is the reader's.
    const l = withLatest(700);
    l.tail.promptSent();
    const c = counted(l);
    l.box.scrollTop = 690;
    l.gesture();
    // Act
    l.box.scrollTop = 700;
    l.gesture();
    // Assert
    expect([l.tail.isFollowing(), c.reached]).toEqual([true, 1]);
  });

  it("does not tell onTailReached for a latch no reader input is behind", () => {
    // Arrange
    const l = withLatest(100);
    const c = counted(l);
    // Act — the box's own movement: a scroll event with no input before it.
    l.box.scrollTop = 670;
    l.scroll();
    // Assert
    expect([l.tail.isFollowing(), c.reached]).toEqual([true, 0]);
  });

  it("does not tell onTailReached for a latch a centering reveal made", () => {
    // Arrange
    const l = withLatest(100);
    const c = counted(l);
    // Act — centering brings the box to its tail, where the latest entry shows.
    l.tail.entryJumped(at({ boxTop: 0, boxHeight: 300, nodeTop: 800, nodeHeight: 50 }));
    // Assert
    expect([l.tail.isFollowing(), c.reached]).toEqual([true, 0]);
  });

  it("does not tell onTailReached for a latch a resize made", () => {
    // Arrange
    const l = withLatest(100);
    const c = counted(l);
    // Act — the viewport grows until the latest entry shows.
    l.box.clientHeight = 900;
    l.resize();
    // Assert
    expect([l.tail.isFollowing(), c.reached]).toEqual([true, 0]);
  });

  it("throws and reports at ERROR when the latest entry's geometry is not a real layout as a refusal is recorded", async () => {
    // Arrange
    const l = withLatest(100);
    l.entry.height = Number.NaN;
    const capture = captureLogRecords();
    // Act
    l.box.scrollTop = 650;
    expect(() => l.gesture()).toThrow(/not a real layout/);
    // Assert
    const record = await forwardedRecord(capture, "scroll.latest-geometry-invalid");
    expect(record.level.case).toBe("error");
  });

  describe("TailFollow.readerClicked", () => {
    /** A reader following at the tail, with the clicked entry as the latest. */
    function following(): ReturnType<typeof withLatest> {
      const l = withLatest(670);
      l.scroll();
      return l;
    }
    const clicked = (): Element => document.createElement("div");

    it("ends a standing follow", () => {
      // Arrange
      const l = following();
      // Act
      l.tail.readerClicked(clicked());
      // Assert
      expect(l.tail.isFollowing()).toBe(false);
    });

    it("moves nothing when the clicked entry grows", () => {
      // Arrange
      const l = following();
      l.input();
      l.tail.readerClicked(clicked());
      // Act -- the entry the click expanded is 200px taller.
      l.entry.height += 200;
      l.box.scrollHeight += 200;
      l.resize();
      // Assert
      expect(l.box.scrollTop).toBe(670);
    });

    it("holds the at-tail latch off through a scroll event with no reader input", () => {
      // Arrange
      const l = following();
      l.tail.readerClicked(clicked());
      // Act
      l.scroll();
      // Assert
      expect(l.tail.isFollowing()).toBe(false);
    });

    it("lets the reader's next input latch the follow again", () => {
      // Arrange
      const l = following();
      l.tail.readerClicked(clicked());
      // Act
      l.gesture();
      // Assert
      expect(l.tail.isFollowing()).toBe(true);
    });

    it("records the hold with where the view stands", async () => {
      // Arrange
      const l = following();
      const capture = captureLogRecords("debug");
      // Act
      l.tail.readerClicked(clicked());
      // Assert
      const record = await forwardedRecord(capture, "scroll.click-held");
      expect([record.level.case, record.context]).toMatchObject(["info", { by: "click", at: 670, row: "none" }]);
    });
  });

  describe("TailFollow.selectionClicked", () => {
    it("holds the follow off for as long as a clicked selection stands", () => {
      // Arrange
      const l = withLatest(670);
      l.scroll();
      l.tail.selectionClicked(null, true);
      // Act -- the reader's next input would latch, but a selection stands.
      l.gesture();
      // Assert
      expect(l.tail.isFollowing()).toBe(false);
    });

    it("lets the reader's next input latch once a click ended the selection", () => {
      // Arrange
      const l = withLatest(670);
      l.scroll();
      l.tail.selectionClicked(null, true);
      l.tail.selectionClicked(null, false);
      // Act
      l.gesture();
      // Assert
      expect(l.tail.isFollowing()).toBe(true);
    });

    it("moves nothing when the clicked bubble expands", () => {
      // Arrange
      const l = withLatest(670);
      l.scroll();
      l.input();
      l.tail.selectionClicked(null, true);
      // Act
      l.entry.height += 200;
      l.box.scrollHeight += 200;
      l.resize();
      // Assert
      expect(l.box.scrollTop).toBe(670);
    });

    it.each([
      [true, "selection-click"],
      [false, "deselection-click"],
    ])("records a click that selected=%s as %s", async (selected, by) => {
      // Arrange
      const l = withLatest(670);
      const capture = captureLogRecords("debug");
      // Act
      l.tail.selectionClicked(null, selected);
      // Assert
      const record = await forwardedRecord(capture, "scroll.click-held");
      expect(record.context).toMatchObject({ by });
    });
  });
});

/**
 * A READER SCROLLING UP IS IN CONTROL (owner ruling, 2026-11-20): while a
 * response streams in, the follow never re-latches under the reader's upward
 * gesture or its momentum, so no write pulls them back to the tail. The
 * follow re-latches only once the reader is back at the tail.
 *
 * The box CLAMPS like a browser's: 300px of viewport over content whose last
 * entry is a streaming response taller than the view (offset 400, 600px), so
 * any small scroll up leaves part of it in sight.
 */
describe("TailFollow leaves a reader scrolling up in control", () => {
  interface Streaming extends TailTriggers {
    tail: TailFollow;
    box: ReanchorBox;
    /** The streaming response grows by PX at the tail. */
    grow: (px: number) => void;
    /** The feed's one root row, the streaming response. */
    row: Element;
  }

  /** A box clamping its position into [0, scrollHeight - clientHeight], like a browser's. */
  function clampedBox(scrollHeight: number, clientHeight: number, scrollTop: number): ReanchorBox {
    let top = scrollTop;
    const box: ReanchorBox = {
      scrollHeight,
      clientHeight,
      get scrollTop(): number {
        return top;
      },
      set scrollTop(next: number) {
        top = Math.max(0, Math.min(next, box.scrollHeight - box.clientHeight));
      },
    };
    return box;
  }

  /**
   * A feed following its tail while a response streams: CONTENT px of content
   * in a VIEW px viewport, the response spanning from OFFSET to the end.
   */
  function streaming(content = 1000, view = 300, offset = 400): Streaming {
    const box = clampedBox(content, view, content - view);
    const host = document.createElement("main");
    const row = document.createElement("div");
    host.append(row);
    const rows: AnchorRows = {
      host,
      viewportTop: () => 0,
      edges: () => ({ top: offset - box.scrollTop, bottom: box.scrollHeight - box.scrollTop }),
    };
    const tail = new TailFollow(
      box,
      () => ({
        boxTop: 0,
        boxHeight: box.clientHeight,
        nodeTop: offset - box.scrollTop,
        nodeHeight: box.scrollHeight - offset,
      }),
      rows,
    );
    tail.promptSent();
    return {
      tail,
      box,
      row,
      ...tailTriggers(tail),
      grow: (px: number) => {
        box.scrollHeight += px;
      },
    };
  }

  /** The operations of the records a capture holds, in order. */
  async function operations(capture: LogCapture): Promise<string[]> {
    capture.logger.flush();
    await Promise.resolve();
    return capture.sent.map((r) => r.operation);
  }

  it("ends the follow when the reader scrolls up while the latest entry stays partly visible", () => {
    // Arrange
    const s = streaming();
    // Act — 50px up; the response's last 350px still show.
    s.box.scrollTop = 650;
    s.gesture();
    // Assert
    expect(s.tail.isFollowing()).toBe(false);
  });

  it("does not move the box on a write after the reader scrolled up with the latest entry partly visible", () => {
    // Arrange
    const s = streaming();
    s.box.scrollTop = 650;
    s.gesture();
    // Act — the response streams 80px more.
    s.grow(80);
    s.tail.follow();
    // Assert
    expect(s.box.scrollTop).toBe(650);
  });

  it("does not re-latch on a flick up that stays inside the tail's slack", () => {
    // Arrange
    const s = streaming();
    // Act — 10px up: within PIN_PX of the tail, but the reader's own rise.
    s.box.scrollTop = 690;
    s.gesture();
    s.grow(5);
    s.tail.follow();
    // Assert
    expect([s.tail.isFollowing(), s.box.scrollTop]).toEqual([false, 690]);
  });

  it("never moves the box down through a momentum flick upward with writes between its frames", () => {
    // Arrange — a flick decelerating: the first frames carry the reader's
    // input, the momentum frames after it only scroll events.
    const s = streaming();
    const frames: Array<[number, boolean]> = [
      [692, true],
      [680, true],
      [664, false],
      [651, false],
      [643, false],
      [640, false],
    ];
    const seen: number[] = [];
    // Act
    for (const [top, input] of frames) {
      s.box.scrollTop = top;
      if (input) s.gesture();
      else s.scroll();
      s.grow(30);
      s.tail.follow();
      s.resize();
      seen.push(s.box.scrollTop);
    }
    // Assert
    expect(seen).toEqual(frames.map(([top]) => top));
  });

  it("re-latches when the reader scrolls back down to the tail", () => {
    // Arrange
    const s = streaming();
    s.box.scrollTop = 400;
    s.gesture();
    s.grow(100);
    s.tail.follow();
    // Act — the tail is now 1100 - 300 = 800.
    s.box.scrollTop = 800;
    s.gesture();
    // Assert
    expect(s.tail.isFollowing()).toBe(true);
  });

  it("follows the next write once the reader is back at the tail", () => {
    // Arrange
    const s = streaming();
    s.box.scrollTop = 400;
    s.gesture();
    s.box.scrollTop = 700;
    s.gesture();
    // Act
    s.grow(120);
    s.tail.follow();
    // Assert
    expect(s.box.scrollTop).toBe(820);
  });

  it("does not re-latch a reader scrolling down who stops short of the tail", () => {
    // Arrange
    const s = streaming();
    s.box.scrollTop = 400;
    s.gesture();
    // Act — 50px above the tail, beyond PIN_PX.
    s.box.scrollTop = 650;
    s.gesture();
    s.grow(30);
    s.tail.follow();
    // Assert
    expect([s.tail.isFollowing(), s.box.scrollTop]).toEqual([false, 650]);
  });

  const parks: Array<[string, (s: Streaming) => void]> = [
    ["promptSent", (s) => s.tail.promptSent()],
    ["promptHeld", (s) => s.tail.promptHeld()],
    ["initialPlacement", (s) => s.tail.initialPlacement()],
    ["replaceRestore", (s) => s.tail.replaceRestore()],
    ["rolledBack", (s) => s.tail.rolledBack()],
    ["workspaceSelected", (s) => s.tail.workspaceSelected()],
    ["tailJumped", (s) => s.tail.jumpToTail()],
  ];

  it.each(parks)("%s still lands a reader who scrolled up at the tail, and follows", (_cause, act) => {
    // Arrange
    const s = streaming();
    s.box.scrollTop = 400;
    s.gesture();
    s.grow(100);
    s.tail.follow();
    // Act
    act(s);
    // Assert
    expect([s.box.scrollTop, s.tail.isFollowing()]).toEqual([800, true]);
  });

  it("records a refused latch at DEBUG once per gesture, with how far the view is from the tail", async () => {
    // Arrange
    const s = streaming();
    const capture = captureLogRecords("debug");
    // Act — one gesture up over two frames, with a write after each.
    s.box.scrollTop = 680;
    s.gesture();
    s.tail.follow();
    s.box.scrollTop = 650;
    s.gesture();
    s.tail.follow();
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    const refused = capture.sent.filter((r) => r.operation === "scroll.follow-latch-refused");
    expect([refused.length, refused[0]?.level.case, refused[0]?.context]).toMatchObject([
      1,
      "debug",
      { trigger: "scroll", at: 680, from_tail: 20, reader_rose: true, latest_visible: true },
    ]);
  });

  it("records a refused latch again for a new gesture in the other direction", async () => {
    // Arrange
    const s = streaming();
    s.box.scrollTop = 400;
    s.gesture();
    const capture = captureLogRecords("debug");
    // Act — the reader turns and scrolls down, short of the tail.
    s.box.scrollTop = 500;
    s.gesture();
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    const refused = capture.sent.filter((r) => r.operation === "scroll.follow-latch-refused");
    expect(refused.map((r) => r.context)).toMatchObject([
      { trigger: "scroll", at: 500, from_tail: 200, reader_rose: false, latest_visible: true },
    ]);
  });

  it("keeps the black-friday-ceac-prep reader where they scrolled: no re-latch, no pull to the tail", async () => {
    // Arrange — the logged sequence (2026-11-20 15:15:17.985): following at
    // the tail, 2470, under a response taller than the 600px view; the reader
    // wheeled up to 2188 with the response still in sight, the follow ended
    // and re-latched in the same ms, and the next write moved the feed
    // latestVisible from=2188 to=2470.
    const s = streaming(3070, 600, 1900);
    const capture = captureLogRecords("debug");
    // Act
    s.box.scrollTop = 2188;
    s.gesture();
    s.grow(40);
    s.tail.follow();
    // Assert
    const ops = await operations(capture);
    const ended = ops.indexOf("scroll.follow-ended");
    expect({
      at: s.box.scrollTop,
      endedFollow: ended >= 0,
      restarted: ops.slice(ended).includes("scroll.follow-started"),
      moved: await moves(capture),
    }).toEqual({ at: 2188, endedFollow: true, restarted: false, moved: [] });
  });
});

describe("revealGeometry", () => {
  it("reads the box and the node off their live rects", () => {
    // Arrange — jsdom lays nothing out, so each rect is scripted.
    const rect = (top: number, height: number): (() => DOMRect) => () => boxRect({ top, height });
    const box = document.createElement("div");
    const node = document.createElement("div");
    box.getBoundingClientRect = rect(40, 300);
    node.getBoundingClientRect = rect(500, 120);
    // Act + Assert
    expect(revealGeometry(box, node)).toEqual({ boxTop: 40, boxHeight: 300, nodeTop: 500, nodeHeight: 120 });
  });
});

describe("bubbleNodeRevealed", () => {
  it("moves a bubble's box by the amount the node needs", () => {
    // Arrange
    const box = { scrollTop: 40 };
    // Act
    bubbleNodeRevealed(box, 70);
    // Assert
    expect(box.scrollTop).toBe(110);
  });
});

describe("collapseClicked", () => {
  it("shows a collapsed section's preview from its top", () => {
    // Arrange — the reader left the expanded box scrolled.
    const section = { scrollTop: 240 };
    // Act
    collapseClicked(section);
    // Assert
    expect(section.scrollTop).toBe(0);
  });
});

/**
 * THE USER OWNS THE SCROLL, MECHANIZED (owner rule, 2026-09-23).
 *
 * scroll.ts is the ONE module that writes a scroll position. Every other
 * module is scanned for a scroll write -- a `scrollTop`/`scrollLeft`
 * assignment, a `scrollIntoView`/`scrollTo`/`scrollBy` call, a park, place or
 * shift -- and a hit fails here, so an implicit mover cannot come back quietly.
 * Inside scroll.ts, the writes are held to the named list: the tail owner's
 * two primitives, the reader's own wheel redirect, the reader's own
 * collapse click, a response node's reveal inside its capped bubble, the
 * reader's own drag at the rail's edge, and the reader's own switch to a
 * workspace whose rail row is out of view. A bubble's scroll box has no other
 * writer.
 */
describe("no scroll write outside scroll.ts", () => {
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
  const sources = import.meta.glob("../src/**/*.ts", {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;

  /** Every src module's text except scroll.ts, which IS the owner. */
  const others = Object.entries(sources).filter(([path]) => !path.endsWith("/scroll.ts"));
  const own = Object.entries(sources).find(([path]) => path.endsWith("/scroll.ts"))?.[1] ?? "";

  /** The modules whose text matches PATTERN, by path. */
  const offending = (pattern: RegExp): string[] =>
    others.filter(([, src]) => pattern.test(src)).map(([path]) => path);

  it("scans a real set of sibling modules, so an empty glob cannot pass it", () => {
    // Arrange + Act + Assert
    expect(others.length).toBeGreaterThan(5);
  });

  it("finds no scrollTop or scrollLeft assignment in any other module", () => {
    // Arrange + Act
    const found = offending(/\.scroll(?:Top|Left)\s*(?:=(?!=)|\+=|-=)/);
    // Assert
    expect(found).toEqual([]);
  });

  it("finds no scrollIntoView, scrollTo or scrollBy call in any other module", () => {
    // Arrange + Act
    const found = offending(/\b(?:scrollIntoView|scrollTo|scrollBy)\s*\(/);
    // Assert
    expect(found).toEqual([]);
  });

  it("finds no park or place call in any other module", () => {
    // Arrange + Act
    const found = offending(/\.(?:park|place)\s*\(/);
    // Assert
    expect(found).toEqual([]);
  });

  it("finds no shift-by-an-amount call in any other module", () => {
    // Arrange + Act — an argument-less `shift()` is Array.prototype.shift.
    const found = offending(/\.shift\s*\(\s*[^)\s]/);
    // Assert
    expect(found).toEqual([]);
  });

  it("holds scroll.ts's own writes to the named list", () => {
    // Arrange + Act
    const writes = own
      .split("\n")
      .filter((line) => /\.scroll(?:Top|Left)\s*(?:=(?!=)|\+=|-=)/.test(line))
      .map((line) => line.trim());
    // Assert
    expect(writes).toEqual([
      "this.box.scrollTop = this.box.scrollHeight;",
      "if (delta !== 0) this.box.scrollTop = to;",
      "redirect.to.scrollTop += redirect.delta;",
      "section.scrollTop = 0;",
      "box.scrollTop += delta;",
      "scroller.scrollTop += step;",
      "region.scrollTop += delta;",
      "region.scrollTop = top;",
      "box.scrollTop += delta;",
      "box.scrollTop = chatEndTop(box);",
      "box.scrollTop = to;",
    ]);
  });

  it("holds scroll.ts to no scrollIntoView, scrollTo or scrollBy call", () => {
    // Arrange + Act + Assert
    expect(/\b(?:scrollIntoView|scrollTo|scrollBy)\s*\(/.test(own)).toBe(false);
  });
});

describe("every centering jump goes through the one centering jump", () => {
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- as above: tsc reads import.meta.glob's raw result as `unknown` without it.
  const sources = import.meta.glob("../src/**/*.ts", {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;
  const own = Object.entries(sources).find(([path]) => path.endsWith("/scroll.ts"))?.[1] ?? "";
  const others = Object.entries(sources).filter(([path]) => !path.endsWith("/scroll.ts"));

  /** The body of TailFollow's method NAME, from its signature to its closing brace. */
  function methodBody(name: string): string {
    const start = own.indexOf(`\n  ${name}(`);
    if (start < 0) throw new Error(`no method ${name}`);
    return own.slice(start, own.indexOf("\n  }\n", start));
  }

  it.each(["selectionMoved", "detachedWorkSelected", "entryJumped"])("routes %s through centerOn", (name) => {
    // Arrange, Act
    const body = methodBody(name);
    // Assert
    expect(/this\.(?:centerOn|revealCentered)\(/.test(body)).toBe(true);
  });

  it("computes a centering move in one place only, the hold's", () => {
    // Arrange, Act
    const calls = own.match(/revealCenterDelta\(/g)?.length;
    // Assert: its definition and the one call in `recenter`.
    expect([calls, methodBody("private recenter").includes("revealCenterDelta(")]).toEqual([2, true]);
  });

  it("hands every jump a target made by centerTarget, never a measured geometry", () => {
    // Arrange
    const jumpCalls = others.flatMap(([path, src]) =>
      [...src.matchAll(/\.(selectionMoved|detachedWorkSelected|entryJumped)\(([^;]*)\);/g)].map((m) => [path, m[2]]),
    );
    // Act
    const strays = jumpCalls.filter(([, arg]) => arg !== "null" && arg !== "target" && !arg.startsWith("centerTarget("));
    // Assert
    expect([jumpCalls.length > 0, strays]).toEqual([true, []]);
  });
});

describe("sectionFor detached box", () => {
  it("falls back to the box when the chain runs out before reaching the feed", () => {
    // Arrange — a box whose ancestors end at a root that is NOT the feed, the
    // shape a section removed from the document leaves mid-render.
    const feed = node("feed");
    const orphanRoot = node("orphan-root");
    const output = node("bare-output", { parentElement: orphanRoot });
    // Act + Assert — no card was found and the walk still terminated.
    expect(sectionFor(output, feed, isSection).name).toBe("bare-output");
  });
});

/**
 * `observeScrollBox` — THE FOOTER OCCLUSION, at its source.
 *
 * The docked progress footer is laid out BELOW the scroll box, so the box's
 * height is already the window's minus the footer's: the space is reserved by
 * the layout and never needed reserving again. What went stale was the
 * POSITION — a footer that appeared or grew AFTER the render that parked the
 * tail shrank the box under a `scrollTop` nobody moved, leaving the last bubble
 * that many pixels below the fold and clipped by the footer's top edge.
 *
 * These drive a real element with scripted geometry, because the subscription
 * being wired to THAT element is half of what is under test.
 */
describe("observeScrollBox", () => {
  /**
   * Let the MutationObserver keeping the watched-child set in step with the
   * DOM deliver. Its callback is a microtask checkpoint rather than a
   * synchronous call, so a child appended a line earlier is not yet watched.
   */
  function flushMutations(): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, 0));
  }

  /**
   * A real element that answers scroll geometry, since jsdom lays nothing out.
   * `scrollTop` clamps into the scrollable range on write, as a browser's does
   * — which is what turns the tail owner's "assign scrollHeight" into the bottom.
   */
  function scrollBox(init: { scrollHeight: number; clientHeight: number; scrollTop: number }) {
    const element = document.createElement("div");
    // The box's CONTENT root, as the real markup has one (`#feed` inside
    // `#feed-scroll`): the box's scrollHeight is what this child measures, so
    // growth is reported against the child and never against the box.
    const content = document.createElement("main");
    element.append(content);
    let scrollHeight = init.scrollHeight;
    let clientHeight = init.clientHeight;
    let scrollTop = init.scrollTop;
    Object.defineProperties(element, {
      scrollHeight: { get: () => scrollHeight },
      clientHeight: { get: () => clientHeight },
      scrollTop: {
        get: () => scrollTop,
        set: (next: number) => {
          scrollTop = Math.max(0, Math.min(next, scrollHeight - clientHeight));
        },
      },
    });
    return {
      element,
      content,
      /** The footer appearing or growing by PX: the box loses that height. */
      loseHeight: (px: number) => {
        clientHeight -= px;
      },
      /** A deferred expansion settling: the content gains PX, the box none. */
      contentGrows: (px: number) => {
        scrollHeight += px;
        scrollTop = Math.max(0, Math.min(scrollTop, scrollHeight - clientHeight));
      },
      top: () => scrollTop,
    };
  }

  it("re-lands the tail on the shrunken viewport when the footer takes height", () => {
    // Arrange — a reader parked at the tail: 700 + 300 viewport = 1000.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act — the footer settles after the render and eats 48px of the box.
    box.loseHeight(48);
    fireResize(box.element);
    // Assert — the tail is the new bottom, so the last bubble clears the strip.
    expect(box.top()).toBe(748);
  });

  it("re-lands by exactly the height the footer took", () => {
    // Arrange — the same box, so the delta is the only thing being read.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const before = box.top();
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act
    box.loseHeight(48);
    fireResize(box.element);
    // Assert — the reservation IS the footer's height, not an approximation.
    expect(box.top() - before).toBe(48);
  });

  it("records the move a resize made at INFO, with its cause", async () => {
    // Arrange — the default threshold: a move the reader sees is one the log writes.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act
    box.loseHeight(48);
    fireResize(box.element);
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved");
    expect([record.level.case, record.context]).toMatchObject([
      "info",
      { cause: "initialPlacement", from: 700, to: 748, follow: true },
    ]);
  });

  it("records a move no cause wrote and no reader input made, at INFO", async () => {
    // Arrange — a standing follow at the tail of 1000 - 300.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act — the browser pulls the box 32px off its tail on its own.
    box.element.scrollTop = 668;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved-uncaused");
    expect([record.level.case, record.context]).toMatchObject([
      "info",
      { from: 700, to: 668, scroll_height: 1000, client_height: 300, following: true, cause: "initialPlacement" },
    ]);
  });

  it("parks the tail again when the browser moves a standing follow off it", async () => {
    // Arrange — a standing follow at the tail of 1000 - 300.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act — the browser pulls the box 16px up on its own, at an unchanged size.
    box.element.scrollTop = 684;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved");
    expect([box.top(), tail.isFollowing(), record.context]).toMatchObject([
      700,
      true,
      { cause: "initialPlacement", from: 684, to: 700, follow: true },
    ]);
  });

  it("parks the tail on the scroll event even when a render read the move first", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act — a render asks before the browser's scroll event arrives.
    box.element.scrollTop = 684;
    tail.isFollowing();
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(box.top()).toBe(700);
  });

  it("leaves the reader's own move where the reader put it", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act — a wheel precedes the movement.
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 684;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect([box.top(), tail.isFollowing()]).toEqual([684, false]);
  });

  it("does not move a box no follow stands on", () => {
    // Arrange — the reader scrolled away, so no follow stands.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Act — the browser then moves the box on its own.
    box.element.scrollTop = 384;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect([box.top(), tail.isFollowing()]).toEqual([384, false]);
  });

  it("does not park a move a release took over before its scroll event", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act — the browser moves the box, then the reader steps the selection
    // (which releases the follow) before the scroll event arrives.
    box.element.scrollTop = 684;
    tail.selectionMoved(null);
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect([box.top(), tail.isFollowing()]).toEqual([684, false]);
  });

  it("leaves the box's clamp on shrinking content where the clamp put it", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act — the content loses 100px and the box clamps to its new tail.
    box.contentGrows(-100);
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect([box.top(), tail.isFollowing()]).toEqual([600, true]);
  });

  it("states the content's last change on a move no cause wrote", async () => {
    // Arrange — the stamp's text changes inside the box, then the box moves.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const stamp = document.createElement("span");
    stamp.className = "q-when";
    stamp.textContent = "1m 5s ago";
    box.content.append(stamp);
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act
    stamp.textContent = "1m 6s ago";
    await Promise.resolve();
    box.element.scrollTop = 684;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved-uncaused");
    expect(record.context).toMatchObject({
      from: 700,
      to: 684,
      mutated: ["span.q-when"],
      mutations: 1,
    });
  });

  it("names the change still undelivered when a follow inside its task finds the move", async () => {
    // Arrange — a standing follow, an old delivered change.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const stamp = document.createElement("span");
    stamp.className = "merge-clock";
    box.content.append(stamp);
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    stamp.textContent = "3s";
    await Promise.resolve();
    const capture = captureLogRecords("info");
    // Act — in ONE task: a render changes the content, the box moves, the render's follow runs.
    const card = document.createElement("article");
    card.className = "feed-item";
    box.content.append(card);
    box.element.scrollTop = 684;
    tail.follow();
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved-uncaused");
    expect(record.context).toMatchObject({
      from: 700,
      to: 684,
      detected_by: "follow",
      mutated: ["main"],
      undelivered: true,
    });
  });

  it.each([
    { name: "a scroll event", act: (el: HTMLElement) => el.dispatchEvent(new Event("scroll")), want: "scroll" },
    { name: "a read of the follow", act: (_el: HTMLElement, tail: TailFollow) => tail.isFollowing(), want: "read" },
  ])("names $name as what found the move", async ({ act, want }) => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act
    box.element.scrollTop = 684;
    act(box.element, tail);
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved-uncaused");
    expect(record.context).toMatchObject({ detected_by: want });
  });

  it("says how many of its own writes ago the box stood where the browser moved it", async () => {
    // Arrange — parked at 600, then (content grown) at 700.
    const box = scrollBox({ scrollHeight: 900, clientHeight: 300, scrollTop: 600 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    box.contentGrows(100);
    tail.follow();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act — the browser takes the box back to the earlier placement.
    box.element.scrollTop = 600;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved-uncaused");
    expect([record.context, typeof record.context?.ms_since_write]).toMatchObject([
      { from: 700, to: 600, returned_to: 1 },
      "number",
    ]);
  });

  it("says the browser moved the box to none of its own writes", async () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act
    box.element.scrollTop = 650;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved-uncaused");
    expect(record.context).toMatchObject({ returned_to: null });
  });

  it("says no change was witnessed when the content never changed", async () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act
    box.element.scrollTop = 684;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved-uncaused");
    expect(record.context).toMatchObject({ from: 700, to: 684, mutated: "none" });
  });

  it("stops witnessing the content once unsubscribed", async () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const stamp = document.createElement("span");
    box.content.append(stamp);
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    const stop = observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act
    stop();
    stamp.textContent = "changed";
    await Promise.resolve();
    tail.isFollowing();
    box.element.scrollTop = 684;
    tail.isFollowing();
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved-uncaused");
    expect(record.context).toMatchObject({ mutated: "none" });
  });

  it("does not record the reader's own move as uncaused", async () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act — a wheel precedes the movement, so the reader made it.
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 668;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.filter((r) => r.operation === "scroll.feed-moved-uncaused")).toEqual([]);
  });

  it("does not record the box's clamp on shrinking content as uncaused", async () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act — the content loses 100px and the box clamps its position to 600.
    box.contentGrows(-100);
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.filter((r) => r.operation === "scroll.feed-moved-uncaused")).toEqual([]);
  });

  it("records the box's viewport height change at INFO", async () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act — the footer takes 48px of the box.
    box.loseHeight(48);
    fireResize(box.element);
    // Assert — the size as the resize found it, before the follow re-landed it.
    const record = await forwardedRecord(capture, "scroll.box-resized");
    expect([record.level.case, record.context]).toMatchObject([
      "info",
      {
        scroll_height: 1000,
        scroll_height_delta: 0,
        client_height: 252,
        client_height_delta: -48,
        at: 700,
        following: true,
        cause: "initialPlacement",
      },
    ]);
  });

  it("records the content height change at INFO", async () => {
    // Arrange — a reader away from the tail, so no follow stands.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 200 });
    const tail = new TailFollow(box.element);
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act
    box.contentGrows(120);
    fireResize(box.content);
    // Assert
    const record = await forwardedRecord(capture, "scroll.box-resized");
    expect(record.context).toMatchObject({
      scroll_height: 1120,
      scroll_height_delta: 120,
      client_height_delta: 0,
      at: 200,
      following: false,
      cause: "none",
    });
  });

  it("records no resize when neither height changed", async () => {
    // Arrange — an observer delivery for a box whose sizes stand where they were.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const capture = captureLogRecords("info");
    // Act
    fireResize(box.element);
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.filter((r) => r.operation === "scroll.box-resized")).toEqual([]);
  });

  it("leaves a reader who scrolled away exactly where they are", () => {
    // Arrange — the reader left the tail, so nothing may pull them back.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 200;
    box.element.dispatchEvent(new Event("scroll"));
    // Act
    box.loseHeight(48);
    fireResize(box.element);
    // Assert
    expect(box.top()).toBe(200);
  });

  it("hears the box's own scroll events, so a gesture ends the follow", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act — the reader wheels up and the browser dispatches the scroll event.
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("re-lands the tail when the content grows with no render behind it", () => {
    // Arrange — parked at the tail, and nothing else will touch this feed.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act — a bubble the wire pushed unfolded finishes fetching its page and
    // paints 120px of rows into its panel: growth no render performed.
    box.contentGrows(120);
    fireResize(box.content);
    // Assert — the tail is the new bottom, not 120px below the fold.
    expect(box.top()).toBe(820);
  });

  it("leaves a reader who scrolled away where they are when the content grows", () => {
    // Arrange — the reader left the tail, so growth below them is not theirs.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 200;
    box.element.dispatchEvent(new Event("scroll"));
    // Act
    box.contentGrows(120);
    fireResize(box.content);
    // Assert
    expect(box.top()).toBe(200);
  });

  it("watches a content root mounted after the subscription", async () => {
    // Arrange — the box is wired before its content exists, as a mount that
    // creates the scroll zone first and fills it after would leave it.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    const late = document.createElement("section");
    // Act
    box.element.append(late);
    await flushMutations();
    // Assert — the child set follows the DOM, so the late root is watched.
    expect(() => fireResize(late)).not.toThrow();
  });

  it("stops watching a content root removed from the box", async () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act
    box.content.remove();
    await flushMutations();
    // Assert — an element the box no longer holds is nothing to re-park for.
    expect(() => fireResize(box.content)).toThrow(/no ResizeObserver/);
  });

  it("hears the reader's wheel, so their gesture is attributable to them", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("hears a pointer on its scrollbar", () => {
    // Arrange — a scrollbar drag issues no wheel and no key.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act
    box.element.dispatchEvent(new Event("pointerdown"));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("hears a key pressed inside it", () => {
    // Arrange — PageUp and the arrows scroll the box that holds the focus.
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act — dispatched on a descendant, so the bubbling is under test too.
    box.content.dispatchEvent(new Event("keydown", { bubbles: true }));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("hears a touch drag", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    observeScrollBox(box.element, tail);
    // Act
    box.element.dispatchEvent(new Event("touchstart"));
    box.element.scrollTop = 400;
    box.element.dispatchEvent(new Event("scroll"));
    // Assert
    expect(tail.isFollowing()).toBe(false);
  });

  it("stops hearing the reader's input when its unsubscriber is called", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    const unobserve = observeScrollBox(box.element, tail);
    // Act
    unobserve();
    box.element.dispatchEvent(new Event("wheel"));
    box.element.scrollTop = 400;
    // Assert — a released mount leaves no listener on the next one's element.
    expect(tail.isFollowing()).toBe(true);
  });

  it("stops observing the content when its unsubscriber is called", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    const unobserve = observeScrollBox(box.element, tail);
    // Act
    unobserve();
    // Assert — the content half is released with the box half, not left on.
    expect(() => fireResize(box.content)).toThrow(/no ResizeObserver/);
  });

  it("stops observing the box when its unsubscriber is called", () => {
    // Arrange
    const box = scrollBox({ scrollHeight: 1000, clientHeight: 300, scrollTop: 700 });
    const tail = new TailFollow(box.element);
    tail.initialPlacement();
    const unobserve = observeScrollBox(box.element, tail);
    // Act
    unobserve();
    // Assert — nothing watches it any more, so a fire finds no observer.
    expect(() => fireResize(box.element)).toThrow(/no ResizeObserver/);
  });
});

/**
 * THE DETACHED-WORK SELECTION'S ARITHMETIC: how far the feed moves to CENTER
 * the card the reader picked in the footer (owner ruling, 2026-09-23).
 */
describe("revealCenterDelta", () => {
  /** A 300px viewport at the top of the screen over a 1000px feed. */
  const view = { boxTop: 0, boxHeight: 300 };

  it.each([
    {
      name: "centers a card in the middle of the feed",
      node: { nodeTop: 400, nodeHeight: 100 },
      box: { scrollTop: 200, scrollHeight: 1000, clientHeight: 300 },
      // midpoint 450 onto 150: +300, inside the range.
      want: 300,
    },
    {
      name: "moves nothing for a card already centered",
      node: { nodeTop: 100, nodeHeight: 100 },
      box: { scrollTop: 200, scrollHeight: 1000, clientHeight: 300 },
      want: 0,
    },
    {
      name: "clamps at the feed's TOP for a card near its start",
      node: { nodeTop: 20, nodeHeight: 40 },
      box: { scrollTop: 30, scrollHeight: 1000, clientHeight: 300 },
      // centering asks -110, but the feed is only 30px from its start.
      want: -30,
    },
    {
      name: "clamps at the feed's BOTTOM for a card near its end",
      node: { nodeTop: 250, nodeHeight: 40 },
      box: { scrollTop: 650, scrollHeight: 1000, clientHeight: 300 },
      // centering asks +120, but only 50px of range remain below.
      want: 50,
    },
    {
      name: "aligns a card taller than the viewport at the viewport's top",
      node: { nodeTop: 180, nodeHeight: 900 },
      box: { scrollTop: 0, scrollHeight: 3000, clientHeight: 300 },
      want: 180,
    },
    {
      name: "brings a card above the viewport down to center",
      node: { nodeTop: -200, nodeHeight: 100 },
      box: { scrollTop: 500, scrollHeight: 1000, clientHeight: 300 },
      // midpoint -150 onto 150: -300.
      want: -300,
    },
  ])("$name", ({ node, box, want }) => {
    // Arrange, Act
    const got = revealCenterDelta({ ...view, ...node }, box);

    // Assert
    expect(got).toBe(want);
  });
});

/** THE COMPOSER'S REVEAL'S ARITHMETIC: how far the feed moves to show the boxed subtree whole. */
describe("revealNearestDelta", () => {
  /** A 300px viewport at the top of the screen. */
  const view = { boxTop: 0, boxHeight: 300 };
  const box = { scrollTop: 200, scrollHeight: 1000, clientHeight: 300 };

  it.each([
    { name: "moves nothing for a subtree wholly in view", node: { nodeTop: 50, nodeHeight: 100 }, box, want: 0 },
    { name: "brings a subtree below the view to its bottom edge", node: { nodeTop: 280, nodeHeight: 60 }, box, want: 40 },
    { name: "brings a subtree above the view to its top edge", node: { nodeTop: -90, nodeHeight: 60 }, box, want: -90 },
    { name: "brings a subtree taller than the view to its top edge", node: { nodeTop: 100, nodeHeight: 500 }, box, want: 100 },
    {
      name: "clamps at the feed's end",
      node: { nodeTop: 280, nodeHeight: 60 },
      box: { scrollTop: 690, scrollHeight: 1000, clientHeight: 300 },
      // the subtree asks +40, but only 10px of range remain below.
      want: 10,
    },
    {
      name: "clamps at the feed's start",
      node: { nodeTop: -90, nodeHeight: 60 },
      box: { scrollTop: 30, scrollHeight: 1000, clientHeight: 300 },
      want: -30,
    },
  ])("$name", ({ node, box, want }) => {
    // Arrange, Act
    const got = revealNearestDelta({ ...view, ...node }, box);
    // Assert
    expect(got).toBe(want);
  });
});

/** THE ONLY-AS-FAR-AS-NEEDED ARITHMETIC every non-centering scroll into view shares. */
describe("nearestEdgeOffset", () => {
  /** A 300px view starting 100px down. */
  const view = { top: 100, bottom: 400 };

  it.each([
    { name: "moves nothing for a node whole inside the view", node: { top: 150, bottom: 250 }, want: 0 },
    { name: "moves nothing for a node filling the view exactly", node: { top: 100, bottom: 400 }, want: 0 },
    { name: "brings a node above the view to its top edge", node: { top: 40, bottom: 90 }, want: -60 },
    { name: "brings a node cut by the view's top to its top edge", node: { top: 80, bottom: 130 }, want: -20 },
    { name: "brings a node below the view to its bottom edge", node: { top: 500, bottom: 550 }, want: 150 },
    { name: "brings a node cut by the view's bottom to its bottom edge", node: { top: 380, bottom: 430 }, want: 30 },
    { name: "brings a node taller than the view, starting inside it, to its top edge", node: { top: 200, bottom: 700 }, want: 100 },
    { name: "brings a node taller than the view, starting above it, to its top edge", node: { top: -100, bottom: 500 }, want: -200 },
  ])("$name", ({ node, want }) => {
    // Arrange, Act
    const got = nearestEdgeOffset(view, node);
    // Assert
    expect(got).toBe(want);
  });

  it("is the one arithmetic the sidebar's selection scroll and the feed's composer reveal share", () => {
    // Arrange, Act, Assert: neither hand-rolls an edge comparison of its own;
    // scroll.ts holds the definition and the feed's one use.
    expect([
      selectionScrollSource.match(/nearestEdgeOffset\(/g)?.length,
      selectionScrollSource.includes("viewBottom"),
      scrollSource.match(/nearestEdgeOffset\(/g)?.length,
    ]).toEqual([1, false, 2]);
  });
});

/** THE ONE CENTERING ARITHMETIC, for the feed's box and a bubble's own box alike. */
describe("centerOffset", () => {
  /** A 300px view starting 100px down the screen. */
  const view = { top: 100, bottom: 400 };

  it.each([
    { name: "puts a node that fits midpoint on midpoint", node: { top: 500, bottom: 600 }, want: 300 },
    { name: "moves nothing for a node already centered", node: { top: 200, bottom: 300 }, want: 0 },
    { name: "brings a node above the view down to center", node: { top: -100, bottom: 0 }, want: -300 },
    { name: "puts a node taller than the view top on top", node: { top: 700, bottom: 1100 }, want: 600 },
    { name: "centers a node exactly as tall as the view", node: { top: 150, bottom: 450 }, want: 50 },
  ])("$name", ({ node, want }) => {
    // Arrange, Act
    const got = centerOffset(view, node);
    // Assert
    expect(got).toBe(want);
  });
});

describe("installIntentScroll", () => {
  // The intent-arm gate on a real feed: only the section the reader
  // deliberately entered keeps the wheel; every other one — including a
  // section the feed scrolled under a still cursor — redirects to the feed.
  afterEach(() => {
    document.body.innerHTML = "";
  });

  /** A feed region that reports itself scrollable and stores its scrollTop. */
  function makeFeed(): HTMLElement {
    const feed = document.createElement("div");
    Object.defineProperty(feed, "scrollHeight", { value: 1000, configurable: true });
    Object.defineProperty(feed, "clientHeight", { value: 300, configurable: true });
    let top = 0;
    Object.defineProperty(feed, "scrollTop", {
      get: () => top,
      set: (v: number) => {
        top = v;
      },
      configurable: true,
    });
    document.body.append(feed);
    return feed;
  }

  /** An inner scroll box `innerScrollerAt` will recognize as a section. */
  function makeSection(feed: HTMLElement): HTMLElement {
    const box = document.createElement("div");
    box.style.overflowY = "auto";
    Object.defineProperty(box, "scrollHeight", { value: 400, configurable: true });
    Object.defineProperty(box, "clientHeight", { value: 100, configurable: true });
    feed.append(box);
    return box;
  }

  /** Dispatch a vertical wheel at `target`; return it so its default can be read. */
  function wheelAt(target: HTMLElement, deltaY = 40): WheelEvent {
    const e = new Event("wheel", { bubbles: true, cancelable: true }) as WheelEvent;
    Object.defineProperty(e, "deltaY", { value: deltaY });
    Object.defineProperty(e, "deltaMode", { value: 0 });
    target.dispatchEvent(e);
    return e;
  }

  /** Dispatch a bubbling pointer/mouse event at `target`. */
  function pointerAt(type: string, target: HTMLElement): void {
    target.dispatchEvent(new Event(type, { bubbles: true }));
  }

  it("redirects a wheel over a non-armed section to the feed", () => {
    // Arrange — a section nobody entered.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    // Act
    wheelAt(box, 40);
    // Assert — the feed took the delta instead of the box.
    expect(feed.scrollTop).toBe(40);
  });

  it("consumes a wheel redirected onto a feed the walk holds still, moving nothing", () => {
    // Arrange — an older page is loading (createWalkHold).
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    feed.setAttribute(WALK_HOLD_ATTRIBUTE, "");
    // Act
    const e = wheelAt(box, -40);
    // Assert
    expect({ top: feed.scrollTop, prevented: e.defaultPrevented }).toEqual({ top: 0, prevented: true });
  });

  it("prevents the browser default when it redirects", () => {
    // Arrange — the redirect must stop the browser scrolling the section too.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    // Act
    const e = wheelAt(box, 40);
    // Assert
    expect(e.defaultPrevented).toBe(true);
  });

  it("lets the armed section keep its own wheel", () => {
    // Arrange — the reader moved the pointer INTO the box, arming it.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    pointerAt("pointermove", box);
    // Act
    wheelAt(box, 40);
    // Assert — the feed did not move; the box keeps the wheel.
    expect(feed.scrollTop).toBe(0);
  });

  it("arms a section on a pointerdown inside it", () => {
    // Arrange — a click inside a box is a deliberate entry.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    pointerAt("pointerdown", box);
    // Act
    wheelAt(box, 40);
    // Assert
    expect(feed.scrollTop).toBe(0);
  });

  it("does NOT arm a section the feed scrolled under a still cursor", () => {
    // Arrange — the browser fires mouseenter/mouseover but no pointermove when
    // the feed slides a box under a stationary pointer, so the box stays unarmed.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    pointerAt("mouseenter", box);
    pointerAt("mouseover", box);
    // Act
    wheelAt(box, 40);
    // Assert — the wheel redirects; scrolling is not stuck in the bubble.
    expect(feed.scrollTop).toBe(40);
  });

  it("re-arms to a different section on a pointermove into it", () => {
    // Arrange — the reader armed one box, then moved the pointer into another.
    const feed = makeFeed();
    const first = makeSection(feed);
    const second = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    pointerAt("pointermove", first);
    pointerAt("pointermove", second);
    // Act
    wheelAt(second, 40);
    // Assert — the newly entered box keeps the wheel.
    expect(feed.scrollTop).toBe(0);
  });

  it("disarms the previous section when the pointer moves to another", () => {
    // Arrange — same re-arm, seen from the box the pointer LEFT.
    const feed = makeFeed();
    const first = makeSection(feed);
    const second = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    pointerAt("pointermove", first);
    pointerAt("pointermove", second);
    // Act — a wheel back over the box the reader left.
    wheelAt(first, 40);
    // Assert — it no longer keeps the wheel; the feed does.
    expect(feed.scrollTop).toBe(40);
  });

  it("arms nothing over bare feed, so a wheel over a section is the feed's", () => {
    // Arrange — the pointer sits over the feed itself, not any section.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    pointerAt("pointermove", feed);
    // Act
    wheelAt(box, 40);
    // Assert — nothing armed, so the section hands the wheel to the feed.
    expect(feed.scrollTop).toBe(40);
  });

  it("leaves a purely horizontal wheel to the browser", () => {
    // Arrange — a wide code block inside a section must still pan.
    const feed = makeFeed();
    const box = makeSection(feed);
    installIntentScroll(feed, noneOpen);
    // Act — a wheel with no vertical component.
    wheelAt(box, 0);
    // Assert — the feed is untouched; the browser owns the horizontal pan.
    expect(feed.scrollTop).toBe(0);
  });

  it("stops redirecting once its unsubscriber is called", () => {
    // Arrange
    const feed = makeFeed();
    const box = makeSection(feed);
    const { uninstall } = installIntentScroll(feed, noneOpen);
    // Act
    uninstall();
    wheelAt(box, 40);
    // Assert — a released mount leaves no wheel listener on the element.
    expect(feed.scrollTop).toBe(0);
  });

  it("arms a section programmatically, without any prior pointermove", () => {
    // Arrange — the owner bug: a click that expands a bubble fires no
    // pointermove, so the box it just revealed must still be armable.
    const feed = makeFeed();
    const box = makeSection(feed);
    const { arm } = installIntentScroll(feed, noneOpen);
    // Act — arm the box the same way a pointermove into it would, but without
    // dispatching any pointer event at all.
    arm(box);
    wheelAt(box, 40);
    // Assert — the box keeps its own wheel; the feed did not move.
    expect(feed.scrollTop).toBe(0);
  });

  it("arm() resolves the innermost scroll box enclosing the given element", () => {
    // Arrange — arm is called with a descendant of the section (as the click
    // handler calls it with the section itself, but the lookup must still
    // walk up from whatever element it is handed).
    const feed = makeFeed();
    const box = makeSection(feed);
    const inner = document.createElement("span");
    box.append(inner);
    const { arm } = installIntentScroll(feed, noneOpen);
    // Act
    arm(inner);
    wheelAt(box, 40);
    // Assert
    expect(feed.scrollTop).toBe(0);
  });

  it("hands a wheel over an unarmed box inside the armed one to the armed box, never the feed", () => {
    // Arrange — the reader armed the outer box; its own scrolling then slid an
    // inner box (a suite's output) under the still cursor.
    const feed = makeFeed();
    const outer = makeSection(feed);
    let outerTop = 0;
    Object.defineProperty(outer, "scrollTop", {
      get: () => outerTop,
      set: (v: number) => {
        outerTop = v;
      },
      configurable: true,
    });
    const inner = makeSection(feed);
    outer.append(inner);
    installIntentScroll(feed, noneOpen);
    pointerAt("pointermove", outer);
    // Act
    const e = wheelAt(inner, 40);
    // Assert — the armed box moved, the feed did not, and the browser did not scroll the inner box.
    expect([outerTop, feed.scrollTop, e.defaultPrevented]).toEqual([40, 0, true]);
  });

  it("programmatic arm() does not arm a different, unarmed section", () => {
    // Arrange — arming one box must not blanket-arm every section.
    const feed = makeFeed();
    const armedBox = makeSection(feed);
    const other = makeSection(feed);
    const { arm } = installIntentScroll(feed, noneOpen);
    // Act
    arm(armedBox);
    wheelAt(other, 40);
    // Assert — the other section still redirects to the feed.
    expect(feed.scrollTop).toBe(40);
  });
});

describe("click-to-expand arms the just-expanded box (the feed.ts wiring)", () => {
  // OWNER BUG: a click that expands a bubble fires no pointermove, so the
  // just-expanded box stayed unarmed until the cursor moved. feed.ts fixes
  // this by calling installIntentScroll's `arm` from installClickExpand's
  // `afterToggle`, on EXPAND only. These tests wire the two real modules
  // together exactly as feed.ts does and drive real click/wheel events.
  afterEach(() => {
    document.body.innerHTML = "";
  });

  /** A feed region that reports itself scrollable and stores its scrollTop. */
  function makeFeed(): HTMLElement {
    const feed = document.createElement("div");
    Object.defineProperty(feed, "scrollHeight", { value: 1000, configurable: true });
    Object.defineProperty(feed, "clientHeight", { value: 300, configurable: true });
    let top = 0;
    Object.defineProperty(feed, "scrollTop", {
      get: () => top,
      set: (v: number) => {
        top = v;
      },
      configurable: true,
    });
    document.body.append(feed);
    return feed;
  }

  /** A `.tool-fold` capped section that also overflows once expanded. */
  function makeCappedSection(feed: HTMLElement): HTMLElement {
    const box = document.createElement("div");
    box.classList.add("tool-fold");
    box.style.overflowY = "auto";
    Object.defineProperty(box, "scrollHeight", { value: 400, configurable: true });
    Object.defineProperty(box, "clientHeight", { value: 100, configurable: true });
    feed.append(box);
    return box;
  }

  /** Dispatch a vertical wheel at `target`. */
  function wheelAt(target: HTMLElement, deltaY = 40): void {
    const e = new Event("wheel", { bubbles: true, cancelable: true }) as WheelEvent;
    Object.defineProperty(e, "deltaY", { value: deltaY });
    Object.defineProperty(e, "deltaMode", { value: 0 });
    target.dispatchEvent(e);
  }

  /** Wire the two modules exactly as feed.ts does, recording every `arm` call. */
  function wire(feed: HTMLElement) {
    const { arm } = installIntentScroll(feed, (el) => expandedSectionAt(el, feed));
    const armCalls: HTMLElement[] = [];
    installClickExpand(feed, () => "", (section, expanded) => {
      if (expanded) {
        armCalls.push(section);
        arm(section);
      }
    });
    return { armCalls };
  }

  it("arms the expanded section's own box on EXPAND, with no pointermove at all", () => {
    // Arrange
    const feed = makeFeed();
    const box = makeCappedSection(feed);
    const { armCalls } = wire(feed);
    // Act — click expands the box; no pointer event of any kind precedes it.
    box.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    wheelAt(box, 40);
    // Assert — the box kept its own wheel immediately, and `arm` ran once.
    expect(feed.scrollTop).toBe(0);
    expect(armCalls).toEqual([box]);
  });

  it("does NOT arm on COLLAPSE", () => {
    // Arrange — an already-expanded (and thus armed) box.
    const feed = makeFeed();
    const box = makeCappedSection(feed);
    const { armCalls } = wire(feed);
    box.dispatchEvent(new MouseEvent("click", { bubbles: true })); // expand -> arms
    // Act
    box.dispatchEvent(new MouseEvent("click", { bubbles: true })); // collapse
    // Assert — only the expand called `arm`, never the collapse.
    expect(armCalls).toEqual([box]);
  });

  it("a wheel over a DIFFERENT, unarmed section still redirects to the feed", () => {
    // Arrange — expanding one bubble must not blanket-arm every section.
    const feed = makeFeed();
    const box = makeCappedSection(feed);
    const other = makeCappedSection(feed);
    wire(feed);
    box.dispatchEvent(new MouseEvent("click", { bubbles: true })); // expand+arm `box`
    // Act
    wheelAt(other, 40);
    // Assert
    expect(feed.scrollTop).toBe(40);
  });

  it("pointermove arming still works for a box no click has touched", () => {
    // Arrange — regression: the original arming path is untouched.
    const feed = makeFeed();
    const box = makeCappedSection(feed);
    wire(feed);
    // Act
    box.dispatchEvent(new Event("pointermove", { bubbles: true }));
    wheelAt(box, 40);
    // Assert
    expect(feed.scrollTop).toBe(0);
  });
});

describe("movesToward", () => {
  /** A 100px-tall box over 400px of content, scrolled to TOP. */
  const at = (scrollTop: number, overflowY = "auto") => ({ scrollHeight: 400, clientHeight: 100, overflowY, scrollTop });

  it.each([
    ["down, mid-scroll", 150, 40, true],
    ["down, at the bottom edge", 300, 40, false],
    ["up, mid-scroll", 150, -40, true],
    ["up, at the top edge", 0, -40, false],
    ["with no vertical delta", 150, 0, false],
  ] as const)("answers %s", (_label, scrollTop, deltaY, moves) => {
    expect(movesToward(at(scrollTop), deltaY)).toBe(moves);
  });

  it("answers a box that does not scroll at all as never moving", () => {
    expect(movesToward(at(150, "hidden"), 40)).toBe(false);
  });
});

describe("sectionTakesDelta", () => {
  afterEach(() => {
    document.body.innerHTML = "";
  });

  /** A scrollable box (100px over 400px) scrolled to TOP, appended to PARENT. */
  function box(parent: HTMLElement, scrollTop: number): HTMLElement {
    const el = document.createElement("div");
    el.style.overflowY = "auto";
    Object.defineProperty(el, "scrollHeight", { value: 400, configurable: true });
    Object.defineProperty(el, "clientHeight", { value: 100, configurable: true });
    Object.defineProperty(el, "scrollTop", { value: scrollTop, configurable: true });
    parent.append(el);
    return el;
  }

  it("answers true while a box inside the section can still move", () => {
    // Arrange — the section is at its bottom; the box inside it is not.
    const section = box(document.body, 300);
    const inner = box(section, 100);
    // Act / Assert
    expect(sectionTakesDelta(inner, section, 40)).toBe(true);
  });

  it("answers false once no box up to the section can move", () => {
    // Arrange — both at their bottom edge.
    const section = box(document.body, 300);
    const inner = box(section, 300);
    // Act / Assert
    expect(sectionTakesDelta(inner, section, 40)).toBe(false);
  });

  it("never looks past the section, however far its ancestors could move", () => {
    // Arrange — the section's own parent could scroll down; the section cannot.
    const outer = box(document.body, 0);
    const section = box(outer, 300);
    // Act / Assert
    expect(sectionTakesDelta(section, section, 40)).toBe(false);
  });

  it("fails loudly when handed a section that does not contain its start", () => {
    // Arrange
    const section = box(document.body, 0);
    const stray = box(document.body, 300);
    // Act / Assert
    expect(() => sectionTakesDelta(stray, section, 40)).toThrow(/does not contain its start/);
  });
});

describe("installIntentScroll: an open section keeps its whole wheel", () => {
  // Owner ruling, 2026-09-27: while the reader scrolls inside an expanded box,
  // the FEED never moves — not mid-box, and not at the box's edges either.
  let uninstall: Array<() => void> = [];
  afterEach(() => {
    for (const fn of uninstall) fn();
    uninstall = [];
    document.body.innerHTML = "";
  });

  /** A scrollable feed holding one response bubble's scroll box. */
  function mount(open: boolean, scrollTop = 0): { feed: HTMLElement; scroll: HTMLElement; text: HTMLElement } {
    const feed = document.createElement("div");
    Object.defineProperty(feed, "scrollHeight", { value: 1000, configurable: true });
    Object.defineProperty(feed, "clientHeight", { value: 300, configurable: true });
    let feedTop = 0;
    Object.defineProperty(feed, "scrollTop", {
      configurable: true,
      get: () => feedTop,
      set: (v: number) => {
        feedTop = v;
      },
    });
    feed.innerHTML = `<div class="bubble" data-role="response"><div class="bubble-scroll"><p>text</p></div></div>`;
    const scroll = feed.querySelector(".bubble-scroll") as HTMLElement;
    // An open box scrolls (overflow-y auto); a collapsed one clips (hidden).
    scroll.style.overflowY = open ? "auto" : "hidden";
    if (open) scroll.classList.add("expanded");
    Object.defineProperty(scroll, "scrollHeight", { value: 400, configurable: true });
    Object.defineProperty(scroll, "clientHeight", { value: 100, configurable: true });
    Object.defineProperty(scroll, "scrollTop", { value: scrollTop, configurable: true, writable: true });
    document.body.append(feed);
    uninstall.push(installIntentScroll(feed, (el) => expandedSectionAt(el, feed)).uninstall);
    return { feed, scroll, text: scroll.querySelector("p") as HTMLElement };
  }

  /** Dispatch a vertical wheel at TARGET; answer it so its default can be read. */
  function wheelAt(target: HTMLElement, deltaY: number): WheelEvent {
    const e = new Event("wheel", { bubbles: true, cancelable: true }) as WheelEvent;
    Object.defineProperty(e, "deltaY", { value: deltaY });
    Object.defineProperty(e, "deltaMode", { value: 0 });
    target.dispatchEvent(e);
    return e;
  }

  it("leaves a wheel inside an open box at mid-scroll to the box, never the feed", () => {
    // Arrange — never armed: no pointer act has touched the box.
    const { feed, text } = mount(true, 150);
    // Act
    const e = wheelAt(text, 40);
    // Assert — no redirect, and the browser is left to scroll the box.
    expect([feed.scrollTop, e.defaultPrevented]).toEqual([0, false]);
  });

  it("consumes a wheel down at the open box's bottom edge, so the feed does not move", () => {
    // Arrange
    const { feed, text } = mount(true, 300);
    // Act
    const e = wheelAt(text, 40);
    // Assert
    expect([feed.scrollTop, e.defaultPrevented]).toEqual([0, true]);
  });

  it("consumes a wheel up at the open box's top edge, so the feed does not move", () => {
    // Arrange
    const { feed, text } = mount(true, 0);
    // Act
    const e = wheelAt(text, -40);
    // Assert
    expect([feed.scrollTop, e.defaultPrevented]).toEqual([0, true]);
  });

  it("consumes the wheel of an open box too short to scroll at all", () => {
    // Arrange — the open content fits: nothing inside can move.
    const { feed, scroll, text } = mount(true, 0);
    Object.defineProperty(scroll, "scrollHeight", { value: 100, configurable: true });
    // Act
    const e = wheelAt(text, 40);
    // Assert
    expect([feed.scrollTop, e.defaultPrevented]).toEqual([0, true]);
  });

  it("leaves a horizontal-only wheel inside an open box to the browser", () => {
    // Arrange
    const { text } = mount(true, 300);
    // Act
    const e = wheelAt(text, 0);
    // Assert
    expect(e.defaultPrevented).toBe(false);
  });

  it("leaves a collapsed box's wheel to the feed, as before", () => {
    // Arrange — a collapsed box clips rather than scrolls, so it is no scroll
    // box at all and the wheel is the feed's, the browser's native scroll.
    const { feed, text } = mount(false, 0);
    // Act
    const e = wheelAt(text, 40);
    // Assert
    expect([feed.scrollTop, e.defaultPrevented]).toEqual([0, false]);
  });

  it("composes with auto-collapse: a wheel inside neither closes the box nor moves the feed", () => {
    // Arrange — the click owner and its auto-collapse, wired as feed.ts does.
    const { feed, scroll, text } = mount(true, 300);
    uninstall.push(installClickExpand(feed, () => ""));
    // Act
    wheelAt(text, 40);
    // Assert
    expect([scroll.classList.contains("expanded"), feed.scrollTop]).toEqual([true, 0]);
  });

  it("composes with auto-collapse: a wheel on the feed outside is the feed's, and leaves the box open", () => {
    // Arrange
    const { feed, scroll } = mount(true, 300);
    uninstall.push(installClickExpand(feed, () => ""));
    // Act
    const e = wheelAt(feed, 40);
    // Assert — left to the browser to scroll the feed; a box the reader opened
    // stays open however far that scroll takes it (owner ruling, 2026-10-01).
    expect([scroll.classList.contains("expanded"), e.defaultPrevented]).toEqual([true, false]);
  });
});

/**
 * A feed laid out as a stack of rows of HEIGHTS, the box's viewport top at 0,
 * so a row's viewport top is its content top minus `scrollTop`. HIDDEN rows
 * draw no box. The box does not clamp, and ROUND makes it round `scrollTop`
 * the way a real box does.
 */
function anchoredFeed(
  heights: number[],
  scrollTop: number,
  opts: { round?: boolean; clamp?: boolean; floor?: ScrollFloor } = {},
) {
  const host = document.createElement("div");
  const h = [...heights];
  const hidden = new Set<Element>();
  const row = (i: number): HTMLElement => {
    const el = document.createElement("div");
    el.dataset.feedRow = `r${i.toString()}`;
    return el;
  };
  heights.forEach((_, i) => host.append(row(i)));
  let top = scrollTop;
  const box = {
    get scrollTop() {
      return top;
    },
    set scrollTop(next: number) {
      const wanted = opts.round === true ? Math.round(next) : next;
      top =
        opts.clamp === true
          ? Math.min(Math.max(wanted, 0), Math.max(0, h.reduce((sum, x) => sum + x, 0) - 300))
          : wanted;
    },
    clientHeight: 300,
    get scrollHeight() {
      return h.reduce((sum, x) => sum + x, 0);
    },
  };
  const rows: AnchorRows = {
    host,
    viewportTop: () => 0,
    edges: (el) => {
      if (hidden.has(el)) return null;
      const i = [...host.children].indexOf(el);
      if (i < 0) return null;
      const at = h.slice(0, i).reduce((sum, x) => sum + x, 0) - box.scrollTop;
      return { top: at, bottom: at + (h[i] ?? 0) };
    },
  };
  const tail = new TailFollow(box, () => null, rows, opts.floor ?? null);
  const triggers = tailTriggers(tail);
  return {
    box,
    host,
    h,
    hidden,
    tail,
    scroll: triggers.scroll,
    resize: triggers.resize,
    input: () => triggers.input(),
    /** Insert a row of HEIGHT at the top, as a prepend does. */
    prepend: (height: number) => {
      host.prepend(row(-1));
      h.unshift(height);
    },
  };
}

/**
 * THE LAST BUBBLE'S FOLD IS ANY BUBBLE'S FOLD (owner ruling, 2026-11-20,
 * superseding the `lastBubbleExpanded`/`lastBubbleCollapsed` parks of
 * 2026-10-17): the click holds the view, and nothing lands the feed at its
 * tail. Rows at content tops 0, 400, 800, 1200 in a 300px viewport.
 */
describe("TailFollow: folding the feed's last bubble", () => {
  /** The feed at TOP after the reader's click on a bubble inside the last row. */
  function clickedLast(top: number, opts: { follow?: boolean; clamp?: boolean } = {}) {
    const f = anchoredFeed([400, 400, 400, 400], top, { clamp: opts.clamp ?? false });
    if (opts.follow === true) f.tail.promptSent();
    f.scroll();
    const bubble = document.createElement("div");
    f.host.children[3].append(bubble);
    f.tail.readerClicked(bubble);
    return f;
  }

  it("leaves the position unchanged when the last bubble expands while scrolled up", () => {
    // Arrange
    const f = clickedLast(500);
    // Act — the sub-feed lands, 300px taller.
    f.h[3] += 300;
    f.resize();
    // Assert
    expect([f.box.scrollTop, f.tail.isFollowing()]).toEqual([500, false]);
  });

  it("leaves the position unchanged when the last bubble collapses while scrolled up", () => {
    // Arrange — the last row holds a 300px open sub-feed.
    const f = anchoredFeed([400, 400, 400, 700], 500);
    f.scroll();
    const bubble = document.createElement("div");
    f.host.children[3].append(bubble);
    f.tail.readerClicked(bubble);
    // Act
    f.h[3] -= 300;
    f.resize();
    // Assert
    expect([f.box.scrollTop, f.tail.isFollowing()]).toEqual([500, false]);
  });

  it("ends at the new tail when the last bubble expands while following", () => {
    // Arrange — parked at the tail (1600 - 300 = 1300, clamped).
    const f = clickedLast(1300, { follow: true, clamp: true });
    // Act — the sub-feed lands, 300px taller.
    f.h[3] += 300;
    f.resize();
    // Assert
    expect([f.box.scrollTop, f.tail.isFollowing()]).toEqual([1600, true]);
  });

  it("ends at the new tail when the last bubble collapses while following, with no blank below and no floor", () => {
    // Arrange — the last row holds a 300px open sub-feed; parked at 1600.
    const tops: Array<number | null> = [];
    const floor: ScrollFloor = { hold: (top) => tops.push(top) };
    const f = anchoredFeed([400, 400, 400, 700], 1600, { clamp: true, floor });
    f.tail.promptSent();
    f.scroll();
    const bubble = document.createElement("div");
    f.host.children[3].append(bubble);
    f.tail.readerClicked(bubble);
    // Act — the collapse; the box clamps with the shrink.
    f.h[3] -= 300;
    f.box.scrollTop = f.box.scrollTop;
    f.scroll();
    f.resize();
    // Assert — the feed's end sits at the viewport's bottom.
    expect([f.box.scrollTop, f.box.scrollHeight - f.box.scrollTop - f.box.clientHeight, f.tail.isFollowing(), tops]).toEqual([1300, 0, true, []]);
  });

  it("records the last row's click under a standing follow at INFO with its row and fold", async () => {
    // Arrange
    const f = anchoredFeed([400, 400, 400, 400], 1300, { clamp: true });
    f.tail.promptSent();
    const bubble = document.createElement("div");
    bubble.setAttribute("data-expanded", "false");
    f.host.children[3].append(bubble);
    const capture = captureLogRecords("info");
    // Act
    f.tail.readerClicked(bubble);
    // Assert
    const record = await forwardedRecord(capture, "scroll.last-row-click-followed");
    expect([record.level.case, record.context]).toMatchObject(["info", { row: "r3", fold: "expand" }]);
  });

  it("still takes the hold for a click on a non-last bubble while following", () => {
    // Arrange
    const f = anchoredFeed([400, 400, 400, 400], 1300, { clamp: true });
    f.tail.promptSent();
    const bubble = document.createElement("div");
    f.host.children[2].append(bubble);
    // Act
    f.tail.readerClicked(bubble);
    f.h[2] += 300;
    f.resize();
    // Assert
    expect([f.box.scrollTop, f.tail.isFollowing()]).toEqual([1300, false]);
  });

  it("still takes the hold for a click on the last bubble while scrolled up", async () => {
    // Arrange
    const capture = captureLogRecords("info");
    // Act
    clickedLast(500);
    // Assert
    const record = await forwardedRecord(capture, "scroll.click-held");
    expect(record.context).toMatchObject({ by: "click", row: "r3" });
  });
});

describe("TailFollow's click hold anchors on the clicked row", () => {
  // Rows at content tops 0, 400, 800, 1200; the reader at 500 sees r1's tail
  // and r2 from its top, so the first row in view is r2.
  const rows = [400, 400, 400, 400];

  /** The feed with the reader's click on an element inside row I. */
  function clickedIn(i: number) {
    const f = anchoredFeed(rows, 500);
    f.scroll();
    const inner = document.createElement("span");
    f.host.children[i].append(inner);
    f.tail.readerClicked(inner);
    return f;
  }

  it("holds a clicked row that starts above the viewport still while it grows", () => {
    // Arrange
    const f = clickedIn(1);
    // Act
    f.h[1] = 800;
    f.resize();
    // Assert -- anchored on r2, the view would have moved to 900.
    expect(f.box.scrollTop).toBe(500);
  });

  it("holds a clicked row that starts above the viewport still while it shrinks", () => {
    // Arrange
    const f = clickedIn(1);
    // Act
    f.h[1] = 200;
    f.resize();
    // Assert -- anchored on r2, the view would have moved to 300.
    expect(f.box.scrollTop).toBe(500);
  });

  it("keeps the clicked row still through a later growth until the reader's next input", () => {
    // Arrange
    const f = clickedIn(1);
    f.h[1] = 600;
    f.resize();
    // Act -- the clicked row's sub-feed lands a frame later.
    f.h[1] = 900;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(500);
  });

  it("moves the view with a growth above the clicked row", () => {
    // Arrange
    const f = clickedIn(1);
    // Act
    f.h[0] = 600;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(700);
  });

  it("anchors on the first row in view again after the reader's next input", () => {
    // Arrange
    const f = clickedIn(1);
    f.input();
    f.scroll();
    // Act
    f.h[1] = 800;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(900);
  });

  it("anchors on the first row in view for a click outside every row", () => {
    // Arrange
    const f = anchoredFeed(rows, 500);
    f.scroll();
    f.tail.readerClicked(document.createElement("div"));
    // Act
    f.h[1] = 800;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(900);
  });

  it("records the clicked row it holds", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    clickedIn(1);
    // Assert
    const record = await forwardedRecord(capture, "scroll.click-held");
    expect(record.context).toMatchObject({ by: "click", row: "r1" });
  });
});

/**
 * A feed of root rows of HEIGHTS whose row GROUP is a group of entries of
 * ENTRIES (`g0`, `g1`, ...), stacked inside it as a run of final responses is:
 * the group's height is its entries'. The box's viewport top is at 0, and the
 * box does not clamp.
 */
function groupedFeed(heights: number[], group: number, entries: number[], scrollTop: number) {
  const host = document.createElement("div");
  const h = [...heights];
  const e = [...entries];
  heights.forEach((_, i) => {
    const el = document.createElement("div");
    if (i !== group) el.dataset.feedRow = `r${i.toString()}`;
    host.append(el);
  });
  const groupRow = host.children[group];
  e.forEach((_, i) => {
    const entry = document.createElement("div");
    entry.dataset.feedRow = `g${i.toString()}`;
    entry.append(document.createElement("span"));
    groupRow.append(entry);
  });
  const height = (i: number): number => (i === group ? e.reduce((sum, x) => sum + x, 0) : (h[i] ?? 0));
  const rootTop = (i: number): number => Array.from({ length: i }, (_, j) => height(j)).reduce((sum, x) => sum + x, 0);
  let top = scrollTop;
  const box = {
    get scrollTop() {
      return top;
    },
    set scrollTop(next: number) {
      top = next;
    },
    clientHeight: 300,
    get scrollHeight() {
      return rootTop(h.length);
    },
  };
  const rows: AnchorRows = {
    host,
    viewportTop: () => 0,
    edges: (el) => {
      const root = [...host.children].indexOf(el);
      if (root >= 0) return { top: rootTop(root) - box.scrollTop, bottom: rootTop(root) + height(root) - box.scrollTop };
      const i = [...groupRow.children].indexOf(el);
      if (i < 0) return null;
      const at = rootTop(group) + e.slice(0, i).reduce((sum, x) => sum + x, 0) - box.scrollTop;
      return { top: at, bottom: at + (e[i] ?? 0) };
    },
  };
  const tail = new TailFollow(box, () => null, rows);
  const triggers = tailTriggers(tail);
  return {
    box,
    e,
    groupRow,
    tail,
    scroll: triggers.scroll,
    resize: triggers.resize,
    /** The reader clicks inside the group's entry I. */
    click: (i: number) => {
      const inner = groupRow.children[i].firstElementChild;
      if (inner === null) throw new Error("the entry drew no child");
      tail.readerClicked(inner);
    },
  };
}

describe("TailFollow's click hold anchors on the clicked entry inside a group", () => {
  // Root rows r0 (400px) and a group at content top 400 holding g0 (400px) and
  // g1 (200px) at content tops 400 and 800; the reader at 500 sees g1 whole.
  function clickedInLast() {
    const f = groupedFeed([400, 0], 1, [400, 200], 500);
    f.scroll();
    f.click(1);
    return f;
  }

  it("holds the clicked entry still while the entry above it in its group shrinks", () => {
    // Arrange
    const f = clickedInLast();
    // Act -- selecting g1 closes g0, selected before it.
    f.e[0] = 100;
    f.resize();
    // Assert -- g1's content top moved from 800 to 500, and the view with it.
    expect(f.box.scrollTop).toBe(200);
  });

  it("holds the clicked entry still while the entry above it in its group grows", () => {
    // Arrange
    const f = clickedInLast();
    // Act
    f.e[0] = 600;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(700);
  });

  it("holds the view while the clicked entry itself grows below its top", () => {
    // Arrange
    const f = clickedInLast();
    // Act
    f.e[1] = 900;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(500);
  });

  it("holds the group row still once a redraw takes the clicked entry away", () => {
    // Arrange
    const f = clickedInLast();
    f.groupRow.children[1].remove();
    // Act -- g0 shrinks, which moves nothing above the group's own top.
    f.e[0] = 100;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(500);
  });

  it("records the entry it holds beside its root row", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    // Act
    clickedInLast();
    // Assert
    const record = await forwardedRecord(capture, "scroll.click-held");
    expect(record.context).toMatchObject({ by: "click", row: "div", entry: "g1" });
  });
});

describe("entryOf", () => {
  /** A root row holding a group entry holding a nested entry holding a span. */
  function nested() {
    const row = document.createElement("article");
    const entry = document.createElement("div");
    entry.dataset.feedRow = "g1";
    const inner = document.createElement("div");
    inner.dataset.feedRow = "g1.c0";
    const span = document.createElement("span");
    inner.append(span);
    entry.append(inner);
    row.append(entry);
    return { row, entry, inner, span };
  }

  it("answers the innermost feed row holding the target", () => {
    // Arrange
    const n = nested();
    // Act / Assert
    expect(entryOf(n.span, n.row)).toBe(n.inner);
  });

  it("answers the target itself when it is a feed row", () => {
    // Arrange
    const n = nested();
    // Act / Assert
    expect(entryOf(n.entry, n.row)).toBe(n.entry);
  });

  it("answers the root row when no feed row inside it holds the target", () => {
    // Arrange
    const n = nested();
    const strip = document.createElement("div");
    n.row.append(strip);
    // Act / Assert
    expect(entryOf(strip, n.row)).toBe(n.row);
  });

  it("answers the root row when the nearest feed row lies outside it", () => {
    // Arrange
    const outer = document.createElement("div");
    outer.dataset.feedRow = "r0";
    const row = document.createElement("article");
    const span = document.createElement("span");
    row.append(span);
    outer.append(row);
    // Act / Assert
    expect(entryOf(span, row)).toBe(row);
  });
});

describe("TailFollow's scroll anchoring", () => {
  // Rows at content tops 0, 400, 800, 1200; the reader at 500 sees r1's tail
  // and r2 from its top, so r2 is the anchor.
  const rows = [400, 400, 400, 400];

  it("shifts the view by exactly a growth above the reader", () => {
    // Arrange
    const f = anchoredFeed(rows, 500);
    f.scroll();
    // Act — r0, wholly above, lays out 200px taller.
    f.h[0] = 600;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(700);
  });

  it("moves nothing for a growth below the reader", () => {
    // Arrange
    const f = anchoredFeed(rows, 500);
    f.scroll();
    // Act
    f.h[3] = 900;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(500);
  });

  it("moves nothing when the anchor row itself grows, since its top stays put", () => {
    // Arrange
    const f = anchoredFeed(rows, 500);
    f.scroll();
    // Act
    f.h[2] = 700;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(500);
  });

  it("keeps the rows below a partly visible row still when that row grows", () => {
    // Arrange — r1 runs from above the viewport into it.
    const f = anchoredFeed(rows, 500);
    f.scroll();
    // Act
    f.h[1] = 450;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(550);
  });

  it("keeps a reader who follows the tail at the tail", () => {
    // Arrange
    const f = anchoredFeed(rows, 500);
    f.tail.promptSent();
    // Act
    f.h[0] = 600;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(1800);
  });

  it("measures a growth the scroll event's layout already holds before taking a new anchor", () => {
    // Arrange — THE PROTOTYPE'S FLAW: a row laid out between two frames, seen
    // first by a scroll event rather than a resize.
    const f = anchoredFeed(rows, 500);
    f.scroll();
    // Act
    f.h[0] = 600;
    f.scroll();
    // Assert
    expect(f.box.scrollTop).toBe(700);
  });

  it("takes the anchor afresh after the reader scrolls", () => {
    // Arrange — the reader moves down to 900: r3 becomes the anchor.
    const f = anchoredFeed(rows, 500);
    f.scroll();
    f.input();
    f.box.scrollTop = 900;
    f.scroll();
    // Act — r2, now above the anchor, grows.
    f.h[2] = 500;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(1000);
  });

  it("counts a prepend's own measure once, not again at the next size change", () => {
    // Arrange
    const f = anchoredFeed(rows, 500);
    f.scroll();
    f.prepend(300);
    f.tail.prependCompensation(300);
    // Act
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(800);
  });

  it("follows the anchor when a caller's measure misses a change it did not make", () => {
    // Arrange — r0 grew 50px as well, before the prepend's own 300px.
    const f = anchoredFeed(rows, 500);
    f.scroll();
    f.h[0] = 450;
    f.prepend(300);
    // Act
    f.tail.prependCompensation(300);
    // Assert
    expect(f.box.scrollTop).toBe(850);
  });

  it("takes the caller's measure when it holds no anchor", () => {
    // Arrange — no scroll event or resize yet, so no anchor was taken.
    const f = anchoredFeed(rows, 500);
    f.prepend(300);
    // Act
    f.tail.prependCompensation(300);
    // Assert
    expect(f.box.scrollTop).toBe(800);
  });

  it("anchors on the last row when none starts in view", () => {
    // Arrange — the reader is inside r3, the last row; r2 then grows.
    const f = anchoredFeed(rows, 1300);
    f.scroll();
    // Act
    f.h[2] = 500;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(1400);
  });

  it("skips a row that draws no box", () => {
    // Arrange — r2 is hidden, so r3 anchors; r0 then grows.
    const f = anchoredFeed(rows, 500);
    f.hidden.add(f.host.children[2]);
    f.scroll();
    // Act
    f.h[0] = 600;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(700);
  });

  it("carries what the box's rounding did not take into the next correction", () => {
    // Arrange
    const f = anchoredFeed(rows, 500, { round: true });
    f.scroll();
    // Act — two growths of 100.4px: rounded one at a time they would lose 0.8px.
    f.h[0] = 500.4;
    f.resize();
    f.h[0] = 600.8;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(701);
  });

  it("records each correction at DEBUG with its cause, delta and anchor row", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const f = anchoredFeed(rows, 500);
    f.scroll();
    // Act
    f.h[0] = 600;
    f.resize();
    // Assert
    const record = await forwardedRecord(capture, "scroll.anchor-corrected");
    expect([record.level.case, record.context]).toMatchObject([
      "debug",
      { cause: "prependCompensation", trigger: "resize", delta: 200, anchor: "r2", from: 500, to: 700 },
    ]);
  });

  it("records a lost anchor at DEBUG and moves nothing", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const f = anchoredFeed(rows, 500);
    f.scroll();
    // Act
    f.host.children[2]?.remove();
    f.h.splice(2, 1);
    f.resize();
    // Assert
    const record = await forwardedRecord(capture, "scroll.anchor-lost");
    expect([record.level.case, record.context?.anchor, f.box.scrollTop]).toEqual(["debug", "r2", 500]);
  });

  it("records an ERROR when a height changes off the tail with no row to anchor on", async () => {
    // Arrange — every row is drawn but none draws a box.
    const capture = captureLogRecords("debug");
    const f = anchoredFeed(rows, 500);
    for (const el of f.host.children) f.hidden.add(el);
    // Act
    f.resize();
    // Assert
    const record = await forwardedRecord(capture, "scroll.anchor-missing");
    expect(record.level.case).toBe("error");
  });

  it("reports a missing anchor once per spell", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const f = anchoredFeed(rows, 500);
    for (const el of f.host.children) f.hidden.add(el);
    // Act
    f.resize();
    f.resize();
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.filter((r) => r.operation === "scroll.anchor-missing")).toHaveLength(1);
  });
});

// EVERY GROWTH AT THE TAIL IS FOLLOWED (owner ruling, 2026-10-10): a held item
// in the feed's held section is laid out after the rows, so its growth pushes
// the rows up exactly as any row's does.
describe("TailFollow follows any growth at the tail", () => {
  // Rows at content tops 0, 400, 800, 1200 in a 300px viewport; following at
  // the tail stands at 1300, with r3 partly in view.
  const rows = [400, 400, 400, 400];

  it("keeps a following reader at the tail when the last item grows", () => {
    // Arrange
    const f = anchoredFeed(rows, 0, { clamp: true });
    f.tail.initialPlacement();
    f.resize();
    // Act
    f.h[3] = 600;
    f.resize();
    // Assert
    expect(f.box.scrollTop).toBe(1500);
  });
});

describe("feedAnchorRows", () => {

  it("reads a row that draws no box as null", () => {
    // Arrange — jsdom draws no box for anything.
    const box = document.createElement("div");
    const host = document.createElement("div");
    const row = document.createElement("div");
    host.append(row);
    // Act
    const edges = feedAnchorRows(box, host).edges(row);
    // Assert
    expect(edges).toBeNull();
  });

  it("reads a drawn row's edges off its bounding rect", () => {
    // Arrange
    const box = document.createElement("div");
    const host = document.createElement("div");
    const row = document.createElement("div");
    row.getClientRects = () => [{}] as unknown as DOMRectList;
    row.getBoundingClientRect = () => ({ top: 30, bottom: 90 }) as DOMRect;
    // Act
    const edges = feedAnchorRows(box, host).edges(row);
    // Assert
    expect(edges).toEqual({ top: 30, bottom: 90 });
  });

  it("reads the viewport top off the box", () => {
    // Arrange
    const box = document.createElement("div");
    box.getBoundingClientRect = () => ({ top: 12 }) as DOMRect;
    // Act
    const top = feedAnchorRows(box, document.createElement("div")).viewportTop();
    // Assert
    expect(top).toBe(12);
  });
});

/**
 * `MoveWitness` — what the feed's content last changed, beside every move the
 * browser made on its own.
 */
describe("MoveWitness", () => {
  /** A witness, its clock and frames driven by hand. */
  function witnessAt() {
    let now = 1000;
    const frames = frameQueue();
    const witness = new MoveWitness(() => now, frames.frame);
    return {
      witness,
      advance: (ms: number) => {
        now += ms;
      },
      /** Start one frame: run every callback waiting for it. */
      frame: frames.run,
    };
  }

  /** An element of TAG wearing CLASSES. */
  function el(tag: string, ...classes: string[]): HTMLElement {
    const e = document.createElement(tag);
    e.classList.add(...classes);
    return e;
  }

  it("reports nothing before any change has landed", () => {
    // Arrange
    const { witness } = witnessAt();
    // Act
    const report = witness.report();
    // Assert
    expect(report).toBeNull();
  });

  it.each([
    { name: "an element by its tag and classes", target: () => el("span", "q-when"), want: "span.q-when" },
    { name: "an element with no class by its tag", target: () => el("div"), want: "div" },
    {
      name: "a text node by the element it sits in",
      target: () => el("span", "perm-when").appendChild(document.createTextNode("3s ago")),
      want: "span.perm-when",
    },
    { name: "a detached text node by its node name", target: () => document.createTextNode("x"), want: "#text" },
  ])("names $name", ({ target, want }) => {
    // Arrange
    const { witness } = witnessAt();
    // Act
    witness.mutated([target()]);
    // Assert
    expect(witness.report()?.mutated).toEqual([want]);
  });

  it("names each element once, however many of its mutations landed", () => {
    // Arrange
    const { witness } = witnessAt();
    const stamp = el("span", "q-when");
    // Act
    witness.mutated([stamp, stamp, stamp]);
    // Assert
    expect(witness.report()).toMatchObject({ mutated: ["span.q-when"], mutations: 3 });
  });

  it("names at most six elements of a large change, and counts every mutation", () => {
    // Arrange
    const { witness } = witnessAt();
    const targets = Array.from({ length: 9 }, (_, i) => el("div", `c${i.toString()}`));
    // Act
    witness.mutated(targets);
    // Assert
    expect(witness.report()).toMatchObject({
      mutated: ["div.c0", "div.c1", "div.c2", "div.c3", "div.c4", "div.c5"],
      mutations: 9,
    });
  });

  it("states how long ago the change landed", () => {
    // Arrange
    const { witness, advance } = witnessAt();
    witness.mutated([el("span")]);
    // Act
    advance(12.4);
    // Assert
    expect(witness.report()?.ms_since_mutation).toBe(12);
  });

  it.each([
    { frames: 0, want: 0 },
    { frames: 1, want: 1 },
    { frames: 2, want: 2 },
    { frames: 5, want: 2 },
  ])("counts $frames started frames as $want", ({ frames, want }) => {
    // Arrange
    const { witness, frame } = witnessAt();
    witness.mutated([el("span")]);
    // Act
    for (let i = 0; i < frames; i++) frame();
    // Assert
    expect(witness.report()?.frames_since_mutation).toBe(want);
  });

  it("reports only the latest change, its frames counted afresh", () => {
    // Arrange
    const { witness, frame } = witnessAt();
    witness.mutated([el("div", "old")]);
    frame();
    frame();
    // Act
    witness.mutated([el("span", "new")]);
    // Assert
    expect(witness.report()).toMatchObject({ mutated: ["span.new"], frames_since_mutation: 0 });
  });

  it("reports a delivered change as delivered", () => {
    // Arrange
    const { witness } = witnessAt();
    // Act
    witness.mutated([el("span", "q-when")]);
    // Assert
    expect(witness.report()?.undelivered).toBe(false);
  });

  it("reports the reporting task's own undelivered change as the last one", () => {
    // Arrange
    const { witness, advance, frame } = witnessAt();
    witness.mutated([el("span", "merge-clock")]);
    advance(886);
    frame();
    frame();
    // Act
    const report = witness.report([el("article", "feed-item"), el("div", "feed-group")]);
    // Assert
    expect(report).toMatchObject({
      mutated: ["article.feed-item", "div.feed-group"],
      mutations: 2,
      undelivered: true,
      ms_since_mutation: 0,
      frames_since_mutation: 0,
    });
  });

  it("keeps the last delivered change when nothing is pending", () => {
    // Arrange
    const { witness } = witnessAt();
    witness.mutated([el("span", "merge-clock")]);
    // Act
    const report = witness.report([]);
    // Assert
    expect(report).toMatchObject({ mutated: ["span.merge-clock"], undelivered: false });
  });

  it("hands out a copy its caller cannot alter", () => {
    // Arrange
    const { witness } = witnessAt();
    witness.mutated([el("span", "q-when")]);
    const first: MoveWitnessReport | null = witness.report();
    // Act
    first?.mutated.push("tampered");
    // Assert
    expect(witness.report()?.mutated).toEqual(["span.q-when"]);
  });
});

/**
 * THE SCROLL FLOOR UNDER A CLICK-HELD VIEW (owner request, 2026-10-16): a
 * reader's click never moves the feed, not even by the box's own clamp.
 *
 * The box here CLAMPS like a real one: `scrollTop` never sits past
 * `scrollHeight - clientHeight`, and the floor counts in `scrollHeight`
 * exactly as the real floor's block does: one box-height tall at its top, in
 * every layout, so a clamp under it reads the box's height of that moment.
 */
describe("TailFollow: the scroll floor under a click-held view", () => {
  /** A clamping box over CONTENT px, a 300px viewport at TOP, with a fake floor. */
  function floored(top: number, content = 1000, opts: { clampOnRead?: boolean } = {}) {
    const state: { content: number; floorTop: number | null; top: number; clientHeight: number } = {
      content,
      floorTop: null,
      top,
      clientHeight: 300,
    };
    /** The scrollable height a layout gives: the content, or the floor's bottom when that reaches further. */
    const scrollHeight = (): number =>
      Math.max(state.content, state.floorTop === null ? 0 : state.floorTop + state.clientHeight);
    const box: ReanchorBox = {
      get clientHeight() {
        return state.clientHeight;
      },
      get scrollHeight() {
        return scrollHeight();
      },
      get scrollTop() {
        // A real box lays out before it answers, so a lowered floor has
        // clamped the position by the time it is read.
        if (opts.clampOnRead === true) state.top = Math.max(0, Math.min(state.top, scrollHeight() - state.clientHeight));
        return state.top;
      },
      set scrollTop(v: number) {
        state.top = Math.max(0, Math.min(v, scrollHeight() - state.clientHeight));
      },
    };
    const holds: (number | null)[] = [];
    const floor: ScrollFloor = {
      hold: (at) => {
        holds.push(at);
        state.floorTop = at;
      },
    };
    const tail = new TailFollow(box, () => null, null, floor);
    const { scroll: onScroll, resize: onResize, input: onInput } = tailTriggers(tail);
    /** The box lays out: its position re-clamped into the range, as a browser's layout does. */
    const layout = (): void => {
      box.scrollTop = state.top;
    };
    return {
      tail,
      box,
      state,
      holds,
      layout,
      scroll: () => onScroll(),
      resize: () => onResize(),
      input: () => onInput("wheel"),
      /** The content below the view shrinks by PX, the box lays out, then tells its owner. */
      shrink: (px: number) => {
        state.content -= px;
        layout();
        onResize();
      },
      /** The box's viewport grows by PX (the gate dock hiding), lays out, then tells its owner. */
      grow: (px: number) => {
        state.clientHeight += px;
        layout();
        onResize();
      },
    };
  }

  it("lands a correction under the held view at its target when lowering the floor clamps the box first", () => {
    // Arrange -- at the tail: 700 + 300 is the whole content.
    const f = floored(700, 1000, { clampOnRead: true });
    f.tail.readerClicked(document.createElement("div"));
    f.state.content = 800;
    // Act -- the 200px above the reader closed; lowering the floor to 500
    // clamps the box to 500 before the correction writes.
    f.tail.prependCompensation(-200);
    // Assert -- a relative write would have landed at 300.
    expect(f.box.scrollTop).toBe(500);
  });

  it("arms the floor at the held view's bottom when the reader clicks", () => {
    // Arrange
    const f = floored(700);
    // Act
    f.tail.readerClicked(document.createElement("div"));
    // Assert
    expect(f.holds).toEqual([700]);
  });

  it("holds the view when the content below the clicked card shrinks", () => {
    // Arrange -- at the tail: 700 + 300 is the whole content.
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    // Act -- the answered card redraws 200px shorter.
    f.shrink(200);
    // Assert
    expect([f.box.scrollTop, f.box.scrollHeight]).toEqual([700, 1000]);
  });

  it("holds the view when the box grows under it", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    // Act -- the docked gate hides, and the feed's viewport takes its 210px.
    f.grow(210);
    // Assert
    expect([f.box.scrollTop, f.box.scrollHeight]).toEqual([700, 1210]);
  });

  it("lets the floor down with the box when the box shrinks back", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    f.grow(210);
    // Act -- Emacs shows the input window again, and the viewport gives the 210px back.
    f.grow(-210);
    // Assert -- nothing is left to scroll into below the held view.
    expect([f.box.scrollTop, f.box.scrollHeight]).toEqual([700, 1000]);
  });

  it("holds the view when the box lays out a shorter card and tells its scroll first", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    // Act -- the box laid out the shorter card; WebKit tells the scroll first.
    f.state.content -= 200;
    f.layout();
    f.scroll();
    // Assert
    expect([f.box.scrollTop, f.box.scrollHeight]).toEqual([700, 1000]);
  });

  it("does not undo a move under the hold that is no clamp", () => {
    // Arrange -- content enough below that the box is nowhere near its end.
    const f = floored(300, 2000);
    f.tail.readerClicked(document.createElement("div"));
    // Act -- the box moves up with no input behind it, and is not at its end.
    f.box.scrollTop = 200;
    f.scroll();
    // Assert
    expect(f.box.scrollTop).toBe(200);
  });

  it("holds the view where a move under the hold left it, against a later clamp", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    f.box.scrollTop = 600;
    f.scroll();
    // Act
    f.shrink(300);
    // Assert
    expect(f.box.scrollTop).toBe(600);
  });

  it("holds the view through a redraw that shrinks the content and grows it back before anyone is told", () => {
    // Arrange -- a merge tab swap: the old panel leaves a layout before the new one lands.
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    // Act
    f.state.content -= 217;
    f.layout();
    f.state.content += 217;
    f.layout();
    f.resize();
    f.scroll();
    // Assert
    expect([f.box.scrollTop, f.box.scrollHeight]).toEqual([700, 1000]);
  });

  it("never moves the floor while the held view stands still", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    // Act
    f.shrink(200);
    f.grow(210);
    f.grow(-210);
    // Assert -- set once at the click, and never measured or set again.
    expect(f.holds).toEqual([700]);
  });

  it("records the reader input that ends a standing hold", async () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    const capture = captureLogRecords("info");
    // Act
    f.input();
    // Assert
    const record = await forwardedRecord(capture, "scroll.click-hold-ended");
    expect([record.level.case, record.context]).toMatchObject(["info", { by: "wheel", at: 700, held_top: 700, row: "none" }]);
  });

  it("records the named cause that supersedes a standing hold", async () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    const capture = captureLogRecords("info");
    // Act
    f.tail.workspaceSelected();
    // Assert
    const record = await forwardedRecord(capture, "scroll.click-hold-ended");
    expect(record.context).toMatchObject({ by: "workspaceSelected", held_top: 700 });
  });

  it.each<[string, (tail: TailFollow) => void]>([
    ["promptSent", (tail) => tail.promptSent()],
    ["promptHeld", (tail) => tail.promptHeld()],
    ["initialPlacement", (tail) => tail.initialPlacement()],
    ["replaceRestore", (tail) => tail.replaceRestore()],
    ["rolledBack", (tail) => tail.rolledBack()],
    ["workspaceSelected", (tail) => tail.workspaceSelected()],
    ["tailJumped", (tail) => tail.jumpToTail()],
  ])("ends a standing hold under %s and parks at the tail", async (cause, act) => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    const capture = captureLogRecords("info");
    // Act
    act(f.tail);
    // Assert
    const record = await forwardedRecord(capture, "scroll.click-hold-ended");
    expect([record.context, f.tail.isFollowing()]).toMatchObject([{ by: cause }, true]);
  });

  it("hand-rolls no click supersede followed by a park outside parkSuperseding", () => {
    // Arrange + Act
    const pairs = scrollSource.match(/this\.supersedeClick\((\w+|"\w+")\);\s*this\.park\(\1\)/g) ?? [];
    // Assert -- the one pair left is parkSuperseding's own body.
    expect(pairs).toEqual(["this.supersedeClick(cause);\n    this.park(cause)"]);
  });

  it("records no hold ending when no hold stands", async () => {
    // Arrange
    const f = floored(700);
    const capture = captureLogRecords("debug");
    // Act
    f.input();
    f.tail.workspaceSelected();
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.filter((r) => r.operation === "scroll.click-hold-ended")).toEqual([]);
  });

  it("records a move under the hold that it takes as the new held position", async () => {
    // Arrange -- content enough below that the box is nowhere near its end.
    const f = floored(300, 2000);
    f.tail.readerClicked(document.createElement("div"));
    const capture = captureLogRecords("info");
    // Act
    f.box.scrollTop = 200;
    f.scroll();
    // Assert
    const record = await forwardedRecord(capture, "scroll.click-hold-moved");
    expect([record.level.case, record.context]).toMatchObject(["info", { from: 300, to: 200, end: 1700, scroll_height: 2000 }]);
  });

  it("records no move under the hold when the view stands still", async () => {
    // Arrange
    const f = floored(300, 2000);
    f.tail.readerClicked(document.createElement("div"));
    const capture = captureLogRecords("debug");
    // Act
    f.scroll();
    // Assert
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.filter((r) => r.operation === "scroll.click-hold-moved")).toEqual([]);
  });

  it("records the floor it sets at debug, with no geometry read", async () => {
    // Arrange
    const f = floored(700);
    const capture = captureLogRecords("debug");
    // Act
    f.tail.readerClicked(document.createElement("div"));
    // Assert
    const record = await forwardedRecord(capture, "scroll.floor-set");
    expect([record.level.case, record.context]).toEqual(["debug", { connection_id: "test-connection", floor_top: 700, held: true }]);
  });

  it("records the clicked element and the floor standing when the hold is taken", async () => {
    // Arrange
    const f = floored(700);
    const target = document.createElement("span");
    target.className = "merge-tab is-active";
    const capture = captureLogRecords("info");
    // Act
    f.tail.readerClicked(target);
    // Assert
    const record = await forwardedRecord(capture, "scroll.click-held");
    // The floor the click armed: the held position.
    expect(record.context).toMatchObject({ target: "span.merge-tab.is-active", floor_top: 700 });
  });

  it("recedes the floor as the reader scrolls back up once the hold ends", () => {
    // Arrange -- the card shrank 200px under the held view.
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    f.shrink(200);
    // Act -- the reader scrolls up 100px.
    f.input();
    f.box.scrollTop = 600;
    f.scroll();
    // Assert
    expect([f.box.scrollTop, f.box.scrollHeight]).toEqual([600, 900]);
  });

  it("leaves no blank once the reader is back above it", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    f.shrink(200);
    // Act
    f.input();
    f.box.scrollTop = 400;
    f.scroll();
    // Assert -- the floor followed the reader up to where the content alone reaches.
    expect([f.box.scrollHeight, f.holds.at(-1)]).toEqual([800, 400]);
  });

  it("lifts the floor at the next scroll once the content reaches past it", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    f.shrink(200);
    f.input();
    f.box.scrollTop = 400;
    f.scroll();
    // Act
    f.scroll();
    // Assert
    expect([f.box.scrollHeight, f.state.floorTop]).toEqual([800, null]);
  });

  it("lifts the floor once new content fills the blank", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    f.shrink(200);
    f.input();
    // Act -- the next rows land, taller than the blank.
    f.shrink(-300);
    // Assert -- the content stands on its own, and the floor holds nothing.
    expect([f.box.scrollHeight, f.state.floorTop]).toEqual([1100, null]);
  });

  it("keeps a floor the reader has not scrolled above, after the hold ends", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    f.shrink(200);
    // Act -- a wheel input that has not moved the view yet.
    f.input();
    f.scroll();
    // Assert
    expect([f.box.scrollTop, f.box.scrollHeight]).toEqual([700, 1000]);
  });

  it("lifts the floor and lands on the content's own tail for a named cause", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    f.shrink(200);
    // Act
    f.tail.promptSent();
    // Assert
    expect([f.box.scrollTop, f.box.scrollHeight, f.tail.isFollowing()]).toEqual([500, 800, true]);
  });

  it("ends the click hold for a named cause, so the next shrink is no longer held", () => {
    // Arrange
    const f = floored(700);
    f.tail.readerClicked(document.createElement("div"));
    f.tail.entryJumped(at({ boxTop: 0, boxHeight: 300, nodeTop: 0, nodeHeight: 50 }, f.box));
    // Act
    f.shrink(400);
    // Assert -- the box's own clamp, which only a click hold resists.
    expect(f.box.scrollTop).toBe(300);
  });

  it("takes the reader's press before the click for no gesture: a later move is uncaused", async () => {
    // Arrange -- the pointerdown that began the click is an input on the box.
    const box: ReanchorBox = { scrollTop: 700, scrollHeight: 1000, clientHeight: 300 };
    const tail = new TailFollow(box);
    const { scroll: onScroll, input: onInput } = tailTriggers(tail);
    onInput("wheel");
    tail.readerClicked(document.createElement("div"));
    const capture = captureLogRecords();
    // Act -- the box moves with nothing behind it but the click.
    box.scrollTop = 600;
    onScroll();
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved-uncaused");
    expect(record.context).toMatchObject({ from: 700, to: 600 });
  });
});

describe("holdViewOnClicks", () => {
  afterEach(() => {
    document.body.replaceChildren();
  });

  /** A tail that records the clicks it was told of, and whether a target handler saw the hold first. */
  function recording() {
    const told: Element[] = [];
    const tail = { readerClicked: (target: Element) => told.push(target) } as unknown as TailFollow;
    return { told, tail };
  }

  it("holds the view on a click anywhere in the zone, with the clicked element", () => {
    // Arrange
    const zone = document.createElement("div");
    const option = document.createElement("button");
    zone.append(option);
    document.body.append(zone);
    const r = recording();
    holdViewOnClicks(zone, r.tail);
    // Act
    option.click();
    // Assert
    expect(r.told).toEqual([option]);
  });

  it("takes the hold before the clicked element's own handler runs", () => {
    // Arrange
    const zone = document.createElement("div");
    const option = document.createElement("button");
    zone.append(option);
    document.body.append(zone);
    const r = recording();
    holdViewOnClicks(zone, r.tail);
    const heldFirst: boolean[] = [];
    option.addEventListener("click", () => heldFirst.push(r.told.length === 1));
    // Act
    option.click();
    // Assert
    expect(heldFirst).toEqual([true]);
  });

  it("holds the view even when the clicked element stops the click's propagation", () => {
    // Arrange
    const zone = document.createElement("div");
    const option = document.createElement("button");
    option.addEventListener("click", (e) => e.stopPropagation());
    zone.append(option);
    document.body.append(zone);
    const r = recording();
    holdViewOnClicks(zone, r.tail);
    // Act
    option.click();
    // Assert
    expect(r.told).toEqual([option]);
  });

  it("holds nothing once uninstalled", () => {
    // Arrange
    const zone = document.createElement("div");
    document.body.append(zone);
    const r = recording();
    holdViewOnClicks(zone, r.tail)();
    // Act
    zone.click();
    // Assert
    expect(r.told).toEqual([]);
  });

  it("is what observeScrollBox wires on the box: a click in the box holds the view", async () => {
    // Arrange
    const box = document.createElement("div");
    const card = document.createElement("div");
    box.append(card);
    document.body.append(box);
    const tail = new TailFollow(box);
    const unobserve = observeScrollBox(box, tail);
    const capture = captureLogRecords("debug");
    // Act
    card.click();
    // Assert
    const record = await forwardedRecord(capture, "scroll.click-held");
    expect(record.context).toMatchObject({ by: "click" });
    unobserve();
  });
});

describe("scrollFloor", () => {
  afterEach(() => {
    document.body.replaceChildren();
  });

  /** A box over some content, with its floor. */
  function boxWithFloor() {
    const box = document.createElement("div");
    box.append(document.createElement("main"));
    document.body.append(box);
    const floor = scrollFloor(box);
    const block = box.lastElementChild as HTMLElement;
    return { box, floor, block };
  }

  it("hangs a hidden block in the box", () => {
    // Arrange + Act
    const { block } = boxWithFloor();
    // Assert
    expect([block.className, block.hidden, block.getAttribute("aria-hidden")]).toEqual([SCROLL_FLOOR_CLASS, true, "true"]);
  });

  it("makes the box the block's containing block", () => {
    // Arrange + Act
    const { box } = boxWithFloor();
    // Assert
    expect(box.classList.contains(SCROLL_FLOOR_HOST_CLASS)).toBe(true);
  });

  it("shows the block at the held position", () => {
    // Arrange
    const { floor, block } = boxWithFloor();
    // Act
    floor.hold(700);
    // Assert
    expect([block.hidden, block.style.top]).toEqual([false, "700px"]);
  });

  it("moves the block to a new held position", () => {
    // Arrange
    const { floor, block } = boxWithFloor();
    floor.hold(700);
    // Act
    floor.hold(400);
    // Assert
    expect(block.style.top).toBe("400px");
  });

  it("hides the block when the floor is lifted", () => {
    // Arrange
    const { floor, block } = boxWithFloor();
    floor.hold(700);
    // Act
    floor.hold(null);
    // Assert
    expect([block.hidden, block.style.top]).toEqual([true, ""]);
  });
});

describe("heldRowName", () => {
  it.each([
    ["a row carrying a FeedId", { "data-feed-row": "f1.r7" }, "f1.r7"],
    ["a row without one", {}, "article"],
  ])("names %s", (_label, attrs: Record<string, string>, want) => {
    // Arrange
    const row = document.createElement("article");
    for (const [k, v] of Object.entries(attrs)) row.setAttribute(k, v);
    // Act / Assert
    expect(heldRowName(row)).toBe(want);
  });

  it("names a click outside every row none", () => {
    expect(heldRowName(null)).toBe("none");
  });

  it("is the one way TailFollow names its held row", () => {
    expect([scrollSource.includes('this.clickedRow === null ? "none"'), scrollSource.match(/heldRowName\(this\.clickedRow\)/g)?.length]).toEqual([false, 2]);
  });
});

describe("dragEdgeScroll", () => {
  it("moves the scroller by the step the drag asks for", () => {
    // Arrange
    const scroller = { scrollTop: 40 };
    // Act
    dragEdgeScroll(scroller, -8);
    // Assert
    expect(scroller.scrollTop).toBe(32);
  });
});

describe("selectionRowScroll", () => {
  it("moves the section by the delta the selection asks for", () => {
    // Arrange
    const region = { scrollTop: 40 };
    // Act
    selectionRowScroll(region, 60);
    // Assert
    expect(region.scrollTop).toBe(100);
  });
});

describe("scrollKept", () => {
  it("puts the scroller back at the position the draw found it at", () => {
    // Arrange
    const region = { scrollTop: 0 };
    // Act
    scrollKept(region, 140);
    // Assert
    expect(region.scrollTop).toBe(140);
  });

  it.each([
    ["sidebar/scroll-keep.ts", scrollKeepSource],
    ["sidebar/band-fit.ts", bandFitSource],
    ["host/sidebar-popups.ts", sidebarPopupsSource],
  ])("is how %s puts a redrawn scroller back", (_file, source) => {
    // Act
    const keeps = source.includes("scrollKept(");
    // Assert
    expect(keeps).toBe(true);
  });
});

describe("TailFollow.onViewChanged", () => {
  /** A's box with every view report its listener received. */
  function reported(a: ReturnType<typeof armed>): Array<{ scrollHeight: number; scrollTop: number; clientHeight: number }> {
    const views: Array<{ scrollHeight: number; scrollTop: number; clientHeight: number }> = [];
    a.tail.onViewChanged((box) => views.push(box));
    return views;
  }

  it("reports where the box stands after a scroll event", () => {
    // Arrange
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    const views = reported(a);
    box.scrollTop = 250;
    // Act
    a.gesture();
    // Assert
    expect(views).toEqual([{ scrollHeight: 1000, scrollTop: 250, clientHeight: 300 }]);
  });

  it("reports where the box stands after a resize pass, with the follow's move already made", () => {
    // Arrange
    const f = following();
    const views = reported(f);
    f.box.scrollHeight = 1400;
    // Act
    f.resize();
    // Assert
    expect(views).toEqual([{ scrollHeight: 1400, scrollTop: 1400, clientHeight: 300 }]);
  });

  it("reports nothing for a park until the park's own scroll event arrives", () => {
    // Arrange
    const box = { scrollTop: 100, scrollHeight: 1000, clientHeight: 300 };
    const a = armed(box);
    const views = reported(a);
    // Act
    a.tail.jumpToTail();
    // Assert
    expect(views).toEqual([]);
  });
});

describe("openAtNewest", () => {
  afterEach(() => {
    document.body.innerHTML = "";
    vi.unstubAllGlobals();
  });

  /** A sub-feed bubble mounted in the page, the bound every opening stays inside. */
  function makeBubble(): HTMLElement {
    const bubble = document.createElement("div");
    bubble.className = "tool-card bubble-fold";
    document.body.append(bubble);
    return bubble;
  }

  /** Make EL a laid-out box at RECT, scrolling SCROLL_HEIGHT within CLIENT_HEIGHT when it overflows. */
  function layOut(el: HTMLElement, rect: { top: number; height: number }, scroll?: { scrollHeight: number; clientHeight: number }): void {
    el.getClientRects = () => [boxRect(rect)] as unknown as DOMRectList;
    el.getBoundingClientRect = () => boxRect(rect);
    if (scroll === undefined) return;
    el.style.overflowY = "auto";
    Object.defineProperty(el, "scrollHeight", { value: scroll.scrollHeight, configurable: true });
    Object.defineProperty(el, "clientHeight", { value: scroll.clientHeight, configurable: true });
  }

  it("takes content that is its own scroll box to its end", () => {
    // Arrange
    const view = document.createElement("div");
    makeBubble().append(view);
    layOut(view, { top: 0, height: 300 }, { scrollHeight: 1000, clientHeight: 300 });
    // Act
    openAtNewest(view);
    // Assert
    expect(view.scrollTop).toBe(700);
  });

  it("brings content's last line to the bottom of the box enclosing it", () => {
    // Arrange — a 300px box at 100, content ending 900px down the page.
    const box = document.createElement("div");
    const content = document.createElement("div");
    box.append(content);
    makeBubble().append(box);
    layOut(box, { top: 100, height: 300 }, { scrollHeight: 2000, clientHeight: 300 });
    layOut(content, { top: 200, height: 700 });
    // Act
    openAtNewest(content);
    // Assert — 900 - (100 + 300).
    expect(box.scrollTop).toBe(500);
  });

  it("moves nothing when nothing inside the bubble scrolls", () => {
    // Arrange
    const content = document.createElement("div");
    makeBubble().append(content);
    layOut(content, { top: 0, height: 50 });
    // Act
    openAtNewest(content);
    // Assert
    expect(content.scrollTop).toBe(0);
  });

  it("never moves a scroll box outside the bubble", () => {
    // Arrange — the box above the bubble stands for the feed.
    const feedBox = document.createElement("div");
    document.body.append(feedBox);
    const bubble = document.createElement("div");
    bubble.className = "bubble-fold";
    feedBox.append(bubble);
    const content = document.createElement("div");
    bubble.append(content);
    layOut(feedBox, { top: 0, height: 300 }, { scrollHeight: 3000, clientHeight: 300 });
    layOut(content, { top: 0, height: 50 });
    // Act
    openAtNewest(content);
    // Assert
    expect(feedBox.scrollTop).toBe(0);
  });

  it("waits for content's first layout, then moves once", () => {
    // Arrange — opened before it is laid out, as a nested strip's panel is.
    const view = document.createElement("div");
    makeBubble().append(view);
    openAtNewest(view);
    const before = view.scrollTop;
    layOut(view, { top: 0, height: 300 }, { scrollHeight: 1000, clientHeight: 300 });
    // Act
    fireResize(view);
    // Assert — moved on the layout, and no longer watched after it.
    expect([before, view.scrollTop, (() => { try { fireResize(view); return "watched"; } catch { return "released"; } })()]).toEqual([0, 700, "released"]);
  });

  it("keeps waiting through a resize that leaves content unlaid", () => {
    // Arrange — the bubble's sub-feed is still hidden.
    const view = document.createElement("div");
    makeBubble().append(view);
    openAtNewest(view);
    // Act
    fireResize(view);
    layOut(view, { top: 0, height: 300 }, { scrollHeight: 1000, clientHeight: 300 });
    fireResize(view);
    // Assert
    expect(view.scrollTop).toBe(700);
  });

  it("refuses content laid out outside any sub-feed bubble", () => {
    // Arrange
    const content = document.createElement("div");
    document.body.append(content);
    layOut(content, { top: 0, height: 50 });
    // Act + Assert
    expect(() => openAtNewest(content)).toThrow("outside a sub-feed bubble");
  });

  it("refuses to wait on a page with no ResizeObserver", () => {
    // Arrange
    const content = document.createElement("div");
    makeBubble().append(content);
    vi.stubGlobal("ResizeObserver", undefined);
    // Act + Assert
    expect(() => openAtNewest(content)).toThrow("no ResizeObserver");
  });

  it("records each opening at DEBUG with the box it moved", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const view = document.createElement("div");
    makeBubble().append(view);
    layOut(view, { top: 0, height: 300 }, { scrollHeight: 1000, clientHeight: 300 });
    // Act
    openAtNewest(view);
    // Assert
    const record = await forwardedRecord(capture, "scroll.open-at-newest");
    expect(record.context).toMatchObject({ moved: true, delta: 700 });
  });
});

describe("chatEndTop", () => {
  const box = (scrollHeight: number, clientHeight: number): HTMLElement => {
    const el = document.createElement("div");
    Object.defineProperty(el, "scrollHeight", { get: () => scrollHeight });
    Object.defineProperty(el, "clientHeight", { get: () => clientHeight });
    return el;
  };

  it("is the extent less the box, with no room past the last answer", () => {
    expect(chatEndTop(box(1000, 100))).toBe(900);
  });

  it("is the top for a chat shorter than its box", () => {
    expect(chatEndTop(box(90, 100))).toBe(0);
  });
});

describe("revealChatExchange", () => {
  /** A scroll box 600 tall over 3000 of content at TOP, holding an exchange whose top is AT below the box's. */
  const arrange = (top: number, at: number) => {
    const box = document.createElement("div");
    const exchange = document.createElement("div");
    box.append(exchange);
    let scrollTop = top;
    Object.defineProperty(box, "scrollHeight", { get: () => 3000 });
    Object.defineProperty(box, "clientHeight", { get: () => 600 });
    Object.defineProperty(box, "scrollTop", { get: () => scrollTop, set: (v: number) => (scrollTop = v) });
    box.getBoundingClientRect = () => ({ top: 0 }) as DOMRect;
    exchange.getBoundingClientRect = () => ({ top: at }) as DOMRect;
    return { box, exchange };
  };

  it("puts the exchange's top a sixth of the box below the box's top", () => {
    const { box, exchange } = arrange(1000, 400);
    revealChatExchange(box, exchange);
    expect(box.scrollTop).toBe(1000 + 400 - 100);
  });

  it("stops at the top", () => {
    const { box, exchange } = arrange(50, 0);
    revealChatExchange(box, exchange);
    expect(box.scrollTop).toBe(0);
  });

  it("stops at the end the box can reach", () => {
    const { box, exchange } = arrange(2300, 500);
    revealChatExchange(box, exchange);
    expect(box.scrollTop).toBe(2400);
  });
});

// THE FEED HOLDS STILL WHILE IT WALKS INTO THE PAST (owner request, 2026-11-21):
// the hold takes the box's scrollability from the ask until the page settles,
// and lets go only once the reader's wheel stream has gone quiet.
describe("createWalkHold", () => {
  afterEach(() => {
    document.body.innerHTML = "";
  });

  /** A box, its hold, and the frame queue the test runs by hand. */
  function arrange() {
    const box = document.createElement("div");
    document.body.append(box);
    const frames: Array<() => void> = [];
    const hold = createWalkHold(box, (fn) => frames.push(fn));
    /** Run the frames queued so far, one rendering update's worth. */
    const frame = (): void => {
      for (const fn of frames.splice(0)) fn();
    };
    /** Dispatch one wheel event at the box. */
    const wheel = (): void => {
      box.dispatchEvent(new Event("wheel", { bubbles: true }));
    };
    return { box, hold, frame, frames, wheel };
  }

  it("holds the box still from the first begin", () => {
    // Arrange
    const { box, hold } = arrange();
    // Act
    hold.begin();
    // Assert
    expect(walkHeld(box)).toBe(true);
  });

  it("keeps holding while a page is still out", () => {
    // Arrange
    const { box, hold, frame } = arrange();
    hold.begin();
    hold.begin();
    // Act
    hold.end();
    for (let i = 0; i < WALK_HOLD_QUIET_FRAMES + 1; i += 1) frame();
    // Assert
    expect(walkHeld(box)).toBe(true);
  });

  it("keeps holding for the frames short of the quiet bound after the last page settles", () => {
    // Arrange
    const { box, hold, frame } = arrange();
    hold.begin();
    // Act
    hold.end();
    for (let i = 0; i < WALK_HOLD_QUIET_FRAMES - 1; i += 1) frame();
    // Assert
    expect(walkHeld(box)).toBe(true);
  });

  it("lets go once the wheel has been quiet for the bound's frames", () => {
    // Arrange
    const { box, hold, frame } = arrange();
    hold.begin();
    // Act
    hold.end();
    for (let i = 0; i < WALK_HOLD_QUIET_FRAMES; i += 1) frame();
    // Assert
    expect(walkHeld(box)).toBe(false);
  });

  it("keeps holding while the gesture's momentum still arrives after the page landed", () => {
    // Arrange
    const { box, hold, frame, wheel } = arrange();
    hold.begin();
    hold.end();
    // Act: a wheel every frame, as a momentum tail is delivered.
    for (let i = 0; i < WALK_HOLD_QUIET_FRAMES * 3; i += 1) {
      wheel();
      frame();
    }
    // Assert
    expect(walkHeld(box)).toBe(true);
  });

  it("counts a wheel that arrived while the page loaded against the first frame after it lands", () => {
    // Arrange
    const { box, hold, frame, wheel } = arrange();
    hold.begin();
    wheel();
    hold.end();
    // Act
    for (let i = 0; i < WALK_HOLD_QUIET_FRAMES; i += 1) frame();
    // Assert
    expect(walkHeld(box)).toBe(true);
  });

  it("stops counting the quiet when another page is asked for", () => {
    // Arrange
    const { box, hold, frame } = arrange();
    hold.begin();
    hold.end();
    frame();
    // Act
    hold.begin();
    for (let i = 0; i < WALK_HOLD_QUIET_FRAMES + 1; i += 1) frame();
    // Assert
    expect(walkHeld(box)).toBe(true);
  });

  it("records the hold and its release with what it absorbed", async () => {
    // Arrange
    const capture = captureLogRecords("info");
    const { box, hold, frame, wheel } = arrange();
    box.scrollTop = 0;
    hold.begin();
    wheel();
    hold.end();
    frame();
    frame();
    wheel();
    // Act
    for (let i = 0; i < WALK_HOLD_QUIET_FRAMES + 1; i += 1) frame();
    // Assert
    const held = await forwardedRecord(capture, "scroll.walk-held");
    const released = await forwardedRecord(capture, "scroll.walk-released");
    expect({ held: held.context, released: released.context }).toMatchObject({
      held: { at: 0 },
      released: { by: "quiet", wheels_absorbed: 2, frames_waited: 6, longest_gap_frames: 1 },
    });
  });

  it("refuses an end with no page out, recording it and leaving the box as it was", async () => {
    // Arrange
    const capture = captureLogRecords("error");
    const { box, hold, frames } = arrange();
    // Act
    const act = (): void => hold.end();
    // Assert
    expect(act).toThrow("walk hold: end without a matching begin");
    const record = await forwardedRecord(capture, "scroll.walk-hold-unbalanced");
    expect({ context: record.context, held: walkHeld(box), frames: frames.length }).toMatchObject({
      context: { held: false },
      held: false,
      frames: 0,
    });
  });

  it("lets go at once on dispose, and stops listening for the wheel", async () => {
    // Arrange
    const capture = captureLogRecords("info");
    const { box, hold, frame, wheel } = arrange();
    hold.begin();
    // Act
    hold.dispose();
    wheel();
    frame();
    // Assert
    const record = await forwardedRecord(capture, "scroll.walk-released");
    expect({ held: walkHeld(box), context: record.context }).toMatchObject({
      held: false,
      context: { by: "dispose", wheels_absorbed: 0, frames_waited: 0, longest_gap_frames: 0 },
    });
  });
});
