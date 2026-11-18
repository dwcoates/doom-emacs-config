// @vitest-environment jsdom
// A page the webhost is not showing runs no animations, and a page it shows
// again has every animation where it would have been. See
// src/host/unshown-animations.ts; the engine's side of it is
// test/webkit/unshown-animations.webkit.test.ts.
import { afterEach, describe, expect, it } from "vitest";

import {
  PAGE_UNSHOWN_ATTRIBUTE,
  animatedElements,
  insertedElements,
  pauseAnimationsWhileUnshown,
  type AnimatedElement,
  type UnshownAnimationsEnv,
} from "../../src/host/unshown-animations.js";
import { PSEUDO_DELAY } from "../../src/animation-delays.js";
import { createPresence, type PresenceSource } from "../../src/presence.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** One animated element, recording each move and whether the pause stood then. */
class FakeAnimated implements AnimatedElement {
  readonly element = document.createElement("div");
  readonly moves: { by: number; underPause: boolean; flushedBefore: boolean }[] = [];

  constructor(private readonly page: { flushes: number }) {}

  advance(byMs: number): void {
    this.moves.push({
      by: byMs,
      underPause: document.documentElement.hasAttribute(PAGE_UNSHOWN_ATTRIBUTE),
      flushedBefore: this.page.flushes > 0,
    });
  }
}

/** A page whose clock, animated elements and insertions the test drives. */
interface Page {
  presence: PresenceSource;
  clock: { ms: number };
  animated: FakeAnimated[];
  flushes: number;
  /** A fresh animated element, not yet in the page. */
  make(): FakeAnimated;
  /** Report ELEMENTS inserted, as the observer would. */
  insert(...elements: FakeAnimated[]): void;
  /** Whether the page watches insertions now. */
  watching(): boolean;
  stop: () => void;
}

let page: Page | null = null;

afterEach(() => {
  page?.stop();
  page?.presence.dispose();
  page = null;
});

/** Start the pause over a page shown or not, with COUNT animated elements in it. */
function start(shown: boolean, count = 0): Page {
  const presence = createPresence(document, true);
  if (shown) presence.setShown(true);
  let observer: ((inserted: Iterable<Element>) => void) | null = null;
  const p: Page = {
    presence,
    clock: { ms: 1000 },
    animated: [],
    flushes: 0,
    make: () => new FakeAnimated(p),
    insert: (...elements) => {
      p.animated.push(...elements);
      observer?.(elements.map((e) => e.element));
    },
    watching: () => observer !== null,
    stop: () => {},
  };
  for (let i = 0; i < count; i++) p.animated.push(p.make());
  const env: UnshownAnimationsEnv = {
    doc: document,
    presence,
    now: () => p.clock.ms,
    animated: () => [...p.animated],
    flush: () => {
      p.flushes += 1;
    },
    observe: (fn) => {
      observer = fn;
      return () => {
        observer = null;
      };
    },
  };
  p.stop = pauseAnimationsWhileUnshown(env);
  page = p;
  return p;
}

const paused = (): boolean => document.documentElement.hasAttribute(PAGE_UNSHOWN_ATTRIBUTE);

describe("pausing a page the webhost does not show", () => {
  it("pauses a page that boots not shown", () => {
    // Arrange / Act
    start(false);

    // Assert
    expect(paused()).toBe(true);
  });

  it("leaves a page that boots shown running", () => {
    // Arrange / Act
    start(true);

    // Assert
    expect(paused()).toBe(false);
  });

  it("pauses when the webhost hides the page", () => {
    // Arrange
    const p = start(true);

    // Act
    p.presence.setShown(false);

    // Assert
    expect(paused()).toBe(true);
  });

  it("lifts the pause when the webhost shows the page", () => {
    // Arrange
    const p = start(false);

    // Act
    p.presence.setShown(true);

    // Assert
    expect(paused()).toBe(false);
  });

  it("ignores a change that leaves the page shown", () => {
    // Arrange
    const p = start(true, 1);

    // Act
    document.dispatchEvent(new Event("visibilitychange"));

    // Assert
    expect([paused(), p.animated[0].moves]).toEqual([false, []]);
  });
});

describe("resuming a page the webhost shows again", () => {
  it("moves an element animated at the hide on by how long the page was hidden", () => {
    // Arrange
    const p = start(true, 1);
    p.presence.setShown(false);
    p.clock.ms += 700;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(p.animated[0].moves.map((m) => m.by)).toEqual([700]);
  });

  it("moves an element inserted while hidden on by how long since it was inserted", () => {
    // Arrange
    const p = start(false);
    p.clock.ms += 300;
    const made = p.make();
    p.insert(made);
    p.clock.ms += 500;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(made.moves.map((m) => m.by)).toEqual([500]);
  });

  it("dates an element inserted again by its latest insertion", () => {
    // Arrange
    const p = start(false);
    const made = p.make();
    p.insert(made);
    p.clock.ms += 200;
    p.insert(made);
    p.animated.pop();
    p.clock.ms += 100;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(made.moves.map((m) => m.by)).toEqual([100]);
  });

  it("moves every element after the pause is lifted and flushed, so a one-shot past its end stays ended", () => {
    // Arrange
    const p = start(true, 1);
    p.presence.setShown(false);
    p.clock.ms += 100;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(p.animated[0].moves.map((m) => [m.underPause, m.flushedBefore])).toEqual([[false, true]]);
  });

  it("dates an element inserted during an earlier hide from the later hide", () => {
    // Arrange
    const p = start(false);
    const made = p.make();
    p.insert(made);
    p.clock.ms += 100;
    p.presence.setShown(true);
    p.clock.ms += 50;
    p.presence.setShown(false);
    p.clock.ms += 400;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(made.moves.map((m) => m.by)).toEqual([100, 400]);
  });

  it("stops watching insertions once shown", () => {
    // Arrange
    const p = start(false);

    // Act
    p.presence.setShown(true);

    // Assert
    expect(p.watching()).toBe(false);
  });
});

describe("what the pause and the resume record", () => {
  it("records the resume at info with how many elements, how long hidden and how long it took", async () => {
    // Arrange
    const capture = captureLogRecords();
    const p = start(true, 2);
    p.presence.setShown(false);
    p.clock.ms += 1234;

    // Act
    p.presence.setShown(true);

    // Assert
    const record = await forwardedRecord(capture, "page.animations-resumed");
    expect({
      level: record.level.case,
      animated: record.context?.["animated_elements"],
      hiddenMs: record.context?.["hidden_ms"],
      took: typeof record.context?.["took_ms"],
    }).toEqual({ level: "info", animated: 2, hiddenMs: 1234, took: "number" });
  });

  it("records the pause at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const p = start(true);

    // Act
    p.presence.setShown(false);

    // Assert
    const record = await forwardedRecord(capture, "page.animations-paused");
    expect(record.level.case).toBe("debug");
  });
});

describe("stopping", () => {
  it("lifts the pause and stops following the webhost", () => {
    // Arrange
    const p = start(false);

    // Act
    p.stop();
    p.presence.setShown(true);
    p.presence.setShown(false);

    // Assert
    expect(paused()).toBe(false);
  });
});

describe("reading the page's elements", () => {
  afterEach(() => {
    document.body.replaceChildren();
  });

  it("names every element a batch inserted, with everything under it", () => {
    // Arrange
    const outer = document.createElement("div");
    const inner = document.createElement("span");
    outer.append(inner);
    const records = [{ addedNodes: [outer, document.createTextNode("x")] }] as unknown as MutationRecord[];

    // Act
    const found = insertedElements(records);

    // Assert
    expect(found).toEqual([outer, inner]);
  });

  it("finds an element whose style runs an animation, and passes over one that runs none", () => {
    // Arrange
    const running = document.createElement("div");
    running.style.animationName = "spin";
    running.style.animationDelay = "0s";
    const still = document.createElement("div");
    document.body.append(running, still);

    // Act
    const found = animatedElements(document, window).map((a) => a.element);

    // Assert
    expect([found.includes(running), found.includes(still)]).toEqual([true, false]);
  });

  it("moves each of an element's animations' delays, repeating a shorter delay list as CSS does", () => {
    // Arrange
    const el = document.createElement("div");
    el.style.animationName = "a, b";
    el.style.animationDelay = "-200ms";
    document.body.append(el);
    const animated = animatedElements(document, window).find((a) => a.element === el)!;

    // Act
    animated.advance(300);

    // Assert
    expect(el.style.animationDelay).toBe("-500ms, -500ms");
  });

  it("moves a pseudo-element's delays through its element", () => {
    // Arrange: a view whose ::after runs a loop, as WebKit computes one.
    const el = document.createElement("div");
    document.body.append(el);
    const loop = { animationName: "shimmer", animationDuration: "1s", animationIterationCount: "infinite", animationDirection: "normal", animationDelay: "-100ms" };
    const view = new Proxy(window, {
      get: (target, key) =>
        key === "getComputedStyle"
          ? (element: Element, pseudo?: string | null) =>
              element === el && pseudo === "::after" ? (loop as unknown as CSSStyleDeclaration) : target.getComputedStyle(element, pseudo)
          : (Reflect.get(target, key) as unknown),
    });
    const animated = animatedElements(document, view).find((a) => a.element === el)!;

    // Act
    animated.advance(400);

    // Assert
    expect(el.style.getPropertyValue(PSEUDO_DELAY["::after"].property)).toBe("-500ms");
  });
});
