// @vitest-environment jsdom
// A page the webhost is not showing runs no animations, and a page it shows
// again has every animation where it would have been. See
// src/host/unshown-animations.ts; the engine's side of it is
// test/webkit/unshown-animations.webkit.test.ts.
import { afterEach, describe, expect, it } from "vitest";

import {
  PAGE_UNSHOWN_ATTRIBUTE,
  pauseAnimationsWhileUnshown,
  type FrozenAnimation,
  type UnshownAnimationsEnv,
} from "../../src/host/unshown-animations.js";
import { createPresence, type PresenceSource } from "../../src/presence.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { frameQueue } from "../frame-queue.js";

/** One animation, recording whether the pause stood each time its time was set. */
class FakeAnimation implements FrozenAnimation {
  readonly setUnderPause: boolean[] = [];
  private time: CSSNumberish | null;

  constructor(time: number | null) {
    this.time = time;
  }

  get currentTime(): CSSNumberish | null {
    return this.time;
  }

  set currentTime(time: CSSNumberish | null) {
    this.setUnderPause.push(document.documentElement.hasAttribute(PAGE_UNSHOWN_ATTRIBUTE));
    this.time = time;
  }
}

/** A page whose clock, animations, mutations and frames the test drives. */
interface Page {
  presence: PresenceSource;
  clock: { ms: number };
  animations: FakeAnimation[];
  /** Report a batch of mutations, as the observer would. */
  mutate(): void;
  /** Run the frames asked for so far. */
  runFrames(): void;
  /** How many frames were asked for. */
  framesAsked(): number;
  stop: () => void;
}

let page: Page | null = null;

afterEach(() => {
  page?.stop();
  page?.presence.dispose();
  page = null;
});

/** Start the pause over a page shown or not, with ANIMATIONS in it. */
function start(shown: boolean, animations: FakeAnimation[] = []): Page {
  const presence = createPresence(document, true);
  if (shown) presence.setShown(true);
  const clock = { ms: 1000 };
  let observer: (() => void) | null = null;
  const frames = frameQueue();
  const env: UnshownAnimationsEnv = {
    doc: document,
    presence,
    now: () => clock.ms,
    animations: () => [...animations],
    observe: (fn) => {
      observer = fn;
      return () => {
        observer = null;
      };
    },
    frame: frames.frame,
  };
  const stop = pauseAnimationsWhileUnshown(env);
  page = {
    presence,
    clock,
    animations,
    mutate: () => observer?.(),
    runFrames: frames.run,
    framesAsked: frames.asked,
    stop,
  };
  return page;
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
    const running = new FakeAnimation(100);
    start(true, [running]);

    // Act
    document.dispatchEvent(new Event("visibilitychange"));

    // Assert
    expect([paused(), running.setUnderPause]).toEqual([false, []]);
  });
});

describe("resuming a page the webhost shows again", () => {
  it("moves an animation running at the hide on by how long the page was hidden", () => {
    // Arrange
    const running = new FakeAnimation(250);
    const p = start(true, [running]);
    p.presence.setShown(false);
    p.clock.ms += 700;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(running.currentTime).toBe(950);
  });

  it("moves an animation made while hidden on by how long since it was made", () => {
    // Arrange
    const p = start(false);
    p.clock.ms += 300;
    const made = new FakeAnimation(0);
    p.animations.push(made);
    p.mutate();
    p.clock.ms += 10;
    p.runFrames();
    p.clock.ms += 490;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(made.currentTime).toBe(500);
  });

  it("dates an animation made just before the show, whose frame has not run", () => {
    // Arrange
    const p = start(false);
    p.clock.ms += 300;
    const made = new FakeAnimation(0);
    p.animations.push(made);
    p.mutate();
    p.clock.ms += 40;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(made.currentTime).toBe(40);
  });

  it("asks for one frame for a run of mutations", () => {
    // Arrange
    const p = start(false);

    // Act
    p.mutate();
    p.mutate();
    p.mutate();

    // Assert
    expect(p.framesAsked()).toBe(1);
  });

  it("asks for a fresh frame for mutations after a scan", () => {
    // Arrange
    const p = start(false);
    p.mutate();
    p.runFrames();

    // Act
    p.mutate();

    // Assert
    expect(p.framesAsked()).toBe(2);
  });

  it("keeps the first date of an animation seen by a later scan", () => {
    // Arrange
    const p = start(false);
    const made = new FakeAnimation(0);
    p.animations.push(made);
    p.mutate();
    p.runFrames();
    p.clock.ms += 200;
    p.mutate();
    p.runFrames();
    p.clock.ms += 100;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(made.currentTime).toBe(300);
  });

  it("sets every time after the pause is lifted, so a one-shot past its end stays ended", () => {
    // Arrange
    const running = new FakeAnimation(10);
    const p = start(true, [running]);
    p.presence.setShown(false);
    p.clock.ms += 100;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(running.setUnderPause).toEqual([false]);
  });

  it("leaves an animation no mutation announced where it paused", () => {
    // Arrange
    const p = start(false);
    const unannounced = new FakeAnimation(40);
    p.animations.push(unannounced);
    p.clock.ms += 500;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(unannounced.setUnderPause).toEqual([]);
  });

  it("resumes again after a second hide, from that hide", () => {
    // Arrange
    const running = new FakeAnimation(0);
    const p = start(true, [running]);
    p.presence.setShown(false);
    p.clock.ms += 100;
    p.presence.setShown(true);
    p.clock.ms += 50;
    running.currentTime = 150;
    p.presence.setShown(false);
    p.clock.ms += 400;

    // Act
    p.presence.setShown(true);

    // Assert
    expect(running.currentTime).toBe(550);
  });

  it("stops watching mutations once shown", () => {
    // Arrange
    const p = start(false);
    p.presence.setShown(true);

    // Act
    p.mutate();

    // Assert
    expect(p.framesAsked()).toBe(0);
  });
});

describe("what the resume records", () => {
  it("records the resume at info with how many animations, how long hidden and how long it took", async () => {
    // Arrange
    const capture = captureLogRecords();
    const p = start(true, [new FakeAnimation(0), new FakeAnimation(5)]);
    p.presence.setShown(false);
    p.clock.ms += 1234;

    // Act
    p.presence.setShown(true);

    // Assert
    const record = await forwardedRecord(capture, "page.animations-resumed");
    expect({
      level: record.level.case,
      animations: record.context?.["animations"],
      unrecorded: record.context?.["unrecorded"],
      hiddenMs: record.context?.["hidden_ms"],
      took: typeof record.context?.["took_ms"],
    }).toEqual({ level: "info", animations: 2, unrecorded: 0, hiddenMs: 1234, took: "number" });
  });

  it("counts an animation no mutation announced", async () => {
    // Arrange
    const capture = captureLogRecords();
    const p = start(false);
    p.animations.push(new FakeAnimation(0));

    // Act
    p.presence.setShown(true);

    // Assert
    const record = await forwardedRecord(capture, "page.animations-resumed");
    expect(record.context?.["unrecorded"]).toBe(1);
  });

  it("records the pause at debug with how many animations it holds", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const p = start(true, [new FakeAnimation(0)]);

    // Act
    p.presence.setShown(false);

    // Assert
    const record = await forwardedRecord(capture, "page.animations-paused");
    expect([record.level.case, record.context?.["animations"]]).toEqual(["debug", 1]);
  });
});

describe("an animation holding no time", () => {
  it("throws on the show rather than resuming the page at a guess", () => {
    // Arrange
    const p = start(false, [new FakeAnimation(null)]);

    // Act / Assert
    expect(() => p.presence.setShown(true)).toThrow(/holds no numeric time/);
  });

  it("records the failure at error with the held value", async () => {
    // Arrange
    const capture = captureLogRecords();
    const p = start(false, [new FakeAnimation(null)]);

    // Act
    expect(() => p.presence.setShown(true)).toThrow();

    // Assert
    const record = await forwardedRecord(capture, "page.animations-resume-failed");
    expect([record.level.case, record.context?.["held"]]).toEqual(["error", "null"]);
  });

  it("sets no animation's time when one cannot resume", () => {
    // Arrange
    const fine = new FakeAnimation(10);
    const p = start(false, [fine, new FakeAnimation(null)]);

    // Act
    expect(() => p.presence.setShown(true)).toThrow();

    // Assert
    expect(fine.setUnderPause).toEqual([]);
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
