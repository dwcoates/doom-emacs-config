// @vitest-environment jsdom
import { createPresence } from "../../src/presence.js";
import { beforeEach, describe, expect, it } from "vitest";
import { partAnimations, PSEUDO_DELAY, type AnimationPart, type PartAnimations } from "../../src/animation-delays.js";
import {
  installWallPhase,
  partLoops,
  seekToWallClock,
  wallPhaseMs,
  WALL_PHASE_BASE_ATTRIBUTE,
  type PhasedAnimation,
} from "../../src/sidebar/wall-phase.js";
import { captureLogRecords, type LogCapture } from "../log-capture.js";
import { frameQueue } from "../frame-queue.js";

/** A running animation with TIMING, at no particular phase yet. */
function animation(timing: EffectTiming): PhasedAnimation {
  return { currentTime: null, effect: { getTiming: () => timing } };
}

/** An endless loop of DURATION milliseconds, as `ws-pulse` is. */
function loop(duration: number, extra: EffectTiming = {}): PhasedAnimation {
  return animation({ duration, iterations: Infinity, delay: 0, direction: "normal", ...extra });
}

/** A document whose visibility the test sets and whose events it fires. */
function fakeDocument(visibility: DocumentVisibilityState): Document & { show(): void; hide(): void } {
  const target = new EventTarget() as EventTarget & { visibilityState: DocumentVisibilityState };
  target.visibilityState = visibility;
  return Object.assign(target, {
    show(): void {
      target.visibilityState = "visible";
      target.dispatchEvent(new Event("visibilitychange"));
    },
    hide(): void {
      target.visibilityState = "hidden";
      target.dispatchEvent(new Event("visibilitychange"));
    },
  }) as unknown as Document & { show(): void; hide(): void };
}

describe("the wall clock's phase in a cycle", () => {
  it.each([
    { name: "an instant on a cycle boundary is at its start", now: 2600, period: 1300, want: 0 },
    { name: "an instant mid-cycle is that far in", now: 2900, period: 1300, want: 300 },
    { name: "the same instant agrees for any page", now: 1_700_000_000_123, period: 1000, want: 123 },
    { name: "an instant before the epoch still lands inside the cycle", now: -100, period: 1300, want: 1200 },
  ])("$name", ({ now, period, want }) => {
    // Arrange / Act
    const phase = wallPhaseMs(now, period);

    // Assert
    expect(phase).toBe(want);
  });
});

describe("seeking animations to the wall clock", () => {
  let capture: LogCapture;
  beforeEach(() => {
    capture = captureLogRecords("debug");
  });

  /** The forwarded records under OPERATION. */
  async function records(operation: string): Promise<Array<{ level?: string; context: string }>> {
    capture.logger.flush();
    await Promise.resolve();
    return capture.sent
      .filter((r) => r.operation === operation)
      .map((r) => ({ level: r.level.case, context: JSON.stringify(r.context) }));
  }

  it("moves an endless loop to the wall clock's phase", () => {
    // Arrange
    const pulse = loop(1300);

    // Act
    seekToWallClock([pulse], 2900);

    // Assert
    expect(pulse.currentTime).toBe(300);
  });

  it("keeps an animation's own delay ahead of the phase", () => {
    // Arrange
    const delayed = loop(1300, { delay: 200 });

    // Act
    seekToWallClock([delayed], 2900);

    // Assert
    expect(delayed.currentTime).toBe(500);
  });

  // THE LOOPS RUN ON THE COMPOSITOR (src/sidebar/name-ink.ts): a breath is a
  // resting copy fading out as a peak copy fades in, and a shimmer is a band
  // box moving one way as the copies inside it move back. Each pair must
  // land on one phase, or the crossfade's weights stop summing to one and
  // the text slides while the band passes.
  it("puts a name's paired copies on one phase each, a breath's two and a shimmer's three", () => {
    // Arrange: ws-pulse-rest and ws-pulse at the 2s mark cycle, then
    // ws-revive-shimmer and two ws-revive-counter at the 2.4s revive cycle.
    const [rest, peak, band, restCounter, peakCounter] = [loop(2000), loop(2000), loop(2400), loop(2400), loop(2400)];

    // Act
    seekToWallClock([rest, peak, band, restCounter, peakCounter], 1_700_000_000_500);

    // Assert
    expect([rest, peak, band, restCounter, peakCounter].map((a) => a.currentTime)).toEqual([500, 500, 1300, 1300, 1300]);
  });

  it("loops an alternating animation over two iterations", () => {
    // Arrange
    const alternating = loop(1000, { direction: "alternate" });

    // Act
    seekToWallClock([alternating], 1500);

    // Assert
    expect(alternating.currentTime).toBe(1500);
  });

  it("leaves a one-shot animation where it is", () => {
    // Arrange
    const oneShot = animation({ duration: 200, iterations: 1 });

    // Act
    seekToWallClock([oneShot], 2900);

    // Assert
    expect(oneShot.currentTime).toBeNull();
  });

  it("answers how many loops it sought", () => {
    // Arrange
    const animations = [loop(1300), animation({ duration: 200, iterations: 1 }), loop(1400)];

    // Act
    const sought = seekToWallClock(animations, 2900);

    // Assert
    expect(sought).toBe(2);
  });

  it("throws on an endless animation with no positive duration", () => {
    // Arrange
    const broken = loop(0);

    // Act
    const act = (): number => seekToWallClock([broken], 2900);

    // Assert
    expect(act).toThrow(/duration 0/);
  });

  it("logs the endless animation with no positive duration as an error", async () => {
    // Arrange
    const broken = loop(0);

    // Act
    expect(() => seekToWallClock([broken], 2900)).toThrow();

    // Assert
    const [record] = await records("sidebar.wall-phase.bad-duration");
    expect(record?.level).toBe("error");
    expect(record?.context).toContain('"duration":"0"');
  });

  it("moves no animation when a later one is broken", () => {
    // Arrange: the broken loop comes after a good one.
    const pulse = loop(1300);

    // Act
    expect(() => seekToWallClock([pulse, loop(Number.NaN)], 2900)).toThrow();

    // Assert
    expect(pulse.currentTime).toBeNull();
  });

  it("throws on a running animation with no effect", () => {
    // Arrange
    const bare: PhasedAnimation = { currentTime: 40, effect: null };

    // Act
    const act = (): number => seekToWallClock([bare], 2900);

    // Assert
    expect(act).toThrow(/no effect/);
  });

  it("logs the running animation with no effect as an error", async () => {
    // Arrange
    const bare: PhasedAnimation = { currentTime: 40, effect: null };

    // Act
    expect(() => seekToWallClock([bare], 2900)).toThrow();

    // Assert
    const [record] = await records("sidebar.wall-phase.no-effect");
    expect(record?.level).toBe("error");
    expect(record?.context).toContain("current_time");
  });
});

describe("keeping a rail on the wall clock", () => {
  it("seeks the host's own animations on demand", () => {
    // Arrange
    const host = document.createElement("nav");
    const pulse = loop(1300);
    let asked: Element | null = null;
    const wallPhase = installWallPhase(host, {
      animations: (of) => {
        asked = of;
        return [pulse];
      },
      now: () => 2900,
      doc: fakeDocument("visible"),
      frame: frameQueue().frame,
    });

    // Act
    wallPhase.seek();

    // Assert
    expect(asked).toBe(host);
    expect(pulse.currentTime).toBe(300);
  });

  it("re-seeks on the first frame after the page is shown again", () => {
    // Arrange
    const pulse = loop(1300);
    const doc = fakeDocument("hidden");
    const frames = frameQueue();
    installWallPhase(document.createElement("nav"), { animations: () => [pulse], now: () => 2900, doc, frame: frames.frame });

    // Act
    doc.show();
    frames.run();

    // Assert
    expect(pulse.currentTime).toBe(300);
  });

  it("re-seeks on the first frame after its host shows the page", () => {
    // Arrange
    const pulse = loop(1300);
    const doc = fakeDocument("visible");
    const presence = createPresence(doc, true);
    const frames = frameQueue();
    installWallPhase(document.createElement("nav"), { animations: () => [pulse], now: () => 2900, doc, frame: frames.frame, presence });

    // Act
    presence.setShown(true);
    frames.run();

    // Assert
    expect(pulse.currentTime).toBe(300);
    presence.dispose();
  });

  it("waits for that frame rather than seeking on the event", () => {
    // Arrange
    const pulse = loop(1300);
    const doc = fakeDocument("hidden");
    const frames = frameQueue();
    installWallPhase(document.createElement("nav"), { animations: () => [pulse], now: () => 2900, doc, frame: frames.frame });

    // Act
    doc.show();

    // Assert
    expect(pulse.currentTime).toBeNull();
  });

  it("asks for no frame when the page is hidden", () => {
    // Arrange
    const doc = fakeDocument("visible");
    const frames = frameQueue();
    installWallPhase(document.createElement("nav"), { animations: () => [], now: () => 0, doc, frame: frames.frame });

    // Act
    doc.hide();

    // Assert
    expect(frames.pending()).toBe(0);
  });

  it("stops watching the page once disposed", () => {
    // Arrange
    const doc = fakeDocument("hidden");
    const frames = frameQueue();
    const wallPhase = installWallPhase(document.createElement("nav"), {
      animations: () => [],
      now: () => 0,
      doc,
      frame: frames.frame,
    });

    // Act
    wallPhase.dispose();
    doc.show();

    // Assert
    expect(frames.pending()).toBe(0);
  });

  it("drops a frame already asked for when disposed before it runs", () => {
    // Arrange
    const pulse = loop(1300);
    const doc = fakeDocument("hidden");
    const frames = frameQueue();
    const wallPhase = installWallPhase(document.createElement("nav"), {
      animations: () => [pulse],
      now: () => 2900,
      doc,
      frame: frames.frame,
    });
    doc.show();

    // Act
    wallPhase.dispose();
    frames.run();

    // Assert
    expect(pulse.currentTime).toBeNull();
  });
});

/** A computed style naming ANIMATION, as WebKit computes one. */
function computed(animation: {
  name: string;
  duration: string;
  iterations: string;
  direction?: string;
  delay?: string;
}): CSSStyleDeclaration {
  return {
    animationName: animation.name,
    animationDuration: animation.duration,
    animationIterationCount: animation.iterations,
    animationDirection: animation.direction ?? "normal",
    animationDelay: animation.delay ?? "0s",
  } as unknown as CSSStyleDeclaration;
}

/** The part PART of EL running the animations COMPUTED lists. */
function part(el: HTMLElement, computedStyle: CSSStyleDeclaration, at: AnimationPart = ""): PartAnimations {
  const read = partAnimations(el, at, computedStyle);
  if (read === null) throw new Error("the test's computed style runs no animation");
  return read;
}

describe("reading a rail element's loops off its computed style", () => {
  it("seeks an endless loop by a negative delay of its phase", () => {
    // Arrange
    const el = document.createElement("span");
    const loops = partLoops(part(el, computed({ name: "ws-pulse", duration: "1.3s", iterations: "infinite" })));

    // Act
    seekToWallClock(loops, 2900);

    // Assert
    expect(el.style.animationDelay).toBe("-300ms");
  });

  it("reads no loop off an element running only a one-shot", () => {
    // Arrange / Act
    const loops = partLoops(part(document.createElement("span"), computed({ name: "row-flash", duration: "200ms", iterations: "1" })));

    // Assert
    expect(loops).toHaveLength(0);
  });

  it("seeks each loop of a list and leaves a one-shot beside it at its own delay", () => {
    // Arrange
    const el = document.createElement("span");
    const loops = partLoops(
      part(el, computed({ name: "ws-pulse, row-flash", duration: "1.3s, 200ms", iterations: "infinite, 1", delay: "0s, 50ms" })),
    );

    // Act
    seekToWallClock(loops, 2900);

    // Assert
    expect(el.style.animationDelay).toBe("-300ms, 50ms");
  });

  it("keeps the element's own delay, so a second seek does not compound the first", () => {
    // Arrange: the element's own delay is 200ms.
    const el = document.createElement("span");
    seekToWallClock(partLoops(part(el, computed({ name: "ws-pulse", duration: "1.3s", iterations: "infinite", delay: "200ms" }))), 2900);

    // Act: the computed delay now reads the first seek's; the kept base decides.
    seekToWallClock(partLoops(part(el, computed({ name: "ws-pulse", duration: "1.3s", iterations: "infinite", delay: "-500ms" }))), 3000);

    // Assert
    expect([el.getAttribute(WALL_PHASE_BASE_ATTRIBUTE[""]), el.style.animationDelay]).toEqual(["200", "-400ms"]);
  });

  it("seeks a pseudo-element's loop through its element's delay property", () => {
    // Arrange
    const el = document.createElement("span");
    const loops = partLoops(part(el, computed({ name: "ws-pulse", duration: "1.3s", iterations: "infinite" }), "::after"));

    // Act
    seekToWallClock(loops, 2900);

    // Assert
    expect([el.style.getPropertyValue(PSEUDO_DELAY["::after"].property), el.style.animationDelay]).toEqual(["-300ms", ""]);
  });

  it("keeps a pseudo-element's own delay apart from its element's", () => {
    // Arrange
    const el = document.createElement("span");
    const loop = computed({ name: "ws-pulse", duration: "1.3s", iterations: "infinite", delay: "100ms" });

    // Act
    seekToWallClock(partLoops(part(el, loop, "::before")), 2900);

    // Assert
    expect([el.getAttribute(WALL_PHASE_BASE_ATTRIBUTE["::before"]), el.getAttribute(WALL_PHASE_BASE_ATTRIBUTE[""])]).toEqual(["100", null]);
  });
});

describe("a reveal restarts the loops it seeks", () => {
  it("asks its source to restart on the first frame after the page is shown, never on a draw", () => {
    // Arrange
    const restarts: boolean[] = [];
    const doc = fakeDocument("hidden");
    const frames = frameQueue();
    const wallPhase = installWallPhase(document.createElement("nav"), {
      animations: (_host, restart) => {
        restarts.push(restart);
        return [];
      },
      now: () => 0,
      doc,
      frame: frames.frame,
    });

    // Act
    wallPhase.seek();
    doc.show();
    frames.run();

    // Assert
    expect(restarts).toEqual([false, true]);
  });
});
