// @vitest-environment jsdom
import { createPresence } from "../../src/presence.js";
import { beforeEach, describe, expect, it } from "vitest";
import {
  installWallPhase,
  seekToWallClock,
  wallPhaseMs,
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
