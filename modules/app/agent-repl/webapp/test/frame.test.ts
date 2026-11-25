// The one way a page module waits for the next animation frame. See
// src/frame.ts.
import { afterEach, describe, expect, it, vi } from "vitest";

import { nextFrame } from "../src/frame.js";

/** Stub the page's frame scheduler; answers the callbacks it was handed. */
function stubFrames(): FrameRequestCallback[] {
  const queued: FrameRequestCallback[] = [];
  vi.stubGlobal("requestAnimationFrame", (callback: FrameRequestCallback) => {
    queued.push(callback);
    return queued.length;
  });
  return queued;
}

afterEach(() => {
  vi.unstubAllGlobals();
});

describe("nextFrame", () => {
  it("runs nothing before the frame", () => {
    // Arrange
    stubFrames();
    let ran = false;

    // Act
    nextFrame(() => {
      ran = true;
    });

    // Assert
    expect(ran).toBe(false);
  });

  it("runs the callback at the next frame", () => {
    // Arrange
    const queued = stubFrames();
    let ran = 0;
    nextFrame(() => {
      ran += 1;
    });

    // Act
    queued[0](16);

    // Assert
    expect(ran).toBe(1);
  });

  it("hands the callback nothing, not the frame's timestamp", () => {
    // Arrange
    const queued = stubFrames();
    let handed: unknown[] = [];
    nextFrame((...args: unknown[]) => {
      handed = args;
    });

    // Act
    queued[0](16);

    // Assert
    expect(handed).toEqual([]);
  });
});
