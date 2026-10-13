// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { FRAME_MS, installAnimationFrames } from "./animation-frames.js";

afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
});

describe("the hand-driven animation frames", () => {
  it("runs exactly the frames asked for, re-queued callbacks included", () => {
    // Arrange
    const frames = installAnimationFrames();
    let ran = 0;
    const step = (): void => {
      ran += 1;
      requestAnimationFrame(step);
    };
    requestAnimationFrame(step);
    // Act
    frames.run(3);
    // Assert
    expect(ran).toBe(3);
  });

  it("moves the faked clock one frame before each frame runs", () => {
    // Arrange
    const frames = installAnimationFrames();
    const seen: number[] = [];
    requestAnimationFrame(() => seen.push(Date.now()));
    // Act
    frames.run(1);
    // Assert
    expect(seen).toEqual([FRAME_MS]);
  });

  it("moves the clock even when no frame is queued", () => {
    // Arrange
    const frames = installAnimationFrames();
    // Act
    frames.run(2);
    // Assert
    expect(Date.now()).toBe(FRAME_MS * 2);
  });
});
