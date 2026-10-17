// The one way a page module waits for the next animation frame. See
// src/frame.ts.
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it, vi } from "vitest";

import { nextFrame } from "../src/frame.js";
import { codeUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");


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

describe("who waits for a frame", () => {
  it("leaves every page module to nextFrame, but those reading the scheduler off a view", () => {
    // Arrange: the modules that own a view's missing scheduler (src/frame.ts),
    // and the busy probe, which wraps the scheduler to time every frame
    // callback and never waits on a frame itself (src/busy-probe.ts).
    const ownView = new Set(["frame.ts", "type-out.ts", path.join("bubble", "body.ts"), "busy-probe.ts"]);

    // Act
    const rolled = [...codeUnder(SRC)]
      .filter(([file, code]) => !ownView.has(file) && /requestAnimationFrame/.test(code))
      .map(([file]) => file);

    // Assert
    expect(rolled).toEqual([]);
  });
});
