// @vitest-environment jsdom
//
// The WebKit pages' shared painted-frame clock (test/webkit/painted-frames.ts),
// under jsdom: the animation frame is queued by hand and the sentinel's resize
// is delivered through the ResizeObserver stand-in, so each phase is a step.
import { afterEach, describe, expect, it, vi } from "vitest";
import { paintedFrames } from "./webkit/painted-frames";
import { fireResize } from "./resize-observer";
import { frameQueue } from "./frame-queue";

/** A clock over a fresh body, with its sentinel and the animation frames it queued. */
function clock() {
  document.body.replaceChildren();
  const queue = frameQueue();
  vi.stubGlobal("requestAnimationFrame", (cb: FrameRequestCallback) => {
    queue.frame(() => cb(0));
  });
  const frame = paintedFrames();
  const sentinel = document.body.lastElementChild as HTMLElement;
  // Running the queued animation frames is a rendering update starting.
  return { frame, sentinel, animate: queue.run };
}

afterEach(() => {
  vi.unstubAllGlobals();
  document.body.replaceChildren();
});

describe("paintedFrames", () => {
  it("answers READ once the frame's resize observer phase delivers", async () => {
    // Arrange
    const c = clock();
    const answer = c.frame(() => "painted");
    // Act
    c.animate();
    fireResize(c.sentinel);
    // Assert
    await expect(answer).resolves.toBe("painted");
  });

  it("does not read in the animation frame, before the layout is settled", () => {
    // Arrange
    const c = clock();
    const read = vi.fn(() => 0);
    void c.frame(read);
    // Act
    c.animate();
    // Assert
    expect(read).not.toHaveBeenCalled();
  });

  it("resizes its sentinel on every frame, so the observer fires each one", () => {
    // Arrange
    const c = clock();
    const widths: string[] = [];
    // Act
    for (let i = 0; i < 3; i++) {
      void c.frame(() => null);
      c.animate();
      widths.push(c.sentinel.style.width);
      fireResize(c.sentinel);
    }
    // Assert
    expect(widths).toEqual(["2px", "1px", "2px"]);
  });

  it("is the one painted-frame clock: no WebKit page observes a resize of its own", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const pages = import.meta.glob("./webkit/*-page.ts", {
      query: "?raw",
      import: "default",
      eager: true,
    }) as Record<string, string>;
    const measuring = Object.entries(pages).filter(([, src]) => /paintedFrames\(/.test(src));
    // Act
    const ownObservers = Object.entries(pages)
      .filter(([, src]) => /new ResizeObserver\(/.test(src))
      .map(([path]) => path);
    // Assert
    expect([measuring.length >= 2, ownObservers]).toEqual([true, []]);
  });
});
