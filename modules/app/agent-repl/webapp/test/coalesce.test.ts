import { describe, expect, it } from "vitest";
import { FrameHost, RenderCoalescer, windowFrameHost } from "../src/coalesce.js";

/** A FrameHost whose frames fire only when the test says so. */
function fakeFrameHost(): {
  host: FrameHost;
  firePending(): void;
  pendingCount(): number;
  cancelled: number[];
} {
  const pending = new Map<number, () => void>();
  const cancelled: number[] = [];
  let nextId = 1;
  return {
    host: {
      requestAnimationFrame(callback: () => void): number {
        const id = nextId++;
        pending.set(id, callback);
        return id;
      },
      cancelAnimationFrame(id: number): void {
        pending.delete(id);
        cancelled.push(id);
      },
    },
    firePending(): void {
      const callbacks = [...pending.values()];
      pending.clear();
      for (const cb of callbacks) cb();
    },
    pendingCount: () => pending.size,
    cancelled,
  };
}

describe("RenderCoalescer", () => {
  it("renders on the next animation frame, not synchronously", () => {
    // Arrange
    const fake = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(fake.host, () => renders++);
    // Act
    coalescer.schedule();
    // Assert
    expect(renders).toBe(0);
    fake.firePending();
    expect(renders).toBe(1);
  });

  it("collapses a burst of schedules into one render", () => {
    // Arrange
    const fake = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(fake.host, () => renders++);
    // Act — a message burst before the frame fires.
    coalescer.schedule();
    coalescer.schedule();
    coalescer.schedule();
    fake.firePending();
    // Assert
    expect(renders).toBe(1);
  });

  it("starts a fresh frame for a schedule after the last one fired", () => {
    // Arrange
    const fake = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(fake.host, () => renders++);
    // Act
    coalescer.schedule();
    fake.firePending();
    coalescer.schedule();
    fake.firePending();
    // Assert
    expect(renders).toBe(2);
  });

  it("cancel drops the pending render", () => {
    // Arrange
    const fake = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(fake.host, () => renders++);
    coalescer.schedule();
    // Act
    coalescer.cancel();
    fake.firePending();
    // Assert
    expect(renders).toBe(0);
  });

  it("cancel with nothing pending cancels nothing", () => {
    // Arrange
    const fake = fakeFrameHost();
    const coalescer = new RenderCoalescer(fake.host, () => {});
    // Act
    coalescer.cancel();
    // Assert
    expect(fake.cancelled).toHaveLength(0);
  });

  it("a schedule after cancel renders again", () => {
    // Arrange
    const fake = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(fake.host, () => renders++);
    coalescer.schedule();
    coalescer.cancel();
    // Act
    coalescer.schedule();
    fake.firePending();
    // Assert
    expect(renders).toBe(1);
  });

  it("a schedule from inside the render lands on the NEXT frame", () => {
    // Arrange — the pending slot must be cleared before the render runs,
    // or a render-triggered schedule would be swallowed as a duplicate.
    const fake = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(fake.host, () => {
      renders++;
      if (renders === 1) coalescer.schedule();
    });
    coalescer.schedule();
    // Act
    fake.firePending();
    // Assert
    expect(renders).toBe(1);
    expect(fake.pendingCount()).toBe(1);
  });
});

describe("windowFrameHost", () => {
  it("delegates to the window's own scheduler", () => {
    // Arrange
    const calls: string[] = [];
    const win = {
      requestAnimationFrame: (cb: FrameRequestCallback): number => {
        void cb;
        calls.push("raf");
        return 7;
      },
      cancelAnimationFrame: (id: number): void => {
        calls.push(`caf:${id}`);
      },
    } as unknown as Window;
    const host = windowFrameHost(win);
    // Act
    const id = host.requestAnimationFrame(() => {});
    host.cancelAnimationFrame(id);
    // Assert
    expect(id).toBe(7);
    expect(calls).toEqual(["raf", "caf:7"]);
  });
});
