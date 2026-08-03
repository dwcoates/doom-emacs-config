import { describe, expect, it } from "vitest";
import {
  EagerHost,
  FrameHost,
  RenderCoalescer,
  windowEagerHost,
  windowFrameHost,
} from "../src/coalesce.js";

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

/** An EagerHost whose deferred callbacks fire only when the test says so. */
function fakeEagerHost(): {
  host: EagerHost;
  firePending(): void;
  pendingCount(): number;
  cancelled: number[];
} {
  const pending = new Map<number, () => void>();
  const cancelled: number[] = [];
  let nextId = 1;
  return {
    host: {
      scheduleEager(callback: () => void): number {
        const id = nextId++;
        pending.set(id, callback);
        return id;
      },
      cancelEager(id: number): void {
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

describe("RenderCoalescer stall watchdog", () => {
  it("reports a stall when the host never services the pending frame", () => {
    // Arrange — a frozen clockless host, frames keep being asked for.
    const fake = fakeFrameHost();
    let clock = 0;
    const stalls: number[] = [];
    const coalescer = new RenderCoalescer(fake.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      onStall: (ms) => stalls.push(ms),
    });
    coalescer.schedule();
    // Act — the next ask lands after the threshold with the frame unserviced.
    clock = 1500;
    coalescer.schedule();
    // Assert
    expect(stalls).toEqual([1500]);
  });

  it("stays quiet while the pending frame is younger than the threshold", () => {
    // Arrange
    const fake = fakeFrameHost();
    let clock = 0;
    const stalls: number[] = [];
    const coalescer = new RenderCoalescer(fake.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      onStall: (ms) => stalls.push(ms),
    });
    coalescer.schedule();
    // Act
    clock = 999;
    coalescer.schedule();
    // Assert
    expect(stalls).toEqual([]);
  });

  it("reports one stall per episode, not one per schedule", () => {
    // Arrange
    const fake = fakeFrameHost();
    let clock = 0;
    const stalls: number[] = [];
    const coalescer = new RenderCoalescer(fake.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      onStall: (ms) => stalls.push(ms),
    });
    coalescer.schedule();
    clock = 1500;
    coalescer.schedule();
    // Act — the wedge persists through more frames.
    clock = 2500;
    coalescer.schedule();
    clock = 3500;
    coalescer.schedule();
    // Assert
    expect(stalls).toHaveLength(1);
  });

  it("reports recovery with the stalled frame's total age when it finally fires", () => {
    // Arrange — a detected stall, then the host wakes up.
    const fake = fakeFrameHost();
    let clock = 0;
    const recoveries: number[] = [];
    const coalescer = new RenderCoalescer(fake.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      onStall: () => {},
      onStallRecover: (ms) => recoveries.push(ms),
    });
    coalescer.schedule();
    clock = 1500;
    coalescer.schedule();
    // Act
    clock = 2000;
    fake.firePending();
    // Assert
    expect(recoveries).toEqual([2000]);
  });

  it("does not report recovery for a frame that was never stalled", () => {
    // Arrange
    const fake = fakeFrameHost();
    let clock = 0;
    const recoveries: number[] = [];
    const coalescer = new RenderCoalescer(fake.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      onStallRecover: (ms) => recoveries.push(ms),
    });
    coalescer.schedule();
    // Act
    clock = 20;
    fake.firePending();
    // Assert
    expect(recoveries).toEqual([]);
  });

  it("cancel clears the stall episode", () => {
    // Arrange — a stalled frame is cancelled (e.g. a session rebind).
    const fake = fakeFrameHost();
    let clock = 0;
    const recoveries: number[] = [];
    const coalescer = new RenderCoalescer(fake.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      onStall: () => {},
      onStallRecover: (ms) => recoveries.push(ms),
    });
    coalescer.schedule();
    clock = 1500;
    coalescer.schedule();
    // Act — cancel, then a healthy schedule/fire cycle.
    coalescer.cancel();
    coalescer.schedule();
    fake.firePending();
    // Assert — the cancelled episode never reports a recovery.
    expect(recoveries).toEqual([]);
  });
});

describe("RenderCoalescer while the page is hidden", () => {
  /** Wire a coalescer whose visibility the test drives directly. */
  function hiddenAwareCoalescer(): {
    frames: fakeFrameHostResult;
    eager: fakeEagerHostResult;
    coalescer: RenderCoalescer;
    renders: () => number;
    setHidden(hidden: boolean): void;
  } {
    const frames = fakeFrameHost();
    const eager = fakeEagerHost();
    let hidden = false;
    let renders = 0;
    const coalescer = new RenderCoalescer(frames.host, () => renders++, {
      isHidden: () => hidden,
      eagerHost: eager.host,
    });
    return {
      frames,
      eager,
      coalescer,
      renders: () => renders,
      setHidden: (next) => {
        hidden = next;
      },
    };
  }
  type fakeFrameHostResult = ReturnType<typeof fakeFrameHost>;
  type fakeEagerHostResult = ReturnType<typeof fakeEagerHost>;

  it("schedules off the eager host rather than rAF", () => {
    // Arrange
    const w = hiddenAwareCoalescer();
    w.setHidden(true);
    // Act
    w.coalescer.schedule();
    // Assert
    expect(w.eager.pendingCount()).toBe(1);
    expect(w.frames.pendingCount()).toBe(0);
  });

  it("renders when the eager deferral runs, with no frame needed", () => {
    // Arrange
    const w = hiddenAwareCoalescer();
    w.setHidden(true);
    w.coalescer.schedule();
    // Act
    w.eager.firePending();
    // Assert
    expect(w.renders()).toBe(1);
  });

  it("still collapses a burst into one eager render", () => {
    // Arrange — the bound the eager path must preserve: one apply per tick.
    const w = hiddenAwareCoalescer();
    w.setHidden(true);
    // Act
    w.coalescer.schedule();
    w.coalescer.schedule();
    w.coalescer.schedule();
    w.eager.firePending();
    // Assert
    expect(w.renders()).toBe(1);
  });

  it("moves a frame armed while visible onto the eager host", () => {
    // Arrange — the page goes hidden with a rAF already pending, which
    // that host will now never service.
    const w = hiddenAwareCoalescer();
    w.coalescer.schedule();
    w.setHidden(true);
    // Act
    w.coalescer.schedule();
    // Assert
    expect(w.eager.pendingCount()).toBe(1);
    expect(w.frames.pendingCount()).toBe(0);
  });

  it("cancel drops a pending eager render through the eager host", () => {
    // Arrange
    const w = hiddenAwareCoalescer();
    w.setHidden(true);
    w.coalescer.schedule();
    // Act
    w.coalescer.cancel();
    w.eager.firePending();
    // Assert
    expect(w.renders()).toBe(0);
  });

  it("keeps the rAF path when the page is visible", () => {
    // Arrange
    const w = hiddenAwareCoalescer();
    // Act
    w.coalescer.schedule();
    // Assert
    expect(w.frames.pendingCount()).toBe(1);
    expect(w.eager.pendingCount()).toBe(0);
  });

  it("keeps the rAF path when no eager host is supplied", () => {
    // Arrange — an isHidden with no eager scheduler must not change behavior.
    const frames = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(frames.host, () => renders++, {
      isHidden: () => true,
    });
    // Act
    coalescer.schedule();
    // Assert
    expect(frames.pendingCount()).toBe(1);
  });

  it("does not report a frame-threshold stall for the hidden path", () => {
    // Arrange — a hidden-page timer ticking about once a second is healthy.
    const frames = fakeFrameHost();
    const eager = fakeEagerHost();
    let clock = 0;
    const stalls: number[] = [];
    const coalescer = new RenderCoalescer(frames.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      eagerStallAfterMs: 5000,
      onStall: (ms) => stalls.push(ms),
      isHidden: () => true,
      eagerHost: eager.host,
    });
    coalescer.schedule();
    // Act
    clock = 1500;
    coalescer.schedule();
    // Assert
    expect(stalls).toEqual([]);
  });

  it("still reports a stall when the hidden host stops running timers", () => {
    // Arrange — past the eager threshold nothing is ticking at all.
    const frames = fakeFrameHost();
    const eager = fakeEagerHost();
    let clock = 0;
    const stalls: Array<[number, string]> = [];
    const coalescer = new RenderCoalescer(frames.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      eagerStallAfterMs: 5000,
      onStall: (ms, kind) => stalls.push([ms, kind]),
      isHidden: () => true,
      eagerHost: eager.host,
    });
    coalescer.schedule();
    // Act
    clock = 6000;
    coalescer.schedule();
    // Assert
    expect(stalls).toEqual([[6000, "eager"]]);
  });

  it("reports a visible-state stall as a frame stall", () => {
    // Arrange — the genuine wedge the warning exists for.
    const frames = fakeFrameHost();
    const eager = fakeEagerHost();
    let clock = 0;
    const stalls: Array<[number, string]> = [];
    const coalescer = new RenderCoalescer(frames.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      onStall: (ms, kind) => stalls.push([ms, kind]),
      isHidden: () => false,
      eagerHost: eager.host,
    });
    coalescer.schedule();
    // Act
    clock = 1500;
    coalescer.schedule();
    // Assert
    expect(stalls).toEqual([[1500, "frame"]]);
  });
});

describe("RenderCoalescer flush on reveal", () => {
  it("renders a pending eager update synchronously", () => {
    // Arrange
    const frames = fakeFrameHost();
    const eager = fakeEagerHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(frames.host, () => renders++, {
      isHidden: () => true,
      eagerHost: eager.host,
    });
    coalescer.schedule();
    // Act
    coalescer.flush();
    // Assert
    expect(renders).toBe(1);
  });

  it("drops the pending deferral so the flushed render does not run twice", () => {
    // Arrange
    const frames = fakeFrameHost();
    const eager = fakeEagerHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(frames.host, () => renders++, {
      isHidden: () => true,
      eagerHost: eager.host,
    });
    coalescer.schedule();
    // Act
    coalescer.flush();
    eager.firePending();
    // Assert
    expect(renders).toBe(1);
  });

  it("renders a pending animation frame synchronously", () => {
    // Arrange — a frame armed before the page was hidden is still pending.
    const frames = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(frames.host, () => renders++);
    coalescer.schedule();
    // Act
    coalescer.flush();
    // Assert
    expect(renders).toBe(1);
    expect(frames.pendingCount()).toBe(0);
  });

  it("reports nothing flushed when no render is pending", () => {
    // Arrange
    const frames = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(frames.host, () => renders++);
    // Act
    const flushed = coalescer.flush();
    // Assert
    expect(flushed).toBe(false);
    expect(renders).toBe(0);
  });

  it("reports a flush that rendered", () => {
    // Arrange
    const frames = fakeFrameHost();
    const coalescer = new RenderCoalescer(frames.host, () => {});
    coalescer.schedule();
    // Act
    const flushed = coalescer.flush();
    // Assert
    expect(flushed).toBe(true);
  });

  it("reports stall recovery for a stalled frame it flushes", () => {
    // Arrange — the reveal is what finally ends a suspended-rAF episode.
    const frames = fakeFrameHost();
    let clock = 0;
    const recoveries: number[] = [];
    const coalescer = new RenderCoalescer(frames.host, () => {}, {
      now: () => clock,
      stallAfterMs: 1000,
      onStall: () => {},
      onStallRecover: (ms) => recoveries.push(ms),
    });
    coalescer.schedule();
    clock = 1500;
    coalescer.schedule();
    // Act
    clock = 2000;
    coalescer.flush();
    // Assert
    expect(recoveries).toEqual([2000]);
  });

  it("leaves a later schedule free to arm a fresh render", () => {
    // Arrange
    const frames = fakeFrameHost();
    let renders = 0;
    const coalescer = new RenderCoalescer(frames.host, () => renders++);
    coalescer.schedule();
    coalescer.flush();
    // Act
    coalescer.schedule();
    frames.firePending();
    // Assert
    expect(renders).toBe(2);
  });
});

describe("windowEagerHost", () => {
  it("delegates to the window's own timers", () => {
    // Arrange
    const calls: string[] = [];
    const win = {
      setTimeout: (cb: () => void, ms: number): number => {
        void cb;
        calls.push(`set:${ms}`);
        return 11;
      },
      clearTimeout: (id: number): void => {
        calls.push(`clear:${id}`);
      },
    } as unknown as Window;
    const host = windowEagerHost(win);
    // Act
    const id = host.scheduleEager(() => {});
    host.cancelEager(id);
    // Assert
    expect(id).toBe(11);
    expect(calls).toEqual(["set:0", "clear:11"]);
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
