// A latency environment whose clocks, frames and drawn mark a test drives,
// shared by the suites of the latencies that end at the page's drawn mark or
// its frames (switch-latency.test.ts, reload-blank.test.ts).
import type { SwitchLatencyEnv } from "../src/switch-latency.js";
import { frameQueue, type FrameQueue } from "./frame-queue.js";

/** An environment whose clocks, frames and drawn mark a test drives. */
export class FakeLatencyEnv implements SwitchLatencyEnv {
  wall = 0;
  mono = 0;
  isDrawn = false;
  private readonly frames: FrameQueue = frameQueue();
  private drawnWaiters = new Set<() => void>();

  readonly wallNow = (): number => this.wall;
  readonly monoNow = (): number => this.mono;
  readonly frame = (fn: () => void): void => this.frames.frame(fn);
  readonly drawn = (): boolean => this.isDrawn;
  readonly onDrawn = (fn: () => void): (() => void) => {
    if (this.isDrawn) {
      fn();
      return () => undefined;
    }
    this.drawnWaiters.add(fn);
    return () => this.drawnWaiters.delete(fn);
  };

  /** Start one animation frame. */
  runFrames(): void {
    this.frames.run();
  }

  /** The page draws its conversation. */
  draw(): void {
    this.isDrawn = true;
    for (const fn of [...this.drawnWaiters]) fn();
    this.drawnWaiters.clear();
  }
}

