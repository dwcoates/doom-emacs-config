/**
 * The topbar's live task timer: how long the running turn has been going.
 *
 * The turn's start is the daemon's own `user-turn` stamp rather than the
 * moment this tab noticed the turn, so a tab that joins or reconnects
 * mid-turn picks the count up where the turn actually is instead of
 * restarting it from zero.
 *
 * Nothing here touches the DOM. The clock and the paint are both injected,
 * which is what lets the tick lifecycle be tested at all: the webapp's test
 * dependencies carry no DOM.
 */
import { formatElapsed } from "./duration.js";

/** How often a running timer repaints. The label's finest unit is seconds. */
export const TICK_MS = 1000;

/** The label a header with no running task shows. */
export const IDLE_LABEL = "--";

/**
 * Marks the one span in the topbar the tick repaints. The renderer stamps
 * it, the tick queries for it, so neither has to spell out the other's
 * markup.
 */
export const TIMER_SLOT = "data-task-timer";

/** The clock and scheduler the timer runs on (`window`, in the browser). */
export interface TimerHost {
  setInterval(handler: () => void, ms: number): number;
  clearInterval(id: number): void;
  now(): number;
}

/** `window` as a TimerHost: the browser's own clock and scheduler. */
export function windowHost(win: Window): TimerHost {
  return {
    setInterval: (handler, ms) => win.setInterval(handler, ms),
    clearInterval: (id) => win.clearInterval(id),
    now: () => Date.now(),
  };
}

/**
 * The elapsed-time label for a turn that started at STARTED-AT (the
 * `user-turn` envelope ts), or the idle label when no turn is running.
 */
export function timerLabel(startedAt: string | null, now: number): string {
  if (startedAt === null) return IDLE_LABEL;
  return formatElapsed(now - Date.parse(startedAt));
}

/**
 * A once-a-second repaint of the topbar's timer, for exactly as long as a
 * turn is in flight.
 *
 * The tick is deliberately NOT a re-render: the topbar is rebuilt on every
 * frame of a streaming turn already, and a whole-strip rewrite once a
 * second on top of that would be churn in service of one changing digit.
 * PAINT writes the label and nothing else.
 */
export class TaskTimer {
  private tick: number | null = null;
  private startedAt: string | null = null;

  constructor(
    private readonly host: TimerHost,
    private readonly paint: (label: string) => void,
  ) {}

  /**
   * Reconcile the timer against the store's turn start (null when no turn
   * is running). Cheap to call on every frame: the interval is only touched
   * on the edges, and the label is repainted immediately at both of them so
   * the header never trails the spinner by a tick.
   */
  sync(startedAt: string | null): void {
    if (startedAt === this.startedAt) return;
    this.startedAt = startedAt;
    if (startedAt === null) {
      this.clear();
    } else if (this.tick === null) {
      this.tick = this.host.setInterval(() => this.repaint(), TICK_MS);
    }
    this.repaint();
  }

  /** Stop ticking and leave the header idle. Used when the view is torn down. */
  stop(): void {
    this.startedAt = null;
    this.clear();
    this.repaint();
  }

  private clear(): void {
    if (this.tick === null) return;
    this.host.clearInterval(this.tick);
    this.tick = null;
  }

  private repaint(): void {
    this.paint(timerLabel(this.startedAt, this.host.now()));
  }
}
