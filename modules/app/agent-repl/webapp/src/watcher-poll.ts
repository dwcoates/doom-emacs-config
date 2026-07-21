/**
 * Watcher poller — the only-while-expanded transport for watcher progress
 * (§ watcher-bubble expansion).
 *
 * Backgrounded shells and workflows already stream their tail over the WS
 * (task-output-delta into the store), but a detached AGENT is not tailed
 * and a settled call's elapsed clock freezes. This poller fills both gaps
 * by hitting the daemon's task-output route ONLY while a watcher fold is
 * open, so nothing enters the durable replay ring and nothing polls once
 * every fold is shut.
 */
import { clearDedup, log, logDedup } from "./wslog.js";

/** The daemon's task-output route response (see server.handleTaskOutput). */
export interface TaskTailResponse {
  text: string;
  offset: number;
  done: boolean;
  elapsed_ms: number;
}

/** Fetches one bounded chunk of a task's tail from the daemon. */
export type FetchTail = (taskId: string, offset: number) => Promise<TaskTailResponse>;

/** A task's accumulated tail across polls. */
export interface TaskTail {
  text: string;
  offset: number;
  done: boolean;
  elapsedMs: number;
}

/** GET the daemon's bounded, session-scoped task-output tail. */
export async function fetchTaskTail(
  httpBase: string,
  sessionId: string,
  taskId: string,
  offset: number,
): Promise<TaskTailResponse> {
  const res = await fetch(
    `${httpBase}/sessions/${encodeURIComponent(sessionId)}/tasks/${encodeURIComponent(
      taskId,
    )}/output?offset=${offset}`,
  );
  if (!res.ok) {
    throw new Error(`task output ${res.status}`);
  }
  return (await res.json()) as TaskTailResponse;
}

export class WatcherPoller {
  private tails = new Map<string, TaskTail>();
  private active = new Set<string>();
  private inflight = new Set<string>();
  private timer: ReturnType<typeof setInterval> | null = null;
  /** Task ids whose most recent poll failed, so recovery logs only once. */
  private failed = new Set<string>();

  constructor(
    private fetchTail: FetchTail,
    private onUpdate: () => void,
    private intervalMs = 1000,
  ) {}

  /** The accumulated tail for TASKID, or undefined if never polled. */
  tail(taskId: string): TaskTail | undefined {
    return this.tails.get(taskId);
  }

  /** Whether the poll interval is currently running (test seam). */
  pollingActive(): boolean {
    return this.timer !== null;
  }

  /**
   * Reconcile the set of task ids that should be polled — those in open
   * watcher folds. An id that just opened begins with an immediate fetch so
   * the fold does not sit blank; ids no longer open stop. The interval runs
   * only while at least one id is active. A task already known done is never
   * (re-)polled, so reopening its fold shows the cached final tail for free.
   */
  sync(active: ReadonlySet<string>): void {
    const next = new Set<string>();
    for (const id of active) {
      if (!this.tails.get(id)?.done) next.add(id);
    }
    const added = [...next].filter((id) => !this.active.has(id));
    this.active = next;
    if (this.active.size > 0 && this.timer === null) {
      this.timer = setInterval(() => void this.tick(), this.intervalMs);
    } else if (this.active.size === 0 && this.timer !== null) {
      clearInterval(this.timer);
      this.timer = null;
    }
    for (const id of added) void this.poll(id);
  }

  /** Poll every active id once; resolves when all settle (test seam). */
  async tick(): Promise<void> {
    await Promise.all([...this.active].map((id) => this.poll(id)));
  }

  /** Poll one task once, accumulating its tail (test seam). */
  async poll(id: string): Promise<void> {
    if (this.inflight.has(id)) return;
    const prev = this.tails.get(id) ?? { text: "", offset: 0, done: false, elapsedMs: 0 };
    if (prev.done) return;
    this.inflight.add(id);
    try {
      const res = await this.fetchTail(id, prev.offset);
      this.tails.set(id, {
        text: prev.text + res.text,
        offset: res.offset,
        done: res.done,
        elapsedMs: res.elapsed_ms,
      });
      if (res.done) {
        this.active.delete(id);
        if (this.active.size === 0 && this.timer !== null) {
          clearInterval(this.timer);
          this.timer = null;
        }
      }
      if (this.failed.delete(id)) {
        clearDedup(`watcher-poll:${id}`);
        log("info", `watcher poll recovered for task ${id}`);
      }
      this.onUpdate();
    } catch (err) {
      // A failed poll is transient; the next tick retries from the same
      // offset. Dedup per task so a persistently failing route logs once
      // per distinct error rather than once per second.
      this.failed.add(id);
      const reason = err instanceof Error ? err.message : String(err);
      logDedup(`watcher-poll:${id}`, "warn", `watcher poll failed for task ${id}: ${reason}`);
    } finally {
      this.inflight.delete(id);
    }
  }

  /** Stop all polling (webview teardown). */
  dispose(): void {
    this.active.clear();
    if (this.timer !== null) {
      clearInterval(this.timer);
      this.timer = null;
    }
  }
}
