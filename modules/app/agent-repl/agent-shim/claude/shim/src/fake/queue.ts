/**
 * fake/queue.ts — the push→pull bridge the mocked vendor emits through.
 *
 * Harvested from the deleted `src/input-queue.ts` (revision `42655f0fd`), which
 * the shim no longer needs anywhere else: the real query is an async iterable
 * the SDK hands us, and only the MOCK has to turn a push-style producer into
 * one. So the class lives here, owned by the module that is its only consumer,
 * rather than staying a shared utility with a single caller.
 *
 * `fail` exists and `end` is not enough: a producer that died mid-turn and a
 * producer that finished are opposite facts, and a consumer that saw clean EOF
 * for both would silently swallow the death. Buffered values stay readable
 * before the rejection so nothing the producer managed to emit is lost.
 */
export class AsyncQueue<T> implements AsyncIterable<T> {
  private buffer: T[] = [];
  private waiters: Array<{
    resolve: (res: IteratorResult<T>) => void;
    reject: (error: Error) => void;
  }> = [];
  private state: "open" | "ended" | "failed" = "open";
  private failure: Error | null = null;

  /** Push a value; throws if the queue has already ended. */
  push(value: T): void {
    if (this.state !== "open") {
      throw new Error("push after end()");
    }
    const waiter = this.waiters.shift();
    if (waiter) {
      waiter.resolve({ value, done: false });
    } else {
      this.buffer.push(value);
    }
  }

  /** Signal end-of-stream. Idempotent. */
  end(): void {
    if (this.state !== "open") return;
    this.state = "ended";
    for (const waiter of this.waiters.splice(0)) {
      waiter.resolve({ value: undefined as never, done: true });
    }
  }

  /** Fail the producer side; buffered values stay readable first. */
  fail(error: unknown): void {
    if (this.state !== "open") return;
    this.state = "failed";
    this.failure = error instanceof Error ? error : new Error(String(error));
    for (const waiter of this.waiters.splice(0)) {
      waiter.reject(this.failure);
    }
  }

  get isEnded(): boolean {
    return this.state !== "open";
  }

  [Symbol.asyncIterator](): AsyncIterator<T> {
    return {
      next: (): Promise<IteratorResult<T>> => {
        if (this.buffer.length > 0) {
          return Promise.resolve({ value: this.buffer.shift()!, done: false });
        }
        if (this.state === "failed") {
          return Promise.reject(this.failure!);
        }
        if (this.state === "ended") {
          return Promise.resolve({ value: undefined as never, done: true });
        }
        return new Promise((resolve, reject) => this.waiters.push({ resolve, reject }));
      },
      return: (): Promise<IteratorResult<T>> => {
        this.end();
        return Promise.resolve({ value: undefined as never, done: true });
      },
    };
  }
}
