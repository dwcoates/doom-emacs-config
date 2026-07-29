/**
 * An unbounded async queue bridging push-style producers (stdin command
 * handling) to the pull-style `AsyncIterable` the Agent SDK's streaming
 * input mode consumes.
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

  /**
   * Fail the producer side of the queue.
   *
   * Buffered values remain readable in FIFO order, then the iterator rejects
   * with `error`. A consumer already waiting on an empty queue rejects
   * immediately. This distinguishes producer failure from clean EOF without
   * discarding messages the producer emitted before it failed.
   */
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
