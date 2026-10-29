/**
 * engine/prompt-queue.ts — the push-style bridge every query is prompted through.
 */
import type { SdkUserMessage } from "../sdk/types.js";

/**
 * A push-style bridge onto the SDK's pull-style streaming input.
 *
 * The SDK wants an `AsyncIterable` it pulls from; the engine has prompts that
 * arrive when a consumer sends them. This is the ONE submitter — everything
 * that delivers a prompt goes through it, which is what makes "one turn in
 * flight" enforceable rather than merely intended.
 */
export class PromptQueue implements AsyncIterable<SdkUserMessage> {
  private readonly queued: SdkUserMessage[] = [];
  private waiting: ((value: IteratorResult<SdkUserMessage>) => void) | undefined;
  private done = false;

  push(message: SdkUserMessage): void {
    if (this.done) throw new Error("shim session: a prompt was submitted after the query was closed");
    if (this.waiting !== undefined) {
      const resolve = this.waiting;
      this.waiting = undefined;
      resolve({ value: message, done: false });
      return;
    }
    this.queued.push(message);
  }

  close(): void {
    this.done = true;
    if (this.waiting !== undefined) {
      const resolve = this.waiting;
      this.waiting = undefined;
      resolve({ value: undefined, done: true });
    }
  }

  [Symbol.asyncIterator](): AsyncIterator<SdkUserMessage> {
    return {
      next: (): Promise<IteratorResult<SdkUserMessage>> => {
        const next = this.queued.shift();
        if (next !== undefined) return Promise.resolve({ value: next, done: false });
        if (this.done) return Promise.resolve({ value: undefined, done: true });
        return new Promise((resolve) => {
          this.waiting = resolve;
        });
      },
    };
  }
}
