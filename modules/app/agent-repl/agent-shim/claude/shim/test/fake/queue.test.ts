/**
 * AsyncQueue: the push-to-pull bridge the mocked vendor emits through.
 *
 * Every scenario test drives this class indirectly (it is what `createFakeQuery`
 * streams messages through), which already pins push/end/fail/isEnded/next
 * thoroughly. What none of them do is BREAK a `for await` early, so the
 * iterator's own `return()` -- the async-iterator protocol's "the consumer
 * went away" callback -- had never run.
 */
import { describe, expect, it } from "vitest";
import { AsyncQueue } from "../../src/fake/queue.js";

describe("AsyncQueue", () => {
  it("delivers pushed values in order", async () => {
    const queue = new AsyncQueue<number>();
    queue.push(1);
    queue.push(2);

    const iterator = queue[Symbol.asyncIterator]();
    expect(await iterator.next()).toEqual({ value: 1, done: false });
    expect(await iterator.next()).toEqual({ value: 2, done: false });
  });

  it("ends cleanly for a consumer already waiting", async () => {
    const queue = new AsyncQueue<number>();
    const iterator = queue[Symbol.asyncIterator]();
    const pending = iterator.next();

    queue.end();

    expect(await pending).toEqual({ value: undefined, done: true });
    expect(queue.isEnded).toBe(true);
  });

  it("rejects a waiting consumer on fail(), after any buffered values", async () => {
    const queue = new AsyncQueue<number>();
    const iterator = queue[Symbol.asyncIterator]();
    const pending = iterator.next();

    queue.fail(new Error("the vendor died"));

    await expect(pending).rejects.toThrow("the vendor died");
  });

  it("refuses a push after end()", () => {
    const queue = new AsyncQueue<number>();
    queue.end();

    expect(() => queue.push(1)).toThrow(/push after end/);
  });

  it("ends the queue when the iterator's own return() is called, as a for-await break does", async () => {
    const queue = new AsyncQueue<number>();
    queue.push(1);

    for await (const value of queue) {
      expect(value).toBe(1);
      break;
    }

    expect(queue.isEnded).toBe(true);
  });
});
