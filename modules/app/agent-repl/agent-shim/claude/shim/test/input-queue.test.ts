import { describe, expect, it } from "vitest";
import { AsyncQueue } from "../src/input-queue.js";

describe("AsyncQueue", () => {
  it("yields a value pushed before iteration", async () => {
    // Arrange
    const q = new AsyncQueue<number>();
    q.push(1);
    // Act
    const it1 = q[Symbol.asyncIterator]();
    const res = await it1.next();
    // Assert
    expect(res).toEqual({ value: 1, done: false });
  });

  it("resolves a waiting consumer when a value is pushed later", async () => {
    // Arrange
    const q = new AsyncQueue<string>();
    const it1 = q[Symbol.asyncIterator]();
    const pending = it1.next();
    // Act
    q.push("late");
    // Assert
    await expect(pending).resolves.toEqual({ value: "late", done: false });
  });

  it("terminates iteration on end()", async () => {
    // Arrange
    const q = new AsyncQueue<number>();
    const it1 = q[Symbol.asyncIterator]();
    // Act
    q.end();
    // Assert
    await expect(it1.next()).resolves.toMatchObject({ done: true });
  });

  it("throws when pushing after end()", () => {
    // Arrange
    const q = new AsyncQueue<number>();
    q.end();
    // Act + Assert
    expect(() => q.push(1)).toThrow(/end/);
  });

  it("keeps end() idempotent", () => {
    // Arrange
    const q = new AsyncQueue<number>();
    q.end();
    // Act + Assert
    expect(() => q.end()).not.toThrow();
    expect(q.isEnded).toBe(true);
  });

  it("ends the queue when the iterator's return() is called", async () => {
    // Arrange
    const q = new AsyncQueue<number>();
    const it1 = q[Symbol.asyncIterator]();
    // Act
    await it1.return!();
    // Assert
    expect(q.isEnded).toBe(true);
  });

  it("drains buffered values in FIFO order before reporting done", async () => {
    // Arrange
    const q = new AsyncQueue<number>();
    q.push(1);
    q.push(2);
    q.end();
    // Act
    const seen: number[] = [];
    for await (const v of q) seen.push(v);
    // Assert
    expect(seen).toEqual([1, 2]);
  });

  it("rejects a waiting consumer when the producer fails", async () => {
    const q = new AsyncQueue<number>();
    const pending = q[Symbol.asyncIterator]().next();
    const failure = new Error("producer failed");
    q.fail(failure);
    await expect(pending).rejects.toBe(failure);
    expect(q.isEnded).toBe(true);
  });

  it("drains buffered values before surfacing producer failure", async () => {
    const q = new AsyncQueue<number>();
    const failure = new Error("producer failed");
    q.push(1);
    q.push(2);
    q.fail(failure);
    const iterator = q[Symbol.asyncIterator]();
    await expect(iterator.next()).resolves.toEqual({ value: 1, done: false });
    await expect(iterator.next()).resolves.toEqual({ value: 2, done: false });
    await expect(iterator.next()).rejects.toBe(failure);
  });
});
