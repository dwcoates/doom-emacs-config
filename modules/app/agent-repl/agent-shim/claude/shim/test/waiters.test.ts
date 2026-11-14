/**
 * The one shape of a test's wait on an event.
 */
import { readFileSync } from "node:fs";
import { dirname, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";
import { Waiters } from "./waiters.js";

describe("Waiters", () => {
  it("resolves at once when the check already holds", async () => {
    // Arrange
    const waiters = new Waiters("the source");

    // Act / Assert
    await expect(waiters.until("nothing", () => true)).resolves.toBeUndefined();
  });

  it("resolves at the notify after which the check holds, not before", async () => {
    // Arrange
    const waiters = new Waiters("the source");
    let events = 0;
    let done = false;
    const waiting = waiters.until("two events", () => events >= 2).then(() => {
      done = true;
    });

    // Act
    events = 1;
    waiters.notify();
    await Promise.resolve();
    const afterOne = done;
    events = 2;
    waiters.notify();
    await waiting;

    // Assert
    expect([afterOne, done]).toEqual([false, true]);
  });

  it("rejects at once when the check throws at a notify", async () => {
    // Arrange
    const waiters = new Waiters("the source");
    let broken = false;
    const waiting = waiters.until("a state", () => {
      if (broken) throw new Error("boom");
      return false;
    });

    // Act
    broken = true;
    waiters.notify();

    // Assert
    await expect(waiting).rejects.toThrow("waiting for a state: the check threw: Error: boom");
  });

  it("rejects a pending wait at once when the source ends", async () => {
    // Arrange
    const waiters = new Waiters("the drive");
    const waiting = waiters.until("a result", () => false);

    // Act
    waiters.end("it failed: boom");

    // Assert
    await expect(waiting).rejects.toThrow("waiting for a result: the drive ended first (it failed: boom)");
  });

  it("rejects a wait begun after the source ended", async () => {
    // Arrange
    const waiters = new Waiters("the drive");
    waiters.end("its iterable closed");

    // Act / Assert
    await expect(waiters.until("a result", () => false)).rejects.toThrow(
      "waiting for a result: the drive ended first (its iterable closed)",
    );
  });
});

describe("the shim's tests", () => {
  it("never hand-roll a set of waiters resolved at an event: they use Waiters (waiters.ts)", () => {
    // Arrange
    const dir = dirname(fileURLToPath(import.meta.url));
    const self = fileURLToPath(import.meta.url);
    const helper = `${dir}/waiters.ts`;

    // Act
    const handRolled = filesUnder(dir).filter(
      (file) =>
        file !== self &&
        file !== helper &&
        file.endsWith(".ts") &&
        /new Set<\{[^}]*\bresolve\b/s.test(readFileSync(file, "utf8")),
    );

    // Assert
    expect(handRolled.map((file) => relative(dir, file))).toEqual([]);
  });
});
