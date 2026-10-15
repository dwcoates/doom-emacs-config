import { readdirSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { frameQueue } from "./frame-queue.js";

describe("frameQueue", () => {
  it("runs nothing until a frame starts", () => {
    // Arrange
    const queue = frameQueue();
    const ran: string[] = [];

    // Act
    queue.frame(() => ran.push("a"));

    // Assert
    expect(ran).toEqual([]);
    expect(queue.pending()).toBe(1);
  });

  it("runs every waiting callback in the order scheduled when a frame starts", () => {
    // Arrange
    const queue = frameQueue();
    const ran: string[] = [];
    queue.frame(() => ran.push("a"));
    queue.frame(() => ran.push("b"));

    // Act
    queue.run();

    // Assert
    expect(ran).toEqual(["a", "b"]);
    expect(queue.pending()).toBe(0);
  });

  it("puts a callback scheduled during a frame on the next frame", () => {
    // Arrange
    const queue = frameQueue();
    const ran: string[] = [];
    queue.frame(() => {
      ran.push("a");
      queue.frame(() => ran.push("b"));
    });

    // Act
    queue.run();

    // Assert
    expect(ran).toEqual(["a"]);
    expect(queue.pending()).toBe(1);
  });

  it("drains frames until none is waiting", () => {
    // Arrange
    const queue = frameQueue();
    const ran: string[] = [];
    queue.frame(() => {
      ran.push("a");
      queue.frame(() => ran.push("b"));
    });

    // Act
    queue.drain();

    // Assert
    expect(ran).toEqual(["a", "b"]);
    expect(queue.pending()).toBe(0);
  });

  it("counts every callback ever scheduled, run or not", () => {
    // Arrange
    const queue = frameQueue();
    queue.frame(() => undefined);
    queue.run();

    // Act
    queue.frame(() => undefined);

    // Assert
    expect(queue.asked()).toBe(2);
  });
});

/** Every test source under DIR, recursively. */
function testSources(dir: string): string[] {
  return readdirSync(dir).flatMap((name) => {
    const path = join(dir, name);
    if (statSync(path).isDirectory()) return testSources(path);
    return path.endsWith(".ts") ? [path] : [];
  });
}

describe("the tests' frame seams", () => {
  it("all take frameQueue rather than a hand-rolled array of callbacks", () => {
    // Arrange
    const root = new URL(".", import.meta.url).pathname;
    const handRolled = /frames\s*(?:\.push\(|:\s*(?:Array<\(\) => void>|\(\(\) => void\)\[\]))/;

    // Act
    const offenders = testSources(root).filter((path) => handRolled.test(readFileSync(path, "utf8")));

    // Assert
    expect(offenders).toEqual([]);
  });
});
