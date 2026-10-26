// The one subscribe-and-notify every webapp module keeps its listeners in.
// See src/listeners.ts.
import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

import { createListeners } from "../src/listeners.js";
import { filesUnder } from "./files-under.js";

describe("createListeners", () => {
  it("tells every listener the arguments, in the order they subscribed", () => {
    // Arrange
    const listeners = createListeners<[string, number]>();
    const told: string[] = [];
    listeners.add((a, n) => told.push(`first ${a} ${n}`));
    listeners.add((a, n) => told.push(`second ${a} ${n}`));

    // Act
    listeners.notify("x", 1);

    // Assert
    expect(told).toEqual(["first x 1", "second x 1"]);
  });

  it("stops telling a listener that unsubscribed", () => {
    // Arrange
    const listeners = createListeners();
    let told = 0;
    const unsubscribe = listeners.add(() => (told += 1));
    unsubscribe();

    // Act
    listeners.notify();

    // Assert
    expect(told).toBe(0);
  });

  it("answers from an unsubscribe whether the listener was still subscribed", () => {
    // Arrange
    const listeners = createListeners();
    const unsubscribe = listeners.add(() => undefined);

    // Act
    const answers = [unsubscribe(), unsubscribe()];

    // Assert
    expect(answers).toEqual([true, false]);
  });

  it("tells every listener even when one unsubscribes another as it runs", () => {
    // Arrange
    const listeners = createListeners();
    const told: string[] = [];
    let unsubscribeSecond = (): boolean => false;
    listeners.add(() => {
      told.push("first");
      unsubscribeSecond();
    });
    unsubscribeSecond = listeners.add(() => told.push("second"));

    // Act
    listeners.notify();

    // Assert: the notify ran over the listeners as they stood when it began.
    expect(told).toEqual(["first", "second"]);
  });

  it("first tells a listener subscribed during a notify on the next one", () => {
    // Arrange
    const listeners = createListeners();
    const told: string[] = [];
    listeners.add(() => {
      told.push("first");
      if (told.length === 1) listeners.add(() => told.push("late"));
    });

    // Act
    listeners.notify();
    listeners.notify();

    // Assert
    expect(told).toEqual(["first", "first", "late"]);
  });

  it("counts the listeners subscribed", () => {
    // Arrange
    const listeners = createListeners();
    listeners.add(() => undefined);
    const unsubscribe = listeners.add(() => undefined);

    // Act
    unsubscribe();

    // Assert
    expect(listeners.size).toBe(1);
  });

  it("unsubscribes every listener at a clear", () => {
    // Arrange
    const listeners = createListeners();
    let told = 0;
    listeners.add(() => (told += 1));

    // Act
    listeners.clear();
    listeners.notify();

    // Assert
    expect([told, listeners.size]).toEqual([0, 0]);
  });
});

/** A set typed as a function, or as a named listener type: a listener set kept by hand. */
const HAND_KEPT = /new Set<\s*(\([^>]*=>\s*void|[A-Za-z]*(Listener|Subscriber|Callback)[A-Za-z]*)\s*>\(\)/;

describe("the webapp's listener sets", () => {
  it.each([
    ["a set of argument-less functions", "const listeners = new Set<() => void>();"],
    ["a set of functions taking arguments", "const subscribers = new Set<(nowMs: number) => void>();"],
    ["a set of a named listener type", "const listeners = new Set<PresenceListener>();"],
  ])("recognizes %s as hand-kept", (_name, line) => {
    expect(HAND_KEPT.test(line)).toBe(true);
  });

  it("does not take a set of values for a listener set", () => {
    expect(HAND_KEPT.test("const ids = new Set<string>();")).toBe(false);
  });

  it("are every one a createListeners, never a hand-kept set of functions", () => {
    // Arrange
    const srcRoot = fileURLToPath(new URL("../src", import.meta.url));
    const own = join(srcRoot, "listeners.ts");

    // Act
    const offenders = filesUnder(srcRoot)
      .filter((file) => file !== own)
      .filter((file) => HAND_KEPT.test(readFileSync(file, "utf8")))
      .map((file) => relative(srcRoot, file));

    // Assert
    expect(offenders).toEqual([]);
  });
});
