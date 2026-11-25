/** Every webapp listener set is a createListeners (src/listeners.ts), never a hand-kept set of functions. */
import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

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
