/**
 * pointer-rest — what the pointer raised stays down until the user is back.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { createPointerRest } from "../src/pointer-rest.js";

describe("createPointerRest", () => {
  it("starts with no suspension", () => {
    // Act / Assert
    expect(createPointerRest().suspended()).toBe(false);
  });

  it("begins a suspension once, however often it is asked", () => {
    // Arrange
    const rest = createPointerRest();
    // Act
    const answers = [rest.suspend(), rest.suspend()];
    // Assert
    expect([answers, rest.suspended()]).toEqual([[true, false], true]);
  });

  it("ends no suspension on a move that is not one", () => {
    // Act / Assert
    expect(createPointerRest().moved(1, 1)).toBe(false);
  });

  it("stays suspended while the pointer is reported where it rested", () => {
    // Arrange
    const rest = createPointerRest();
    rest.moved(10, 10);
    rest.suspend();
    // Act
    const back = rest.moved(10, 10);
    // Assert
    expect([back, rest.suspended()]).toEqual([false, true]);
  });

  it("ends the suspension when the pointer moves from where it rested", () => {
    // Arrange
    const rest = createPointerRest();
    rest.moved(10, 10);
    rest.suspend();
    // Act
    const back = rest.moved(10, 11);
    // Assert
    expect([back, rest.suspended()]).toEqual([true, false]);
  });

  it("ends a suspension begun before any pointer was seen on the first move", () => {
    // Arrange
    const rest = createPointerRest();
    rest.suspend();
    // Act / Assert
    expect(rest.moved(10, 10)).toBe(true);
  });

  it("rests where the pointer was when the suspension began, not where it was before", () => {
    // Arrange
    const rest = createPointerRest();
    rest.moved(1, 1);
    rest.moved(5, 5);
    rest.suspend();
    // Act / Assert
    expect([rest.moved(5, 5), rest.moved(1, 1)]).toEqual([false, true]);
  });

  it("ends a suspension for the owner's own reason", () => {
    // Arrange
    const rest = createPointerRest();
    rest.suspend();
    // Act
    const answers = [rest.resume(), rest.resume()];
    // Assert
    expect([answers, rest.suspended()]).toEqual([[true, false], false]);
  });
});

describe("every surface that suspends on leaving", () => {
  it.each(["../src/sidebar/transients.ts", "../src/feed/response-usage.ts"])(
    "%s judges the user's return through the one latch, not its own",
    (path) => {
      // Arrange
      const source = readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8");
      // Act / Assert
      expect([source.includes("createPointerRest()"), /suspendedAt|lastPoint/.test(source)]).toEqual([true, false]);
    },
  );
});
