// @vitest-environment jsdom
/**
 * The webapp tests' event waits: a recorded call and a DOM state.
 */
import { describe, expect, it } from "vitest";
import { awaitDom, recorded } from "./events.js";

describe("recorded", () => {
  it("resolves reached at once for a count already recorded", async () => {
    // Arrange
    const r = recorded<string>();
    r.record("a");

    // Act / Assert
    await expect(r.reached(1)).resolves.toBeUndefined();
  });

  it("resolves reached when the Nth call is recorded, not before", async () => {
    // Arrange
    const r = recorded<string>();
    let done = false;
    const waiting = r.reached(2).then(() => {
      done = true;
    });

    // Act
    r.record("a");
    await Promise.resolve();
    const afterOne = done;
    r.record("b");
    await waiting;

    // Assert
    expect([afterOne, done, r.calls]).toEqual([false, true, ["a", "b"]]);
  });
});

describe("recorded.seen", () => {
  it("resolves with a matching call already recorded", async () => {
    // Arrange
    const r = recorded<string>();
    r.record("a");

    // Act / Assert
    await expect(r.seen((c) => c === "a")).resolves.toBe("a");
  });

  it("resolves with the next matching call, passing over the rest", async () => {
    // Arrange
    const r = recorded<string>();
    const waiting = r.seen((c) => c === "b");

    // Act
    r.record("a");
    r.record("b");

    // Assert
    await expect(waiting).resolves.toBe("b");
  });

  it("waits on what is recorded after the calls are emptied", async () => {
    // Arrange
    const r = recorded<string>();
    r.record("b");
    r.calls.length = 0;
    let done = false;
    const waiting = r.seen((c) => c === "b").then(() => {
      done = true;
    });

    // Act
    await Promise.resolve();
    const before = done;
    r.record("b");
    await waiting;

    // Assert
    expect([before, done]).toEqual([false, true]);
  });
});

describe("awaitDom", () => {
  it("resolves at once when the state already holds", async () => {
    // Arrange
    const root = document.createElement("div");
    root.innerHTML = "<span>x</span>";

    // Act / Assert
    await expect(awaitDom(root, "the span", () => root.querySelector("span"))).resolves.toBe(root.querySelector("span"));
  });

  it("resolves on the mutation that brings the state", async () => {
    // Arrange
    const root = document.createElement("div");
    const waiting = awaitDom(root, "the span", () => root.querySelector("span"));

    // Act
    root.appendChild(document.createElement("span"));

    // Assert
    await expect(waiting).resolves.toBe(root.querySelector("span"));
  });

  it("rejects at its bound, naming what it waited for and the DOM", async () => {
    // Arrange
    const root = document.createElement("div");

    // Act
    const waiting = awaitDom(root, "the span", () => root.querySelector("span"), undefined, 1);

    // Assert
    await expect(waiting).rejects.toThrow("the span did not happen within 1ms; the DOM holds <div></div>");
  });
});
