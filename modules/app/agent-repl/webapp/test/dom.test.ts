import { describe, expect, it } from "vitest";
import { ancestorMatching } from "../src/dom.js";

/** Fake ancestor-chain node: the shape ancestorMatching walks. */
interface FakeNode {
  name: string;
  parentElement: FakeNode | null;
  match: boolean;
}

function node(name: string, over: Partial<FakeNode> = {}): FakeNode {
  return { name, parentElement: null, match: false, ...over };
}

const matches = (n: FakeNode): boolean => n.match;

describe("ancestorMatching", () => {
  it("returns the start node itself when it matches", () => {
    // Arrange
    const stop = node("stop");
    const start = node("start", { parentElement: stop, match: true });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)?.name).toBe("start");
  });

  it("climbs to the nearest matching ancestor", () => {
    // Arrange
    const stop = node("stop");
    const hit = node("hit", { parentElement: stop, match: true });
    const start = node("start", { parentElement: hit });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)?.name).toBe("hit");
  });

  it("returns the innermost of two matching ancestors", () => {
    // Arrange
    const stop = node("stop");
    const outer = node("outer", { parentElement: stop, match: true });
    const inner = node("inner", { parentElement: outer, match: true });
    const start = node("start", { parentElement: inner });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)?.name).toBe("inner");
  });

  it("returns null when nothing below the stop node matches", () => {
    // Arrange
    const stop = node("stop", { match: true });
    const start = node("start", { parentElement: stop });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)).toBeNull();
  });

  it("never returns the stop node, even when it matches", () => {
    // Arrange
    const stop = node("stop", { match: true });
    // Act + Assert
    expect(ancestorMatching(stop, stop, matches)).toBeNull();
  });

  it("returns null for a walk with no start node", () => {
    // Arrange
    const stop = node("stop", { match: true });
    // Act + Assert
    expect(ancestorMatching(null, stop, matches)).toBeNull();
  });

  it("stops the walk at the stop node rather than running off the chain", () => {
    // Arrange — a matching node ABOVE the stop node is out of bounds.
    const above = node("above", { match: true });
    const stop = node("stop", { parentElement: above });
    const start = node("start", { parentElement: stop });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)).toBeNull();
  });
});
