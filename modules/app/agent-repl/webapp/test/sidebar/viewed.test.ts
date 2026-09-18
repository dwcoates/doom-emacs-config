// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { ViewedRegistry } from "../../src/sidebar/viewed.js";

/** One complete drawing pass over ROWS, answering the modes it resolved. */
function pass(
  registry: ViewedRegistry,
  rows: ReadonlyArray<{ id: string; arm: string; viewed: boolean }>,
): string[] {
  registry.beginPass();
  const modes = rows.map((r) => registry.modeFor(r.id, r.arm, r.viewed));
  registry.endPass();
  return modes;
}

describe("the display mode a row draws in", () => {
  it("is FULL for a row the daemon carries no marker for", () => {
    // Arrange.
    const registry = new ViewedRegistry();

    // Act.
    const [mode] = pass(registry, [{ id: "ws-1", arm: "ready", viewed: false }]);

    // Assert: absent is full.
    expect(mode).toBe("full");
  });

  it("is PARTIAL for a row the daemon carries the marker for", () => {
    // Arrange.
    const registry = new ViewedRegistry();

    // Act.
    const [mode] = pass(registry, [{ id: "ws-1", arm: "ready", viewed: true }]);

    // Assert: present is partial.
    expect(mode).toBe("partial");
  });

  it("stays PARTIAL across a push that restates the same status", () => {
    // Arrange: without this, nothing could ever hold the partial mode.
    const registry = new ViewedRegistry();
    pass(registry, [{ id: "ws-1", arm: "ready", viewed: true }]);

    // Act.
    const [mode] = pass(registry, [{ id: "ws-1", arm: "ready", viewed: true }]);

    // Assert.
    expect(mode).toBe("partial");
  });

  it("restores FULL the moment the row's status changes", () => {
    // Arrange: a row this page has already drawn PARTIAL.
    const registry = new ViewedRegistry();
    pass(registry, [{ id: "ws-1", arm: "ready", viewed: true }]);

    // Act: the status changed, and the marker has not been dropped yet.
    const [mode] = pass(registry, [{ id: "ws-1", arm: "thinking", viewed: true }]);

    // Assert: a status change wins over the marker, with no exceptions.
    expect(mode).toBe("full");
  });

  it("holds the restored FULL after the status change, marker and all", () => {
    // Arrange: the change already restored it once.
    const registry = new ViewedRegistry();
    pass(registry, [{ id: "ws-1", arm: "ready", viewed: true }]);
    pass(registry, [{ id: "ws-1", arm: "thinking", viewed: true }]);

    // Act: a later push restating the NEW arm, still carrying the marker.
    const [mode] = pass(registry, [{ id: "ws-1", arm: "thinking", viewed: true }]);

    // Assert: the page defers to the marker again once the arm is settled —
    // the restore is an edge, not a latch of its own.
    expect(mode).toBe("partial");
  });

  it("resolves one workspace the same way in both groupings of a pass", () => {
    // Arrange: both groupings are drawn on every push, so a row is drawn
    // twice and the two copies may never disagree.
    const registry = new ViewedRegistry();
    pass(registry, [{ id: "ws-1", arm: "ready", viewed: true }]);

    // Act.
    registry.beginPass();
    const first = registry.modeFor("ws-1", "thinking", true);
    const second = registry.modeFor("ws-1", "thinking", true);
    registry.endPass();

    // Assert.
    expect([first, second]).toEqual(["full", "full"]);
  });

  it("leaves a neighbour's mode alone when one row's status changes", () => {
    // Arrange.
    const registry = new ViewedRegistry();
    pass(registry, [
      { id: "ws-1", arm: "ready", viewed: true },
      { id: "ws-2", arm: "ready", viewed: true },
    ]);

    // Act.
    const modes = pass(registry, [
      { id: "ws-1", arm: "thinking", viewed: true },
      { id: "ws-2", arm: "ready", viewed: true },
    ]);

    // Assert: the reset is per row, never roster-wide.
    expect(modes).toEqual(["full", "partial"]);
  });

  it("treats a workspace that left and came back as a first sight", () => {
    // Arrange.
    const registry = new ViewedRegistry();
    pass(registry, [{ id: "ws-1", arm: "ready", viewed: true }]);
    pass(registry, []);

    // Act: it returns with a different arm than it left with.
    const [mode] = pass(registry, [{ id: "ws-1", arm: "thinking", viewed: true }]);

    // Assert: there is no arm it could have "changed" from, so the marker
    // stands.
    expect(mode).toBe("partial");
  });

  it("forgets every arm when the rail goes away", () => {
    // Arrange.
    const registry = new ViewedRegistry();
    pass(registry, [{ id: "ws-1", arm: "ready", viewed: true }]);

    // Act.
    registry.dispose();
    const [mode] = pass(registry, [{ id: "ws-1", arm: "thinking", viewed: true }]);

    // Assert: a disposed registry compares against nothing.
    expect(mode).toBe("partial");
  });
});
