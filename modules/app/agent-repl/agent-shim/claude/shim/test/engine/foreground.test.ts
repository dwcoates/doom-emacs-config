/**
 * test/engine/foreground.test.ts — the table behind DetachForeground's arms.
 *
 * One test per verdict, because each verdict is a different instruction to the
 * consumer: retry later, stop retrying, stop offering the affordance, or you
 * pointed at nothing.
 */
import { describe, expect, it } from "vitest";
import { ForegroundUnitTable, SETTLED_MEMORY } from "../../src/engine/foreground.js";

describe("ForegroundUnitTable", () => {
  it("calls an id it has never seen unknown", () => {
    // Arrange.
    const table = new ForegroundUnitTable();

    // Act, Assert.
    expect(table.verdict("nobody").kind).toBe("unknown");
  });

  it("calls a running bash detachable in kind", () => {
    // Arrange.
    const table = new ForegroundUnitTable();

    // Act.
    table.note("unit-1", "bash", false);

    // Assert.
    expect(table.verdict("unit-1").kind).toBe("live_detachable");
  });

  it("calls a running read not_detachable, which is about its KIND", () => {
    // Arrange.
    const table = new ForegroundUnitTable();

    // Act.
    table.note("unit-1", "read", false);

    // Assert.
    expect(table.verdict("unit-1").kind).toBe("not_detachable");
  });

  it("calls a finished call settled, whatever its kind", () => {
    // A call that finished is already_concluded: that is the fact that stops
    // the consumer retrying.
    // Arrange.
    const table = new ForegroundUnitTable();
    table.note("unit-1", "read", false);

    // Act.
    table.note("unit-1", "read", true);

    // Assert.
    expect(table.verdict("unit-1").kind).toBe("settled");
  });

  it("calls a SETTLED response not_detachable rather than settled", () => {
    // Prose settles per block inside a live turn, so "it already finished"
    // would be said of a turn that is still running.
    // Arrange.
    const table = new ForegroundUnitTable();

    // Act.
    table.note("unit-1", "response", true);

    // Assert.
    expect(table.verdict("unit-1").kind).toBe("not_detachable");
  });

  it("calls a settled thinking block not_detachable for the same reason", () => {
    // Arrange.
    const table = new ForegroundUnitTable();

    // Act.
    table.note("unit-1", "thinking", true);

    // Assert.
    expect(table.verdict("unit-1").kind).toBe("not_detachable");
  });

  it("ignores an empty activity id rather than tracking a unit nobody can address", () => {
    // Arrange.
    const table = new ForegroundUnitTable();

    // Act.
    table.note("", "bash", false);

    // Assert.
    expect(table.inFlightCount).toBe(0);
  });

  it("forgets the oldest settled unit once the bound is reached", () => {
    // The store is the history of the session; this must never become a second
    // one.
    // Arrange.
    const table = new ForegroundUnitTable();
    table.note("oldest", "bash", true);

    // Act.
    for (let index = 0; index < SETTLED_MEMORY; index++) {
      table.note(`unit-${String(index)}`, "bash", true);
    }

    // Assert.
    expect(table.verdict("oldest").kind).toBe("unknown");
  });

  it("keeps the newest settled unit when the bound evicts", () => {
    // Arrange.
    const table = new ForegroundUnitTable();

    // Act.
    for (let index = 0; index <= SETTLED_MEMORY; index++) {
      table.note(`unit-${String(index)}`, "bash", true);
    }

    // Assert.
    expect(table.verdict(`unit-${String(SETTLED_MEMORY)}`).kind).toBe("settled");
  });

  it("drops everything in flight when the session clears it", () => {
    // Arrange.
    const table = new ForegroundUnitTable();
    table.note("unit-1", "bash", false);

    // Act.
    table.clear();

    // Assert.
    expect(table.inFlightCount).toBe(0);
  });
});
