// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { createJumpCollapse, type JumpCollapse } from "../../src/feed/jump-collapse.js";
import { fireIntersection, intersectionObservers } from "../intersection-observer.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** An entry a jump expanded: its row, its open state, and its collapses counted. */
interface Entry {
  row: HTMLElement;
  open: boolean;
  collapses: number;
  isExpanded(): boolean;
  collapse(): void;
}

function entry(box: HTMLElement, id: string): Entry {
  const row = document.createElement("article");
  row.setAttribute("data-feed-row", id);
  box.append(row);
  const e: Entry = {
    row,
    open: true,
    collapses: 0,
    isExpanded: () => e.open,
    collapse: () => {
      e.collapses += 1;
      e.open = false;
    },
  };
  return e;
}

/** A scroll box, the jump watches over it, and two entries in it. */
function watching(): { box: HTMLElement; jumps: JumpCollapse; a: Entry; b: Entry } {
  const box = document.createElement("div");
  document.body.replaceChildren(box);
  const jumps = createJumpCollapse(box);
  if (jumps === null) throw new Error("the jsdom setup installs an IntersectionObserver");
  return { box, jumps, a: entry(box, "a"), b: entry(box, "b") };
}

describe("createJumpCollapse: the guard", () => {
  const saved = (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver;
  afterEach(() => {
    (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver = saved;
  });

  it("is absent (null) when the environment ships no IntersectionObserver", () => {
    // Arrange
    delete (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver;
    // Act
    const jumps = createJumpCollapse(document.createElement("div"));
    // Assert
    expect(jumps).toBeNull();
  });
});

describe("createJumpCollapse", () => {
  it("collapses a jumped entry once it was seen and then left the view wholly", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    fireIntersection(w.a.row, true);
    // Act
    fireIntersection(w.a.row, false);
    // Assert
    expect(w.a.collapses).toBe(1);
  });

  it("keeps a jumped entry open while any part of it is in view", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    // Act
    fireIntersection(w.a.row, true);
    // Assert
    expect(w.a.collapses).toBe(0);
  });

  it("does not collapse a jumped entry reported out of view before it was ever seen", () => {
    // Arrange — the centering scroll has not landed yet.
    const w = watching();
    w.jumps.track(w.a);
    // Act
    fireIntersection(w.a.row, false);
    // Assert
    expect(w.a.collapses).toBe(0);
  });

  it("stops watching a jumped entry once it has collapsed", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    fireIntersection(w.a.row, true);
    // Act
    fireIntersection(w.a.row, false);
    // Assert
    expect(intersectionObservers().some((r) => r.targets.has(w.a.row))).toBe(false);
  });

  it("does not call collapse on an entry already closed when it leaves", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    fireIntersection(w.a.row, true);
    w.a.open = false;
    // Act
    fireIntersection(w.a.row, false);
    // Assert
    expect(w.a.collapses).toBe(0);
  });

  it("does not collapse an entry the reader took over by hand", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    fireIntersection(w.a.row, true);
    w.jumps.release(w.a.row);
    // Act — nothing observes it any more, so it can never leave.
    const observed = intersectionObservers().some((r) => r.targets.has(w.a.row));
    // Assert
    expect([observed, w.a.collapses]).toEqual([false, 0]);
  });

  it("keeps a first jump's watch when a second jump lands on another entry", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    fireIntersection(w.a.row, true);
    w.jumps.track(w.b);
    // Act
    fireIntersection(w.a.row, false);
    // Assert
    expect([w.a.collapses, w.b.collapses]).toEqual([1, 0]);
  });

  it("collapses the second jump's entry on its own departure only", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    w.jumps.track(w.b);
    fireIntersection(w.a.row, true);
    fireIntersection(w.b.row, true);
    // Act
    fireIntersection(w.b.row, false);
    // Assert
    expect([w.a.collapses, w.b.collapses]).toEqual([0, 1]);
  });

  it("does not collapse an entry removed or replaced (a page reload)", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    fireIntersection(w.a.row, true);
    w.a.row.remove();
    // Act
    fireIntersection(w.a.row, false);
    // Assert
    expect(w.a.collapses).toBe(0);
  });

  it("drops every watch on clear, so an entry collapsed elsewhere is never collapsed twice", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    w.jumps.track(w.b);
    // Act
    w.jumps.clear();
    // Assert
    expect(intersectionObservers().some((r) => r.targets.has(w.a.row) || r.targets.has(w.b.row))).toBe(false);
  });

  it("observes nothing once disposed", () => {
    // Arrange
    const w = watching();
    w.jumps.track(w.a);
    // Act
    w.jumps.dispose();
    // Assert
    expect(intersectionObservers().some((r) => r.targets.has(w.a.row))).toBe(false);
  });

  it("logs the collapse at info, naming the row", async () => {
    // Arrange
    const capture = captureLogRecords();
    const w = watching();
    w.jumps.track(w.a);
    fireIntersection(w.a.row, true);
    // Act
    fireIntersection(w.a.row, false);
    // Assert
    const record = await forwardedRecord(capture, "feed.jump-collapse");
    expect([record.level.case, record.context?.row]).toEqual(["info", "a"]);
  });

  it("logs a detached entry at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const w = watching();
    w.jumps.track(w.a);
    w.a.row.remove();
    // Act
    fireIntersection(w.a.row, false);
    // Assert
    const record = await forwardedRecord(capture, "feed.jump-collapse-detached");
    expect(record.context?.row).toBe("a");
  });

  it("logs a release at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const w = watching();
    w.jumps.track(w.a);
    // Act
    w.jumps.release(w.a.row);
    // Assert
    const record = await forwardedRecord(capture, "feed.jump-collapse-released");
    expect(record.context?.row).toBe("a");
  });
});
