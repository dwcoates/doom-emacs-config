// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { createSelectionVisibility } from "../../src/feed/selection-visibility.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { fireIntersection, intersectionObservers } from "../intersection-observer.js";
import { feedId } from "./harness.js";

/** A scroll box holding two rows, and a watch over it recording its reports. */
function watching() {
  const box = document.createElement("div");
  const r1 = document.createElement("article");
  const r2 = document.createElement("article");
  box.append(r1, r2);
  document.body.replaceChildren(box);
  const reported: string[] = [];
  const watch = createSelectionVisibility(box, (row) => reported.push(row.value));
  if (watch === null) throw new Error("the jsdom setup installs an IntersectionObserver");
  return { box, r1, r2, reported, watch };
}

describe("createSelectionVisibility: the guard", () => {
  const saved = (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver;
  afterEach(() => {
    (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver = saved;
  });

  it("is absent (null) when the environment ships no IntersectionObserver", () => {
    // Arrange
    const box = document.createElement("div");
    delete (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver;
    // Act
    const watch = createSelectionVisibility(box, () => undefined);
    // Assert
    expect(watch).toBeNull();
  });
});

describe("createSelectionVisibility", () => {
  it("roots the observer on the scroll box", () => {
    // Arrange / Act
    const w = watching();
    // Assert
    expect(intersectionObservers().some((r) => r.root === w.box)).toBe(true);
  });

  it("reports a selected row that was seen and then left the viewport", () => {
    // Arrange
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(w.reported).toEqual(["r1"]);
  });

  it("reports a row that keeps leaving the viewport only once", () => {
    // Arrange
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    fireIntersection(w.r1, false);
    fireIntersection(w.r1, true);
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(w.reported).toEqual(["r1"]);
  });

  it("does not report a selected row never seen in the viewport", () => {
    // Arrange — the first report comes before the centering scroll landed.
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(w.reported).toEqual([]);
  });

  it("does not report a row the selection has moved away from", () => {
    // Arrange
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    // Act
    w.watch.watch({ element: w.r2, id: feedId("r2") });
    // Assert — nothing observes the old row, so it can never be reported.
    expect(intersectionObservers().some((r) => r.targets.has(w.r1))).toBe(false);
  });

  it("starts the new row unseen when the selection moves", () => {
    // Arrange
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    w.watch.watch({ element: w.r2, id: feedId("r2") });
    // Act
    fireIntersection(w.r2, false);
    // Assert
    expect(w.reported).toEqual([]);
  });

  it("keeps a restated row's state, so it is still reported once", () => {
    // Arrange
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    fireIntersection(w.r1, false);
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(w.reported).toEqual(["r1"]);
  });

  it("does not report after the selection ended", () => {
    // Arrange
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    // Act
    w.watch.watch(null);
    // Assert — nothing observes the row any more.
    expect(intersectionObservers().some((r) => r.targets.has(w.r1))).toBe(false);
  });

  it("does not report a selected row that was detached rather than scrolled away", () => {
    // Arrange
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    w.r1.remove();
    // Act
    fireIntersection(w.r1, false);
    // Assert
    expect(w.reported).toEqual([]);
  });

  it("logs a detached selected row at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    w.r1.remove();
    // Act
    fireIntersection(w.r1, false);
    // Assert
    const record = await forwardedRecord(capture, "feed.selection-row-detached");
    expect(record.context?.row).toBe("r1");
  });

  it("logs the left-view report at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    fireIntersection(w.r1, true);
    // Act
    fireIntersection(w.r1, false);
    // Assert
    const record = await forwardedRecord(capture, "feed.selection-left-view");
    expect([record.level.case, record.context?.row]).toEqual(["info", "r1"]);
  });

  it("observes nothing once disposed", () => {
    // Arrange
    const w = watching();
    w.watch.watch({ element: w.r1, id: feedId("r1") });
    // Act
    w.watch.dispose();
    // Assert
    expect(intersectionObservers().some((r) => r.targets.has(w.r1))).toBe(false);
  });
});
