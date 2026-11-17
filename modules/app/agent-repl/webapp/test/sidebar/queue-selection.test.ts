// @vitest-environment jsdom
/**
 * THE SELECTED WORKSPACE'S MERGES ARE HIGHLIGHTED IN THE MERGE QUEUE, SCROLLED
 * INTO ITS VIEW, AND SCROLLED BACK once the selection leaves it (owner
 * request, 2026-11-17). jsdom lays nothing out, so `queue` states the
 * geometry the engine would give: the Merge Queue's rows region VIEW lines
 * tall at the page's top, scrolling lines of ROW px each, clamped at its end.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { resetLoggingForTests } from "../../src/log.js";
import { createQueueSelectionScroll, markQueueSelection, QUEUE_CURRENT_CLASS } from "../../src/sidebar/queue-selection.js";
import { MERGE_QUEUE_SUBJECT_CLASS, MERGE_QUEUE_WAITING_CLASS } from "../../src/sidebar/merge-queue-view.js";
import { MERGE_QUEUE_SECTION_CLASS } from "../../src/sidebar/band-fit.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { boxRect } from "../dom-rect.js";
import { cascadedValue, inRail } from "../stylesheet.js";

const ROW = 20;

/** A laid-out Merge Queue of LINES waiting merges (credited to `w0`..), VIEW lines tall. */
function queue(lines: number, view: number) {
  const root = document.createElement("div");
  const section = document.createElement("div");
  section.className = `repo ${MERGE_QUEUE_SECTION_CLASS}`;
  const region = document.createElement("div");
  region.className = "rows";
  const body = document.createElement("div");
  body.className = "rows sb-merge-queue";
  region.appendChild(body);
  section.appendChild(region);
  root.appendChild(section);
  let count = lines;
  let top = 0;
  const end = (): number => Math.max(0, count * ROW - view * ROW);
  Object.defineProperty(region, "scrollTop", {
    configurable: true,
    get: () => top,
    set: (v: number) => {
      top = Math.max(0, Math.min(v, end()));
    },
  });
  Object.defineProperty(region, "clientHeight", { configurable: true, get: () => view * ROW });
  Object.defineProperty(region, "scrollHeight", { configurable: true, get: () => Math.max(count, view) * ROW });
  Object.defineProperty(region, "clientTop", { configurable: true, value: 0 });
  region.getBoundingClientRect = () => boxRect({ top: 0, height: view * ROW });
  const computed = window.getComputedStyle.bind(window);
  vi.spyOn(window, "getComputedStyle").mockImplementation((el, pseudo) =>
    el === region ? ({ scrollPaddingTop: "auto", scrollPaddingBottom: "auto" } as CSSStyleDeclaration) : computed(el, pseudo),
  );
  for (let i = 0; i < lines; i += 1) {
    const line = document.createElement("div");
    line.className = `${MERGE_QUEUE_WAITING_CLASS} ${MERGE_QUEUE_SUBJECT_CLASS}`;
    line.setAttribute("data-workspace", `w${i}`);
    line.getBoundingClientRect = () => boxRect({ top: i * ROW - top, height: ROW });
    body.appendChild(line);
  }
  const line = (id: string): HTMLElement => {
    const found = root.querySelector<HTMLElement>(`[data-workspace='${id}']`);
    if (found === null) throw new Error(`no line ${id}`);
    return found;
  };
  /** The queue lost every line from N on, as a landing does; the box clamps. */
  const shrink = (n: number): void => {
    for (const el of [...body.children].slice(n)) el.remove();
    count = n;
    top = Math.min(top, end());
  };
  return { root, section, region, line, shrink };
}

afterEach(() => {
  vi.restoreAllMocks();
  resetLoggingForTests();
});

describe("markQueueSelection", () => {
  it("highlights every line credited to the selected workspace and no other", () => {
    // Arrange: w1 is credited twice, as two async merges are.
    const q = queue(4, 4);
    q.line("w3").setAttribute("data-workspace", "w1");
    // Act
    const marked = markQueueSelection(q.root, "w1");
    // Assert
    const current = [...q.root.querySelectorAll(`.${QUEUE_CURRENT_CLASS}`)];
    expect([marked.length, current.length, current.every((el) => el.getAttribute("data-workspace") === "w1")]).toEqual([2, 2, true]);
  });

  it("moves the highlight off a line whose workspace is no longer selected", () => {
    // Arrange
    const q = queue(4, 4);
    markQueueSelection(q.root, "w1");
    // Act
    markQueueSelection(q.root, "w2");
    // Assert
    expect([...q.root.querySelectorAll(`.${QUEUE_CURRENT_CLASS}`)].map((el) => el.getAttribute("data-workspace"))).toEqual(["w2"]);
  });

  it("highlights nothing when no workspace is selected", () => {
    // Arrange
    const q = queue(4, 4);
    markQueueSelection(q.root, "w1");
    // Act
    const marked = markQueueSelection(q.root, null);
    // Assert
    expect([marked, q.root.querySelectorAll(`.${QUEUE_CURRENT_CLASS}`).length]).toEqual([[], 0]);
  });

  it("paints a highlighted line with the Open section's selected fill", () => {
    // Arrange
    const line = document.createElement("div");
    line.className = `${MERGE_QUEUE_WAITING_CLASS} ${MERGE_QUEUE_SUBJECT_CLASS} ${QUEUE_CURRENT_CLASS}`;
    // Act
    const fill = inRail(line, () => cascadedValue(line, "background-color"));
    // Assert
    expect(fill).toBe("var(--row-highlight-tint)");
  });
});

describe("createQueueSelectionScroll", () => {
  it("scrolls a selected line below the view up with the Open section's neighbors", () => {
    // Arrange: 20 lines, 5 in view, the reader at line 2.
    const q = queue(20, 5);
    q.region.scrollTop = 2 * ROW;
    // Act
    const outcome = createQueueSelectionScroll().follow(q.root, "w12");
    // Assert: w8, four lines above w12, is as far up as a 5-line view keeps.
    expect([outcome, q.region.scrollTop]).toEqual(["scrolled", 8 * ROW]);
  });

  it("answers visible for a selected line already in view, and owes nothing back when the selection leaves", () => {
    // Arrange
    const q = queue(20, 20);
    const follower = createQueueSelectionScroll();
    // Act
    const outcomes = [follower.follow(q.root, "w3"), follower.follow(q.root, "elsewhere")];
    // Assert
    expect([outcomes, q.region.scrollTop]).toEqual([["visible", "none"], 0]);
  });

  it("follows a selection once, so a reader's scroll away from it stands", () => {
    // Arrange
    const q = queue(20, 5);
    const follower = createQueueSelectionScroll();
    follower.follow(q.root, "w12");
    q.region.scrollTop = 0;
    // Act
    const outcome = follower.follow(q.root, "w12");
    // Assert
    expect([outcome, q.region.scrollTop]).toEqual(["followed", 0]);
  });

  it("scrolls back to where the queue stood once the selection leaves it", () => {
    // Arrange
    const q = queue(20, 5);
    q.region.scrollTop = 2 * ROW;
    const follower = createQueueSelectionScroll();
    follower.follow(q.root, "w12");
    // Act
    const outcome = follower.follow(q.root, "elsewhere");
    // Assert
    expect([outcome, q.region.scrollTop]).toEqual(["restored", 2 * ROW]);
  });

  it("scrolls back to where the queue stood before the first of several automatic scrolls", () => {
    // Arrange
    const q = queue(20, 5);
    q.region.scrollTop = 2 * ROW;
    const follower = createQueueSelectionScroll();
    follower.follow(q.root, "w12");
    follower.follow(q.root, "w19");
    // Act
    const outcome = follower.follow(q.root, null);
    // Assert
    expect([outcome, q.region.scrollTop]).toEqual(["restored", 2 * ROW]);
  });

  it("leaves a queue the reader scrolled since the automatic scroll where the reader put it", () => {
    // Arrange
    const q = queue(20, 5);
    q.region.scrollTop = 2 * ROW;
    const follower = createQueueSelectionScroll();
    follower.follow(q.root, "w12");
    q.region.scrollTop = 5 * ROW;
    // Act
    const outcome = follower.follow(q.root, "elsewhere");
    // Assert
    expect([outcome, q.region.scrollTop]).toEqual(["kept", 5 * ROW]);
  });

  it("scrolls back a queue whose lines shrank under it, as the selected merge landed", () => {
    // Arrange: the scroll leaves the queue at line 8; ten lines in a 5-line view clamp it to 5.
    const q = queue(20, 5);
    q.region.scrollTop = 2 * ROW;
    const follower = createQueueSelectionScroll();
    follower.follow(q.root, "w12");
    q.shrink(10);
    // Act
    const outcome = follower.follow(q.root, "w12");
    // Assert
    expect([outcome, q.region.scrollTop]).toEqual(["restored", 2 * ROW]);
  });

  it("owes nothing back once the queue is no longer drawn", () => {
    // Arrange
    const q = queue(20, 5);
    const follower = createQueueSelectionScroll();
    follower.follow(q.root, "w12");
    q.section.remove();
    // Act
    const outcome = follower.follow(q.root, "elsewhere");
    // Assert
    expect(outcome).toBe("none");
  });

  it("leaves a selected line that draws no box for the draw that lays it out", () => {
    // Arrange: w12's group is folded.
    const q = queue(20, 5);
    q.line("w12").getBoundingClientRect = () => boxRect({ top: 0, height: 0 });
    const follower = createQueueSelectionScroll();
    // Act
    const outcome = follower.follow(q.root, "w12");
    // Assert
    expect([outcome, q.region.scrollTop, q.line("w12").classList.contains(QUEUE_CURRENT_CLASS)]).toEqual(["unlaid", 0, true]);
  });

  it("states the scroll at INFO with the workspace and where the queue moved", async () => {
    // Arrange
    const capture = captureLogRecords();
    const q = queue(20, 5);
    // Act
    createQueueSelectionScroll().follow(q.root, "w12");
    // Assert
    const record = await forwardedRecord(capture, "sidebar.queue-selection-scroll");
    expect(record.level.case).toBe("info");
    expect(record.context).toMatchObject({ workspace: "w12", from: 0, to: 8 * ROW });
  });

  it("states the scroll back at INFO with where the queue moved from and to", async () => {
    // Arrange
    const capture = captureLogRecords();
    const q = queue(20, 5);
    const follower = createQueueSelectionScroll();
    follower.follow(q.root, "w12");
    // Act
    follower.follow(q.root, "elsewhere");
    // Assert
    const record = await forwardedRecord(capture, "sidebar.queue-selection-scroll.back");
    expect(record.level.case).toBe("info");
    expect(record.context).toMatchObject({ workspace: "elsewhere", outcome: "restored", from: 8 * ROW, to: 0 });
  });
});
