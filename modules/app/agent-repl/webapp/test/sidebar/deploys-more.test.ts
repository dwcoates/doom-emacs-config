// @vitest-environment jsdom
/**
 * EACH DEPLOYS GROUP SHOWS ITS TEN MOST RECENT DEPLOYS (owner request,
 * 2026-10-31): an arrow shows ten more at a time, and the pages it showed go
 * again once none of them has been in view for fifteen seconds.
 */
import { describe, expect, it } from "vitest";
import { DEPLOYS_GROUP_CLASS, DEPLOYS_REPOSITORY_ATTRIBUTE } from "../../src/sidebar/deploys-fold.js";
import {
  DEPLOYS_MORE_CLASS,
  DEPLOYS_MORE_GLYPH,
  DEPLOYS_MORE_LINGER_MS,
  DEPLOYS_PAGE_CLASS,
  createDeploysMore,
  drawDeploysMoreArrow,
  pagesInView,
} from "../../src/sidebar/deploys-more.js";
import type { Timers } from "../../src/timers.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { scrollable } from "./harness.js";

/** Timers that record each one armed, with its delay, and fire them by hand. */
function recordedTimers(): Timers & { armed: Map<number, { fn: () => void; ms: number }>; fire(): void } {
  let next = 1;
  const armed = new Map<number, { fn: () => void; ms: number }>();
  return {
    armed,
    setTimeout(fn, ms) {
      const handle = next++;
      armed.set(handle, { fn, ms });
      return handle;
    },
    clearTimeout(handle) {
      armed.delete(handle);
    },
    fire() {
      const due = [...armed.values()];
      armed.clear();
      for (const { fn } of due) fn();
    },
  };
}

/** Stub EL's rectangle as spanning TOP to BOTTOM. */
function placeAt(el: HTMLElement, top: number, bottom: number): void {
  el.getBoundingClientRect = () => new DOMRect(0, top, 0, bottom - top);
}

/** A Deploys group of REPOSITORY holding PAGES pages of older deploys and the arrow, in a Deploys viewport. */
function drawn(pages: number, repository = "r1"): { root: HTMLElement; region: HTMLElement; group: HTMLElement; pages: HTMLElement[] } {
  const root = document.createElement("div");
  const section = document.createElement("div");
  section.className = "repo merged-section";
  const region = document.createElement("div");
  region.className = "rows";
  const group = document.createElement("div");
  group.className = DEPLOYS_GROUP_CLASS;
  group.setAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE, repository);
  const body = document.createElement("div");
  const drawnPages = Array.from({ length: pages }, () => {
    const page = document.createElement("div");
    page.className = DEPLOYS_PAGE_CLASS;
    return page;
  });
  body.append(...drawnPages);
  group.appendChild(body);
  region.appendChild(group);
  section.appendChild(region);
  root.appendChild(section);
  placeAt(region, 0, 100);
  return { root, region, group, pages: drawnPages };
}

/** Each of PAGES' hidden flags. */
const hiddenFlags = (pages: readonly HTMLElement[]): boolean[] => pages.map((page) => page.hidden);

describe("pagesInView", () => {
  it.each([
    { name: "a page inside the viewport", top: 20, bottom: 40, want: true },
    { name: "a page straddling the viewport's foot", top: 90, bottom: 110, want: true },
    { name: "a page wholly below the viewport", top: 100, bottom: 120, want: false },
    { name: "a page wholly above the viewport", top: -20, bottom: 0, want: false },
    { name: "a page drawn as nothing", top: 50, bottom: 50, want: false },
  ])("answers $want for $name", ({ top, bottom, want }) => {
    // Arrange
    const region = document.createElement("div");
    placeAt(region, 0, 100);
    const page = document.createElement("div");
    placeAt(page, top, bottom);
    // Act + Assert
    expect(pagesInView([page], region)).toBe(want);
  });
});

describe("createDeploysMore", () => {
  it("paints every page of a group hidden until its arrow is clicked", () => {
    // Arrange
    const { root, group, pages } = drawn(2);
    // Act
    createDeploysMore(root, recordedTimers(), () => undefined).paint(group);
    // Assert
    expect(hiddenFlags(pages)).toEqual([true, true]);
  });

  it.each([
    { name: "shows the first page on the first click", clicks: 1, want: [false, true] },
    { name: "shows the next page on the second click", clicks: 2, want: [false, false] },
  ])("$name", ({ clicks, want }) => {
    // Arrange
    const { root, group, pages } = drawn(2);
    const more = createDeploysMore(root, recordedTimers(), () => undefined);
    // Act
    for (let i = 0; i < clicks; i += 1) more.more(group);
    // Assert
    expect(hiddenFlags(pages)).toEqual(want);
  });

  it.each([
    { name: "shows the arrow while a page is hidden", clicks: 1, want: false },
    { name: "hides the arrow once every page shows", clicks: 2, want: true },
  ])("$name", ({ clicks, want }) => {
    // Arrange
    const { root, group } = drawn(2);
    const more = createDeploysMore(root, recordedTimers(), () => undefined);
    const arrow = drawDeploysMoreArrow(group, more);
    group.appendChild(arrow);
    // Act
    for (let i = 0; i < clicks; i += 1) more.more(group);
    // Assert
    expect(arrow.hidden).toBe(want);
  });

  it("paints a group drawn again with the pages its repository had", () => {
    // Arrange
    const first = drawn(2);
    const more = createDeploysMore(document, recordedTimers(), () => undefined);
    more.more(first.group);
    const again = drawn(2);
    // Act
    more.paint(again.group);
    // Assert
    expect(hiddenFlags(again.pages)).toEqual([false, true]);
  });

  it("tells the bands to fit again on a click", () => {
    // Arrange
    const { root, group } = drawn(1);
    let changes = 0;
    const more = createDeploysMore(root, recordedTimers(), () => {
      changes += 1;
    });
    // Act
    more.more(group);
    // Assert
    expect(changes).toBe(1);
  });

  it("logs the click with the repository and the pages shown", async () => {
    // Arrange
    const { root, group } = drawn(1);
    const capture = captureLogRecords("debug");
    // Act
    createDeploysMore(root, recordedTimers(), () => undefined).more(group);
    // Assert
    const record = await forwardedRecord(capture, "sidebar.deploys-more");
    expect([record.level.case, record.context]).toMatchObject([
      "debug",
      { repository: "r1", pages: 1, why: "its arrow was clicked" },
    ]);
  });

  it("arms no linger while a page shown is in view", () => {
    // Arrange
    const { root, group, pages } = drawn(1);
    placeAt(pages[0], 20, 40);
    const timers = recordedTimers();
    const more = createDeploysMore(root, timers, () => undefined);
    // Act
    more.more(group);
    // Assert
    expect(timers.armed.size).toBe(0);
  });

  it("arms the fifteen-second linger once no page shown is in view", () => {
    // Arrange
    const { root, group, pages } = drawn(1);
    placeAt(pages[0], 120, 140);
    const timers = recordedTimers();
    const more = createDeploysMore(root, timers, () => undefined);
    // Act
    more.more(group);
    // Assert
    expect([...timers.armed.values()].map((t) => t.ms)).toEqual([DEPLOYS_MORE_LINGER_MS]);
  });

  it("arms one linger however often an out-of-view group is looked at", () => {
    // Arrange
    const { root, group, pages } = drawn(1);
    placeAt(pages[0], 120, 140);
    const timers = recordedTimers();
    const more = createDeploysMore(root, timers, () => undefined);
    more.more(group);
    // Act
    more.check();
    more.check();
    // Assert
    expect(timers.armed.size).toBe(1);
  });

  it("stops the linger when a page scrolls back into view", () => {
    // Arrange
    const { root, group, pages } = drawn(1);
    placeAt(pages[0], 120, 140);
    const timers = recordedTimers();
    const more = createDeploysMore(root, timers, () => undefined);
    more.more(group);
    placeAt(pages[0], 60, 80);
    // Act
    more.check();
    // Assert
    expect(timers.armed.size).toBe(0);
  });

  it("hides the pages again once the linger ends", () => {
    // Arrange
    const { root, group, pages } = drawn(1);
    placeAt(pages[0], 120, 140);
    const timers = recordedTimers();
    const more = createDeploysMore(root, timers, () => undefined);
    more.more(group);
    // Act
    timers.fire();
    // Assert
    expect(hiddenFlags(pages)).toEqual([true]);
  });

  it("draws the group's next redraw with no page once the linger ends", () => {
    // Arrange
    const first = drawn(1);
    placeAt(first.pages[0], 120, 140);
    const timers = recordedTimers();
    const more = createDeploysMore(first.root, timers, () => undefined);
    more.more(first.group);
    timers.fire();
    const again = drawn(1);
    // Act
    more.paint(again.group);
    // Assert
    expect(hiddenFlags(again.pages)).toEqual([true]);
  });

  it("tells the bands to fit again once the linger ends", () => {
    // Arrange
    const { root, group, pages } = drawn(1);
    placeAt(pages[0], 120, 140);
    const timers = recordedTimers();
    let changes = 0;
    const more = createDeploysMore(root, timers, () => {
      changes += 1;
    });
    more.more(group);
    // Act
    timers.fire();
    // Assert
    expect(changes).toBe(2);
  });

  it("keeps what the viewport shows when the pages let go lie above it", () => {
    // Arrange
    const { root, region, group, pages } = drawn(1);
    placeAt(pages[0], -60, -20);
    const timers = recordedTimers();
    const more = createDeploysMore(root, timers, () => undefined);
    more.more(group);
    scrollable(region).scrollTop = 100;
    // Act
    timers.fire();
    // Assert
    expect(region.scrollTop).toBe(60);
  });

  it("logs letting the pages go with the repository and why", async () => {
    // Arrange
    const { root, group, pages } = drawn(1);
    placeAt(pages[0], 120, 140);
    const timers = recordedTimers();
    createDeploysMore(root, timers, () => undefined).more(group);
    const capture = captureLogRecords("debug");
    // Act
    timers.fire();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.deploys-more");
    expect(record.context).toMatchObject({ repository: "r1", why: `none was in view for ${DEPLOYS_MORE_LINGER_MS}ms`, drawn: true });
  });

  it("stops every linger when disposed", () => {
    // Arrange
    const { root, group, pages } = drawn(1);
    placeAt(pages[0], 120, 140);
    const timers = recordedTimers();
    const more = createDeploysMore(root, timers, () => undefined);
    more.more(group);
    // Act
    more.dispose();
    // Assert
    expect(timers.armed.size).toBe(0);
  });

  it("refuses a group that names no repository, logging why", async () => {
    // Arrange
    const { root, group } = drawn(1);
    group.removeAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE);
    const capture = captureLogRecords("error");
    // Act
    const act = (): void => createDeploysMore(root, recordedTimers(), () => undefined).more(group);
    // Assert
    expect(act).toThrow(/carries no data-repository/);
    const record = await forwardedRecord(capture, "sidebar.deploys-more.malformed");
    expect(record.context).toMatchObject({ attribute: DEPLOYS_REPOSITORY_ATTRIBUTE, found: null });
  });

  it("refuses to look at a group hanging in no Deploys viewport, logging why", async () => {
    // Arrange
    const { group } = drawn(1);
    const root = document.createElement("div");
    root.appendChild(group);
    const more = createDeploysMore(root, recordedTimers(), () => undefined);
    const capture = captureLogRecords("error");
    // Act
    const act = (): void => more.more(group);
    // Assert
    expect(act).toThrow(/hangs in no Deploys viewport/);
    const record = await forwardedRecord(capture, "sidebar.deploys-more.malformed");
    expect(record.context).toMatchObject({ repository: "r1" });
  });
});

describe("drawDeploysMoreArrow", () => {
  it("draws the small downward arrow", () => {
    // Arrange
    const { root, group } = drawn(1);
    // Act
    const arrow = drawDeploysMoreArrow(group, createDeploysMore(root, recordedTimers(), () => undefined));
    // Assert
    expect([arrow.className, arrow.textContent]).toEqual([DEPLOYS_MORE_CLASS, DEPLOYS_MORE_GLYPH]);
  });

  it("shows the group's next page on a click", () => {
    // Arrange
    const { root, group, pages } = drawn(1);
    const arrow = drawDeploysMoreArrow(group, createDeploysMore(root, recordedTimers(), () => undefined));
    // Act
    arrow.click();
    // Assert
    expect(hiddenFlags(pages)).toEqual([false]);
  });
});
