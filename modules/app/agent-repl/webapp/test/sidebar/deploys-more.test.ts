// @vitest-environment jsdom
/**
 * EACH RECENTLY MERGED GROUP SHOWS ITS TEN MOST RECENT MERGES (owner request,
 * 2026-11-08): scrolling to the group's end shows ten more at a time, and
 * scrolling back above a page lets it go.
 */
import { describe, expect, it } from "vitest";
import {
  DEPLOYS_GROUP_CLASS,
  DEPLOYS_GROUP_FOLDED_CLASS,
  DEPLOYS_REPOSITORY_ATTRIBUTE,
  DEPLOYS_SECTION_ATTRIBUTE,
} from "../../src/sidebar/deploys-fold.js";
import {
  DEPLOYS_MORE_ATTRIBUTE,
  DEPLOYS_MORE_CLASS,
  DEPLOYS_PAGE_CLASS,
  createDeploysMore,
  drawDeploysMoreSpinner,
  inView,
} from "../../src/sidebar/deploys-more.js";
import { WALK_LOADING_ATTRIBUTE, WALK_SPINNER_CLASS } from "../../src/feed/walk-control.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** Stub EL's rectangle as spanning TOP to BOTTOM. */
function placeAt(el: HTMLElement, top: number, bottom: number): void {
  el.getBoundingClientRect = () => new DOMRect(0, top, 0, bottom - top);
}

/** A Deploys group of REPOSITORY holding PAGES pages of older deploys and the loading spinner, in a Recently Merged viewport. */
function drawn(pages: number, repository = "r1"): { root: HTMLElement; region: HTMLElement; group: HTMLElement; pages: HTMLElement[]; spinner: HTMLElement } {
  const root = document.createElement("div");
  const section = document.createElement("div");
  section.className = "repo merged-section";
  const region = document.createElement("div");
  region.className = "rows";
  const group = document.createElement("div");
  group.className = DEPLOYS_GROUP_CLASS;
  group.setAttribute(DEPLOYS_SECTION_ATTRIBUTE, "recently-merged");
  group.setAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE, repository);
  const body = document.createElement("div");
  const drawnPages = Array.from({ length: pages }, () => {
    const page = document.createElement("div");
    page.className = DEPLOYS_PAGE_CLASS;
    return page;
  });
  body.append(...drawnPages, drawDeploysMoreSpinner());
  group.appendChild(body);
  region.appendChild(group);
  section.appendChild(region);
  root.appendChild(section);
  placeAt(region, 0, 100);
  return { root, region, group, pages: drawnPages, spinner: body.querySelector<HTMLElement>(`.${DEPLOYS_MORE_CLASS}`)! };
}

/** Each of PAGES' hidden flags. */
const hiddenFlags = (pages: readonly HTMLElement[]): boolean[] => pages.map((page) => page.hidden);


const noop = (): void => undefined;

describe("inView", () => {
  it.each([
    { name: "inside the viewport", top: 20, bottom: 40, want: true },
    { name: "straddling the viewport's foot", top: 90, bottom: 110, want: true },
    { name: "wholly below the viewport", top: 100, bottom: 120, want: false },
    { name: "wholly above the viewport", top: -20, bottom: 0, want: false },
    { name: "drawn as nothing", top: 50, bottom: 50, want: false },
  ])("answers $want for an element $name", ({ top, bottom, want }) => {
    // Arrange
    const region = document.createElement("div");
    placeAt(region, 0, 100);
    const el = document.createElement("div");
    placeAt(el, top, bottom);
    // Act + Assert
    expect(inView(el, region)).toBe(want);
  });
});

describe("drawDeploysMoreSpinner", () => {
  it("draws the feed's loading spinner, turning", () => {
    // Act
    const spinner = drawDeploysMoreSpinner();
    // Assert
    expect([
      spinner.classList.contains(DEPLOYS_MORE_CLASS),
      spinner.hasAttribute(DEPLOYS_MORE_ATTRIBUTE),
      spinner.hasAttribute(WALK_LOADING_ATTRIBUTE),
      spinner.querySelector(`.${WALK_SPINNER_CLASS}`) !== null,
    ]).toEqual([true, true, true, true]);
  });
});

describe("createDeploysMore", () => {
  it("paints every page of a group hidden until its end is scrolled to", () => {
    const { root, group, pages } = drawn(2);
    createDeploysMore(root, noop).paint(group);
    expect(hiddenFlags(pages)).toEqual([true, true]);
  });

  it.each([
    { name: "shows the first page on the first ask", asks: 1, want: [false, true] },
    { name: "shows the next page on the second ask", asks: 2, want: [false, false] },
  ])("$name", ({ asks, want }) => {
    const { root, group, pages } = drawn(2);
    const more = createDeploysMore(root, noop);
    for (let i = 0; i < asks; i += 1) more.more(group);
    expect(hiddenFlags(pages)).toEqual(want);
  });

  it.each([
    { name: "shows the spinner while a page is hidden", asks: 1, want: false },
    { name: "hides the spinner once every page shows", asks: 2, want: true },
  ])("$name", ({ asks, want }) => {
    const { root, group, spinner } = drawn(2);
    const more = createDeploysMore(root, noop);
    for (let i = 0; i < asks; i += 1) more.more(group);
    expect(spinner.hidden).toBe(want);
  });

  it("hides the last page shown on less", () => {
    const { root, group, pages } = drawn(2);
    const more = createDeploysMore(root, noop);
    more.more(group);
    more.more(group);
    more.less(group);
    expect(hiddenFlags(pages)).toEqual([false, true]);
  });

  it("paints a group drawn again with the pages its repository had", () => {
    const first = drawn(2);
    const more = createDeploysMore(first.root, noop);
    more.more(first.group);
    const again = drawn(2);
    more.paint(again.group);
    expect(hiddenFlags(again.pages)).toEqual([false, true]);
  });

  it("tells the bands to fit again on every change", () => {
    const { root, group } = drawn(1);
    let changes = 0;
    const more = createDeploysMore(root, () => {
      changes += 1;
    });
    more.more(group);
    more.less(group);
    expect(changes).toBe(2);
  });

  it("logs a page shown with the repository and the pages shown", async () => {
    const { root, group } = drawn(1);
    const capture = captureLogRecords("debug");
    createDeploysMore(root, noop).more(group);
    const record = await forwardedRecord(capture, "sidebar.deploys-more");
    expect([record.level.case, record.context]).toMatchObject(["debug", { repository: "r1", pages: 1 }]);
  });

  describe("check", () => {
    it("shows a page while the spinner is in view", () => {
      const { root, group, pages, spinner } = drawn(2);
      placeAt(spinner, 80, 90);
      placeAt(pages[0], 0, 80);
      const more = createDeploysMore(root, noop);
      more.check();
      // Static stubs keep the spinner in view, so every page loads.
      expect(hiddenFlags(pages)).toEqual([false, false]);
      void group;
    });

    it("shows nothing while the spinner is below the viewport", () => {
      const { root, group, pages, spinner } = drawn(2);
      placeAt(spinner, 150, 160);
      const more = createDeploysMore(root, noop);
      more.paint(group);
      more.check();
      expect(hiddenFlags(pages)).toEqual([true, true]);
    });

    it("lets the last page go once it lies wholly below the viewport", () => {
      const { root, group, pages, spinner } = drawn(2);
      const more = createDeploysMore(root, noop);
      more.more(group);
      more.more(group);
      placeAt(pages[0], 50, 90);
      placeAt(pages[1], 100, 140);
      placeAt(spinner, 140, 150);
      spinner.hidden = false;
      more.check();
      expect(hiddenFlags(pages)).toEqual([false, true]);
    });

    it("keeps a page that still shows a sliver", () => {
      const { root, group, pages, spinner } = drawn(1);
      const more = createDeploysMore(root, noop);
      more.more(group);
      placeAt(pages[0], 90, 130);
      placeAt(spinner, 130, 140);
      spinner.hidden = false;
      more.check();
      expect(hiddenFlags(pages)).toEqual([false]);
    });

    it("lets every page of a folded group go", () => {
      const { root, group, pages } = drawn(2);
      const more = createDeploysMore(root, noop);
      more.more(group);
      more.more(group);
      group.classList.add(DEPLOYS_GROUP_FOLDED_CLASS);
      more.check();
      expect(hiddenFlags(pages)).toEqual([true, true]);
    });

    it("lets go of the Recently Merged group's pages, never the same repository's Merge Queue group's", () => {
      const { root, group, pages, spinner } = drawn(1);
      const queueGroup = document.createElement("div");
      queueGroup.className = DEPLOYS_GROUP_CLASS;
      queueGroup.setAttribute(DEPLOYS_SECTION_ATTRIBUTE, "merge-queue");
      queueGroup.setAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE, "r1");
      root.prepend(queueGroup);
      const more = createDeploysMore(root, noop);
      more.more(group);
      placeAt(pages[0], 120, 140);
      placeAt(spinner, 140, 150);
      spinner.hidden = false;
      more.check();
      expect(hiddenFlags(pages)).toEqual([true]);
    });

    it("refuses a group hanging in no Recently Merged viewport, logging why", async () => {
      const { group } = drawn(1);
      const root = document.createElement("div");
      root.appendChild(group);
      const capture = captureLogRecords("error");
      const act = (): void => createDeploysMore(root, noop).check();
      expect(act).toThrow(/hangs in no Recently Merged viewport/);
      const record = await forwardedRecord(capture, "sidebar.deploys-more.malformed");
      expect(record.context).toMatchObject({ repository: "r1" });
    });
  });

  it("refuses a group that names no repository, logging why", async () => {
    const { root, group } = drawn(1);
    group.removeAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE);
    const capture = captureLogRecords("error");
    const act = (): void => createDeploysMore(root, noop).more(group);
    expect(act).toThrow(/carries no data-repository/);
    const record = await forwardedRecord(capture, "sidebar.deploys-more.malformed");
    expect(record.context).toMatchObject({ attribute: DEPLOYS_REPOSITORY_ATTRIBUTE, found: null });
  });
});
