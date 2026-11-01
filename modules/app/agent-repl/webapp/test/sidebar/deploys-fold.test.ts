// @vitest-environment jsdom
/**
 * THE REPOSITORY GROUPS' FOLDS (owner requests, 2026-10-31 and 2026-11-01):
 * each Merge Queue and Recently Merged group open until a click folds it,
 * the click lasting until its queue changes or the user is done with the
 * rail, and a repository's two groups folding apart.
 */
import { describe, expect, it } from "vitest";
import {
  DEPLOYS_GROUP_CLASS,
  DEPLOYS_GROUP_FOLDED_CLASS,
  DEPLOYS_LINE_ATTRIBUTE,
  DEPLOYS_REPOSITORY_ATTRIBUTE,
  DEPLOYS_SECTION_ATTRIBUTE,
  RECENTLY_MERGED_GROUP_SELECTOR,
  createDeploysFold,
  deploysGroupRepository,
  deploysGroupSection,
  foldKeyOf,
  installDeploysFoldReset,
  type DeploysGroupSection,
} from "../../src/sidebar/deploys-fold.js";
import { createFocusLeft, type FocusLeftSource } from "../../src/focus-left.js";
import { createPresence } from "../../src/presence.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import foldSource from "../../src/sidebar/deploys-fold.ts?raw";
import moreSource from "../../src/sidebar/deploys-more.ts?raw";

/**
 * A group of REPOSITORY in SECTION: a Merge Queue group stating LINE when one
 * is given, a Recently Merged group unless said.
 */
function group(repository: string, line: string | null = null, section: DeploysGroupSection = line === null ? "recently-merged" : "merge-queue"): HTMLElement {
  const el = document.createElement("div");
  el.className = DEPLOYS_GROUP_CLASS;
  el.setAttribute(DEPLOYS_SECTION_ATTRIBUTE, section);
  el.setAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE, repository);
  if (line !== null) el.setAttribute(DEPLOYS_LINE_ATTRIBUTE, line);
  return el;
}

/** A rail holding GROUPS. */
function rail(...groups: HTMLElement[]): HTMLElement {
  const root = document.createElement("div");
  root.append(...groups);
  return root;
}

/** The page's focus-left signal, fired by hand through its own writer. */
function manualFocus(): FocusLeftSource {
  return createFocusLeft(window, createPresence(document, true));
}

describe("foldKeyOf", () => {
  it.each([
    { name: "keys a Merge Queue group by its section and repository", g: () => group("r1", "w1"), want: "merge-queue:r1" },
    { name: "keys a Recently Merged group by its section and repository", g: () => group("r1"), want: "recently-merged:r1" },
  ])("$name", ({ g, want }) => {
    expect(foldKeyOf(g())).toBe(want);
  });
});

describe("createDeploysFold", () => {
  it.each([
    { name: "paints a Recently Merged group open", line: null },
    { name: "paints a Merge Queue group open", line: "w1" },
  ])("$name", ({ line }) => {
    const g = group("r1", line);
    createDeploysFold(rail(g), () => undefined).paint(g);
    expect(g.classList.contains(DEPLOYS_GROUP_FOLDED_CLASS)).toBe(false);
  });

  it.each([
    { name: "folds a group on the first toggle", toggles: 1, want: true },
    { name: "opens it again on the second toggle", toggles: 2, want: false },
  ])("$name, repainting the group", ({ toggles, want }) => {
    const g = group("r1");
    const folds = createDeploysFold(rail(g), () => undefined);
    for (let i = 0; i < toggles; i += 1) folds.toggle(g);
    expect([folds.folded(g), g.classList.contains(DEPLOYS_GROUP_FOLDED_CLASS)]).toEqual([want, want]);
  });

  it("keeps a choice for a group drawn again with the same line", () => {
    const folds = createDeploysFold(document, () => undefined);
    folds.toggle(group("r1", "w1"));
    expect(folds.folded(group("r1", "w1"))).toBe(true);
  });

  it.each([
    { name: "a merge joining the line", after: "w1 w2" },
    { name: "the queue draining", after: null },
  ])("drops a choice on $name, the group opening again", ({ after }) => {
    const folds = createDeploysFold(document, () => undefined);
    folds.toggle(group("r1", "w1"));
    expect(folds.folded(group("r1", after, "merge-queue"))).toBe(false);
  });

  it("keeps one repository's choice off another's group", () => {
    const folds = createDeploysFold(document, () => undefined);
    folds.toggle(group("r1"));
    expect(folds.folded(group("r2"))).toBe(false);
  });

  it("keeps a repository's queue group's choice off its merged group", () => {
    const folds = createDeploysFold(document, () => undefined);
    folds.toggle(group("r1", "w1"));
    expect([folds.folded(group("r1", "w1")), folds.folded(group("r1"))]).toEqual([true, false]);
  });

  it.each([
    { name: "drops every choice on a reset", reset: true, want: false },
    { name: "keeps the choices without one", reset: false, want: true },
  ])("$name", ({ reset, want }) => {
    const g = group("r1");
    const folds = createDeploysFold(rail(g), () => undefined);
    folds.toggle(g);
    if (reset) folds.reset("the focus left");
    expect([folds.folded(g), g.classList.contains(DEPLOYS_GROUP_FOLDED_CLASS)]).toEqual([want, want]);
  });

  it("tells its rail to refit on each change, and not on a reset with no choice standing", () => {
    const g = group("r1");
    let refits = 0;
    const folds = createDeploysFold(rail(g), () => (refits += 1));
    folds.reset("nothing to reset");
    folds.toggle(g);
    folds.reset("the focus left");
    expect(refits).toBe(2);
  });

  it("refuses a group that names no repository", () => {
    const folds = createDeploysFold(document, () => undefined);
    const nameless = group("r1");
    nameless.removeAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE);
    expect(() => folds.toggle(nameless)).toThrow(/carries no data-repository/);
  });

  it("refuses a group that names no section", () => {
    const folds = createDeploysFold(document, () => undefined);
    const sectionless = group("r1");
    sectionless.removeAttribute(DEPLOYS_SECTION_ATTRIBUTE);
    expect(() => folds.toggle(sectionless)).toThrow(/carries no data-group-section/);
  });

  it("records a toggle with its group and what caused it, at debug", async () => {
    const capture = captureLogRecords("debug");
    createDeploysFold(document, () => undefined).toggle(group("r1"));
    const record = await forwardedRecord(capture, "sidebar.deploys-fold");
    expect([record.level.case, record.context]).toMatchObject([
      "debug",
      { group: "recently-merged:r1", folded: true, why: "its header was clicked" },
    ]);
  });

  it("records a reset with the groups it let go and why, at debug", async () => {
    const folds = createDeploysFold(document, () => undefined);
    folds.toggle(group("r1"));
    const capture = captureLogRecords("debug");
    folds.reset("the focus left");
    const record = await forwardedRecord(capture, "sidebar.deploys-fold");
    expect(record.context).toMatchObject({ groups: ["recently-merged:r1"], why: "the focus left" });
  });
});

describe("installDeploysFoldReset", () => {
  /** A rail host with an open row, and a Deploys group holding a merged row and a queue head. */
  function host(): { el: HTMLElement; g: HTMLElement } {
    const el = document.createElement("div");
    el.innerHTML =
      '<div data-roster-row="open"><span class="name">open</span></div>' +
      `<div class="${DEPLOYS_GROUP_CLASS}" ${DEPLOYS_SECTION_ATTRIBUTE}="recently-merged" ${DEPLOYS_REPOSITORY_ATTRIBUTE}="r1">` +
      '<div class="sb-mq-head"><span class="sb-mq-name">head</span></div>' +
      '<div data-roster-row="merged"><span class="name">merged</span></div></div>' +
      '<div class="sb-empty"></div>';
    document.body.replaceChildren(el);
    return { el, g: el.querySelector<HTMLElement>(`.${DEPLOYS_GROUP_CLASS}`)! };
  }

  it.each([
    { name: "resets on a click on an open row", target: '[data-roster-row="open"] .name', want: false },
    { name: "keeps the choice on a click on a merge in a group's queue", target: ".sb-mq-name", want: true },
    { name: "keeps the choice on a click on a group's merged row", target: '[data-roster-row="merged"] .name', want: true },
    { name: "keeps the choice on a click on no workspace", target: ".sb-empty", want: true },
  ])("$name", ({ target, want }) => {
    // Arrange
    const { el, g } = host();
    const folds = createDeploysFold(el, () => undefined);
    folds.toggle(g);
    installDeploysFoldReset(el, folds, manualFocus());
    // Act
    (el.querySelector(target) as HTMLElement).click();
    // Assert
    expect(folds.folded(g)).toBe(want);
  });

  it("resets when the focus leaves the page", () => {
    const { el, g } = host();
    const focus = manualFocus();
    const folds = createDeploysFold(el, () => undefined);
    folds.toggle(g);
    installDeploysFoldReset(el, folds, focus);
    focus.left("the page lost focus");
    expect(folds.folded(g)).toBe(false);
  });

  it("resets on a click a row's own handler stops, since it listens first", () => {
    const { el, g } = host();
    el.querySelector('[data-roster-row="open"]')!.addEventListener("click", (event) => event.stopPropagation());
    const folds = createDeploysFold(el, () => undefined);
    folds.toggle(g);
    installDeploysFoldReset(el, folds, manualFocus());
    (el.querySelector('[data-roster-row="open"] .name') as HTMLElement).click();
    expect(folds.folded(g)).toBe(false);
  });

  it("hears nothing once uninstalled", () => {
    const { el, g } = host();
    const focus = manualFocus();
    const folds = createDeploysFold(el, () => undefined);
    folds.toggle(g);
    installDeploysFoldReset(el, folds, focus)();
    focus.left("the page lost focus");
    (el.querySelector('[data-roster-row="open"] .name') as HTMLElement).click();
    expect(folds.folded(g)).toBe(true);
  });
});

describe("deploysGroupSection", () => {
  it.each([
    { name: "answers a Merge Queue group's section", section: "merge-queue" as const },
    { name: "answers a Recently Merged group's section", section: "recently-merged" as const },
  ])("$name", ({ section }) => {
    expect(deploysGroupSection(group("r1", null, section), "op")).toBe(section);
  });

  it("refuses and logs a group naming another section under the caller's operation", async () => {
    const g = group("r1");
    g.setAttribute(DEPLOYS_SECTION_ATTRIBUTE, "deploys");
    const capture = captureLogRecords("error");
    expect(() => deploysGroupSection(g, "sidebar.test-op")).toThrow(/^sidebar\.test-op: a repository group carries no data-group-section$/);
    const record = await forwardedRecord(capture, "sidebar.test-op");
    expect(record.context).toMatchObject({ attribute: DEPLOYS_SECTION_ATTRIBUTE, found: "deploys" });
  });
});

describe("RECENTLY_MERGED_GROUP_SELECTOR", () => {
  it("matches a Recently Merged group and no Merge Queue group", () => {
    const root = rail(group("r1", "w1"), group("r1"));
    expect([...root.querySelectorAll<HTMLElement>(RECENTLY_MERGED_GROUP_SELECTOR)].map((g) => foldKeyOf(g))).toEqual(["recently-merged:r1"]);
  });
});

describe("deploysGroupRepository", () => {
  it("answers the repository a group names", () => {
    expect(deploysGroupRepository(group("r1"), "op")).toBe("r1");
  });

  it.each([
    { name: "a group naming no repository", value: null },
    { name: "a group naming an empty repository", value: "" },
  ])("refuses $name under the caller's operation", ({ value }) => {
    // Arrange
    const g = group("r1");
    if (value === null) g.removeAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE);
    else g.setAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE, value);
    // Act + Assert
    expect(() => deploysGroupRepository(g, "sidebar.test-op")).toThrow(/^sidebar\.test-op: a repository group carries no data-repository$/);
  });

  it("logs a refusal under the caller's operation with what it found", async () => {
    // Arrange
    const g = group("r1");
    g.setAttribute(DEPLOYS_REPOSITORY_ATTRIBUTE, "");
    const capture = captureLogRecords("error");
    // Act
    expect(() => deploysGroupRepository(g, "sidebar.test-op")).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.test-op");
    expect(record.context).toMatchObject({ attribute: DEPLOYS_REPOSITORY_ATTRIBUTE, found: "" });
  });

  it.each([
    { name: "the fold", source: foldSource },
    { name: "the older deploys", source: moreSource },
  ])("is the one read of a group's repository for $name", ({ source }) => {
    const reads = source.match(/getAttribute\(DEPLOYS_REPOSITORY_ATTRIBUTE\)/g) ?? [];
    const calls = source.match(/deploysGroupRepository\(group, /g) ?? [];
    expect([reads.length, calls.length]).toEqual([source === foldSource ? 1 : 0, 1]);
  });
});
