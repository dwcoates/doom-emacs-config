// @vitest-environment jsdom
/**
 * THE DEPLOYS GROUPS' FOLDS (owner request, 2026-10-31): each group open while
 * its queue is active and folded while idle, a click overriding that until
 * the queue changes or the user is done with the rail.
 */
import { describe, expect, it } from "vitest";
import {
  DEPLOYS_GROUP_CLASS,
  DEPLOYS_GROUP_FOLDED_CLASS,
  DEPLOYS_LINE_ATTRIBUTE,
  DEPLOYS_REPOSITORY_ATTRIBUTE,
  automaticFold,
  createDeploysFold,
  installDeploysFoldReset,
} from "../../src/sidebar/deploys-fold.js";
import { createFocusLeft, type FocusLeftSource } from "../../src/focus-left.js";
import { createPresence } from "../../src/presence.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** A group of REPOSITORY, queued with LINE when one is given and idle otherwise. */
function group(repository: string, line: string | null = null): HTMLElement {
  const el = document.createElement("div");
  el.className = DEPLOYS_GROUP_CLASS;
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

describe("automaticFold", () => {
  it.each([
    { name: "folds an idle group", line: null, want: true },
    { name: "opens a queued group", line: "w1", want: false },
  ])("$name", ({ line, want }) => {
    expect(automaticFold(group("r1", line))).toBe(want);
  });
});

describe("createDeploysFold", () => {
  it.each([
    { name: "paints an idle group folded", line: null, want: true },
    { name: "paints a queued group open", line: "w1", want: false },
  ])("$name", ({ line, want }) => {
    const g = group("r1", line);
    createDeploysFold(rail(g), () => undefined).paint(g);
    expect(g.classList.contains(DEPLOYS_GROUP_FOLDED_CLASS)).toBe(want);
  });

  it.each([
    { name: "opens an idle group on the first toggle", toggles: 1, want: false },
    { name: "folds it again on the second toggle", toggles: 2, want: true },
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
  ])("drops a choice on $name, the automatic fold winning", ({ after }) => {
    const folds = createDeploysFold(document, () => undefined);
    folds.toggle(group("r1", "w1"));
    const redrawn = group("r1", after);
    expect(folds.folded(redrawn)).toBe(automaticFold(redrawn));
  });

  it("keeps one repository's choice off another's group", () => {
    const folds = createDeploysFold(document, () => undefined);
    folds.toggle(group("r1"));
    expect(folds.folded(group("r2"))).toBe(true);
  });

  it.each([
    { name: "drops every choice on a reset", reset: true, want: true },
    { name: "keeps the choices without one", reset: false, want: false },
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
    const nameless = document.createElement("div");
    nameless.className = DEPLOYS_GROUP_CLASS;
    expect(() => folds.toggle(nameless)).toThrow(/carries no data-repository/);
  });

  it("records a toggle with its repository and what caused it, at debug", async () => {
    const capture = captureLogRecords("debug");
    createDeploysFold(document, () => undefined).toggle(group("r1"));
    const record = await forwardedRecord(capture, "sidebar.deploys-fold");
    expect([record.level.case, record.context]).toMatchObject([
      "debug",
      { repository: "r1", folded: false, why: "its header was clicked" },
    ]);
  });

  it("records a reset with the repositories it let go and why, at debug", async () => {
    const folds = createDeploysFold(document, () => undefined);
    folds.toggle(group("r1"));
    const capture = captureLogRecords("debug");
    folds.reset("the focus left");
    const record = await forwardedRecord(capture, "sidebar.deploys-fold");
    expect(record.context).toMatchObject({ repositories: ["r1"], why: "the focus left" });
  });
});

describe("installDeploysFoldReset", () => {
  /** A rail host with an open row, and a Deploys group holding a merged row and a queue head. */
  function host(): { el: HTMLElement; g: HTMLElement } {
    const el = document.createElement("div");
    el.innerHTML =
      '<div data-roster-row="open"><span class="name">open</span></div>' +
      `<div class="${DEPLOYS_GROUP_CLASS}" ${DEPLOYS_REPOSITORY_ATTRIBUTE}="r1">` +
      '<div class="sb-mq-head"><span class="sb-mq-name">head</span></div>' +
      '<div data-roster-row="merged"><span class="name">merged</span></div></div>' +
      '<div class="sb-empty"></div>';
    document.body.replaceChildren(el);
    return { el, g: el.querySelector<HTMLElement>(`.${DEPLOYS_GROUP_CLASS}`)! };
  }

  it.each([
    { name: "resets on a click on an open row", target: '[data-roster-row="open"] .name', want: true },
    { name: "keeps the choice on a click on a merge in a group's queue", target: ".sb-mq-name", want: false },
    { name: "keeps the choice on a click on a group's merged row", target: '[data-roster-row="merged"] .name', want: false },
    { name: "keeps the choice on a click on no workspace", target: ".sb-empty", want: false },
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
    expect(folds.folded(g)).toBe(true);
  });

  it("resets on a click a row's own handler stops, since it listens first", () => {
    const { el, g } = host();
    el.querySelector('[data-roster-row="open"]')!.addEventListener("click", (event) => event.stopPropagation());
    const folds = createDeploysFold(el, () => undefined);
    folds.toggle(g);
    installDeploysFoldReset(el, folds, manualFocus());
    (el.querySelector('[data-roster-row="open"] .name') as HTMLElement).click();
    expect(folds.folded(g)).toBe(true);
  });

  it("hears nothing once uninstalled", () => {
    const { el, g } = host();
    const focus = manualFocus();
    const folds = createDeploysFold(el, () => undefined);
    folds.toggle(g);
    installDeploysFoldReset(el, folds, focus)();
    focus.left("the page lost focus");
    (el.querySelector('[data-roster-row="open"] .name') as HTMLElement).click();
    expect(folds.folded(g)).toBe(false);
  });
});
