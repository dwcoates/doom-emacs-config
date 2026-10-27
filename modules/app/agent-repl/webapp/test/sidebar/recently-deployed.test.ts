// @vitest-environment jsdom
/**
 * THE "RECENTLY DEPLOYED" EXPANSION (owner request, 2026-10-27): collapsed
 * until its heading is clicked, and collapsed again the moment the user is
 * done with it.
 */
import { describe, expect, it } from "vitest";
import {
  RECENTLY_DEPLOYED_COLLAPSED_ATTRIBUTE,
  RECENTLY_DEPLOYED_LABEL_CLASS,
  createRecentlyDeployed,
  installRecentlyDeployedCollapse,
} from "../../src/sidebar/recently-deployed.js";
import { createFocusLeft, type FocusLeftSource } from "../../src/focus-left.js";
import { createPresence } from "../../src/presence.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** A rail holding one heading with its chevron, and a recorder of every refit. */
function rail(): { root: HTMLElement; heading: HTMLElement; refits: number[] } {
  const root = document.createElement("div");
  root.innerHTML = `<div class="${RECENTLY_DEPLOYED_LABEL_CLASS}"><span class="tri"></span></div>`;
  return { root, heading: root.firstElementChild as HTMLElement, refits: [] };
}

/** The page's focus-left signal, fired by hand through its own writer. */
function manualFocus(): FocusLeftSource {
  return createFocusLeft(window, createPresence(document, true));
}

describe("createRecentlyDeployed", () => {
  it("starts collapsed", () => {
    expect(createRecentlyDeployed(document, () => undefined).expanded()).toBe(false);
  });

  it("paints a fresh heading collapsed, its chevron pointing right", () => {
    const { heading } = rail();
    createRecentlyDeployed(document, () => undefined).paint(heading);
    expect([heading.hasAttribute(RECENTLY_DEPLOYED_COLLAPSED_ATTRIBUTE), heading.querySelector(".tri")?.textContent]).toEqual([true, "▸"]);
  });

  it.each([
    { name: "expands on the first toggle", toggles: 1, want: [true, false, "▾"] },
    { name: "collapses on the second toggle", toggles: 2, want: [false, true, "▸"] },
  ])("$name, repainting every heading in its rail", ({ toggles, want }) => {
    const { root, heading } = rail();
    const recently = createRecentlyDeployed(root, () => undefined);
    for (let i = 0; i < toggles; i += 1) recently.toggle();
    expect([recently.expanded(), heading.hasAttribute(RECENTLY_DEPLOYED_COLLAPSED_ATTRIBUTE), heading.querySelector(".tri")?.textContent]).toEqual(want);
  });

  it("tells its rail to refit on each change, and not on a collapse of collapsed rows", () => {
    const { root } = rail();
    let refits = 0;
    const recently = createRecentlyDeployed(root, () => (refits += 1));
    recently.collapse("nothing to collapse");
    recently.toggle();
    recently.collapse("the focus left");
    expect(refits).toBe(2);
  });

  it("records each change with what caused it, at debug", async () => {
    const capture = captureLogRecords("debug");
    const recently = createRecentlyDeployed(document, () => undefined);
    recently.toggle();
    const record = await forwardedRecord(capture, "sidebar.recently-deployed");
    expect([record.level.case, record.context]).toMatchObject(["debug", { expanded: true, why: "its heading was clicked" }]);
  });
});

describe("installRecentlyDeployedCollapse", () => {
  /** A rail host with an open row, a queue head, and a merged group's row. */
  function host(): HTMLElement {
    const el = document.createElement("div");
    el.innerHTML =
      '<div data-roster-row="open"><span class="name">open</span></div>' +
      '<div class="sb-mq-head"><span class="sb-mq-name">head</span></div>' +
      '<div class="merged-group"><div data-roster-row="merged"><span class="name">merged</span></div></div>' +
      '<div class="sb-empty"></div>';
    document.body.replaceChildren(el);
    return el;
  }

  it.each([
    { name: "collapses on a click on an open row", target: '[data-roster-row="open"] .name', want: false },
    { name: "collapses on a click on a merge in the deploy queue", target: ".sb-mq-name", want: false },
    { name: "stays expanded on a click on a Recently Deployed row", target: '[data-roster-row="merged"] .name', want: true },
    { name: "stays expanded on a click on no workspace", target: ".sb-empty", want: true },
  ])("$name", ({ target, want }) => {
    // Arrange
    const el = host();
    const recently = createRecentlyDeployed(el, () => undefined);
    recently.toggle();
    installRecentlyDeployedCollapse(el, recently, manualFocus());
    // Act
    (el.querySelector(target) as HTMLElement).click();
    // Assert
    expect(recently.expanded()).toBe(want);
  });

  it("collapses when the focus leaves the page", () => {
    const el = host();
    const focus = manualFocus();
    const recently = createRecentlyDeployed(el, () => undefined);
    recently.toggle();
    installRecentlyDeployedCollapse(el, recently, focus);
    focus.left("the page lost focus");
    expect(recently.expanded()).toBe(false);
  });

  it("collapses on a click a row's own handler stops, since it listens first", () => {
    const el = host();
    el.querySelector('[data-roster-row="open"]')!.addEventListener("click", (event) => event.stopPropagation());
    const recently = createRecentlyDeployed(el, () => undefined);
    recently.toggle();
    installRecentlyDeployedCollapse(el, recently, manualFocus());
    (el.querySelector('[data-roster-row="open"] .name') as HTMLElement).click();
    expect(recently.expanded()).toBe(false);
  });

  it("hears nothing once uninstalled", () => {
    const el = host();
    const focus = manualFocus();
    const recently = createRecentlyDeployed(el, () => undefined);
    recently.toggle();
    installRecentlyDeployedCollapse(el, recently, focus)();
    focus.left("the page lost focus");
    (el.querySelector('[data-roster-row="open"] .name') as HTMLElement).click();
    expect(recently.expanded()).toBe(true);
  });
});
