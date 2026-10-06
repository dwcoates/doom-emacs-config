// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { createDropdowns, toggleHiddenDropdown, type Dropdown } from "../../src/sidebar/dropdowns.js";
import { captureLogRecords } from "../log-capture.js";

/** A dropdown whose box and opener are fresh elements, recording its closes. */
function dropdown(init: { key?: string } = {}): Dropdown & { closes: number; opener: HTMLElement } {
  const element = document.createElement("div");
  element.appendChild(document.createElement("button"));
  const opener = document.createElement("button");
  const d = {
    kind: "test",
    element,
    opener,
    openers: [opener],
    closes: 0,
    close: () => {
      d.closes += 1;
    },
    ...(init.key === undefined ? {} : { key: init.key }),
  };
  return d;
}

describe("dismissOutside", () => {
  it("closes an open dropdown on a click outside it", () => {
    const dropdowns = createDropdowns();
    const d = dropdown();
    dropdowns.opened(d);
    dropdowns.dismissOutside(document.createElement("div"));
    expect(d.closes).toBe(1);
  });

  it("keeps it open on a click inside it", () => {
    const dropdowns = createDropdowns();
    const d = dropdown();
    dropdowns.opened(d);
    dropdowns.dismissOutside(d.element.firstChild);
    expect(d.closes).toBe(0);
  });

  it("keeps it open on a click on its own opener", () => {
    const dropdowns = createDropdowns();
    const d = dropdown();
    dropdowns.opened(d);
    dropdowns.dismissOutside(d.opener);
    expect(d.closes).toBe(0);
  });

  it("closes it only once, however many clicks follow", () => {
    const dropdowns = createDropdowns();
    const d = dropdown();
    dropdowns.opened(d);
    dropdowns.dismissOutside(null);
    dropdowns.dismissOutside(null);
    expect(d.closes).toBe(1);
  });

  it("closes nothing a release already let go", () => {
    const dropdowns = createDropdowns();
    const d = dropdown();
    dropdowns.opened(d);
    dropdowns.released(d.element);
    dropdowns.dismissOutside(null);
    expect(d.closes).toBe(0);
  });

  it("forgets the dropdowns of a roster a new draw replaced", () => {
    const dropdowns = createDropdowns();
    const d = dropdown();
    dropdowns.opened(d);
    dropdowns.beginDraw();
    dropdowns.dismissOutside(null);
    expect(d.closes).toBe(0);
  });

  it("keeps every copy of a dropdown when a click lands inside one", () => {
    const dropdowns = createDropdowns();
    const one = dropdown({ key: "row-detail:ws-1" });
    const two = dropdown({ key: "row-detail:ws-1" });
    dropdowns.opened(one);
    dropdowns.opened(two);
    dropdowns.dismissOutside(one.element.firstChild);
    expect([one.closes, two.closes]).toEqual([0, 0]);
  });

  it("logs nothing when nothing is open", async () => {
    const capture = captureLogRecords("debug");
    createDropdowns().dismissOutside(null);
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.filter((r) => r.operation.startsWith("sidebar.dropdowns"))).toEqual([]);
  });
});

describe("opened", () => {
  it("closes whichever other dropdown was open", () => {
    const dropdowns = createDropdowns();
    const first = dropdown();
    const second = dropdown();
    dropdowns.opened(first);
    dropdowns.opened(second);
    expect([first.closes, second.closes]).toEqual([1, 0]);
  });

  it("never closes another copy of the same dropdown", () => {
    const dropdowns = createDropdowns();
    const one = dropdown({ key: "row-detail:ws-1" });
    const two = dropdown({ key: "row-detail:ws-1" });
    dropdowns.opened(one);
    dropdowns.opened(two);
    expect(one.closes).toBe(0);
  });

  it("leaves the newly opened one as the one a later outside click closes", () => {
    const dropdowns = createDropdowns();
    const first = dropdown();
    const second = dropdown();
    dropdowns.opened(first);
    dropdowns.opened(second);
    dropdowns.dismissOutside(null);
    expect([first.closes, second.closes]).toEqual([1, 1]);
  });
});

describe("toggleHiddenDropdown", () => {
  it("reveals a hidden box and makes it the open dropdown", () => {
    const dropdowns = createDropdowns();
    const box = document.createElement("div");
    box.hidden = true;
    toggleHiddenDropdown(dropdowns, "test", box, []);
    dropdowns.dismissOutside(null);
    expect(box.hidden).toBe(true);
  });

  it("hides a revealed box and lets it go", () => {
    const dropdowns = createDropdowns();
    const box = document.createElement("div");
    box.hidden = true;
    toggleHiddenDropdown(dropdowns, "test", box, []);
    toggleHiddenDropdown(dropdowns, "test", box, []);
    box.hidden = false;
    dropdowns.dismissOutside(null);
    expect(box.hidden).toBe(false);
  });

  it("closes the other open box when it reveals one", () => {
    const dropdowns = createDropdowns();
    const [one, two] = [document.createElement("div"), document.createElement("div")];
    one.hidden = true;
    two.hidden = true;
    toggleHiddenDropdown(dropdowns, "test", one, []);
    toggleHiddenDropdown(dropdowns, "test", two, []);
    expect([one.hidden, two.hidden]).toEqual([true, false]);
  });
});
