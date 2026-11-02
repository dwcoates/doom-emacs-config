// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  DETAILS_ENTRY_CLASS,
  DETAILS_LABEL,
  DETAILS_PANEL_CLASS,
  FACT_HOMES,
  closeDetails,
  detailsEntry,
  detailsPanel,
  detailsPlacement,
  factHome,
  openDetails,
  placeDetailsPanel,
  requireFactHome,
  syncDetailsEntry,
  wireDetailsEntry,
} from "../../src/sidebar/detail-more.js";
import { clampBeside, REVEAL_MARGIN_PX, type Rect } from "../../src/topbar/clamp.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

const rect = (left: number, top: number, width: number, height: number): Rect => ({
  left,
  top,
  width,
  height,
  right: left + width,
  bottom: top + height,
});

const VIEWPORT = { width: 1000, height: 800 };

/** A row panel in the document, with one ordinary line. */
function panel(): HTMLElement {
  const detail = document.createElement("div");
  detail.className = "detail";
  detail.appendChild(document.createElement("dl"));
  document.body.appendChild(detail);
  return detail;
}

/** A panel whose details popup states one fact, its entry drawn. */
function panelWithDetails(): { detail: HTMLElement; entry: HTMLElement; sub: HTMLElement } {
  const detail = panel();
  const sub = detailsPanel(detail);
  sub.textContent = "agent 98 MB";
  syncDetailsEntry(detail);
  const entry = detailsEntry(detail);
  if (entry === null) throw new Error("no entry drawn");
  return { detail, entry, sub };
}

afterEach(() => {
  document.body.replaceChildren();
});

describe("the fact classification", () => {
  it.each([
    ["branch", "panel"],
    ["from", "panel"],
    ["commits", "panel"],
    ["summary", "panel"],
    ["children", "panel"],
    ["agent", "details"],
    ["page", "details"],
  ])("homes %s in the %s", (label, home) => {
    expect(factHome(label)).toBe(home);
  });

  it("classifies exactly the facts a row panel states", () => {
    expect(Object.keys(FACT_HOMES).sort()).toEqual(["agent", "branch", "children", "commits", "from", "page", "summary"]);
  });

  it("refuses a fact nobody classified", () => {
    expect(() => factHome("cpu")).toThrow(/'cpu' has no home/);
  });

  it("records the unclassified fact in the canonical log", async () => {
    // ARRANGE
    const capture = captureLogRecords("error");
    // ACT
    expect(() => factHome("cpu")).toThrow();
    // ASSERT
    const record = await forwardedRecord(capture, "sidebar.row.detail-fact-unclassified");
    expect(record.context).toMatchObject({ label: "cpu" });
  });

  it("lets a fact be drawn in its own home", () => {
    expect(() => requireFactHome("agent", "details")).not.toThrow();
  });

  it("refuses a fact drawn away from its home", () => {
    expect(() => requireFactHome("branch", "details")).toThrow(/belongs in the panel, not the details/);
  });

  it("records the misplaced fact in the canonical log", async () => {
    // ARRANGE
    const capture = captureLogRecords("error");
    // ACT
    expect(() => requireFactHome("branch", "details")).toThrow();
    // ASSERT
    const record = await forwardedRecord(capture, "sidebar.row.detail-fact-misplaced");
    expect(record.context).toMatchObject({ label: "branch", home: "panel", drawn_in: "details" });
  });
});

describe("the details entry", () => {
  it("is absent from a panel nothing asked a details popup of", () => {
    expect(detailsEntry(panel())).toBeNull();
  });

  it("is drawn last in the panel, wearing its label, with its popup inside it", () => {
    // ARRANGE
    const detail = panel();
    // ACT
    const sub = detailsPanel(detail);
    // ASSERT
    const entry = detail.lastElementChild as HTMLElement;
    expect([entry.className, entry.textContent, sub.parentElement, sub.className]).toEqual([
      DETAILS_ENTRY_CLASS,
      DETAILS_LABEL,
      entry,
      DETAILS_PANEL_CLASS,
    ]);
  });

  it("answers the same popup when asked again", () => {
    const detail = panel();
    expect(detailsPanel(detail)).toBe(detailsPanel(detail));
  });

  it("is hidden while its popup states no fact", () => {
    // ARRANGE
    const detail = panel();
    detailsPanel(detail);
    // ACT
    syncDetailsEntry(detail);
    // ASSERT
    expect(detailsEntry(detail)?.hidden).toBe(true);
  });

  it("is drawn once its popup states a fact", () => {
    expect(panelWithDetails().entry.hidden).toBe(false);
  });

  it("shuts and hides again when its popup is emptied", () => {
    // ARRANGE
    const { detail, entry, sub } = panelWithDetails();
    entry.classList.add("open");
    sub.replaceChildren();
    // ACT
    syncDetailsEntry(detail);
    // ASSERT
    expect([entry.hidden, entry.classList.contains("open")]).toEqual([true, false]);
  });

  it("refuses an entry drawn without its popup", () => {
    // ARRANGE
    const detail = panel();
    const entry = document.createElement("div");
    entry.className = DETAILS_ENTRY_CLASS;
    detail.appendChild(entry);
    // ACT / ASSERT
    expect(() => detailsPanel(detail)).toThrow(/has no popup/);
  });

  it("records an entry drawn without its popup in the canonical log", async () => {
    // ARRANGE
    const capture = captureLogRecords("error");
    const detail = panel();
    const entry = document.createElement("div");
    entry.className = DETAILS_ENTRY_CLASS;
    detail.appendChild(entry);
    // ACT
    expect(() => detailsPanel(detail)).toThrow();
    // ASSERT
    await forwardedRecord(capture, "sidebar.row.details-no-popup");
  });
});

describe("detailsPlacement", () => {
  it("meets the panel's right edge with no gap", () => {
    expect(detailsPlacement(rect(0, 100, 300, 200), rect(0, 260, 300, 20), rect(0, 0, 200, 80), VIEWPORT).left).toBe(300);
  });

  it("puts its top on the entry's line", () => {
    expect(detailsPlacement(rect(0, 100, 300, 200), rect(0, 260, 300, 20), rect(0, 0, 200, 80), VIEWPORT).top).toBe(260);
  });

  it("stands to the panel as the panel stands to its row: clampBeside against the panel's right edge on the entry's line", () => {
    // ARRANGE
    const panelBox = rect(0, 600, 300, 180);
    const entryBox = rect(0, 750, 300, 20);
    const sub = rect(0, 0, 200, 120);
    // ACT
    const placement = detailsPlacement(panelBox, entryBox, sub, VIEWPORT);
    // ASSERT
    expect(placement).toEqual(clampBeside(rect(0, 750, 300, 20), sub, VIEWPORT));
  });

  it("slides up only as far as the window's bottom edge demands", () => {
    expect(detailsPlacement(rect(0, 600, 300, 180), rect(0, 750, 300, 20), rect(0, 0, 200, 120), VIEWPORT).top).toBe(
      800 - REVEAL_MARGIN_PX - 120,
    );
  });
});

describe("placing the details popup", () => {
  const measured = new Map<string, Rect>();
  const measure = (el: Element): Rect =>
    measured.get(el.className.split(" ")[0] ?? "") ?? rect(0, 0, 0, 0);

  it("places an open popup beside the panel, on the entry's line", () => {
    // ARRANGE
    const { detail, entry, sub } = panelWithDetails();
    entry.classList.add("open");
    measured.set("detail", rect(0, 40, 260, 160));
    measured.set(DETAILS_ENTRY_CLASS, rect(0, 180, 260, 18));
    measured.set(DETAILS_PANEL_CLASS, rect(0, 0, 180, 60));
    // ACT
    placeDetailsPanel(detail, measure, VIEWPORT);
    // ASSERT
    expect([sub.style.left, sub.style.top, sub.style.maxHeight]).toEqual(["260px", "180px", `${800 - 180 - REVEAL_MARGIN_PX}px`]);
  });

  it("places nothing while the popup is shut", () => {
    // ARRANGE
    const { detail, sub } = panelWithDetails();
    // ACT
    placeDetailsPanel(detail, measure, VIEWPORT);
    // ASSERT
    expect(sub.style.left).toBe("");
  });
});

describe("opening and shutting the details popup", () => {
  it("opens a drawn entry and places it", () => {
    // ARRANGE
    const { detail, entry } = panelWithDetails();
    const place = vi.fn();
    // ACT
    openDetails(detail, place);
    // ASSERT
    expect([entry.classList.contains("open"), place.mock.calls.length]).toEqual([true, 1]);
  });

  it("opens nothing for an entry hidden for want of facts", () => {
    // ARRANGE
    const detail = panel();
    detailsPanel(detail);
    syncDetailsEntry(detail);
    const place = vi.fn();
    // ACT
    openDetails(detail, place);
    // ASSERT
    expect([detailsEntry(detail)?.classList.contains("open"), place.mock.calls.length]).toEqual([false, 0]);
  });

  it("says it opened, in the canonical log", async () => {
    // ARRANGE
    const capture = captureLogRecords("debug");
    const { detail } = panelWithDetails();
    // ACT
    openDetails(detail, () => undefined);
    // ASSERT
    await forwardedRecord(capture, "sidebar.row.details-open");
  });

  it("shuts an open popup", () => {
    // ARRANGE
    const { detail, entry } = panelWithDetails();
    entry.classList.add("open");
    // ACT
    closeDetails(detail);
    // ASSERT
    expect(entry.classList.contains("open")).toBe(false);
  });
});

describe("the details entry's gesture", () => {
  /** A wired panel with a drawn entry; PLACE counts placements. */
  function wired() {
    const drawn = panelWithDetails();
    const place = vi.fn();
    wireDetailsEntry(drawn.detail, place);
    const label = drawn.entry.querySelector(".detail-more-label") as HTMLElement;
    const isOpen = (): boolean => drawn.entry.classList.contains("open");
    return { ...drawn, place, label, isOpen };
  }

  it("opens as the pointer reaches the entry", () => {
    const { label, isOpen, place } = wired();
    label.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
    expect([isOpen(), place.mock.calls.length]).toEqual([true, 1]);
  });

  it("opens on a click of the entry", () => {
    const { label, isOpen } = wired();
    label.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(isOpen()).toBe(true);
  });

  it("opens as the focus reaches the entry", () => {
    const { entry, isOpen } = wired();
    entry.dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    expect(isOpen()).toBe(true);
  });

  it("opens on Enter", () => {
    const { entry, isOpen } = wired();
    entry.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
    expect(isOpen()).toBe(true);
  });

  it("opens nothing for the pointer on the panel's other lines", () => {
    const { detail, isOpen } = wired();
    detail.querySelector("dl")?.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
    expect(isOpen()).toBe(false);
  });

  it("stays open as the pointer moves from the entry into its popup", () => {
    // ARRANGE
    const { label, sub, isOpen } = wired();
    label.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
    // ACT
    label.dispatchEvent(new MouseEvent("mouseout", { bubbles: true, relatedTarget: sub }));
    // ASSERT
    expect(isOpen()).toBe(true);
  });

  it("places once for a pointer moving within the entry and its popup", () => {
    // ARRANGE
    const { label, sub, place } = wired();
    label.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
    // ACT
    sub.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
    // ASSERT
    expect(place.mock.calls.length).toBe(1);
  });

  it("shuts as the pointer leaves the popup for the panel's other lines", () => {
    // ARRANGE
    const { detail, label, sub, isOpen } = wired();
    label.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
    // ACT
    sub.dispatchEvent(new MouseEvent("mouseout", { bubbles: true, relatedTarget: detail.querySelector("dl") }));
    // ASSERT
    expect(isOpen()).toBe(false);
  });

  it("shuts as the pointer leaves both popups for outside them", () => {
    // ARRANGE
    const { label, sub, isOpen } = wired();
    label.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
    // ACT
    sub.dispatchEvent(new MouseEvent("mouseout", { bubbles: true, relatedTarget: document.body }));
    // ASSERT
    expect(isOpen()).toBe(false);
  });

  it("shuts as the focus leaves the entry", () => {
    // ARRANGE
    const { entry, isOpen } = wired();
    entry.dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    // ACT
    entry.dispatchEvent(new FocusEvent("focusout", { bubbles: true, relatedTarget: document.body }));
    // ASSERT
    expect(isOpen()).toBe(false);
  });

  it("wires an entry drawn after the wiring", () => {
    // ARRANGE
    const detail = panel();
    wireDetailsEntry(detail, () => undefined);
    const sub = detailsPanel(detail);
    sub.textContent = "page not loaded";
    syncDetailsEntry(detail);
    // ACT
    detail.querySelector(".detail-more-label")?.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
    // ASSERT
    expect(detailsEntry(detail)?.classList.contains("open")).toBe(true);
  });
});
