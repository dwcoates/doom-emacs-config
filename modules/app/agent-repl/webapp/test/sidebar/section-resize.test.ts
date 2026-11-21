// @vitest-environment jsdom
/**
 * THE RULES BETWEEN THE RAIL'S SECTIONS ARE DRAGGED (owner request,
 * 2026-11-11): a drag of the rule above a band sizes it, in lines, against
 * the open section above, the heights painted as the pointer moves and asked
 * of the daemon in one request on release. jsdom lays nothing out, so each
 * case states what the last fit measured of the sections (`FITS`).
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
  type UpdateSidebarViewResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { BAND_SIZED_ATTRIBUTE, type FittedSection } from "../../src/sidebar/band-fit.js";
import { HIDDEN_FOLD_KEY, MERGED_FOLD_KEY, MERGE_QUEUE_FOLD_KEY, OPEN_FOLD_KEY } from "../../src/sidebar/context.js";
import { recorded } from "../events.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  RESIZE_HANDLE_CLASS,
  RESIZING_CLASS,
  drawRosterSectionHeight,
  drawSectionHeight,
  draggedLines,
  installSectionResize,
  measuredSection,
  nearestStop,
  paintSectionHeight,
  resizePair,
} from "../../src/sidebar/section-resize.js";
import { heightViewKey } from "../../src/sidebar/view.js";
import { primaryPointer, settleOwedDragClick } from "../drag-pointer.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { appContext, sidebarContext } from "./harness.js";

afterEach(() => settleOwedDragClick());

const ROW = 20;

/** The stops of a section of N lines, ROW px each. */
const stops = (n: number): number[] => Array.from({ length: n + 1 }, (_, k) => k * ROW);

const OK = create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
const REFUSED = create(UpdateSidebarViewResponseSchema, {
  result: { case: "error", value: { cause: { case: "unknownRepository", value: {} } } },
});

/** The four sections a pane draws: class, fold key, whether folded, the lines the wire sized it to. */
interface Drawn {
  folded?: Partial<Record<"open" | "queue" | "merged" | "hidden", boolean>>;
  sized?: Partial<Record<"queue" | "merged" | "hidden", number>>;
}

/**
 * A pane of the four sections drawn as DRAWN says, each band through
 * `drawSectionHeight` (its grip and its tracked height), the daemon
 * answering ANSWER and recording every request.
 */
function rail({ folded = {}, sized = {} }: Drawn = {}, answer: () => UpdateSidebarViewResponse = () => OK) {
  const requests = recorded<UpdateSidebarViewRequest>();
  const asked = requests.calls;
  const sc = sidebarContext(
    appContext({
      updateSidebarView: (request) => {
        requests.record(request);
        return answer();
      },
    }),
  );
  const pane = document.createElement("div");
  pane.className = "sb-pane";
  const section = (cls: string, key: string, isFolded: boolean | undefined): HTMLElement => {
    const el = document.createElement("div");
    el.className = `repo ${cls}`;
    el.setAttribute("data-section", key);
    if (isFolded === true) el.classList.add("folded");
    pane.appendChild(el);
    return el;
  };
  const open = section("open-section", OPEN_FOLD_KEY, folded.open);
  const queue = section("merge-queue-section", MERGE_QUEUE_FOLD_KEY, folded.queue);
  const merged = section("merged-section", MERGED_FOLD_KEY, folded.merged);
  const hidden = section("hidden-section", HIDDEN_FOLD_KEY, folded.hidden);
  drawSectionHeight(queue, MERGE_QUEUE_FOLD_KEY, sized.queue ?? null, sc);
  drawSectionHeight(merged, MERGED_FOLD_KEY, sized.merged ?? null, sc);
  drawSectionHeight(hidden, HIDDEN_FOLD_KEY, sized.hidden ?? null, sc);
  document.body.replaceChildren(pane);
  return { asked, requests, sc, pane, open, queue, merged, hidden };
}

/** The grip over SECTION's rule, its pointer capture stubbed (jsdom has none). */
function grip(section: HTMLElement): HTMLElement {
  const el = section.querySelector<HTMLElement>(`:scope > .${RESIZE_HANDLE_CLASS}`);
  if (el === null) throw new Error("the section draws no grip");
  el.setPointerCapture = () => undefined;
  return el;
}

/** Install the drag over R with FITS as the last fit's measurements, counting refits. */
function install(r: ReturnType<typeof rail>, fits: Map<HTMLElement, FittedSection>) {
  const refits = recorded<void>();
  const refit = vi.fn(() => refits.record());
  const remove = installSectionResize(r.pane, r.sc, refit, (section) => fits.get(section) ?? null);
  return { refit, refits, remove };
}

/** Press the grip over SECTION at Y=100, move it to TO, and release it unless HOLD. */
function drag(section: HTMLElement, to: number, hold = false): HTMLElement {
  const g = grip(section);
  primaryPointer("pointerdown", g, 100);
  primaryPointer("pointermove", g, to);
  if (!hold) primaryPointer("pointerup", g, to);
  return g;
}

describe("drawRosterSectionHeight", () => {
  it("reads a fitted section as null", () => {
    // Arrange / Act / Assert
    expect(drawRosterSectionHeight({ case: "fitted", value: {} }, "hidden.height")).toBeNull();
  });

  it("reads a sized section's lines", () => {
    // Arrange / Act / Assert
    expect(drawRosterSectionHeight({ case: "sized", value: { lines: 6 } }, "hidden.height")).toBe(6);
  });

  it("refuses an unset height", () => {
    // Arrange / Act / Assert
    expect(() => drawRosterSectionHeight({ case: undefined }, "hidden.height")).toThrow(MalformedView);
  });
});

describe("paintSectionHeight", () => {
  it("states a sized section's lines, and drops them once it is fitted", () => {
    // Arrange
    const section = document.createElement("div");
    // Act
    paintSectionHeight(section, 4);
    const sized = section.getAttribute(BAND_SIZED_ATTRIBUTE);
    paintSectionHeight(section, null);
    // Assert
    expect([sized, section.hasAttribute(BAND_SIZED_ATTRIBUTE)]).toEqual(["4", false]);
  });
});

describe("drawSectionHeight", () => {
  it("draws the wire's height and a grip as the section's first child", () => {
    // Arrange / Act
    const r = rail({ sized: { hidden: 3 } });
    // Assert
    expect([r.hidden.getAttribute(BAND_SIZED_ATTRIBUTE), r.hidden.firstElementChild?.className]).toEqual(["3", RESIZE_HANDLE_CLASS]);
  });

  it("draws the height this page asked for over a wire that has not caught up", () => {
    // Arrange
    const r = rail();
    r.sc.view.ask(heightViewKey(HIDDEN_FOLD_KEY), 7);
    const redrawn = document.createElement("div");
    // Act
    drawSectionHeight(redrawn, HIDDEN_FOLD_KEY, null, r.sc);
    // Assert
    expect(redrawn.getAttribute(BAND_SIZED_ATTRIBUTE)).toBe("7");
  });
});

describe("resizePair", () => {
  it("pairs a band with the Open section above it", () => {
    // Arrange
    const r = rail();
    // Act / Assert
    expect(resizePair(r.queue)).toEqual({ upper: r.open, lower: r.queue });
  });

  it("pairs two bands", () => {
    // Arrange
    const r = rail();
    // Act / Assert
    expect(resizePair(r.hidden)).toEqual({ upper: r.merged, lower: r.hidden });
  });

  it("passes over a folded section to the open one above it", () => {
    // Arrange
    const r = rail({ folded: { queue: true } });
    // Act / Assert
    expect(resizePair(r.merged)).toEqual({ upper: r.open, lower: r.merged });
  });

  it("pairs nothing with a folded band", () => {
    // Arrange
    const r = rail({ folded: { hidden: true } });
    // Act / Assert
    expect(resizePair(r.hidden)).toBeNull();
  });

  it("pairs nothing with a band under no open section", () => {
    // Arrange
    const r = rail({ folded: { open: true } });
    // Act / Assert
    expect(resizePair(r.queue)).toBeNull();
  });
});

describe("nearestStop", () => {
  it.each([
    { px: -50, want: 0 },
    { px: 29, want: 1 },
    { px: 31, want: 2 },
    { px: 500, want: 4 },
  ])("snaps $px px to stop $want", ({ px, want }) => {
    // Arrange / Act / Assert
    expect(nearestStop(stops(4), px)).toBe(want);
  });
});

describe("draggedLines", () => {
  it.each([
    { name: "a band under the Open section grown by a rule dragged up", upper: null, lower: { stops: stops(6), lines: 2 }, dy: -41, want: { upper: null, lower: 4 } },
    { name: "a band under the Open section shrunk to its header at most", upper: null, lower: { stops: stops(6), lines: 2 }, dy: 300, want: { upper: null, lower: 0 } },
    { name: "a band grown no further than its lines", upper: null, lower: { stops: stops(3), lines: 1 }, dy: -300, want: { upper: null, lower: 3 } },
    { name: "the room between two bands moved down", upper: { stops: stops(5), lines: 3 }, lower: { stops: stops(4), lines: 2 }, dy: 20, want: { upper: 4, lower: 1 } },
    { name: "the room between two bands moved up", upper: { stops: stops(5), lines: 3 }, lower: { stops: stops(8), lines: 2 }, dy: -40, want: { upper: 1, lower: 4 } },
    { name: "a lower band kept to the lines it holds", upper: { stops: stops(5), lines: 3 }, lower: { stops: stops(2), lines: 2 }, dy: -60, want: { upper: 0, lower: 2 } },
  ])("gives $name", ({ upper, lower, dy, want }) => {
    // Arrange / Act / Assert
    expect(draggedLines(upper, lower, dy)).toEqual(want);
  });
});

describe("measuredSection", () => {
  it("refuses a section the fit never measured", () => {
    // Arrange
    const section = document.createElement("div");
    // Act / Assert
    expect(() => measuredSection(section, () => null)).toThrow(/never fitted/);
  });

  it("logs a section the fit never measured", async () => {
    // Arrange
    const section = document.createElement("div");
    section.className = "repo hidden-section";
    const capture = captureLogRecords("error");
    // Act
    expect(() => measuredSection(section, () => null)).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "sidebar.section-resize.unfitted");
    expect(record.context).toMatchObject({ section: "repo hidden-section" });
  });
});

describe("installSectionResize", () => {
  it("paints the lines a drag under the Open section gives the band, and re-fits", () => {
    // Arrange
    const r = rail();
    const { refit } = install(r, new Map([[r.queue, { stops: stops(6), lines: 2 }]]));
    // Act
    drag(r.queue, 60, true);
    // Assert
    expect([r.queue.getAttribute(BAND_SIZED_ATTRIBUTE), refit.mock.calls.length, document.documentElement.classList.contains(RESIZING_CLASS)]).toEqual(["4", 1, true]);
  });

  it("asks for the band's lines alone on release, the Open section never sized", async () => {
    // Arrange
    const r = rail();
    install(r, new Map([[r.queue, { stops: stops(6), lines: 2 }]]));
    // Act
    drag(r.queue, 60);
    // Assert
    await r.requests.reached(1);
    const change = r.asked[0].change;
    expect(change.case === "sizeSections" && change.value.sizes.map((s) => [s.section.case, s.height.case, s.height.value])).toEqual([
      ["mergeQueue", "lines", expect.objectContaining({ lines: 4 })],
    ]);
  });

  it("asks for both bands beside a rule between them in one request", async () => {
    // Arrange
    const r = rail();
    install(
      r,
      new Map([
        [r.merged, { stops: stops(5), lines: 3 }],
        [r.hidden, { stops: stops(4), lines: 2 }],
      ]),
    );
    // Act
    drag(r.hidden, 120);
    // Assert
    await r.requests.reached(1);
    const change = r.asked[0].change;
    expect(change.case === "sizeSections" && change.value.sizes.map((s) => [s.section.case, s.height.case === "lines" ? s.height.value.lines : null])).toEqual([
      ["recentlyMerged", 4],
      ["hidden", 1],
    ]);
  });

  it("logs the drag it asks for", async () => {
    // Arrange
    const r = rail();
    install(r, new Map([[r.queue, { stops: stops(6), lines: 2 }]]));
    const capture = captureLogRecords("info");
    // Act
    drag(r.queue, 60);
    // Assert
    const record = await forwardedRecord(capture, "sidebar.section-resize");
    expect(record.context).toMatchObject({ lower: "repo merge-queue-section", from: "null/2", to: "null/4" });
  });

  it("asks nothing for a rule let go where it was pressed", () => {
    // Arrange
    const r = rail();
    install(r, new Map([[r.queue, { stops: stops(6), lines: 2 }]]));
    // Act
    drag(r.queue, 102);
    // Assert
    expect([r.asked.length, r.queue.hasAttribute(BAND_SIZED_ATTRIBUTE)]).toEqual([0, false]);
  });

  it("clears the resize cursor when the grip is detached before release", () => {
    // Arrange
    const r = rail({ sized: { queue: 1 } });
    install(r, new Map([[r.queue, { stops: stops(6), lines: 1 }]]));
    const g = drag(r.queue, 40, true);
    g.remove();
    // Act
    primaryPointer("pointerup", document.body, 40);
    // Assert
    expect(document.documentElement.classList.contains(RESIZING_CLASS)).toBe(false);
  });

  it("clears the resize cursor when the pointer is cancelled", () => {
    // Arrange
    const r = rail({ sized: { queue: 1 } });
    install(r, new Map([[r.queue, { stops: stops(6), lines: 1 }]]));
    drag(r.queue, 40, true);
    // Act
    primaryPointer("pointercancel", document.body, 40);
    // Assert
    expect(document.documentElement.classList.contains(RESIZING_CLASS)).toBe(false);
  });

  it("puts the wire's height back and asks nothing when Escape cancels the drag", () => {
    // Arrange
    const r = rail({ sized: { queue: 1 } });
    const { refit } = install(r, new Map([[r.queue, { stops: stops(6), lines: 1 }]]));
    drag(r.queue, 40, true);
    // Act
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape", bubbles: true }));
    // Assert
    expect([r.queue.getAttribute(BAND_SIZED_ATTRIBUTE), r.asked.length, refit.mock.calls.length, document.documentElement.classList.contains(RESIZING_CLASS)]).toEqual([
      "1",
      0,
      2,
      false,
    ]);
  });

  it("puts the wire's height back and re-fits when the daemon refuses", async () => {
    // Arrange
    const r = rail({}, () => REFUSED);
    const { refits } = install(r, new Map([[r.queue, { stops: stops(6), lines: 2 }]]));
    // Act
    drag(r.queue, 60);
    // Assert
    await refits.reached(2);
    expect(r.queue.hasAttribute(BAND_SIZED_ATTRIBUTE)).toBe(false);
  });

  it("presses nothing on a folded band's rule", () => {
    // Arrange
    const r = rail({ folded: { hidden: true } });
    const { refit } = install(r, new Map());
    // Act
    drag(r.hidden, 60);
    // Assert
    expect([r.asked.length, refit.mock.calls.length]).toEqual([0, 0]);
  });

  it("fits the sections beside a double-clicked rule again", async () => {
    // Arrange
    const r = rail({ sized: { merged: 2, hidden: 5 } });
    const { refit } = install(r, new Map());
    // Act
    grip(r.hidden).dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
    // Assert
    await r.requests.reached(1);
    const change = r.asked[0].change;
    expect([
      change.case === "sizeSections" && change.value.sizes.map((s) => [s.section.case, s.height.case]),
      r.merged.hasAttribute(BAND_SIZED_ATTRIBUTE),
      r.hidden.hasAttribute(BAND_SIZED_ATTRIBUTE),
      refit.mock.calls.length,
    ]).toEqual([
      [
        ["recentlyMerged", "fitted"],
        ["hidden", "fitted"],
      ],
      false,
      false,
      1,
    ]);
  });

  it("listens no more once uninstalled", () => {
    // Arrange
    const r = rail();
    const { refit, remove } = install(r, new Map([[r.queue, { stops: stops(6), lines: 2 }]]));
    remove();
    // Act
    drag(r.queue, 60);
    // Assert
    expect([r.asked.length, refit.mock.calls.length]).toEqual([0, 0]);
  });
});
