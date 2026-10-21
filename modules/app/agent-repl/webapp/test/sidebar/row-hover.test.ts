// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createDropdowns, type Dropdowns } from "../../src/sidebar/dropdowns.js";
import {
  HOVER_CLOSE_GRACE_MS,
  createRowHover,
  type DrawnRow,
  type RowHover,
} from "../../src/sidebar/row-hover.js";
import { drawRosterFamily, drawRosterRow } from "../../src/sidebar/row.js";
import { log } from "../../src/log.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { openRow, row, sidebarContext } from "./harness.js";

/** A drawn copy of a row, in the document so focus can land in it. */
function drawnRow(): DrawnRow {
  const ws = document.createElement("div");
  const line = document.createElement("div");
  const trigger = document.createElement("span");
  const control = document.createElement("button");
  line.append(trigger, control);
  const detail = document.createElement("div");
  ws.append(line, detail);
  document.body.appendChild(ws);
  return { ws, line, trigger, detail };
}

const isOpen = (copy: DrawnRow): boolean => copy.ws.classList.contains("open");

describe("the row hover gesture", () => {
  let dropdowns: Dropdowns;
  let openDetails: Set<string>;
  let hover: RowHover;

  beforeEach(() => {
    vi.useFakeTimers();
    dropdowns = createDropdowns();
    openDetails = new Set();
    hover = createRowHover(dropdowns, openDetails);
  });

  afterEach(() => {
    hover.dispose();
    vi.restoreAllMocks();
    vi.useRealTimers();
    document.body.replaceChildren();
  });

  /** Draw one copy of ID's row the way a roster draw does. */
  function draw(id: string): DrawnRow {
    const copy = drawnRow();
    if (openDetails.has(id)) copy.ws.classList.add("open");
    hover.drawn(id, copy);
    return copy;
  }

  /** Throw OLD's drawing away and draw ID's row again, as a push changing its section does. */
  function redraw(id: string, old: DrawnRow): DrawnRow {
    hover.forgetWithin(old.ws);
    dropdowns.forgetWithin(old.ws);
    old.ws.remove();
    return draw(id);
  }

  /** Bring the pointer onto ID's trigger. */
  function openByHover(id: string): void {
    hover.enter(id, "trigger");
  }

  describe("forgetting a thrown-away drawing", () => {
    it("opens no copy drawn under the drawing thrown away", () => {
      // ARRANGE
      const gone = draw("ws-1");
      hover.forgetWithin(gone.ws);
      // ACT
      openByHover("ws-1");
      // ASSERT
      expect(isOpen(gone)).toBe(false);
    });

    it("still opens a copy drawn outside the drawing thrown away", () => {
      // ARRANGE
      const gone = draw("ws-1");
      const kept = draw("ws-1");
      hover.forgetWithin(gone.ws);
      // ACT
      openByHover("ws-1");
      // ASSERT
      expect(isOpen(kept)).toBe(true);
    });
  });

  describe("opening", () => {
    it("opens every drawn copy the moment the pointer reaches the trigger", () => {
      // ARRANGE
      const a = draw("ws-1");
      const b = draw("ws-1");
      // ACT
      openByHover("ws-1");
      // ASSERT
      expect([isOpen(a), isOpen(b)]).toEqual([true, true]);
    });

    it("remembers the open row on the page", () => {
      // ARRANGE
      draw("ws-1");
      // ACT
      openByHover("ws-1");
      // ASSERT
      expect([...openDetails]).toEqual(["ws-1"]);
    });

    it("opens without arming any timer", () => {
      // ARRANGE
      const a = draw("ws-1");
      // ACT
      hover.enter("ws-1", "trigger");
      // ASSERT
      expect([isOpen(a), vi.getTimerCount()]).toEqual([true, 0]);
    });

    it("cancels a pending close when the pointer comes back to the trigger", () => {
      // ARRANGE
      const a = draw("ws-1");
      openByHover("ws-1");
      hover.leave("ws-1", "trigger");
      // ACT
      hover.enter("ws-1", "trigger");
      vi.runAllTimers();
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });

    it("says it opened on hover, in the canonical log", async () => {
      // ARRANGE
      const capture = captureLogRecords("debug");
      draw("ws-1");
      // ACT
      hover.enter("ws-1", "trigger");
      // ASSERT
      const record = await forwardedRecord(capture, "sidebar.row.detail-toggle");
      expect(record.context).toMatchObject({ workspace: "ws-1", open: true, via: "hover" });
    });

    it("does not open on the panel alone", () => {
      // ARRANGE
      const a = draw("ws-1");
      // ACT
      hover.enter("ws-1", "panel");
      vi.runAllTimers();
      // ASSERT
      expect(isOpen(a)).toBe(false);
    });

    it("keeps open the row a redraw drew under a resting pointer", () => {
      // ARRANGE
      const old = draw("ws-1");
      hover.enter("ws-1", "trigger");
      // ACT
      const fresh = redraw("ws-1", old);
      // ASSERT
      expect(isOpen(fresh)).toBe(true);
    });

    it("never paints a copy a redraw threw away", () => {
      // ARRANGE
      const old = draw("ws-1");
      const fresh = redraw("ws-1", old);
      // ACT
      hover.enter("ws-1", "trigger");
      // ASSERT
      expect([isOpen(old), isOpen(fresh)]).toEqual([false, true]);
    });
  });

  describe("the pointer leaving a row that never saw it go", () => {
    it("closes a panel a redraw drew open once the pointer leaves the rail", () => {
      // ARRANGE: the pointer rests on the trigger and a push redraws the row.
      const old = draw("ws-1");
      hover.enter("ws-1", "trigger");
      const fresh = redraw("ws-1", old);
      // ACT: the pointer leaves for Emacs; the fresh row never saw it arrive.
      hover.pointerAt(null);
      vi.runAllTimers();
      // ASSERT
      expect([...openDetails]).toEqual([]);
      expect(isOpen(fresh)).toBe(false);
    });

    it("does not resurface the panel on the next push", () => {
      // ARRANGE
      const old = draw("ws-1");
      hover.enter("ws-1", "trigger");
      const fresh = redraw("ws-1", old);
      hover.pointerAt(null);
      vi.runAllTimers();
      // ACT: the push a sent prompt produces.
      const next = redraw("ws-1", fresh);
      // ASSERT
      expect(isOpen(next)).toBe(false);
    });

    it("closes an open panel once the pointer moves elsewhere in the rail", () => {
      // ARRANGE
      const old = draw("ws-1");
      openByHover("ws-1");
      const fresh = redraw("ws-1", old);
      // ACT
      hover.pointerAt(document.body);
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
      // ASSERT
      expect(isOpen(fresh)).toBe(false);
    });

    it("keeps the panel open while the pointer is over the fresh copy's trigger", () => {
      // ARRANGE
      const old = draw("ws-1");
      openByHover("ws-1");
      const fresh = redraw("ws-1", old);
      // ACT
      hover.pointerAt(fresh.trigger);
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS * 10);
      // ASSERT
      expect(isOpen(fresh)).toBe(true);
    });

    it("keeps the panel open while the pointer is over the fresh copy's panel", () => {
      // ARRANGE
      const old = draw("ws-1");
      openByHover("ws-1");
      const fresh = redraw("ws-1", old);
      // ACT
      hover.pointerAt(fresh.detail);
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS * 10);
      // ASSERT
      expect(isOpen(fresh)).toBe(true);
    });

    it("says which row it forgot, in the canonical log", async () => {
      // ARRANGE
      const capture = captureLogRecords("debug");
      const old = draw("ws-1");
      hover.enter("ws-1", "trigger");
      redraw("ws-1", old);
      // ACT
      hover.pointerAt(null);
      // ASSERT
      const record = await forwardedRecord(capture, "sidebar.row.detail-pointer-gone");
      expect(record.context).toMatchObject({ workspace: "ws-1", left_rail: true });
    });

    it("logs nothing for a move while no row holds the pointer", async () => {
      // ARRANGE
      const capture = captureLogRecords("debug");
      draw("ws-1");
      // ACT
      hover.pointerAt(document.body);
      // ASSERT
      capture.logger.flush();
      await Promise.resolve();
      expect(capture.sent.filter((r) => r.operation === "sidebar.row.detail-pointer-gone")).toEqual([]);
    });
  });

  describe("closing", () => {
    it("closes once the pointer has left the trigger and the grace has run out", () => {
      // ARRANGE
      const a = draw("ws-1");
      openByHover("ws-1");
      // ACT
      hover.leave("ws-1", "trigger");
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
      // ASSERT
      expect(isOpen(a)).toBe(false);
      expect([...openDetails]).toEqual([]);
    });

    it("stays open while the pointer travels into the panel", () => {
      // ARRANGE
      const a = draw("ws-1");
      openByHover("ws-1");
      hover.leave("ws-1", "trigger");
      // ACT
      hover.enter("ws-1", "panel");
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS * 10);
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });

    it("closes the fresh copy when the grace a redraw interrupted runs out", () => {
      // ARRANGE
      const old = draw("ws-1");
      openByHover("ws-1");
      hover.leave("ws-1", "trigger");
      // ACT
      const fresh = redraw("ws-1", old);
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
      // ASSERT
      expect(isOpen(fresh)).toBe(false);
    });

    it("ignores a leave from a part the pointer was never over", () => {
      // ARRANGE
      const a = draw("ws-1");
      hover.focusIn("ws-1");
      // ACT
      hover.leave("ws-1", "panel");
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS * 10);
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });

    it("closes every copy when a click outside dismisses it", () => {
      // ARRANGE
      const a = draw("ws-1");
      const b = draw("ws-1");
      openByHover("ws-1");
      // ACT
      dropdowns.dismissOutside(document.body);
      // ASSERT
      expect([isOpen(a), isOpen(b), openDetails.has("ws-1")]).toEqual([false, false, false]);
    });

    it("stays dismissed while the pointer rests on the trigger", () => {
      // ARRANGE
      const a = draw("ws-1");
      openByHover("ws-1");
      hover.leave("ws-1", "trigger");
      hover.enter("ws-1", "trigger");
      // ACT
      dropdowns.dismissOutside(document.body);
      vi.runAllTimers();
      // ASSERT
      expect(isOpen(a)).toBe(false);
    });

    it("registers a row a redraw drew open as the open dropdown", () => {
      // ARRANGE
      const old = draw("ws-1");
      openByHover("ws-1");
      const fresh = redraw("ws-1", old);
      // ACT
      dropdowns.dismissOutside(document.body);
      // ASSERT
      expect(isOpen(fresh)).toBe(false);
    });

    it("cancels a pending close on dispose", () => {
      // ARRANGE
      const a = draw("ws-1");
      openByHover("ws-1");
      hover.leave("ws-1", "trigger");
      // ACT
      hover.dispose();
      vi.runAllTimers();
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });
  });

  describe("the keyboard", () => {
    it("opens at once on focus", () => {
      // ARRANGE
      const a = draw("ws-1");
      // ACT
      hover.focusIn("ws-1");
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });

    it("stays open while the focus moves between the line's own controls", () => {
      // ARRANGE
      const a = draw("ws-1");
      hover.focusIn("ws-1");
      // ACT
      hover.focusOut("ws-1", a.line.querySelector("button"));
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS * 10);
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });

    it("closes once the focus leaves the line", () => {
      // ARRANGE
      const a = draw("ws-1");
      hover.focusIn("ws-1");
      // ACT
      hover.focusOut("ws-1", document.body);
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
      // ASSERT
      expect(isOpen(a)).toBe(false);
    });

    it("stays open after the pointer leaves while the focus is in the line", () => {
      // ARRANGE
      const a = draw("ws-1");
      (a.line.querySelector("button") as HTMLElement).focus();
      openByHover("ws-1");
      // ACT
      hover.leave("ws-1", "trigger");
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS * 10);
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });

    it("lets the pointer close a row whose focused line a redraw removed", () => {
      // ARRANGE
      const old = draw("ws-1");
      (old.line.querySelector("button") as HTMLElement).focus();
      openByHover("ws-1");
      const fresh = redraw("ws-1", old);
      // ACT
      hover.pointerAt(null);
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
      // ASSERT
      expect(isOpen(fresh)).toBe(false);
    });
  });

  describe("the rail losing the user", () => {
    it("closes every open panel on a dismiss", () => {
      // ARRANGE
      const a = draw("ws-1");
      const b = draw("ws-2");
      hover.focusIn("ws-1");
      openByHover("ws-2");
      // ACT
      hover.dismiss("the page lost focus");
      // ASSERT
      expect([isOpen(a), isOpen(b), [...openDetails]]).toEqual([false, false, []]);
    });

    it("logs each panel it closes with why", () => {
      // ARRANGE
      draw("ws-1");
      openByHover("ws-1");
      const debug = vi.spyOn(log, "debug");
      // ACT
      hover.dismiss("the selection moved");
      // ASSERT
      expect(debug).toHaveBeenCalledWith("toggling a row's detail panel", {
        operation: "sidebar.row.detail-toggle",
        context: { workspace: "ws-1", open: false, via: "the selection moved", copies: 1 },
      });
    });

    it("lets the page's dropdown registry go of the panel it closed", () => {
      // ARRANGE
      draw("ws-1");
      openByHover("ws-1");
      hover.dismiss("the page lost focus");
      const other = { kind: "test", element: document.createElement("div"), openers: [], close: vi.fn() };
      dropdowns.opened(other);
      // ACT
      dropdowns.dismissAll("again");
      // ASSERT: only the other dropdown is still registered.
      expect(other.close).toHaveBeenCalledTimes(1);
    });

    it("cancels a close already pending", () => {
      // ARRANGE
      draw("ws-1");
      openByHover("ws-1");
      hover.leave("ws-1", "trigger");
      hover.dismiss("the page lost focus");
      const debug = vi.spyOn(log, "debug");
      // ACT
      vi.runAllTimers();
      // ASSERT
      expect(debug).not.toHaveBeenCalled();
    });

    it("opens no panel for a pointer arriving on a trigger after a dismiss", () => {
      // ARRANGE
      const a = draw("ws-1");
      hover.dismiss("the page lost focus");
      // ACT
      openByHover("ws-1");
      // ASSERT
      expect(isOpen(a)).toBe(false);
    });

    it("opens no panel for the focus WebKit hands back to a line after a dismiss", () => {
      // ARRANGE
      const a = draw("ws-1");
      hover.dismiss("the page lost focus");
      // ACT
      hover.focusIn("ws-1");
      // ASSERT
      expect(isOpen(a)).toBe(false);
    });

    it("opens the panel of the trigger the pointer rests on when the user is back", () => {
      // ARRANGE
      const a = draw("ws-1");
      hover.dismiss("the page lost focus");
      openByHover("ws-1");
      // ACT
      hover.resume();
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });

    it("opens no panel on resume for a pointer that is on no trigger", () => {
      // ARRANGE
      const a = draw("ws-1");
      openByHover("ws-1");
      hover.dismiss("the page lost focus");
      hover.pointerAt(document.body);
      // ACT
      hover.resume();
      // ASSERT
      expect(isOpen(a)).toBe(false);
    });

    it("answers the pointer again after a resume", () => {
      // ARRANGE
      const a = draw("ws-1");
      hover.dismiss("the page lost focus");
      hover.resume();
      // ACT
      openByHover("ws-1");
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });

    it("answers the focus again after a resume", () => {
      // ARRANGE
      const a = draw("ws-1");
      hover.dismiss("the page lost focus");
      hover.resume();
      // ACT
      hover.focusIn("ws-1");
      // ASSERT
      expect(isOpen(a)).toBe(true);
    });
  });
});

describe("the rows that report to the gesture", () => {
  it("registers every drawn row, its children included, with the page's one gesture", () => {
    // ARRANGE
    const sc = sidebarContext();
    const drawn = vi.spyOn(sc.rowHover, "drawn");
    // ACT
    drawRosterFamily(openRow(row({ id: "ws-1", children: [row({ id: "ws-2" })] })), sc, "R");
    // ASSERT
    expect(drawn.mock.calls.map(([id]) => id)).toEqual(["ws-1", "ws-2"]);
  });

  it("hands the gesture the row's own duration field, line and panel", () => {
    // ARRANGE
    const sc = sidebarContext();
    const drawn = vi.spyOn(sc.rowHover, "drawn");
    // ACT
    const ws = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    // ASSERT
    const [, copy] = drawn.mock.calls[0] ?? [];
    expect(copy).toEqual({
      ws,
      line: ws.querySelector(":scope > .row"),
      trigger: ws.querySelector(":scope > .row > [data-when]"),
      detail: ws.querySelector(":scope > .detail"),
    });
  });
});
