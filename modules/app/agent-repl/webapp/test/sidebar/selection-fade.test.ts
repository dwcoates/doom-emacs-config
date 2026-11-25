// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import {
  SELECTION_ANIMATIONS,
  SELECTION_NUMBER_RISE,
  SELECTION_NUMBER_RISE_MS,
  createSelectionFade,
  type SelectionAnimation,
  type SelectionFade,
} from "../../src/sidebar/selection-fade.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** A drawn copy of workspace ID's row, under ROOT. */
function drawnRow(root: HTMLElement, id: string): HTMLElement {
  const ws = document.createElement("div");
  ws.className = "ws";
  ws.setAttribute("data-roster-row", id);
  root.appendChild(ws);
  return ws;
}

/** A row freshly drawn for ID, not yet in the document. */
const fresh = (id: string): HTMLElement => {
  const ws = document.createElement("div");
  ws.setAttribute("data-roster-row", id);
  return ws;
};

/** Which of the number's animations ROW runs, and at what delay: "" for none. */
const runningOn = (row: HTMLElement): [string, string] => {
  const animation: SelectionAnimation | undefined = SELECTION_ANIMATIONS.find((a) =>
    row.classList.contains(a.className),
  );
  return animation === undefined ? ["", ""] : [animation.className, row.style.getPropertyValue(animation.delayProperty)];
};

describe("the selection fade", () => {
  let root: HTMLElement;
  let clock: number;
  let fade: SelectionFade;

  beforeEach(() => {
    root = document.createElement("div");
    document.body.appendChild(root);
    clock = 1_000;
    fade = createSelectionFade(root, () => clock);
  });

  afterEach(() => {
    root.remove();
  });

  it("draws the first highlighted row white at once: the rail's first draw is not a move", () => {
    // Arrange
    const row = fresh("ws-1");

    // Act
    fade.apply("ws-1", row, true);

    // Assert
    expect(runningOn(row)).toEqual(["", ""]);
  });

  it("rises the number of a row the highlight moves to", () => {
    // Arrange
    fade.apply("ws-1", fresh("ws-1"), true);
    const row = fresh("ws-2");

    // Act
    fade.apply("ws-2", row, true);

    // Assert
    expect(runningOn(row)).toEqual([SELECTION_NUMBER_RISE.className, "0ms"]);
  });

  it("draws the number of the row the highlight left grey at once, when that row is drawn after", () => {
    // Arrange
    fade.apply("ws-1", fresh("ws-1"), true);
    fade.apply("ws-2", fresh("ws-2"), true);
    const left = fresh("ws-1");

    // Act
    fade.apply("ws-1", left, false);

    // Assert
    expect(runningOn(left)).toEqual(["", ""]);
  });

  it("draws the number of a row drawn unhighlighted before the row the highlight moved to grey at once", () => {
    // Arrange
    fade.apply("ws-1", fresh("ws-1"), true);
    const left = fresh("ws-1");

    // Act
    fade.apply("ws-1", left, false);

    // Assert
    expect(runningOn(left)).toEqual(["", ""]);
  });

  it("rises the row the highlight moved to after the row it left was drawn first", () => {
    // Arrange
    fade.apply("ws-1", fresh("ws-1"), true);
    fade.apply("ws-1", fresh("ws-1"), false);
    const row = fresh("ws-2");

    // Act
    fade.apply("ws-2", row, true);

    // Assert
    expect(runningOn(row)).toEqual([SELECTION_NUMBER_RISE.className, "0ms"]);
  });

  it("leaves a row the highlight never touched still", () => {
    // Arrange
    fade.apply("ws-1", fresh("ws-1"), true);
    fade.apply("ws-2", fresh("ws-2"), true);
    const other = fresh("ws-3");

    // Act
    fade.apply("ws-3", other, false);

    // Assert
    expect(runningOn(other)).toEqual(["", ""]);
  });

  it("does not restart the rise when the highlighted row is drawn again", () => {
    // Arrange
    fade.apply("ws-1", fresh("ws-1"), true);
    fade.apply("ws-2", fresh("ws-2"), true);
    clock += SELECTION_NUMBER_RISE_MS / 2;
    const redrawn = fresh("ws-2");

    // Act
    fade.apply("ws-2", redrawn, true);

    // Assert
    expect(runningOn(redrawn)).toEqual([SELECTION_NUMBER_RISE.className, `-${SELECTION_NUMBER_RISE_MS / 2}ms`]);
  });

  it("draws the highlighted row's number still once the rise has run its length", () => {
    // Arrange
    fade.apply("ws-1", fresh("ws-1"), true);
    fade.apply("ws-2", fresh("ws-2"), true);
    clock += SELECTION_NUMBER_RISE_MS;
    const redrawn = fresh("ws-2");

    // Act
    fade.apply("ws-2", redrawn, true);

    // Assert
    expect(runningOn(redrawn)).toEqual(["", ""]);
  });

  it("rises the moved-to row and greys the left row at once when the highlight moves ahead of the roster", () => {
    // Arrange
    const left = drawnRow(root, "ws-1");
    const moved = drawnRow(root, "ws-2");
    fade.apply("ws-1", left, true);
    fade.apply("ws-2", moved, false);

    // Act
    fade.highlight("ws-2");

    // Assert
    expect([runningOn(left), runningOn(moved)]).toEqual([
      ["", ""],
      [SELECTION_NUMBER_RISE.className, "0ms"],
    ]);
  });

  it("stops a rising row's rise at once when the highlight leaves it mid-rise", () => {
    // Arrange
    const row = drawnRow(root, "ws-2");
    fade.highlight("ws-1");
    fade.highlight("ws-2");
    clock += 300;

    // Act
    fade.highlight("ws-1");

    // Assert
    expect(runningOn(row)).toEqual(["", ""]);
  });

  it("moves nothing when the row already highlighted is highlighted again", () => {
    // Arrange
    const row = drawnRow(root, "ws-1");
    fade.highlight("ws-1");

    // Act
    fade.highlight("ws-1");

    // Assert
    expect(runningOn(row)).toEqual(["", ""]);
  });

  it("hands a rising row the rise's length", () => {
    // Arrange
    const row = drawnRow(root, "ws-2");
    fade.highlight("ws-1");

    // Act
    fade.highlight("ws-2");

    // Assert
    expect(row.style.getPropertyValue(SELECTION_NUMBER_RISE.durationProperty)).toBe(`${SELECTION_NUMBER_RISE_MS}ms`);
  });

  it("rises over half a second", () => {
    // Arrange, Act, Assert
    expect(SELECTION_NUMBER_RISE_MS).toBe(500);
  });

  it("runs the rise alone, under its own class and properties", () => {
    // Arrange / Act
    const shapes = SELECTION_ANIMATIONS.map((a) => [a.className, a.durationProperty, a.delayProperty, a.ms]);

    // Assert
    expect(shapes).toEqual([
      ["selection-number-rise", "--selection-number-rise-duration", "--selection-number-rise-delay", SELECTION_NUMBER_RISE_MS],
    ]);
  });

  it("states the workspace and the one it left, in the canonical log", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    fade.highlight("ws-1");

    // Act
    fade.highlight("ws-2");

    // Assert
    const record = await forwardedRecord(capture, "sidebar.selection-fade");
    expect(record.context).toMatchObject({ workspace: "ws-2", previous: "ws-1" });
  });
});
