// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import {
  SELECTION_BLINK_CLASS,
  SELECTION_BLINK_DELAY_PROPERTY,
  SELECTION_BLINK_DURATION_PROPERTY,
  SELECTION_BLINK_MS,
  createSelectionBlink,
  type SelectionBlink,
} from "../../src/sidebar/selection-blink.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import stylesheet from "../../src/styles.css?raw";
import { keyframesOf, rulesOf } from "../stylesheet.js";

/** A drawn copy of workspace ID's row, under ROOT. */
function drawnRow(root: HTMLElement, id: string): HTMLElement {
  const ws = document.createElement("div");
  ws.className = "ws";
  ws.setAttribute("data-roster-row", id);
  root.appendChild(ws);
  return ws;
}

const blinking = (row: HTMLElement): boolean => row.classList.contains(SELECTION_BLINK_CLASS);
const delayOf = (row: HTMLElement): string => row.style.getPropertyValue(SELECTION_BLINK_DELAY_PROPERTY);

describe("the selection blink", () => {
  let root: HTMLElement;
  let clock: number;
  let blink: SelectionBlink;

  beforeEach(() => {
    root = document.createElement("div");
    document.body.appendChild(root);
    clock = 1_000;
    blink = createSelectionBlink(root, () => clock);
  });

  afterEach(() => {
    root.remove();
  });

  it("blinks every drawn copy of the selected workspace's row from its start", () => {
    // Arrange
    const copies = [drawnRow(root, "ws-1"), drawnRow(root, "ws-1")];

    // Act
    blink.start("ws-1");

    // Assert
    expect(copies.map((row) => [blinking(row), delayOf(row)])).toEqual([
      [true, "0ms"],
      [true, "0ms"],
    ]);
  });

  it("hands a blinking row the blink's length", () => {
    // Arrange
    const row = drawnRow(root, "ws-1");

    // Act
    blink.start("ws-1");

    // Assert
    expect(row.style.getPropertyValue(SELECTION_BLINK_DURATION_PROPERTY)).toBe(`${SELECTION_BLINK_MS}ms`);
  });

  it("leaves another workspace's row still", () => {
    // Arrange
    const other = drawnRow(root, "ws-2");

    // Act
    blink.start("ws-1");

    // Assert
    expect(blinking(other)).toBe(false);
  });

  it("restarts a row already blinking when it is selected again", () => {
    // Arrange
    const row = drawnRow(root, "ws-1");
    blink.start("ws-1");
    clock += 700;

    // Act
    blink.start("ws-1");

    // Assert
    expect([blinking(row), delayOf(row)]).toEqual([true, "0ms"]);
  });

  it("joins a row redrawn mid-blink at the elapsed time", () => {
    // Arrange
    blink.start("ws-1");
    clock += 600;
    const redrawn = document.createElement("div");

    // Act
    blink.apply("ws-1", redrawn);

    // Assert
    expect([blinking(redrawn), delayOf(redrawn)]).toEqual([true, "-600ms"]);
  });

  it("draws a row still once its blink has run its length", () => {
    // Arrange
    blink.start("ws-1");
    clock += SELECTION_BLINK_MS;
    const redrawn = document.createElement("div");

    // Act
    blink.apply("ws-1", redrawn);

    // Assert
    expect(blinking(redrawn)).toBe(false);
  });

  it("draws a never-selected row still", () => {
    // Arrange
    const drawn = document.createElement("div");

    // Act
    blink.apply("ws-1", drawn);

    // Assert
    expect([blinking(drawn), delayOf(drawn)]).toEqual([false, ""]);
  });

  it("states the workspace and how many copies blink, in the canonical log", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    drawnRow(root, "ws-1");

    // Act
    blink.start("ws-1");

    // Assert
    const record = await forwardedRecord(capture, "sidebar.selection-blink");
    expect(record.context).toMatchObject({ workspace: "ws-1", drawn: 1 });
  });
});

describe("the selection blink's styling", () => {
  /** The declarations of every rule whose selector names SELECTOR. */
  const declarationsOf = (selector: string): string[] =>
    rulesOf(stylesheet)
      .filter((rule) => rule.selectors.includes(selector))
      .map((rule) => rule.declarations);

  it("animates the blinking row's line on the blink's length and delay, once", () => {
    // Arrange / Act
    const declarations = declarationsOf(`#ws-sidebar .ws.${SELECTION_BLINK_CLASS} > .row`);

    // Assert
    expect(declarations).toContainEqual(
      expect.stringMatching(
        /animation:\s*ws-selection-blink var\(--selection-blink-duration\) ease-in-out var\(--selection-blink-delay\) 1;/,
      ),
    );
  });

  it("swells once: the blink's keyframes carry a single stop", () => {
    // Arrange / Act
    const stops = rulesOf(keyframesOf("ws-selection-blink")).map((rule) => rule.selectors);

    // Assert
    expect(stops).toEqual([["50%"]]);
  });

  it("rises from and settles back to the row's own background, so a current row ends on its tint", () => {
    // Arrange / Act
    const stops = rulesOf(keyframesOf("ws-selection-blink")).flatMap((rule) => rule.selectors);

    // Assert
    expect([stops.includes("0%"), stops.includes("100%"), stops.includes("from"), stops.includes("to")]).toEqual([
      false,
      false,
      false,
      false,
    ]);
  });

  it("stops the blink under reduced motion", () => {
    // Arrange
    const reduced = stylesheet
      .split("@media (prefers-reduced-motion: reduce)")
      .slice(1)
      .some((block: string) => /#ws-sidebar \.ws\.selection-blink > \.row \{\s*animation: none;/.test(block));

    // Assert
    expect(reduced).toBe(true);
  });
});
