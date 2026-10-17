// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import {
  SELECTION_BLINK_CLASS,
  SELECTION_BLINK_DELAY_PROPERTY,
  SELECTION_BLINK_DURATION_PROPERTY,
  SELECTION_BLINK_MS,
  SELECTION_ANIMATIONS,
  SELECTION_NUMBER_FADE_CLASS,
  SELECTION_NUMBER_FADE_DELAY_PROPERTY,
  SELECTION_NUMBER_FADE_DURATION_PROPERTY,
  SELECTION_NUMBER_FADE_MS,
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
const fading = (row: HTMLElement): boolean => row.classList.contains(SELECTION_NUMBER_FADE_CLASS);
const fadeDelayOf = (row: HTMLElement): string => row.style.getPropertyValue(SELECTION_NUMBER_FADE_DELAY_PROPERTY);

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

  it("runs one blink for 450 ms", () => {
    // Arrange, Act, Assert
    expect(SELECTION_BLINK_MS).toBe(450);
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
    const elapsed = SELECTION_BLINK_MS / 2;
    clock += elapsed;
    const redrawn = document.createElement("div");

    // Act
    blink.apply("ws-1", redrawn);

    // Assert
    expect([blinking(redrawn), delayOf(redrawn)]).toEqual([true, `-${elapsed}ms`]);
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

  it("fades the selected row's number from its start alongside the blink", () => {
    // Arrange
    const row = drawnRow(root, "ws-1");

    // Act
    blink.start("ws-1");

    // Assert
    expect([fading(row), fadeDelayOf(row)]).toEqual([true, "0ms"]);
  });

  it("runs the number fade for 1600 ms", () => {
    // Arrange, Act, Assert
    expect(SELECTION_NUMBER_FADE_MS).toBe(1600);
  });

  it("hands a fading row the fade's length", () => {
    // Arrange
    const row = drawnRow(root, "ws-1");

    // Act
    blink.start("ws-1");

    // Assert
    expect(row.style.getPropertyValue(SELECTION_NUMBER_FADE_DURATION_PROPERTY)).toBe(`${SELECTION_NUMBER_FADE_MS}ms`);
  });

  it("joins a row redrawn after the blink but mid-fade to the fade alone", () => {
    // Arrange
    blink.start("ws-1");
    const elapsed = SELECTION_BLINK_MS + 100;
    clock += elapsed;
    const redrawn = document.createElement("div");

    // Act
    blink.apply("ws-1", redrawn);

    // Assert
    expect([blinking(redrawn), delayOf(redrawn), fading(redrawn), fadeDelayOf(redrawn)]).toEqual([
      false,
      "",
      true,
      `-${elapsed}ms`,
    ]);
  });

  it("draws a row's number still once its fade has run its length", () => {
    // Arrange
    blink.start("ws-1");
    clock += SELECTION_NUMBER_FADE_MS;
    const redrawn = document.createElement("div");

    // Act
    blink.apply("ws-1", redrawn);

    // Assert
    expect([blinking(redrawn), fading(redrawn)]).toEqual([false, false]);
  });

  it("restarts both animations of a row reselected mid-fade", () => {
    // Arrange
    const row = drawnRow(root, "ws-1");
    blink.start("ws-1");
    clock += SELECTION_BLINK_MS + 100;
    blink.apply("ws-1", row);

    // Act
    blink.start("ws-1");

    // Assert
    expect([blinking(row), delayOf(row), fading(row), fadeDelayOf(row)]).toEqual([true, "0ms", true, "0ms"]);
  });

  it("runs every selection animation under its own class and properties", () => {
    // Arrange / Act
    const shapes = SELECTION_ANIMATIONS.map((a) => [a.className, a.durationProperty, a.delayProperty, a.ms]);

    // Assert
    expect(shapes).toEqual([
      [SELECTION_BLINK_CLASS, SELECTION_BLINK_DURATION_PROPERTY, SELECTION_BLINK_DELAY_PROPERTY, SELECTION_BLINK_MS],
      [
        SELECTION_NUMBER_FADE_CLASS,
        SELECTION_NUMBER_FADE_DURATION_PROPERTY,
        SELECTION_NUMBER_FADE_DELAY_PROPERTY,
        SELECTION_NUMBER_FADE_MS,
      ],
    ]);
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

  it("animates the fading row's number on the fade's length and delay, once", () => {
    // Arrange / Act
    const declarations = declarationsOf(`#ws-sidebar .ws.${SELECTION_NUMBER_FADE_CLASS} > .row .sb-num`);

    // Assert
    expect(declarations).toContainEqual(
      expect.stringMatching(
        /animation:\s*ws-selection-number-fade var\(--selection-number-fade-duration\) ease-in-out var\(--selection-number-fade-delay\) 1;/,
      ),
    );
  });

  it("holds the number white from a quick rise until a second later", () => {
    // Arrange / Act
    const stops = rulesOf(keyframesOf("ws-selection-number-fade"));

    // Assert
    const [whiteFromMs = NaN, whiteUntilMs = NaN] = (stops[0]?.selectors ?? []).map(
      (stop) => (parseFloat(stop) / 100) * SELECTION_NUMBER_FADE_MS,
    );
    expect([stops.length, whiteFromMs, whiteUntilMs - whiteFromMs, stops[0]?.declarations.trim()]).toEqual([
      1,
      160,
      1000,
      "color: white;",
    ]);
  });

  it("stops the number fade under reduced motion", () => {
    // Arrange
    const reduced = stylesheet
      .split("@media (prefers-reduced-motion: reduce)")
      .slice(1)
      .some((block: string) =>
        /#ws-sidebar \.ws\.selection-number-fade > \.row \.sb-num \{\s*animation: none;/.test(block),
      );

    // Assert
    expect(reduced).toBe(true);
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
