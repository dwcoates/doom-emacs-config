// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import {
  SELECTION_ANIMATIONS,
  SELECTION_NUMBER_FADE_CLASS,
  SELECTION_NUMBER_FADE_DELAY_PROPERTY,
  SELECTION_NUMBER_FADE_DURATION_PROPERTY,
  SELECTION_NUMBER_FADE_MS,
  createSelectionFade,
  type SelectionFade,
} from "../../src/sidebar/selection-fade.js";
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

const fading = (row: HTMLElement): boolean => row.classList.contains(SELECTION_NUMBER_FADE_CLASS);
const fadeDelayOf = (row: HTMLElement): string => row.style.getPropertyValue(SELECTION_NUMBER_FADE_DELAY_PROPERTY);

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

  it("fades every drawn copy of the selected workspace's number from its start", () => {
    // Arrange
    const copies = [drawnRow(root, "ws-1"), drawnRow(root, "ws-1")];

    // Act
    fade.start("ws-1");

    // Assert
    expect(copies.map((row) => [fading(row), fadeDelayOf(row)])).toEqual([
      [true, "0ms"],
      [true, "0ms"],
    ]);
  });

  it("runs the number fade for 1600 ms", () => {
    // Arrange, Act, Assert
    expect(SELECTION_NUMBER_FADE_MS).toBe(1600);
  });

  it("hands a fading row the fade's length", () => {
    // Arrange
    const row = drawnRow(root, "ws-1");

    // Act
    fade.start("ws-1");

    // Assert
    expect(row.style.getPropertyValue(SELECTION_NUMBER_FADE_DURATION_PROPERTY)).toBe(`${SELECTION_NUMBER_FADE_MS}ms`);
  });

  it("leaves another workspace's row still", () => {
    // Arrange
    const other = drawnRow(root, "ws-2");

    // Act
    fade.start("ws-1");

    // Assert
    expect(fading(other)).toBe(false);
  });

  it("restarts a row already fading when it is selected again", () => {
    // Arrange
    const row = drawnRow(root, "ws-1");
    fade.start("ws-1");
    clock += 700;

    // Act
    fade.start("ws-1");

    // Assert
    expect([fading(row), fadeDelayOf(row)]).toEqual([true, "0ms"]);
  });

  it("joins a row redrawn mid-fade at the elapsed time", () => {
    // Arrange
    fade.start("ws-1");
    const elapsed = SELECTION_NUMBER_FADE_MS / 2;
    clock += elapsed;
    const redrawn = document.createElement("div");

    // Act
    fade.apply("ws-1", redrawn);

    // Assert
    expect([fading(redrawn), fadeDelayOf(redrawn)]).toEqual([true, `-${elapsed}ms`]);
  });

  it("draws a row's number still once its fade has run its length", () => {
    // Arrange
    fade.start("ws-1");
    clock += SELECTION_NUMBER_FADE_MS;
    const redrawn = document.createElement("div");

    // Act
    fade.apply("ws-1", redrawn);

    // Assert
    expect(fading(redrawn)).toBe(false);
  });

  it("draws a never-selected row still", () => {
    // Arrange
    const drawn = document.createElement("div");

    // Act
    fade.apply("ws-1", drawn);

    // Assert
    expect([fading(drawn), fadeDelayOf(drawn)]).toEqual([false, ""]);
  });

  it("runs the number fade alone, under its own class and properties", () => {
    // Arrange / Act
    const shapes = SELECTION_ANIMATIONS.map((a) => [a.className, a.durationProperty, a.delayProperty, a.ms]);

    // Assert
    expect(shapes).toEqual([
      [
        SELECTION_NUMBER_FADE_CLASS,
        SELECTION_NUMBER_FADE_DURATION_PROPERTY,
        SELECTION_NUMBER_FADE_DELAY_PROPERTY,
        SELECTION_NUMBER_FADE_MS,
      ],
    ]);
  });

  it("states the workspace and how many copies fade, in the canonical log", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    drawnRow(root, "ws-1");

    // Act
    fade.start("ws-1");

    // Assert
    const record = await forwardedRecord(capture, "sidebar.selection-fade");
    expect(record.context).toMatchObject({ workspace: "ws-1", drawn: 1 });
  });
});

describe("the selection fade's styling", () => {
  /** The declarations of every rule whose selector names SELECTOR. */
  const declarationsOf = (selector: string): string[] =>
    rulesOf(stylesheet)
      .filter((rule) => rule.selectors.includes(selector))
      .map((rule) => rule.declarations);

  it("leaves the selected row's line still: the stylesheet carries no selection wash", () => {
    // Arrange / Act / Assert
    expect(stylesheet).not.toContain("selection-blink");
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
});
