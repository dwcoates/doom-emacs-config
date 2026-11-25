// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import {
  FOLD_FADE_DELAY_PROPERTY,
  FOLD_FADE_DURATION_PROPERTY,
  FOLD_FADE_MS,
  FOLD_FADING_CLASS,
  createFoldFade,
} from "../../src/sidebar/fold-fade.js";
import type { FoldFade } from "../../src/sidebar/context.js";
import { SURFACED_ROW_CLASS } from "../../src/sidebar/row.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

/** A row freshly drawn for ID, in ROOT. */
const drawn = (root: HTMLElement, id: string): HTMLElement => {
  const ws = document.createElement("div");
  ws.className = "ws";
  ws.setAttribute("data-roster-row", id);
  root.appendChild(ws);
  return ws;
};

/** Whether ROW is drawn fading, and at what delay: "" when it is not. */
const fadingOn = (row: HTMLElement): string =>
  row.classList.contains(FOLD_FADING_CLASS) && row.classList.contains(SURFACED_ROW_CLASS)
    ? row.style.getPropertyValue(FOLD_FADE_DELAY_PROPERTY)
    : "";

describe("the fold fade", () => {
  let root: HTMLElement;
  let clock: number;
  let timers: Array<{ fn: () => void; ms: number }>;
  let fade: FoldFade;

  beforeEach(() => {
    root = document.createElement("div");
    document.body.appendChild(root);
    clock = 1_000;
    timers = [];
    fade = createFoldFade(root, () => clock, (fn, ms) => timers.push({ fn, ms }));
  });

  afterEach(() => {
    root.remove();
  });

  it("fades a row drawn unsurfaced after it was drawn surfaced", () => {
    // Arrange
    fade.apply("ws-1", drawn(root, "ws-1"), true);
    const row = drawn(root, "ws-1");

    // Act
    fade.apply("ws-1", row, false);

    // Assert
    expect([fadingOn(row), row.style.getPropertyValue(FOLD_FADE_DURATION_PROPERTY)]).toEqual(["0ms", `${FOLD_FADE_MS}ms`]);
  });

  it("leaves a row never drawn surfaced still", () => {
    // Arrange
    const row = drawn(root, "ws-1");

    // Act
    fade.apply("ws-1", row, false);

    // Assert
    expect(fadingOn(row)).toBe("");
  });

  it("draws a row redrawn mid-fade into the fade", () => {
    // Arrange
    fade.apply("ws-1", drawn(root, "ws-1"), true);
    fade.apply("ws-1", drawn(root, "ws-1"), false);
    clock += 2_000;
    const row = drawn(root, "ws-1");

    // Act
    fade.apply("ws-1", row, false);

    // Assert
    expect(fadingOn(row)).toBe("-2000ms");
  });

  it("ends the fade after its length on every drawn copy", () => {
    // Arrange
    fade.apply("ws-1", drawn(root, "ws-1"), true);
    const row = drawn(root, "ws-1");
    fade.apply("ws-1", row, false);

    // Act
    for (const timer of timers) timer.fn();

    // Assert
    expect([timers.map((t) => t.ms), fadingOn(row), row.classList.contains(SURFACED_ROW_CLASS)]).toEqual([
      [FOLD_FADE_MS],
      "",
      false,
    ]);
  });

  it("stops the fade of a row drawn surfaced again", () => {
    // Arrange
    fade.apply("ws-1", drawn(root, "ws-1"), true);
    fade.apply("ws-1", drawn(root, "ws-1"), false);
    const row = drawn(root, "ws-1");
    row.classList.add(SURFACED_ROW_CLASS);

    // Act
    fade.apply("ws-1", row, true);

    // Assert
    expect([row.classList.contains(FOLD_FADING_CLASS), row.classList.contains(SURFACED_ROW_CLASS)]).toEqual([false, true]);
  });

  it("leaves a later fade running when an earlier fade's end comes", () => {
    // Arrange
    fade.apply("ws-1", drawn(root, "ws-1"), true);
    fade.apply("ws-1", drawn(root, "ws-1"), false);
    clock += 1_000;
    fade.apply("ws-1", drawn(root, "ws-1"), true);
    clock += 1_000;
    const row = drawn(root, "ws-1");
    fade.apply("ws-1", row, false);

    // Act
    timers[0].fn();

    // Assert
    expect(fadingOn(row)).toBe("0ms");
  });

  it("records the fade it starts", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    fade.apply("ws-1", drawn(root, "ws-1"), true);

    // Act
    fade.apply("ws-1", drawn(root, "ws-1"), false);

    // Assert
    const record = await forwardedRecord(capture, "sidebar.fold-fade");
    expect(record.context).toMatchObject({ workspace: "ws-1", ms: FOLD_FADE_MS });
  });
});
