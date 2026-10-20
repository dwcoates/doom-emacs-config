// How long a submitted prompt took to be drawn. See src/prompt-echo.ts.
import { describe, expect, it } from "vitest";

import { ReportedLatencyKind } from "../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import { createPromptEcho, installPromptEcho, PROMPT_ECHO_MEMORY, userPromptDrawn } from "../src/prompt-echo.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

type Recorded = [ReportedLatencyKind, number];

/** A timing on a wall clock the test moves. */
function timing(): { clock: { now: number }; recorded: Recorded[]; echo: ReturnType<typeof createPromptEcho> } {
  const clock = { now: 0 };
  const recorded: Recorded[] = [];
  const echo = createPromptEcho(() => clock.now, (kind, seconds) => recorded.push([kind, seconds]));
  return { clock, recorded, echo };
}

describe("createPromptEcho", () => {
  it("measures from the submit instant to the row drawn after the echo", () => {
    // Arrange
    const { clock, recorded, echo } = timing();
    echo.echoed("turn-7", 1_000);

    // Act
    clock.now = 1_080;
    echo.drawn("turn-7");

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.PROMPT_ECHO, 0.08]]);
  });

  it("measures to the row's draw when the row arrived before the echo", () => {
    // Arrange
    const { clock, recorded, echo } = timing();
    clock.now = 1_050;
    echo.drawn("turn-7");

    // Act
    clock.now = 1_400;
    echo.echoed("turn-7", 1_000);

    // Assert
    expect(recorded).toEqual([[ReportedLatencyKind.PROMPT_ECHO, 0.05]]);
  });

  it("measures once, however often the row is redrawn or the echo repeated", () => {
    // Arrange
    const { clock, recorded, echo } = timing();
    echo.echoed("turn-7", 1_000);
    clock.now = 1_080;
    echo.drawn("turn-7");

    // Act
    clock.now = 2_000;
    echo.drawn("turn-7");
    echo.echoed("turn-7", 1_000);

    // Assert
    expect(recorded).toHaveLength(1);
  });

  it("measures nothing for a row drawn with no echo", () => {
    // Arrange
    const { recorded, echo } = timing();

    // Act
    echo.drawn("turn-history");

    // Assert
    expect(recorded).toEqual([]);
  });

  it("forgets the oldest draws past its bound, so only recent rows pair", () => {
    // Arrange
    const { recorded, echo } = timing();
    for (let i = 0; i <= PROMPT_ECHO_MEMORY; i += 1) echo.drawn(`turn-${i.toString()}`);

    // Act
    echo.echoed("turn-0", 0);
    echo.echoed(`turn-${PROMPT_ECHO_MEMORY.toString()}`, 0);

    // Assert
    expect(recorded).toHaveLength(1);
  });

  it("forgets the oldest echoes past its bound", () => {
    // Arrange
    const { recorded, echo } = timing();
    for (let i = 0; i <= PROMPT_ECHO_MEMORY; i += 1) echo.echoed(`turn-${i.toString()}`, 0);

    // Act
    echo.drawn("turn-0");
    echo.drawn(`turn-${PROMPT_ECHO_MEMORY.toString()}`);

    // Assert
    expect(recorded).toHaveLength(1);
  });

  it("reports nothing when the wall clock stepped back across the echo", () => {
    // Arrange
    const { clock, recorded, echo } = timing();
    echo.echoed("turn-7", 5_000);

    // Act
    clock.now = 4_000;
    echo.drawn("turn-7");

    // Assert
    expect(recorded).toEqual([]);
  });

  it("records each timed echo, naming the turn and which half came first", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { clock, echo } = timing();
    echo.echoed("turn-7", 1_000);

    // Act
    clock.now = 1_080;
    echo.drawn("turn-7");

    // Assert
    const record = await forwardedRecord(capture, "latency.prompt-echo");
    expect(record.level.case).toBe("debug");
    expect(JSON.stringify(record)).toContain("turn-7");
    expect(JSON.stringify(record)).toContain("echo");
  });
});

describe("userPromptDrawn", () => {
  it.each([
    ["a turn reaches the installed timing", "turn-7", ["turn-7"]],
    ["a row with no turn reaches nothing", undefined, []],
    ["an empty turn reaches nothing", "", []],
  ])("%s", (_name, turn, want) => {
    // Arrange
    const drawn: string[] = [];
    const uninstall = installPromptEcho({ echoed: () => undefined, drawn: (t) => drawn.push(t) });

    // Act
    userPromptDrawn(turn);
    uninstall();

    // Assert
    expect(drawn).toEqual(want);
  });

  it("reaches nothing once the timing is uninstalled", () => {
    // Arrange
    const drawn: string[] = [];
    installPromptEcho({ echoed: () => undefined, drawn: (t) => drawn.push(t) })();

    // Act
    userPromptDrawn("turn-7");

    // Assert
    expect(drawn).toEqual([]);
  });
});
