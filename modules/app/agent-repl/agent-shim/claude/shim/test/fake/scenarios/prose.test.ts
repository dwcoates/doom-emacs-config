/**
 * The plain-prose family: the default turn and the markdown showcase.
 */
import { describe, expect, it } from "vitest";

import { DAEMON_TREE_WRAP_COLUMNS, MARKDOWN_SHOWCASE } from "../../../src/fake/scenarios/prose.js";
import { driveScenario, ofType, recordsOfType, theResult } from "../harness.js";

const blockTypes = (driven: Awaited<ReturnType<typeof driveScenario>>): string[] =>
  ofType(driven, "assistant").map(
    (a) => ((a.message as { content: { type: string }[] }).content[0]?.type ?? ""),
  );

describe("the default prose turn", () => {
  it("answers any unrecognized text rather than refusing", async () => {
    // Arrange + Act
    const driven = await driveScenario(["what is the plan?"]);

    // Assert
    expect(theResult(driven).subtype).toBe("success");
  });

  it("emits both thinking arms and two text blocks, in that order", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);

    // Assert
    expect(blockTypes(driven)).toEqual(["thinking", "thinking", "text", "text"]);
  });

  it("withholds the FIRST thinking block's reasoning while keeping its signature", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);
    const first = (ofType(driven, "assistant")[0]?.message as {
      content: { thinking: string; signature: string }[];
    }).content[0];

    // Assert
    expect({ thinking: first?.thinking, signed: (first?.signature ?? "").length > 0 }).toEqual({
      thinking: "",
      signed: true,
    });
  });

  it("surfaces the SECOND thinking block's reasoning", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);
    const second = (ofType(driven, "assistant")[1]?.message as {
      content: { thinking: string }[];
    }).content[0];

    // Assert
    expect(second?.thinking).not.toBe("");
  });

  it("names the settled answer by the turn's LAST text block, verbatim", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);
    const last = (ofType(driven, "assistant").at(-1)?.message as { content: { text: string }[] })
      .content[0]?.text;

    // Assert. Two spellings that disagreed would let a consumer pick either.
    expect(theResult(driven).result).toBe(last);
  });

  it("reports the permission mode and model in the conclusion, so a switch is observable", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"], { opts: { model: "fake-sonnet-5" } });

    // Assert
    expect(theResult(driven).result).toBe("echo: hello [mode=default] [model=fake-sonnet-5]");
  });

  it("stamps the transcript's assistant lines with the effort level", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);

    // Assert
    expect(recordsOfType(driven.transcript(), "assistant").map((l) => l.effort)).toEqual([
      "high",
      "high",
      "high",
      "high",
    ]);
  });
});

describe("the markdown showcase", () => {
  it("answers with the whole showcase as one text block", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!md"]);

    // Assert
    expect(theResult(driven).result).toBe(MARKDOWN_SHOWCASE);
  });

  it("carries a tree branch wider than the daemon's wrap width, so the wrap is exercised", () => {
    // Arrange — the branch lines are the ones opening with a connector.
    const branches = MARKDOWN_SHOWCASE.split("\n").filter((line) => /^[│ ]*[├└]── /u.test(line));

    // Act
    const widest = Math.max(...branches.map((line) => [...line].length));

    // Assert — a showcase every branch of which fits would never make the
    // daemon wrap anything, and the wrapped tree is what the webapp is
    // photographed drawing.
    expect(branches.length).toBeGreaterThan(0);
    expect(widest).toBeGreaterThan(DAEMON_TREE_WRAP_COLUMNS);
  });

  it("emits one PROSE block, behind the reasoning every turn opens with", async () => {
    // The vendor's closing API response is `[thinking, text]` in every capture,
    // so the showcase is one TEXT block rather than one block full stop.
    // Arrange + Act
    const driven = await driveScenario(["!md"]);

    // Assert
    expect(blockTypes(driven)).toEqual(["thinking", "text"]);
  });
});
