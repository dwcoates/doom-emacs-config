/**
 * The plain-prose family: the default turn and the markdown showcase.
 */
import { describe, expect, it } from "vitest";

import { MARKDOWN_SHOWCASE } from "../../../src/fake/scenarios/prose.js";
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

  it("emits exactly one block, unlike the default turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!md"]);

    // Assert
    expect(blockTypes(driven)).toEqual(["text"]);
  });
});
