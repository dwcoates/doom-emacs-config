/**
 * The file-tool family: Read, Write, Edit, Grep, Glob, and the IDE diagnostics
 * that join an edit by adjacency.
 *
 * Each test names ONE thing that distinguishes its arm from its siblings — the
 * `startLine` that separates a range read from a head read, the `type` that
 * separates a create from an update, the totals that make a grep or glob
 * partial. Asserting the whole payload would pass on any of them.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, recordsOfType, toolUseResults, toolUses } from "../harness.js";

const firstResult = async (prompt: string): Promise<Record<string, unknown>> => {
  const driven = await driveScenario([prompt]);
  return toolUseResults(driven.transcript())[0] as Record<string, unknown>;
};

describe("Read", () => {
  it("asks for the whole file with neither offset nor limit", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!read"]);

    // Assert
    expect(Object.keys((toolUses(driven)[0] as { input: object }).input)).toEqual(["file_path"]);
  });

  it("answers a whole read with numLines equal to totalLines", async () => {
    // Arrange + Act
    const file = (await firstResult("!read")).file as Record<string, number>;

    // Assert
    expect(file.numLines).toBe(file.totalLines);
  });

  it("answers a head read starting at line 1 with fewer lines than the total", async () => {
    // Arrange + Act
    const file = (await firstResult("!read-head")).file as Record<string, number>;

    // Assert
    expect({ start: file.startLine, partial: (file.numLines ?? 0) < (file.totalLines ?? 0) }).toEqual({
      start: 1,
      partial: true,
    });
  });

  it("answers a range read starting somewhere other than line 1", async () => {
    // Arrange + Act
    const file = (await firstResult("!read-range")).file as Record<string, number>;

    // Assert. The startLine is the ONLY thing separating a range from a head.
    expect(file.startLine).toBe(2);
  });

  it("marks the truncated read `truncatedByTokenCap`, the arm's only signal", async () => {
    // Arrange + Act
    const result = await firstResult("!read-truncated");

    // Assert. Without this the fixture declares a token cap and yields a line
    // cap: the converter reads this field and nothing else for that arm.
    expect((result.file as { truncatedByTokenCap?: boolean }).truncatedByTokenCap).toBe(true);
  });

  it("leaves the LINE-capped head unmarked, which is what separates the two cuts", async () => {
    // Arrange + Act
    const result = await firstResult("!read-head");

    // Assert
    expect((result.file as { truncatedByTokenCap?: boolean }).truncatedByTokenCap).toBeUndefined();
  });

  it("carries the truncation notice as an attachment naming the call", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!read-truncated"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as Record<
      string,
      unknown
    >;

    // Assert
    expect({
      type: attachment.type,
      joined: attachment.toolUseID === (toolUses(driven)[0] as { id: string }).id,
    }).toEqual({ type: "read_truncation_notice", joined: true });
  });

  it("answers an image read with an image toolUseResult carrying dimensions", async () => {
    // Arrange + Act
    const result = await firstResult("!read-image");

    // Assert
    expect({
      type: result.type,
      dimensions: ((result.file as { dimensions?: object }).dimensions ?? null) !== null,
    }).toEqual({ type: "image", dimensions: true });
  });

  it("puts an image CONTENT BLOCK on the tool_result, not prose", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!read-image"]);
    const userLine = recordsOfType(driven.transcript(), "user").find(
      (l) => l.toolUseResult !== undefined,
    );
    const content = (userLine?.message as { content: { content: { type: string }[] }[] }).content[0]
      ?.content;

    // Assert
    expect(content?.[0]?.type).toBe("image");
  });
});

describe("Write", () => {
  it("reports a create with an EMPTY structuredPatch and no prior body", async () => {
    // Arrange + Act
    const result = await firstResult("!write-create");

    // Assert
    expect({
      type: result.type,
      patch: (result.structuredPatch as unknown[]).length,
      prior: result.originalFile,
    }).toEqual({ type: "create", patch: 0, prior: null });
  });

  it("reports an update with the prior body and a non-empty structuredPatch", async () => {
    // Arrange + Act
    const result = await firstResult("!write-update");

    // Assert
    expect({
      type: result.type,
      patch: (result.structuredPatch as unknown[]).length,
      hasPrior: typeof result.originalFile === "string",
    }).toEqual({ type: "update", patch: 1, hasPrior: true });
  });
});

describe("Edit", () => {
  it("answers with the corpus's edit shape rather than a write shape", async () => {
    // Arrange + Act
    const result = await firstResult("!edit");

    // Assert. An edit result names the two strings; a write result names a type.
    expect(Object.keys(result).sort()).toEqual(
      ["filePath", "newString", "oldString", "originalFile", "replaceAll", "structuredPatch", "userModified"].sort(),
    );
  });
});

describe("IDE diagnostics", () => {
  it("follows the edit with a diagnostics attachment naming the edited file", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ide-diagnostics"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      type: string;
      files: { uri: string }[];
    };

    // Assert. The join is ADJACENCY — one remembered value, no id in the record.
    expect({ type: attachment.type, uri: attachment.files[0]?.uri }).toEqual({
      type: "diagnostics",
      uri: "/w/s/example.ts",
    });
  });

  it("states a REAL hunk for the edit the diagnostics concern", async () => {
    // Arrange + Act. An empty structuredPatch is a shape the vendor never
    // sends (testdata/captures/ide-diagnostics-after-edit states the changed
    // line with one context line either side), and it made this the one edit
    // whose diff a consumer could not draw.
    const result = (await firstResult("!ide-diagnostics")) as {
      structuredPatch: { oldStart: number; lines: string[] }[];
    };

    // Assert
    expect(result.structuredPatch[0]?.lines).toEqual([
      " export const two = 2;",
      "-export const three = 3;",
      "+export const three = missing;",
      " export const four = 4;",
    ]);
  });

  it("places the attachment AFTER the edit's tool result, so adjacency resolves", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ide-diagnostics"]);
    const types = driven.transcript().map((l) => l.type);
    const resultIndex = driven.transcript().findIndex((l) => l.toolUseResult !== undefined);

    // Assert
    expect(types.indexOf("attachment")).toBeGreaterThan(resultIndex);
  });
});

describe("Grep", () => {
  it("reports content mode with a total that EXCEEDS the returned lines", async () => {
    // Arrange + Act
    const result = await firstResult("!grep-content");

    // Assert. The omitted figure the converter renders is the difference; equal
    // values could never reach the partial arm.
    expect({ mode: result.mode, partial: (result.totalLines as number) > (result.numLines as number) }).toEqual({
      mode: "content",
      partial: true,
    });
  });

  it("reports files mode with paths and no content", async () => {
    // Arrange + Act
    const result = await firstResult("!grep-files");

    // Assert
    expect({ mode: result.mode, content: result.content }).toEqual({
      mode: "files_with_matches",
      content: undefined,
    });
  });

  it("reports count mode with a match count", async () => {
    // Arrange + Act
    const result = await firstResult("!grep-count");

    // Assert
    expect({ mode: result.mode, matches: result.numMatches }).toEqual({ mode: "count", matches: 4 });
  });
});

describe("Glob", () => {
  it("reports a truncated result whose total exceeds the returned count", async () => {
    // Arrange + Act
    const result = await firstResult("!glob");

    // Assert
    expect({
      truncated: result.truncated,
      omitted: (result.totalMatches as number) - (result.numFiles as number),
    }).toEqual({ truncated: true, omitted: 5 });
  });

  it("marks the total EXACT, which is what separates the two omitted arms", async () => {
    // Arrange + Act
    const result = await firstResult("!glob");

    // Assert
    expect(result.countIsComplete).toBe(true);
  });
});
