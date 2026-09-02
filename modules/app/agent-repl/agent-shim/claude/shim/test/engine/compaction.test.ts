/**
 * The compaction writer, asserted against OBSERVED transcript lines.
 *
 * WHAT THIS GUARDS: that the two records the shim appends have the shape the
 * vendor's own loader understands. The failure mode being excluded is a written
 * line composed from a guessed field union — the CLI's per-line requirements
 * are internal and undocumented, and a wrong field makes the transcript
 * unloadable, which loses the conversation this whole module exists to shorten
 * rather than destroy.
 */
import { mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import {
  appendCompactionLines,
  COMPACT_BOUNDARY_CONTENT,
  COMPACT_SUMMARY_PREFIX,
  compactionLines,
  compactionPrompt,
  contextCleared,
  contextCutCompacted,
  contextCutFailed,
  readAmbient,
} from "../../src/engine/compaction.js";

/** The ambient fields every observed line of one real transcript carries. */
const OBSERVED_AMBIENT = {
  userType: "external",
  entrypoint: "sdk-cli",
  cwd: "/Users/x/.config/doom-worktrees/w",
  sessionId: "4ebb8f3a-05d9-4952-88ca-16eade326d1d",
  version: "2.1.226",
  gitBranch: "DWC/w",
  slug: "golden-sleeping-kettle",
};

function transcript(lines: unknown[]): string {
  const dir = mkdtempSync(path.join(os.tmpdir(), "shim-compaction-"));
  const file = path.join(dir, "session.jsonl");
  writeFileSync(file, `${lines.map((line) => JSON.stringify(line)).join("\n")}\n`, "utf8");
  return file;
}

function lines(): { boundary: Record<string, unknown>; summary: Record<string, unknown> } {
  let minted = 0;
  return compactionLines({
    ambient: { ...OBSERVED_AMBIENT, lastUuid: "b60c9557-0ce9-400c-a845-ee915c0a2315" },
    summary: "what happened",
    preTokens: 435029,
    postTokens: 8639,
    durationMs: 194511,
    trigger: "manual",
    atMs: Date.parse("2026-08-09T23:31:32.812Z"),
    newUuid: () => `uuid-${++minted}`,
  });
}

describe("the summarizing instruction", () => {
  it("asks for the whole conversation on the ALL scope", () => {
    expect(compactionPrompt(conversationv1.SessionCompactScope.ALL)).toMatch(/in full/);
  });

  it("asks only about what the user asked for on the PROMPTS scope", () => {
    expect(compactionPrompt(conversationv1.SessionCompactScope.PROMPTS)).toMatch(/ONLY what the user asked/);
  });

  it("asks only about what the assistant did on the RESPONSES scope", () => {
    expect(compactionPrompt(conversationv1.SessionCompactScope.RESPONSES)).toMatch(
      /ONLY what the assistant did/,
    );
  });
});

describe("reading the transcript's ambient fields", () => {
  it("takes them from the LAST line, because a branch and a slug change over a conversation", () => {
    const file = transcript([
      { ...OBSERVED_AMBIENT, uuid: "u-1", gitBranch: "old" },
      { ...OBSERVED_AMBIENT, uuid: "u-2", gitBranch: "new" },
    ]);

    expect(readAmbient(file).gitBranch).toBe("new");
  });

  it("names the last record's uuid as the boundary's logical parent", () => {
    const file = transcript([{ ...OBSERVED_AMBIENT, uuid: "u-2" }]);

    expect(readAmbient(file).lastUuid).toBe("u-2");
  });

  it("skips an unparsable line rather than failing the compaction", () => {
    const dir = mkdtempSync(path.join(os.tmpdir(), "shim-compaction-"));
    const file = path.join(dir, "torn.jsonl");
    writeFileSync(file, `${JSON.stringify({ ...OBSERVED_AMBIENT, uuid: "u-1" })}\n{"broken`, "utf8");

    expect(readAmbient(file).lastUuid).toBe("u-1");
  });
});

describe("the compact_boundary record", () => {
  it("has a NULL parentUuid — the chain restarts here", () => {
    expect(lines().boundary.parentUuid).toBeNull();
  });

  it("names its predecessor with logicalParentUuid", () => {
    expect(lines().boundary.logicalParentUuid).toBe("b60c9557-0ce9-400c-a845-ee915c0a2315");
  });

  it("carries the observed content string verbatim", () => {
    expect(lines().boundary.content).toBe(COMPACT_BOUNDARY_CONTENT);
  });

  it("is a system line of the compact_boundary subtype", () => {
    expect([lines().boundary.type, lines().boundary.subtype]).toEqual(["system", "compact_boundary"]);
  });

  it("carries the observed compactMetadata figures", () => {
    expect(lines().boundary.compactMetadata).toEqual({
      trigger: "manual",
      preTokens: 435029,
      postTokens: 8639,
      durationMs: 194511,
    });
  });

  it("carries the level the observed record carries", () => {
    expect(lines().boundary.level).toBe("info");
  });

  it("COPIES the ambient fields rather than composing them", () => {
    const boundary = lines().boundary;

    expect({
      userType: boundary.userType,
      entrypoint: boundary.entrypoint,
      cwd: boundary.cwd,
      sessionId: boundary.sessionId,
      version: boundary.version,
      gitBranch: boundary.gitBranch,
      slug: boundary.slug,
    }).toEqual(OBSERVED_AMBIENT);
  });
});

describe("the summary record", () => {
  it("is parented to the boundary", () => {
    const written = lines();

    expect(written.summary.parentUuid).toBe(written.boundary.uuid);
  });

  it("is marked isCompactSummary, which is how the loader recognizes it", () => {
    expect(lines().summary.isCompactSummary).toBe(true);
  });

  it("is marked transcript-only, so it is not replayed as a user prompt", () => {
    expect(lines().summary.isVisibleInTranscriptOnly).toBe(true);
  });

  it("opens with the vendor's own continuation sentence", () => {
    const message = lines().summary.message as { content: string };

    expect(message.content).toBe(`${COMPACT_SUMMARY_PREFIX}what happened`);
  });

  it("is a user record", () => {
    expect([lines().summary.type, (lines().summary.message as { role: string }).role]).toEqual([
      "user",
      "user",
    ]);
  });
});

describe("appending them", () => {
  it("APPENDS rather than truncating — the boundary is a marker, not a deletion", () => {
    const file = transcript([{ ...OBSERVED_AMBIENT, uuid: "u-1" }]);

    appendCompactionLines(file, lines());

    expect(readFileSync(file, "utf8").split("\n").filter(Boolean)).toHaveLength(3);
  });

  it("writes one JSON object per line", () => {
    const file = transcript([{ ...OBSERVED_AMBIENT, uuid: "u-1" }]);

    appendCompactionLines(file, lines());

    const written = readFileSync(file, "utf8").split("\n").filter(Boolean);
    expect((JSON.parse(written[2] ?? "{}") as { isCompactSummary?: boolean }).isCompactSummary).toBe(true);
  });
});

describe("the context_cut page line", () => {
  it("carries the summary, the token delta and the duration", () => {
    const cut = contextCutCompacted({
      summary: "what happened",
      tokensBefore: 100,
      tokensAfter: 10,
      durationMs: 5,
      requested: true,
    });

    expect(
      cut.cut.case === "compacted"
        ? {
            summary: cut.cut.value.summary?.markdown,
            before: cut.cut.value.tokens?.tokensBefore,
            after: cut.cut.value.tokens?.tokensAfter,
            duration: cut.cut.value.durationMs,
            trigger: cut.cut.value.trigger.case,
          }
        : undefined,
    ).toEqual({ summary: "what happened", before: 100n, after: 10n, duration: 5n, trigger: "requested" });
  });

  it("marks a compaction nobody requested as automatic", () => {
    const cut = contextCutCompacted({
      summary: "s",
      tokensBefore: 1,
      tokensAfter: 1,
      durationMs: 1,
      requested: false,
    });

    expect(cut.cut.case === "compacted" ? cut.cut.value.trigger.case : undefined).toBe("automatic");
  });

  it("carries the vendor's own wording on a failure", () => {
    const cut = contextCutFailed("the summarizing session died");

    expect(cut.cut.case === "compactionFailed" ? cut.cut.value.error : undefined).toBe(
      "the summarizing session died",
    );
  });

  it("leaves ContextCleared.tokens unset — nothing declares how many a clear discarded", () => {
    const cut = contextCleared();

    expect(cut.cut.case === "cleared" ? Object.keys(cut.cut.value) : undefined).not.toContain("tokens");
  });
});
