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
import { readTranscriptFacts } from "../../src/engine/cold.js";

/** The ambient fields every observed line of one real transcript carries. */
const OBSERVED_AMBIENT = {
  userType: "external",
  entrypoint: "sdk-cli",
  cwd: "/Users/x/.config/doom-worktrees/w",
  sessionId: "4ebb8f3a-05d9-4952-88ca-16eade326d1d",
  version: "2.1.226",
  gitBranch: "ABC/w",
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
    permissionMode: "acceptEdits",
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

  it("states the SESSION'S permission mode, so a resume does not adopt the summarizer's plan mode", () => {
    expect(lines().summary.permissionMode).toBe("acceptEdits");
  });
});

describe("appending them", () => {
  it("APPENDS rather than truncating — the boundary is a marker, not a deletion", () => {
    const file = transcript([{ ...OBSERVED_AMBIENT, uuid: "u-1" }]);

    appendCompactionLines(file, lines());

    expect(readFileSync(file, "utf8").split("\n").filter(Boolean)).toHaveLength(3);
  });

  it("leaves the SESSION'S mode as the transcript's last word, not the summarizer's plan", () => {
    // The shape a hibernation actually produces: the user's own turn, then the
    // throwaway summarizing query's `plan` record — it resumes the same vendor
    // session id, so it writes into this very file — then the compaction's own
    // two records. A resume reads the LAST stated mode, so before the summary
    // line carried the session's mode this read answered "plan" and the
    // revived session came back in a mode nobody chose.
    const file = transcript([
      { ...OBSERVED_AMBIENT, type: "user", uuid: "u-1", permissionMode: "acceptEdits" },
      { ...OBSERVED_AMBIENT, type: "user", uuid: "u-2", permissionMode: "plan" },
    ]);

    appendCompactionLines(file, lines());

    expect(readTranscriptFacts(file)?.lastPermissionMode).toBe("acceptEdits");
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

describe("reading a transcript whose last line states almost nothing", () => {
  it("keeps the sessionId an earlier line stated, because the conversation did not change", () => {
    const file = transcript([{ ...OBSERVED_AMBIENT, uuid: "u-1" }, { type: "summary" }]);

    expect(readAmbient(file).sessionId).toBe(OBSERVED_AMBIENT.sessionId);
  });

  it("carries every field an EARLIER line stated, because a terse line states nothing about them", () => {
    // AMENDED, AND THE OLD ASSERTION WAS THE DEFECT. This used to require that
    // nothing but the session id survived a terse last line, on the reading
    // that "the ambient is rebuilt from the LAST line". In the field that cost
    // every one of 63 compactions its whole ambient — no cwd, no version, no
    // gitBranch, and no `logicalParentUuid` — because the CLI writes a
    // `last-prompt`/`summary` bookkeeping line at exactly the moment a
    // hibernation compacts (owner's workspace, 2026-09-14). A line that does
    // not state a field says nothing about it.
    const file = transcript([{ ...OBSERVED_AMBIENT, uuid: "u-1" }, { type: "summary" }]);

    expect(readAmbient(file)).toEqual({ ...OBSERVED_AMBIENT, lastUuid: "u-1" });
  });
});

describe("the two records written from a terse ambient", () => {
  const terse = (): { boundary: Record<string, unknown>; summary: Record<string, unknown> } => {
    let minted = 0;
    return compactionLines({
      ambient: { sessionId: "s-1" },
      summary: "what happened",
      preTokens: 1,
      postTokens: 0,
      durationMs: 2,
      trigger: "auto",
      atMs: Date.parse("2026-08-09T23:31:32.812Z"),
      permissionMode: "default",
      newUuid: () => `uuid-${++minted}`,
    });
  };

  it("omits every ambient field the transcript did not state, rather than writing empties", () => {
    const boundary = terse().boundary;

    expect(Object.keys(boundary).filter((key) => AMBIENT_KEYS.includes(key))).toEqual(["sessionId"]);
  });

  it("names no logicalParentUuid when the transcript has no last record to parent to", () => {
    expect("logicalParentUuid" in terse().boundary).toBe(false);
  });
});

const AMBIENT_KEYS = [
  "userType",
  "entrypoint",
  "cwd",
  "sessionId",
  "version",
  "gitBranch",
  "slug",
];

describe("the ambient fields a bookkeeping last line must not erase", () => {
  // THE VENDOR WRITES LINES THAT ARE NOT CONVERSATION RECORDS. `last-prompt`,
  // `queue-operation` and `summary` carry `sessionId` and little else, and a
  // compaction very often lands right after one: all 63 on the owner's
  // chess960-review-failures-enm transcript did (2026-09-14).
  const lastPrompt = {
    type: "last-prompt",
    lastPrompt: "Summarize this conversation in full",
    leafUuid: "11a2666e-5e78-458c-960e-6c4f14a3d4d7",
    sessionId: OBSERVED_AMBIENT.sessionId,
  };
  const said = {
    ...OBSERVED_AMBIENT,
    type: "assistant",
    uuid: "b60c9557-0ce9-400c-a845-ee915c0a2315",
  };

  it("keeps the workspace directory", () => {
    expect(readAmbient(transcript([said, lastPrompt])).cwd).toBe(OBSERVED_AMBIENT.cwd);
  });

  it("keeps the CLI version", () => {
    expect(readAmbient(transcript([said, lastPrompt])).version).toBe(OBSERVED_AMBIENT.version);
  });

  it("keeps the branch", () => {
    expect(readAmbient(transcript([said, lastPrompt])).gitBranch).toBe(OBSERVED_AMBIENT.gitBranch);
  });

  it("keeps the slug", () => {
    expect(readAmbient(transcript([said, lastPrompt])).slug).toBe(OBSERVED_AMBIENT.slug);
  });

  it("keeps the user type", () => {
    expect(readAmbient(transcript([said, lastPrompt])).userType).toBe(OBSERVED_AMBIENT.userType);
  });

  it("keeps the entrypoint", () => {
    expect(readAmbient(transcript([said, lastPrompt])).entrypoint).toBe(OBSERVED_AMBIENT.entrypoint);
  });

  it("keeps the last CHAIN NODE's uuid, not the bookkeeping line's absence of one", () => {
    // THE WORST OF THE SET. `logicalParentUuid` is how the vendor's own
    // boundary says where the chain restarts, and a boundary written without
    // it names no predecessor at all.
    expect(readAmbient(transcript([said, lastPrompt])).lastUuid).toBe(
      "b60c9557-0ce9-400c-a845-ee915c0a2315",
    );
  });

  it("still lets a later line CHANGE a field", () => {
    // The merge is "a line that does not state a field says nothing about it",
    // not "the first value wins": `gitBranch` and `slug` really do move.
    const moved = { ...said, gitBranch: "ABC/later", uuid: "c0000000-0000-4000-8000-000000000001" };

    expect(readAmbient(transcript([said, moved])).gitBranch).toBe("ABC/later");
  });

  it("writes a boundary carrying every field the vendor's own boundary carries", () => {
    // The observed vendor line is
    // `testdata/corpus/transcript-lines/system-compact_boundary.jsonl`.
    const ambient = readAmbient(transcript([said, lastPrompt]));

    const { boundary } = compactionLines({
      ambient,
      summary: "what happened",
      preTokens: 1,
      postTokens: 2,
      durationMs: 3,
      trigger: "manual",
      atMs: 0,
      permissionMode: "default",
      newUuid: () => "d0000000-0000-4000-8000-000000000001",
    });

    expect(
      [
        "logicalParentUuid",
        "userType",
        "entrypoint",
        "cwd",
        "sessionId",
        "version",
        "gitBranch",
        "slug",
      ].filter((field) => boundary[field] === undefined),
    ).toEqual([]);
  });
});
