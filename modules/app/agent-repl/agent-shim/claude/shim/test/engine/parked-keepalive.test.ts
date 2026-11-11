/**
 * A parked conversation's keep-alive: where it resumes, how its cache is
 * judged, and what one beat answers.
 *
 * WHAT THIS GUARDS: that a parked conversation is kept warm without ever
 * building on an earlier keep-alive, and without paying a cold read for a
 * cache that already lapsed.
 */
import path from "node:path";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import { testTempDir } from "../temp-dir.js";
import { writeTranscript } from "../vendor-transcript.js";
import { KEEPALIVE_PROMPT_MARKER } from "../../src/engine/keepalive.js";
import { beatParkedConversation, parkedResumeAnchor, type ParkedBeatQuery } from "../../src/engine/parked-keepalive.js";
import { readTranscriptChain, type ChainRecord } from "../../src/engine/rollback.js";
import { transcriptPath } from "../../src/engine/vendor-paths.js";
import { shimv1 } from "../../src/proto.js";
import { errorResultMessage, initMessage, resultMessage, ScriptedQuery } from "./fakes.js";

const REQUEST_AT_MS = Date.parse("2026-11-10T12:00:00.000Z");
const HOUR_MS = 60 * 60 * 1000;

const user = (uuid: string, parentUuid: string | null, content = `prompt ${uuid}`): Record<string, unknown> => ({
  type: "user",
  uuid,
  parentUuid,
  message: { role: "user", content },
});

const keepalive = (uuid: string, parentUuid: string | null): Record<string, unknown> =>
  user(uuid, parentUuid, `${KEEPALIVE_PROMPT_MARKER}\nRespond with the single character "." and nothing else. (1)`);

/** An answer that bought the hour tier at REQUEST_AT_MS. */
const assistant = (uuid: string, parentUuid: string): Record<string, unknown> => ({
  type: "assistant",
  uuid,
  parentUuid,
  timestamp: new Date(REQUEST_AT_MS).toISOString(),
  message: {
    role: "assistant",
    model: "claude-opus-5",
    content: [{ type: "text", text: "ok" }],
    usage: {
      input_tokens: 2,
      cache_creation_input_tokens: 100,
      cache_read_input_tokens: 1000,
      cache_creation: { ephemeral_1h_input_tokens: 100, ephemeral_5m_input_tokens: 0 },
    },
  },
});

const PARKED_ID = "parked-1";

/** A shim's account root and working directory, with the parked transcript filed under them. */
function filed(lines: Record<string, unknown>[] | undefined): { configDir: string; cwd: string } {
  const root = testTempDir("shim-parked-");
  const configDir = path.join(root, "config");
  const cwd = path.join(root, "work");
  if (lines !== undefined) writeTranscript(configDir, cwd, PARKED_ID, lines);
  return { configDir, cwd };
}

function chainOf(lines: Record<string, unknown>[]): readonly ChainRecord[] {
  const { configDir, cwd } = filed(lines);
  const read = readTranscriptChain(transcriptPath(configDir, cwd, PARKED_ID));
  if (read.kind !== "ok") throw new Error(`no chain: ${read.kind}`);
  return read.transcript.chain;
}

const parkedRequest = (): shimv1.SendKeepaliveParked =>
  create(shimv1.SendKeepaliveParkedSchema, {
    vendorSessionId: PARKED_ID,
    profile: create(shimv1.SessionProfileSchema, {
      kind: { case: "agent", value: { standing: { kind: { case: "linkedWorkspace", value: {} } }, landing: { landing: { case: "throughQueue", value: {} } } } },
    }),
  });

/** A beat's deps over the transcript LINES, its query answering with QUERY. */
function beatDeps(lines: Record<string, unknown>[] | undefined, query: ScriptedQuery, nowMs = REQUEST_AT_MS + 52 * 60 * 1000) {
  const built: ParkedBeatQuery[] = [];
  // THE FIRST PROMPT IS READ AS THE QUERY IS BUILT, as the vendor reads it,
  // before the side query closes its queue.
  const prompts: Promise<IteratorResult<unknown>>[] = [];
  const { configDir, cwd } = filed(lines);
  return {
    built,
    prompts,
    deps: {
      configDir,
      cwd,
      nowMs: () => nowMs,
      createQuery: (spec: ParkedBeatQuery) => {
        built.push(spec);
        prompts.push(spec.prompt[Symbol.asyncIterator]().next());
        return Promise.resolve(query);
      },
    },
  };
}

function answering(): ScriptedQuery {
  const query = new ScriptedQuery();
  query.emit(initMessage({ model: "claude-opus-5" }));
  query.emit(resultMessage());
  return query;
}

describe("where a parked keep-alive resumes", () => {
  it("resumes the whole conversation when it ends on a real turn", () => {
    // Arrange
    const chain = chainOf([user("p1", null), assistant("a1", "p1")]);

    // Act, Assert
    expect(parkedResumeAnchor(chain)).toBeUndefined();
  });

  it("resumes through the record before a trailing keep-alive turn", () => {
    // Arrange
    const chain = chainOf([user("p1", null), assistant("a1", "p1"), keepalive("k1", "a1"), assistant("ka1", "k1")]);

    // Act, Assert
    expect(parkedResumeAnchor(chain)).toBe("a1");
  });

  it("resumes before the first of several trailing keep-alive turns", () => {
    // Arrange
    const chain = chainOf([
      user("p1", null),
      assistant("a1", "p1"),
      keepalive("k1", "a1"),
      assistant("ka1", "k1"),
      keepalive("k2", "ka1"),
      assistant("ka2", "k2"),
    ]);

    // Act, Assert
    expect(parkedResumeAnchor(chain)).toBe("a1");
  });

  it("resumes the whole conversation when a real turn follows the keep-alive", () => {
    // Arrange
    const chain = chainOf([user("p1", null), assistant("a1", "p1"), keepalive("k1", "a1"), assistant("ka1", "k1"), user("p2", "ka1"), assistant("a2", "p2")]);

    // Act, Assert
    expect(parkedResumeAnchor(chain)).toBeUndefined();
  });

  it("fails loudly when the keep-alive follows no record of a real turn", () => {
    // Arrange
    const chain = chainOf([keepalive("k1", null), assistant("ka1", "k1")]);

    // Act, Assert
    expect(() => parkedResumeAnchor(chain)).toThrow(/follow no record of a real turn/);
  });
});

describe("one parked keep-alive", () => {
  it("fails, and says why, when the parked transcript is not on disk", async () => {
    // Arrange
    const { deps, built } = beatDeps(undefined, answering());
    const mark = logSinkMark();

    // Act
    const outcome = await beatParkedConversation(deps, parkedRequest());

    // Assert
    expect(outcome.kind).toBe("failed");
    expect(built).toHaveLength(0);
    expect(logRecordsSince(mark).some((record) => record.level === "error" && record.message.includes("its transcript is not on disk"))).toBe(true);
  });

  it("answers the lapse, and builds no query, when the cache already lapsed", async () => {
    // Arrange
    const { deps, built } = beatDeps([user("p1", null), assistant("a1", "p1")], answering(), REQUEST_AT_MS + HOUR_MS + 1);

    // Act
    const outcome = await beatParkedConversation(deps, parkedRequest());

    // Assert
    expect(outcome).toEqual({ kind: "lapsed", lapsedAtMs: REQUEST_AT_MS + HOUR_MS });
    expect(built).toHaveLength(0);
  });

  it("answers a lapse at zero for a conversation that never reached the model", async () => {
    // Arrange
    const { deps, built } = beatDeps([user("p1", null)], answering());

    // Act
    const outcome = await beatParkedConversation(deps, parkedRequest());

    // Assert
    expect(outcome).toEqual({ kind: "lapsed", lapsedAtMs: 0 });
    expect(built).toHaveLength(0);
  });

  it("is sent on its own query, under the parked profile, resuming before the last keep-alive", async () => {
    // Arrange
    const query = answering();
    const { deps, built } = beatDeps([user("p1", null), assistant("a1", "p1"), keepalive("k1", "a1"), assistant("ka1", "k1")], query);

    // Act
    const outcome = await beatParkedConversation(deps, parkedRequest());

    // Assert
    expect(outcome).toEqual({ kind: "sent" });
    expect(built).toHaveLength(1);
    expect(built[0]?.vendorSessionId).toBe(PARKED_ID);
    expect(built[0]?.resumeSessionAt).toBe("a1");
    expect(built[0]?.model).toBe("claude-opus-5");
    expect(built[0]?.profile.kind).toBe("agent");
  });

  it("sends the keep-alive prompt", async () => {
    // Arrange
    const query = answering();
    const { deps, prompts } = beatDeps([user("p1", null), assistant("a1", "p1")], query);

    // Act
    await beatParkedConversation(deps, parkedRequest());

    // Assert
    const sent = (await prompts[0])?.value as { message: { content: unknown } } | undefined;
    expect(String(sent?.message.content)).toMatch(new RegExp(`^${KEEPALIVE_PROMPT_MARKER}`));
  });

  it("fails, and says why, when the vendor answers with an error", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.emit(errorResultMessage({ errors: ["overloaded"] }));
    const { deps } = beatDeps([user("p1", null), assistant("a1", "p1")], query);
    const mark = logSinkMark();

    // Act
    const outcome = await beatParkedConversation(deps, parkedRequest());

    // Assert
    expect(outcome.kind).toBe("failed");
    expect(logRecordsSince(mark).some((record) => record.level === "error" && record.message.includes("keep-alive failed"))).toBe(true);
  });
});
