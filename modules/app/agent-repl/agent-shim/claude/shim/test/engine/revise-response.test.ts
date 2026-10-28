/**
 * ReviseResponse: one settled response rewritten on an ephemeral fork.
 *
 * WHAT THIS GUARDS: that a revision forks from where the session says a fork
 * taken now would end, prompts the side query with the daemon's instruction
 * verbatim, writes exactly one response_revision page line naming the revised
 * response only when the side query answered with text, and answers every
 * other outcome as its own refusal arm, recorded once with what failed.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { reviseResponse, type ReviseResponseDeps, type RevisionCut } from "../../src/engine/revise-response.js";
import type { PromptQueue } from "../../src/engine/prompt-queue.js";
import { errorResultMessage, initMessage, resultMessage, ScriptedQuery } from "./fakes.js";
import { logRecordsSince, logSinkMark } from "../log-records.js";

/** A harness around one revision: what it built, what it wrote. */
interface Harness {
  readonly deps: ReviseResponseDeps;
  readonly built: { parentSessionId: string; upTo: string | undefined; prompt: PromptQueue; controller: AbortController }[];
  readonly written: conversationv1.AgentResponseRevision[];
}

function harness(
  query: ScriptedQuery,
  options: { vendorSessionId?: string | undefined; cut?: RevisionCut; nowMs?: number } = {},
): Harness {
  const built: Harness["built"] = [];
  const written: conversationv1.AgentResponseRevision[] = [];
  const vendorSessionId = "vendorSessionId" in options ? options.vendorSessionId : "session-1";
  return {
    built,
    written,
    deps: {
      vendorSessionId: () => vendorSessionId,
      cut: () => options.cut ?? { kind: "cut", upTo: "record-9" },
      build: (spec, prompt, controller) => {
        built.push({ ...spec, prompt, controller });
        return Promise.resolve(query);
      },
      writeRevision: (revision) => written.push(revision),
      nowMs: () => options.nowMs ?? 1_700_000_000_000,
    },
  };
}

function request(response = "msg_1:0", instruction = "Revise the response."): shimv1.ReviseResponseRequest {
  return create(shimv1.ReviseResponseRequestSchema, {
    response: create(conversationv1.AgentActivityIdSchema, { value: response }),
    instruction,
  });
}

/** A query that answers TEXT on MODEL. */
function answering(text: string, model = "claude-sonnet-5"): ScriptedQuery {
  const query = new ScriptedQuery();
  query.emit(initMessage({ model }));
  query.emit({ ...(resultMessage() as object), result: text } as never);
  return query;
}

describe("revising a response", () => {
  it("forks the session's conversation at the cut the session names", async () => {
    // Arrange
    const h = harness(answering("revised"), { cut: { kind: "cut", upTo: "record-42" } });
    // Act
    await reviseResponse(h.deps, request());
    // Assert
    expect(h.built.map(({ parentSessionId, upTo }) => ({ parentSessionId, upTo }))).toEqual([
      { parentSessionId: "session-1", upTo: "record-42" },
    ]);
  });

  it("prompts the side query with the instruction verbatim", async () => {
    // Arrange
    const h = harness(answering("revised"));
    // Act
    await reviseResponse(h.deps, request("msg_1:0", "Add a branch explaining the word."));
    // Assert
    const prompt = h.built[0]?.prompt;
    if (prompt === undefined) throw new Error("the side query was never built");
    const first = await prompt[Symbol.asyncIterator]().next();
    expect(first.done === true ? undefined : first.value.message.content).toBe("Add a branch explaining the word.");
  });

  it("writes one revision naming the response, its new prose and when it landed", async () => {
    // Arrange
    const h = harness(answering("1 🔎 revised tree"), { nowMs: 1_234 });
    // Act
    await reviseResponse(h.deps, request("msg_7:2"));
    // Assert
    expect(h.written).toHaveLength(1);
    expect(h.written[0]?.response?.value).toBe("msg_7:2");
    expect(h.written[0]?.prose?.markdown).toBe("1 🔎 revised tree");
    expect(h.written[0]?.settledAt?.atMs).toBe(1_234n);
  });

  it("answers success with the model the side query's opening stated", async () => {
    // Arrange
    const h = harness(answering("revised", "claude-opus-5"));
    // Act
    const answered = await reviseResponse(h.deps, request());
    // Assert
    expect(answered.result).toMatchObject({ case: "success", value: { model: "claude-opus-5" } });
  });

  it("aborts the side query when the caller goes away", async () => {
    // Arrange
    const query = new ScriptedQuery();
    const h = harness(query);
    const caller = new AbortController();
    // Act
    const pending = reviseResponse(h.deps, request(), caller.signal);
    await Promise.resolve();
    caller.abort();
    query.fail(new Error("aborted"));
    const answered = await pending;
    // Assert
    expect(h.built[0]?.controller.signal.aborted).toBe(true);
    expect(answered.result.case).toBe("error");
    expect(h.written).toEqual([]);
  });
});

describe("a revision that cannot be written", () => {
  it("refuses no_session, building nothing, when no session is started", async () => {
    // Arrange
    const h = harness(answering("revised"), { vendorSessionId: undefined });
    // Act
    const answered = await reviseResponse(h.deps, request());
    // Assert
    expect(answered.result).toMatchObject({ case: "error", value: { cause: { case: "noSession" } } });
    expect(h.built).toEqual([]);
    expect(h.written).toEqual([]);
  });

  it("refuses no_fork_point with the session's account, building nothing", async () => {
    // Arrange
    const h = harness(answering("revised"), { cut: { kind: "unresolved", detail: "a turn is in flight" } });
    const mark = logSinkMark();
    // Act
    const answered = await reviseResponse(h.deps, request());
    // Assert
    expect(answered.result).toMatchObject({
      case: "error",
      value: { cause: { case: "noForkPoint", value: { detail: "a turn is in flight" } } },
    });
    expect(h.built).toEqual([]);
    expect(logRecordsSince(mark).filter((r) => r.level === "error").map((r) => r.context)).toEqual([
      expect.objectContaining({ response: "msg_1:0", vendor_session_id: "session-1", cause: "a turn is in flight" }),
    ]);
  });

  it("refuses empty_answer, writing nothing, when the side query answered no text", async () => {
    // Arrange
    const h = harness(answering("   "));
    // Act
    const answered = await reviseResponse(h.deps, request());
    // Assert
    expect(answered.result).toMatchObject({ case: "error", value: { cause: { case: "emptyAnswer" } } });
    expect(h.written).toEqual([]);
  });

  it.each([
    [
      "an error result",
      (query: ScriptedQuery) => query.emit(errorResultMessage({ errors: ["overloaded"] })),
      "the revising query ended with an error result (error_during_execution): overloaded",
    ],
    ["a silent end", (query: ScriptedQuery) => query.end(), "the revising query ended without a result"],
    [
      "a broken stream",
      (query: ScriptedQuery) => query.fail(new Error("child exited")),
      "the revising query's stream broke: child exited",
    ],
  ])("refuses vendor_failed on %s, writing nothing and recording the cause", async (_case, script, detail) => {
    // Arrange
    const query = new ScriptedQuery();
    script(query);
    const h = harness(query);
    const mark = logSinkMark();
    // Act
    const answered = await reviseResponse(h.deps, request());
    // Assert
    expect(answered.result).toMatchObject({ case: "error", value: { cause: { case: "vendorFailed", value: { detail } } } });
    expect(h.written).toEqual([]);
    expect(logRecordsSince(mark).filter((r) => r.level === "error").map((r) => r.context)).toEqual([
      expect.objectContaining({ response: "msg_1:0", cause: detail }),
    ]);
  });

  it("refuses vendor_failed when the side query cannot be built", async () => {
    // Arrange
    const h = harness(new ScriptedQuery());
    const deps: ReviseResponseDeps = { ...h.deps, build: () => Promise.reject(new Error("vendor calls are forbidden")) };
    // Act
    const answered = await reviseResponse(deps, request());
    // Assert
    expect(answered.result).toMatchObject({
      case: "error",
      value: { cause: { case: "vendorFailed", value: { detail: "the revising query could not be built: vendor calls are forbidden" } } },
    });
  });
});
