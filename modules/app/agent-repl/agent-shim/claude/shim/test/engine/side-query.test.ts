/**
 * The side query: one question to a throwaway query, read to its one result.
 *
 * WHAT THIS GUARDS: that every throwaway query the shim asks (the cold gate's
 * summary, a response revision) tells an answer apart from an error result, a
 * stream that ended silently, a query that could not be built and a stream
 * that broke, and that the query is closed whichever way it went.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { askSideQuery } from "../../src/engine/side-query.js";
import type { PromptQueue } from "../../src/engine/prompt-queue.js";
import type { SdkUserMessage } from "../../src/sdk/types.js";
import { errorResultMessage, initMessage, resultMessage, ScriptedQuery } from "./fakes.js";

/** Build ScriptedQuery as a side query, capturing the prompt queue it was handed. */
function scripted(query: ScriptedQuery): {
  build: (prompt: PromptQueue, controller: AbortController) => Promise<ScriptedQuery>;
  prompts: () => PromptQueue;
  controller: () => AbortController;
} {
  let queue: PromptQueue | undefined;
  let abort: AbortController | undefined;
  return {
    build: (prompt, controller) => {
      queue = prompt;
      abort = controller;
      return Promise.resolve(query);
    },
    prompts: () => {
      if (queue === undefined) throw new Error("the side query was never built");
      return queue;
    },
    controller: () => {
      if (abort === undefined) throw new Error("the side query was never built");
      return abort;
    },
  };
}

/** The first message the side query was prompted with. */
async function firstPrompt(queue: PromptQueue): Promise<SdkUserMessage | undefined> {
  const next = await queue[Symbol.asyncIterator]().next();
  return next.done === true ? undefined : next.value;
}

describe("asking a side query", () => {
  it("answers the result's text and the model the opening stated", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.emit(initMessage({ model: "claude-sonnet-5" }));
    query.emit(resultMessage("r-1", { output_tokens: 7 }));
    // Act
    const asked = await askSideQuery(scripted(query).build, "question");
    // Assert
    expect(asked).toMatchObject({ kind: "answered", text: "done", model: "claude-sonnet-5" });
    expect(asked.kind === "answered" ? asked.usage.outputTokens : -1).toBe(7);
  });

  it("answers with no model when the result arrived before any opening", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.emit(resultMessage());
    // Act
    const asked = await askSideQuery(scripted(query).build, "question");
    // Assert
    expect(asked).toMatchObject({ kind: "answered", model: undefined });
  });

  it("prompts the query with the question as one user message", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.emit(resultMessage());
    const side = scripted(query);
    // Act
    await askSideQuery(side.build, "what is the meaning");
    // Assert
    expect(await firstPrompt(side.prompts())).toEqual({
      type: "user",
      message: { role: "user", content: "what is the meaning" },
      parent_tool_use_id: null,
    });
  });

  it("reads a success-subtype result marked is_error as an error result carrying its sentence", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.emit({ ...(resultMessage() as object), is_error: true, result: "API Error: 529", api_error_status: 529 } as never);
    // Act
    const asked = await askSideQuery(scripted(query).build, "question");
    // Assert
    expect(asked).toEqual({ kind: "errorResult", subtype: "success", words: "API Error: 529", status: 529 });
  });

  it("reads a failure-subtype result as an error result carrying its listed errors", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.emit(errorResultMessage({ errors: ["first", "second"] }));
    // Act
    const asked = await askSideQuery(scripted(query).build, "question");
    // Assert
    expect(asked).toEqual({ kind: "errorResult", subtype: "error_during_execution", words: "first; second", status: undefined });
  });

  it("answers noResult when the stream ends without a result", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.emit(initMessage());
    query.end();
    // Act
    const asked = await askSideQuery(scripted(query).build, "question");
    // Assert
    expect(asked).toEqual({ kind: "noResult" });
  });

  it("answers unbuilt with the cause when the query cannot be built", async () => {
    // Arrange
    const build = (): Promise<ScriptedQuery> => Promise.reject(new Error("vendor calls are forbidden"));
    // Act
    const asked = await askSideQuery(build, "question");
    // Assert
    expect(asked).toEqual({ kind: "unbuilt", error: "vendor calls are forbidden" });
  });

  it("answers broken with the cause when the stream throws", async () => {
    // Arrange
    const query = new ScriptedQuery();
    query.fail(new Error("the child went away"));
    // Act
    const asked = await askSideQuery(scripted(query).build, "question");
    // Assert
    expect(asked).toEqual({ kind: "broken", error: "the child went away" });
  });

  it.each([
    ["answered", (query: ScriptedQuery) => query.emit(resultMessage())],
    ["noResult", (query: ScriptedQuery) => query.end()],
    ["broken", (query: ScriptedQuery) => query.fail(new Error("gone"))],
  ])("closes the query, its prompts and its controller once it %s", async (_outcome, script) => {
    // Arrange
    const query = new ScriptedQuery();
    script(query);
    const side = scripted(query);
    // Act
    await askSideQuery(side.build, "question");
    // Assert
    expect(query.calls).toContain("close");
    expect(side.controller().signal.aborted).toBe(true);
    expect(() => side.prompts().push({ type: "user", message: { role: "user", content: "late" }, parent_tool_use_id: null })).toThrow();
  });
});

describe("the side query's call sites", () => {
  it("builds every query but the live one inside a side query", () => {
    // Arrange
    const source = readFileSync(path.join(import.meta.dirname, "../../src/engine/session.ts"), "utf8");
    // Act
    const built = source.split("deps.createQuery(").length - 1;
    // A revision's query is built for reviseResponse, which asks it as a side query.
    const asked = source.split("askSideQuery(").length - 1 + (source.split("reviseResponse(").length - 1);
    // Assert: the live session builds ONE query of its own; every other is a side query's.
    expect(built).toBe(asked + 1);
  });
});
