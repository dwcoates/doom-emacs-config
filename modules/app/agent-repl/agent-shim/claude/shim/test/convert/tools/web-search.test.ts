/**
 * The web-search converter. The load-bearing claim is the HETEROGENEOUS answer:
 * the engine's array interleaves hit groups with the model's own narration
 * lines, and the corpus's one observed search holds exactly one of each — in
 * that order — so order across the two kinds is asserted, not just membership.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { toolProgress, toolResultText } from "../../../src/convert/entries.js";
import { webSearchConverter } from "../../../src/convert/tools/web-search.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

/** The one observed WebSearch result's own `toolUseResult`, verbatim. */
function corpusOutput(): Record<string, unknown> {
  const path = fileURLToPath(
    new URL("../../../../../../testdata/corpus/tool-results/web_search.jsonl", import.meta.url),
  );
  const line = readFileSync(path, "utf8").trim().split("\n")[0] as string;
  return (JSON.parse(line) as { toolUseResult: Record<string, unknown> }).toolUseResult;
}

/** The one observed WebSearch call's own `input`, verbatim. */
function corpusInput(): Record<string, unknown> {
  const path = fileURLToPath(
    new URL("../../../../../../testdata/corpus/tool-inputs/web_search.jsonl", import.meta.url),
  );
  const line = readFileSync(path, "utf8").trim().split("\n")[0] as string;
  return (JSON.parse(line) as { input: Record<string, unknown> }).input;
}

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

function callWith(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_search",
    toolName: "WebSearch",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT_ID,
  };
}

function outcomeWith(structured: unknown, isError = false): ToolOutcome {
  return {
    content: toolResultText("what the model was shown"),
    isError,
    structured,
    settledAtMs: 1_700_000_007_000,
  };
}

function searchOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentWebSearch {
  expect(item?.case).toBe("webSearch");
  return item?.value as conversationv1.AgentWebSearch;
}

function successOf(
  call: PendingCall,
  structured: unknown,
): conversationv1.AgentWebSearchSuccess {
  const search = searchOf(webSearchConverter.settle(call, outcomeWith(structured))!);
  expect(search.result.case).toBe("success");
  return search.result.value as conversationv1.AgentWebSearchSuccess;
}

describe("webSearchConverter kind and arms", () => {
  it("declares the web_search kind", () => {
    // Arrange, Act, Assert.
    expect(webSearchConverter.kind).toBe("web_search");
  });

  it("carries progress, because AgentWebSearch declares the arm", () => {
    // Arrange, Act, Assert.
    expect(webSearchConverter.carriesProgress).toBe(true);
  });

  it("relays a progress beat as the unit's progress arm", () => {
    // Arrange.
    const beat = toolProgress(1_700_000_003_000);

    // Act.
    const search = searchOf(webSearchConverter.progress!(beat));

    // Assert.
    expect(search.result).toEqual({ case: "progress", value: beat });
  });
});

describe("webSearchConverter.start", () => {
  it("takes the query terms from the CALL's own input", () => {
    // Arrange.
    const call = callWith(corpusInput());

    // Act.
    const search = searchOf(webSearchConverter.start(call));

    // Assert.
    expect((search.result.value as conversationv1.AgentWebSearchStart).query?.terms).toBe(
      "Slack API huddle transcript access Web API methods",
    );
  });

  it("stamps the instant the call was announced", () => {
    // Arrange.
    const call = callWith(corpusInput());

    // Act.
    const search = searchOf(webSearchConverter.start(call));

    // Assert.
    expect((search.result.value as conversationv1.AgentWebSearchStart).startedAtMs).toBe(
      1_700_000_000_000n,
    );
  });

  it("still announces a call whose input named no query", () => {
    // Arrange.
    const call = callWith({});

    // Act.
    const search = searchOf(webSearchConverter.start(call));

    // Assert.
    expect((search.result.value as conversationv1.AgentWebSearchStart).query?.terms).toBe("");
  });
});

describe("webSearchConverter.settle", () => {
  it("flattens the corpus's hit group into ordered links", () => {
    // Arrange.
    const call = callWith({ query: "q" });

    // Act.
    const success = successOf(call, corpusOutput());

    // Assert.
    expect(success.results[0]?.entry).toEqual({
      case: "link",
      value: create(conversationv1.AgentWebSearchLinkSchema, {
        title: "Agent design | Slack Developer Docs",
        url: "https://docs.slack.dev/concepts/agent-design/",
      }),
    });
  });

  it("keeps the corpus's narration line AFTER the links it followed", () => {
    // Arrange.
    const call = callWith({ query: "q" });

    // Act.
    const success = successOf(call, corpusOutput());

    // Assert: nine hits, then the one narration line.
    expect(success.results).toHaveLength(10);
    expect(success.results[9]?.entry.case).toBe("note");
  });

  it("carries the vendor's search count", () => {
    // Arrange.
    const call = callWith({ query: "q" });

    // Act.
    const success = successOf(call, corpusOutput());

    // Assert.
    expect(success.searchCount).toBe(1);
  });

  it("carries the vendor's duration in the vendor's own seconds", () => {
    // Arrange.
    const call = callWith({ query: "q" });

    // Act.
    const success = successOf(call, { results: [], durationSeconds: 6.5 });

    // Assert.
    expect(success.durationSeconds).toBe(6.5);
  });

  it("records a search that found nothing as an empty success", () => {
    // Arrange.
    const call = callWith({ query: "q" });

    // Act.
    const success = successOf(call, { results: [], durationSeconds: 1 });

    // Assert.
    expect(success.results).toEqual([]);
  });

  it("drops a hit that named no url rather than drawing a dead link", () => {
    // Arrange.
    const call = callWith({ query: "q" });
    const structured = { results: [{ content: [{ title: "titled but unlinked" }] }] };

    // Act.
    const success = successOf(call, structured);

    // Assert.
    expect(success.results).toEqual([]);
  });

  it("keeps a hit whose title the engine omitted", () => {
    // Arrange.
    const call = callWith({ query: "q" });
    const structured = { results: [{ content: [{ url: "https://example.test/" }] }] };

    // Act.
    const success = successOf(call, structured);

    // Assert.
    expect(success.results[0]?.entry).toEqual({
      case: "link",
      value: create(conversationv1.AgentWebSearchLinkSchema, {
        title: "",
        url: "https://example.test/",
      }),
    });
  });

  it("drops an entry that is neither a narration line nor a hit group", () => {
    // Arrange.
    const call = callWith({ query: "q" });

    // Act.
    const success = successOf(call, { results: [42], durationSeconds: 1 });

    // Assert.
    expect(success.results).toEqual([]);
  });

  it("settles an errored result as the failure arm, with the query restated", () => {
    // Arrange.
    const call = callWith({ query: "refused" });

    // Act.
    const search = searchOf(webSearchConverter.settle(call, outcomeWith(undefined, true))!);

    // Assert.
    const failure = search.result.value as conversationv1.AgentWebSearchFailure;
    expect(search.result.case).toBe("failure");
    expect(failure.query?.terms).toBe("refused");
  });

  it("produces NO frame when a settled search carried no typed output", () => {
    // Arrange.
    const call = callWith({ query: "q" });

    // Act.
    const item = webSearchConverter.settle(call, outcomeWith("just prose"));

    // Assert.
    expect(item).toBeUndefined();
  });
});
