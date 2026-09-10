/**
 * The web-fetch converter. The load-bearing claim is that the TARGET comes from
 * the call and the STATUS from the typed output — the corpus's one observed
 * fetch is a 302 to another host, so a converter that read the result's own
 * `url` would restate a page the agent never asked for.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { toolProgress, toolResultText } from "../../../src/convert/entries.js";
import { webFetchConverter } from "../../../src/convert/tools/web-fetch.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

/** The one observed WebFetch result's own `toolUseResult`, verbatim. */
function corpusOutput(): Record<string, unknown> {
  const path = fileURLToPath(
    new URL("../../../../../../testdata/corpus/tool-results/web_fetch.jsonl", import.meta.url),
  );
  const line = readFileSync(path, "utf8").trim().split("\n")[0];
  return (JSON.parse(line) as { toolUseResult: Record<string, unknown> }).toolUseResult;
}

/** The one observed WebFetch call's own `input`, verbatim. */
function corpusInput(): Record<string, unknown> {
  const path = fileURLToPath(
    new URL("../../../../../../testdata/corpus/tool-inputs/web_fetch.jsonl", import.meta.url),
  );
  const line = readFileSync(path, "utf8").trim().split("\n")[0];
  return (JSON.parse(line) as { input: Record<string, unknown> }).input;
}

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

function callWith(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_fetch",
    toolName: "WebFetch",
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
    settledAtMs: 1_700_000_002_000,
  };
}

/** The AgentWebFetch inside an item, or a failed assertion. */
function fetchOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentWebFetch {
  expect(item?.case).toBe("webFetch");
  return item?.value as conversationv1.AgentWebFetch;
}

describe("webFetchConverter kind and arms", () => {
  it("declares the web_fetch kind", () => {
    // Arrange, Act, Assert.
    expect(webFetchConverter.kind).toBe("web_fetch");
  });

  it("carries progress, because AgentWebFetch declares the arm", () => {
    // Arrange, Act, Assert.
    expect(webFetchConverter.carriesProgress).toBe(true);
  });

  it("relays a progress beat as the unit's progress arm", () => {
    // Arrange.
    const beat = toolProgress(1_700_000_001_000);

    // Act.
    const fetch = fetchOf(webFetchConverter.progress!(beat));

    // Assert.
    expect(fetch.result).toEqual({ case: "progress", value: beat });
  });
});

describe("webFetchConverter.start", () => {
  it("takes the target from the CALL's url", () => {
    // Arrange.
    const call = callWith(corpusInput());

    // Act.
    const fetch = fetchOf(webFetchConverter.start(call));

    // Assert.
    expect(fetch.result.case).toBe("start");
    expect((fetch.result.value as conversationv1.AgentWebFetchStart).target?.url).toBe(
      "https://docs.slack.dev/reference/objects/file-object/",
    );
  });

  it("stamps the instant the call was announced", () => {
    // Arrange.
    const call = callWith(corpusInput());

    // Act.
    const fetch = fetchOf(webFetchConverter.start(call));

    // Assert.
    expect((fetch.result.value as conversationv1.AgentWebFetchStart).startedAtMs).toBe(
      1_700_000_000_000n,
    );
  });

  it("still announces a call whose input named no url", () => {
    // Arrange.
    const call = callWith({ prompt: "no url at all" });

    // Act.
    const fetch = fetchOf(webFetchConverter.start(call));

    // Assert.
    expect((fetch.result.value as conversationv1.AgentWebFetchStart).target?.url).toBe("");
  });
});

describe("webFetchConverter.settle", () => {
  it("builds the success arm from the corpus's own typed output", () => {
    // Arrange.
    const call = callWith({ url: "https://api.slack.com/methods" });
    const output = corpusOutput();

    // Act.
    const fetch = fetchOf(webFetchConverter.settle(call, outcomeWith(output)));

    // Assert.
    const success = fetch.result.value as conversationv1.AgentWebFetchSuccess;
    expect(fetch.result.case).toBe("success");
    expect(success.status).toEqual(
      create(conversationv1.AgentWebFetchHttpStatusSchema, { code: 302, text: "Found" }),
    );
    expect(success.bytes).toBe(626n);
    expect(success.result).toBe(output["result"]);
  });

  it("restates the CALL's url on the settled frame, not the result's redirect", () => {
    // Arrange.
    const call = callWith({ url: "https://api.slack.com/methods" });

    // Act.
    const fetch = fetchOf(webFetchConverter.settle(call, outcomeWith(corpusOutput())));

    // Assert.
    expect((fetch.result.value as conversationv1.AgentWebFetchSuccess).target?.url).toBe(
      "https://api.slack.com/methods",
    );
  });

  it("reads artifact_read from the PRESENCE of the artifact descriptor", () => {
    // Arrange.
    const call = callWith({ url: "https://claude.ai/public/artifacts/x" });
    const output = { code: 200, codeText: "OK", result: "page", bytes: 10, durationMs: 5, artifactRead: { slug: "x" } };

    // Act.
    const fetch = fetchOf(webFetchConverter.settle(call, outcomeWith(output)));

    // Assert.
    expect((fetch.result.value as conversationv1.AgentWebFetchSuccess).artifactRead).toBe(true);
  });

  it("leaves artifact_read false when the vendor stated no descriptor", () => {
    // Arrange.
    const call = callWith({ url: "https://example.test/" });
    const output = { code: 200, codeText: "OK", result: "page", bytes: 10, durationMs: 5 };

    // Act.
    const fetch = fetchOf(webFetchConverter.settle(call, outcomeWith(output)));

    // Assert.
    expect((fetch.result.value as conversationv1.AgentWebFetchSuccess).artifactRead).toBe(false);
  });

  it("carries the vendor's duration as stated", () => {
    // Arrange.
    const call = callWith({ url: "https://example.test/" });
    const output = { code: 200, codeText: "OK", result: "page", bytes: 10, durationMs: 1234 };

    // Act.
    const fetch = fetchOf(webFetchConverter.settle(call, outcomeWith(output)));

    // Assert.
    expect((fetch.result.value as conversationv1.AgentWebFetchSuccess).durationMs).toBe(1234n);
  });

  it("settles an errored result as the failure arm, with the target restated", () => {
    // Arrange.
    const call = callWith({ url: "https://blocked.test/" });
    const outcome = outcomeWith(undefined, true);

    // Act.
    const fetch = fetchOf(webFetchConverter.settle(call, outcome));

    // Assert.
    const failure = fetch.result.value as conversationv1.AgentWebFetchFailure;
    expect(fetch.result.case).toBe("failure");
    expect(failure.target?.url).toBe("https://blocked.test/");
    expect(failure.failure?.settledAt?.atMs).toBe(1_700_000_002_000n);
  });

  it("produces NO frame when a settled fetch carried no typed output", () => {
    // Arrange.
    const call = callWith({ url: "https://example.test/" });

    // Act.
    const item = webFetchConverter.settle(call, outcomeWith("just prose"));

    // Assert.
    expect(item).toBeUndefined();
  });
});

describe("webFetchConverter defaults for fields the vendor left unstated", () => {
  /** The success arm for one typed output, against a fixed call. */
  function successOf(output: Record<string, unknown>): conversationv1.AgentWebFetchSuccess {
    const call = callWith({ url: "https://example.test/" });
    const fetch = fetchOf(webFetchConverter.settle(call, outcomeWith(output)));
    expect(fetch.result.case).toBe("success");
    return fetch.result.value as conversationv1.AgentWebFetchSuccess;
  }

  it("states a zero status code when the output named none", () => {
    // Arrange, Act.
    const success = successOf({ codeText: "no code stated" });

    // Assert.
    expect(success.status?.code).toBe(0);
  });

  it("states zero bytes when the output named none", () => {
    // Arrange, Act.
    const success = successOf({ code: 200, durationMs: 12 });

    // Assert.
    expect(success.bytes).toBe(0n);
  });

  it("states a zero duration when the output named none", () => {
    // Arrange, Act.
    const success = successOf({ code: 200, bytes: 12 });

    // Assert.
    expect(success.durationMs).toBe(0n);
  });
});
