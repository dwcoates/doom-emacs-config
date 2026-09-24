/**
 * The artifact converter. The load-bearing claim is that the answered act is
 * read off the TYPED output's fields — a publish answers with `url`, a listing
 * with `artifacts` — never off the sentence the model was shown.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { toolResultText } from "../../../src/convert/entries.js";
import { artifactConverter } from "../../../src/convert/tools/artifact.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

function callWith(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_artifact",
    toolName: "Artifact",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT_ID,
  };
}

function outcomeWith(structured: unknown, isError = false): ToolOutcome {
  return {
    content: toolResultText("Published to https://claude.ai/public/artifacts/abc"),
    isError,
    structured,
    settledAtMs: 1_700_000_003_000,
  };
}

function artifactOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentArtifact {
  expect(item?.case).toBe("artifact");
  return item?.value as conversationv1.AgentArtifact;
}

function startOf(call: PendingCall): conversationv1.AgentArtifactStart {
  const artifact = artifactOf(artifactConverter.start(call));
  expect(artifact.result.case).toBe("start");
  return artifact.result.value as conversationv1.AgentArtifactStart;
}

function successOf(structured: unknown): conversationv1.AgentArtifactSuccess {
  const artifact = artifactOf(
    artifactConverter.settle(callWith({ file_path: "page.html" }), outcomeWith(structured)),
  );
  expect(artifact.result.case).toBe("success");
  return artifact.result.value as conversationv1.AgentArtifactSuccess;
}

describe("artifactConverter kind and arms", () => {
  it("declares the artifact kind", () => {
    // Arrange, Act, Assert.
    expect(artifactConverter.kind).toBe("artifact");
  });

  it("carries NO progress, because AgentArtifact declares no such arm", () => {
    // Arrange, Act, Assert.
    expect(artifactConverter.carriesProgress).toBe(false);
  });
});

describe("artifactConverter.start", () => {
  it("treats an action-less call as a publish, which is the vendor's default", () => {
    // Arrange.
    const call = callWith({ file_path: "/tmp/page.html", favicon: "📊" });

    // Act, Assert.
    expect(startOf(call).act.case).toBe("publish");
  });

  it("carries the publish's own fields as the call gave them", () => {
    // Arrange.
    const call = callWith({
      file_path: "/tmp/page.html",
      favicon: "📊",
      title: "Quarterly",
      label: "fixed-background",
      description: "the numbers",
      force: true,
    });

    // Act, Assert.
    expect(startOf(call).act.value).toEqual(
      create(conversationv1.AgentArtifactPublishSchema, {
        filePath: "/tmp/page.html",
        favicon: "📊",
        title: "Quarterly",
        label: "fixed-background",
        description: "the numbers",
        force: true,
      }),
    );
  });

  it("marks a REDEPLOY by the presence of the url being updated in place", () => {
    // Arrange.
    const call = callWith({ file_path: "/tmp/page.html", url: "https://claude.ai/public/artifacts/abc" });

    // Act, Assert.
    expect((startOf(call).act.value as conversationv1.AgentArtifactPublish).updatesUrl).toBe(
      "https://claude.ai/public/artifacts/abc",
    );
  });

  it("leaves updates_url UNSET on a fresh publish rather than empty", () => {
    // Arrange.
    const call = callWith({ file_path: "/tmp/page.html" });

    // Act, Assert.
    expect((startOf(call).act.value as conversationv1.AgentArtifactPublish).updatesUrl).toBeUndefined();
  });

  it("states the list act with its own two fields", () => {
    // Arrange.
    const call = callWith({ action: "list", limit: 10, scope: "shared" });

    // Act, Assert.
    expect(startOf(call).act).toEqual({
      case: "list",
      value: create(conversationv1.AgentArtifactListSchema, { limit: 10, scope: "shared" }),
    });
  });

  it("stamps the instant the call was issued", () => {
    // Arrange.
    const call = callWith({ file_path: "/tmp/page.html" });

    // Act, Assert.
    expect(startOf(call).startedAtMs).toBe(1_700_000_000_000n);
  });

  it("still announces a publish whose call named no file", () => {
    // Arrange.
    const call = callWith({ favicon: "📊" });

    // Act, Assert.
    expect((startOf(call).act.value as conversationv1.AgentArtifactPublish).filePath).toBe("");
  });
});

describe("artifactConverter.settle", () => {
  it("reads the published url off the TYPED output", () => {
    // Arrange, Act.
    const success = successOf({
      url: "https://claude.ai/public/artifacts/abc",
      path: "/tmp/page.html",
      title: "Quarterly",
      version: "3",
    });

    // Assert.
    expect(success.outcome).toEqual({
      case: "published",
      value: create(conversationv1.AgentArtifactPublishedSchema, {
        url: "https://claude.ai/public/artifacts/abc",
        title: "Quarterly",
      }),
    });
  });

  it("leaves the published title UNSET when the result named none", () => {
    // Arrange, Act.
    const success = successOf({ url: "https://claude.ai/public/artifacts/abc", path: "/tmp/p.html" });

    // Assert.
    expect((success.outcome.value as conversationv1.AgentArtifactPublished).title).toBeUndefined();
  });

  it("answers a listing with the empty listed arm: nothing draws a listing", () => {
    // Arrange, Act.
    const success = successOf({ artifacts: [{ title: "Quarterly", url: "https://claude.ai/x" }] });

    // Assert.
    expect(success.outcome).toEqual({
      case: "listed",
      value: create(conversationv1.AgentArtifactListedSchema, {}),
    });
  });

  it("recognizes an EMPTY listing by its artifacts array, not by its length", () => {
    // Arrange, Act.
    const success = successOf({ artifacts: [] });

    // Assert.
    expect(success.outcome.case).toBe("listed");
  });

  it("produces NO frame when a settled publish named no url", () => {
    // Arrange.
    const call = callWith({ file_path: "/tmp/page.html" });

    // Act.
    const item = artifactConverter.settle(call, outcomeWith({ path: "/tmp/page.html" }));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("produces NO frame when the settled call carried no typed output", () => {
    // Arrange.
    const call = callWith({ file_path: "/tmp/page.html" });

    // Act.
    const item = artifactConverter.settle(call, outcomeWith("just prose"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("settles an errored call as the failure arm", () => {
    // Arrange.
    const call = callWith({ file_path: "/tmp/page.html" });

    // Act.
    const artifact = artifactOf(artifactConverter.settle(call, outcomeWith(undefined, true)));

    // Assert.
    const failure = artifact.result.value as conversationv1.AgentArtifactFailure;
    expect(artifact.result.case).toBe("failure");
    expect(failure.failure?.settledAt?.atMs).toBe(1_700_000_003_000n);
  });

  it("restates a failed publish's act, so a replayed failure draws its card", () => {
    // Arrange.
    const call = callWith({ file_path: "/tmp/page.html", title: "The Page", favicon: "📄" });

    // Act.
    const artifact = artifactOf(artifactConverter.settle(call, outcomeWith(undefined, true)));

    // Assert.
    const failure = artifact.result.value as conversationv1.AgentArtifactFailure;
    expect(failure.act.case).toBe("publish");
    expect(failure.act.value).toMatchObject({ filePath: "/tmp/page.html", title: "The Page", favicon: "📄" });
  });

  it("restates a failed listing's act as a list, which draws nowhere", () => {
    // Arrange.
    const call = callWith({ action: "list", limit: 5 });

    // Act.
    const artifact = artifactOf(artifactConverter.settle(call, outcomeWith(undefined, true)));

    // Assert.
    const failure = artifact.result.value as conversationv1.AgentArtifactFailure;
    expect(failure.act.case).toBe("list");
  });
});
