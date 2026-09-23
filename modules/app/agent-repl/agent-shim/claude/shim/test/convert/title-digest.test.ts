/**
 * The material a synthesized workspace title is made of, computed from a
 * transcript's records.
 *
 * WHAT THIS GUARDS: that the digest is measured from the RIGHT boundary (no
 * cut, a /clear, or the most recent /compact) and that only the person's own
 * prompts count — never a tool result the SDK wrote as a user record, never a
 * slash-command envelope, never a compaction summary, never a subagent's
 * sidechain prompt. The failure modes excluded are a title synthesized from the
 * whole history after a compaction, and a title synthesized from machine noise.
 */
import { describe, expect, it } from "vitest";
import { computeTitleDigest, promptText, type TitleDigestRecord } from "../../src/convert/title-digest.js";
import { keepalivePromptText } from "../../src/engine/keepalive.js";

/** A person's prompt record. */
function prompt(text: string): TitleDigestRecord {
  return { type: "user", message: { role: "user", content: text } };
}

/** A tool-result record the SDK writes under the user role. */
function toolResult(id: string): TitleDigestRecord {
  return { type: "user", message: { role: "user", content: [{ type: "tool_result", tool_use_id: id }] } };
}

/** A slash-command envelope record, as the vendor writes it. */
function commandEnvelope(command: string): TitleDigestRecord {
  return { type: "user", message: { role: "user", content: `<command-name>${command}</command-name>` } };
}

/** The local-command caveat that precedes a command envelope. */
function caveat(): TitleDigestRecord {
  return { type: "user", isMeta: true, message: { role: "user", content: "<local-command-caveat>Caveat…</local-command-caveat>" } };
}

/** A compaction boundary marker. */
function compactBoundary(): TitleDigestRecord {
  return { type: "system", subtype: "compact_boundary" };
}

/** A compaction summary record. */
function compactSummary(text: string): TitleDigestRecord {
  return { type: "user", isCompactSummary: true, message: { role: "user", content: text } };
}

describe("computeTitleDigest boundary", () => {
  it("is NONE with every prompt when the conversation was never cut", () => {
    // Arrange.
    const records = [prompt("first"), prompt("second")];

    // Act.
    const digest = computeTitleDigest(records);

    // Assert.
    expect(digest).toEqual({ boundary: "none", prompts: ["first", "second"] });
  });

  it("is CLEAR with only the prompts after the clear envelope", () => {
    // Arrange — a /clear rotates to a new transcript whose head is the clear envelope.
    const records = [caveat(), commandEnvelope("/clear"), prompt("after the clear")];

    // Act.
    const digest = computeTitleDigest(records);

    // Assert.
    expect(digest).toEqual({ boundary: "clear", prompts: ["after the clear"] });
  });

  it("is COMPACT with the summary and only the prompts after it", () => {
    // Arrange — a /compact appends a boundary, its summary, then the command echo.
    const records = [
      prompt("before"),
      compactBoundary(),
      compactSummary("This session is being continued. Summary: …"),
      caveat(),
      commandEnvelope("/compact"),
      prompt("after the compaction"),
    ];

    // Act.
    const digest = computeTitleDigest(records);

    // Assert.
    expect(digest).toEqual({
      boundary: "compact",
      lastCompactSummary: "This session is being continued. Summary: …",
      prompts: ["after the compaction"],
    });
  });

  it("measures from the MOST RECENT compaction when there were two", () => {
    // Arrange.
    const records = [
      compactBoundary(),
      compactSummary("first summary"),
      prompt("between"),
      compactBoundary(),
      compactSummary("second summary"),
      prompt("after second"),
    ];

    // Act.
    const digest = computeTitleDigest(records);

    // Assert.
    expect(digest).toEqual({
      boundary: "compact",
      lastCompactSummary: "second summary",
      prompts: ["after second"],
    });
  });
});

describe("computeTitleDigest prompt filtering", () => {
  it("drops tool-result records the SDK wrote under the user role", () => {
    // Arrange.
    const records = [prompt("real"), toolResult("toolu_1")];

    // Act.
    const digest = computeTitleDigest(records);

    // Assert.
    expect(digest.prompts).toEqual(["real"]);
  });

  it("drops a subagent's sidechain prompt", () => {
    // Arrange.
    const records = [prompt("main"), { type: "user", isSidechain: true, message: { role: "user", content: "sub" } }];

    // Act.
    const digest = computeTitleDigest(records);

    // Assert.
    expect(digest.prompts).toEqual(["main"]);
  });

  it("drops the shim's own keep-alive prompt", () => {
    // Arrange: a keep-alive is never served, so it must never name a conversation.
    const records = [prompt("real"), prompt(keepalivePromptText(3))];

    // Act.
    const digest = computeTitleDigest(records);

    // Assert.
    expect(digest.prompts).toEqual(["real"]);
  });
});

describe("promptText extraction", () => {
  it("takes only the text blocks from array content, keeping the words beside an image", () => {
    // Arrange.
    const record: TitleDigestRecord = {
      type: "user",
      message: { role: "user", content: [{ type: "image" }, { type: "text", text: "describe this" }] },
    };

    // Act, Assert.
    expect(promptText(record)).toBe("describe this");
  });

  it("is undefined for an empty prompt", () => {
    // Arrange.
    const record = prompt("   ");

    // Act, Assert.
    expect(promptText(record)).toBeUndefined();
  });
});
