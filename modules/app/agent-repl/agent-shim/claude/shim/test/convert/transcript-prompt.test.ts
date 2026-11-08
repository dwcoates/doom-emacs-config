/**
 * Which of a transcript's records are the person's own prompts, and which mark
 * a context boundary.
 *
 * WHAT THIS GUARDS: that only the person's own prompts count — never a tool
 * result the SDK wrote as a user record, never a slash-command envelope, never
 * a compaction summary, never a subagent's sidechain prompt — and that a
 * /clear and a /compact are told apart.
 */
import { describe, expect, it } from "vitest";
import {
  isClearEnvelope,
  isCompactBoundary,
  promptText,
  userRecordText,
  type PromptRecord,
} from "../../src/convert/transcript-prompt.js";
import { keepalivePromptText } from "../../src/engine/keepalive.js";

/** A person's prompt record. */
function prompt(text: string): PromptRecord {
  return { type: "user", message: { role: "user", content: text } };
}

/** A tool-result record the SDK writes under the user role. */
function toolResult(id: string): PromptRecord {
  return { type: "user", message: { role: "user", content: [{ type: "tool_result", tool_use_id: id }] } };
}

/** A slash-command envelope record, as the vendor writes it. */
function commandEnvelope(command: string): PromptRecord {
  return { type: "user", message: { role: "user", content: `<command-name>${command}</command-name>` } };
}

/** The local-command caveat that precedes a command envelope. */
function caveat(): PromptRecord {
  return { type: "user", isMeta: true, message: { role: "user", content: "<local-command-caveat>Caveat…</local-command-caveat>" } };
}

/** A compaction boundary marker. */
function compactBoundary(): PromptRecord {
  return { type: "system", subtype: "compact_boundary" };
}

/** A compaction summary record. */
function compactSummary(text: string): PromptRecord {
  return { type: "user", isCompactSummary: true, message: { role: "user", content: text } };
}

describe("promptText filtering", () => {
  it("states a person's prompt", () => {
    // Arrange, Act, Assert.
    expect(promptText(prompt("real"))).toBe("real");
  });

  it.each([
    ["a tool-result record the SDK wrote under the user role", toolResult("toolu_1")],
    ["a subagent's sidechain prompt", { type: "user", isSidechain: true, message: { role: "user", content: "sub" } }],
    ["the shim's own keep-alive prompt", prompt(keepalivePromptText(3))],
    ["a slash-command envelope", commandEnvelope("/compact")],
    ["a local-command caveat", caveat()],
    ["a compaction summary", compactSummary("This session is being continued. Summary: …")],
  ])("states nothing for %s", (_name, record) => {
    // Arrange, Act, Assert.
    expect(promptText(record)).toBeUndefined();
  });
});

describe("boundary records", () => {
  it("recognizes the /clear envelope", () => {
    // Arrange, Act, Assert.
    expect(isClearEnvelope(commandEnvelope("/clear"))).toBe(true);
  });

  it("does not take a /compact envelope for a clear", () => {
    // Arrange, Act, Assert.
    expect(isClearEnvelope(commandEnvelope("/compact"))).toBe(false);
  });

  it("recognizes the compaction boundary marker", () => {
    // Arrange, Act, Assert.
    expect(isCompactBoundary(compactBoundary())).toBe(true);
  });

  it("does not take the compaction summary for the boundary", () => {
    // Arrange, Act, Assert.
    expect(isCompactBoundary(compactSummary("summary"))).toBe(false);
  });
});

describe("promptText extraction", () => {
  it("takes only the text blocks from array content, keeping the words beside an image", () => {
    // Arrange.
    const record: PromptRecord = {
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

describe("userRecordText", () => {
  it("states the shim's own keep-alive text, which promptText refuses", () => {
    // Arrange.
    const text = keepalivePromptText(1);

    // Act, Assert.
    expect(userRecordText(prompt(text))).toBe(text);
  });

  it("states a command envelope's text", () => {
    // Arrange, Act, Assert.
    expect(userRecordText(commandEnvelope("/clear"))).toBe("<command-name>/clear</command-name>");
  });

  it("joins an array's text blocks alone", () => {
    // Arrange.
    const record: PromptRecord = {
      type: "user",
      message: { role: "user", content: [{ type: "text", text: " a" }, { type: "image" }, { type: "text", text: "b " }] },
    };

    // Act, Assert.
    expect(userRecordText(record)).toBe("ab");
  });

  it.each([
    ["an assistant record", { type: "assistant", message: { role: "assistant", content: "x" } }],
    ["a harness meta record", caveat()],
    ["a tool-result carrier", toolResult("t")],
    ["a blank message", prompt("   ")],
  ])("states nothing for %s", (_name, record) => {
    // Arrange, Act, Assert.
    expect(userRecordText(record)).toBeUndefined();
  });
});
