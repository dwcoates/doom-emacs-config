/**
 * engine/conversation-text.ts — the conversation rendered as text for the
 * headless summarizer, cut into parts a call can read, and the prompt each
 * part is asked.
 */
import { describe, expect, it } from "vitest";
import {
  chainPermissionMode,
  chunkConversation,
  renderConversation,
  summarizerPrompt,
  SUMMARY_PART_CHARS,
} from "../../src/engine/conversation-text.js";
import { compactionPrompt } from "../../src/engine/compaction.js";
import { conversationv1 } from "../../src/proto.js";

const user = (content: unknown, extra: Record<string, unknown> = {}): Record<string, unknown> => ({
  type: "user",
  message: { role: "user", content },
  ...extra,
});

const assistant = (content: unknown[]): Record<string, unknown> => ({
  type: "assistant",
  message: { role: "assistant", content },
});

describe("renderConversation", () => {
  it.each([
    ["a person's string prompt", user("fix the parser"), ["User:\nfix the parser"]],
    ["a person's text blocks", user([{ type: "text", text: "fix it" }]), ["User:\nfix it"]],
    ["the assistant's prose", assistant([{ type: "text", text: "fixed" }]), ["Assistant:\nfixed"]],
    [
      "a tool call with its input",
      assistant([{ type: "tool_use", id: "toolu_1", name: "Bash", input: { command: "ls" } }]),
      ['Assistant called the tool Bash (toolu_1) with:\n{"command":"ls"}'],
    ],
    [
      "a tool's result",
      user([{ type: "tool_result", tool_use_id: "toolu_1", content: "a.ts" }]),
      ["Tool result for toolu_1:\na.ts"],
    ],
    [
      "a tool's failed result",
      user([{ type: "tool_result", tool_use_id: "toolu_1", is_error: true, content: [{ type: "text", text: "no such file" }] }]),
      ["Tool result for toolu_1 (an error):\nno such file"],
    ],
    ["an attached image", user([{ type: "image", source: {} }]), ["User attached an image."]],
    ["an attached document", user([{ type: "document", source: {} }]), ["User attached a document."]],
    [
      "an earlier compaction's summary",
      user("the summary so far", { isCompactSummary: true }),
      ["[The conversation before this point, as an earlier compaction summarized it]\nthe summary so far"],
    ],
  ] as const)("renders %s", (_case, record, pieces) => {
    expect(renderConversation([record])).toEqual(pieces);
  });

  it.each([
    ["a meta record", user("<command-caveat>", { isMeta: true })],
    ["thinking", assistant([{ type: "thinking", thinking: "hmm" }])],
    ["redacted thinking", assistant([{ type: "redacted_thinking", data: "x" }])],
    ["a compaction boundary", { type: "system", subtype: "compact_boundary" }],
    ["bookkeeping", { type: "last-prompt" }],
    ["a blank prompt", user("   ")],
    ["an answer with no content", { type: "assistant", message: { model: "claude-opus-5" } }],
  ])("renders nothing for %s", (_case, record) => {
    expect(renderConversation([record])).toEqual([]);
  });

  it("keeps the chain's order", () => {
    expect(renderConversation([user("one"), assistant([{ type: "text", text: "two" }])])).toEqual([
      "User:\none",
      "Assistant:\ntwo",
    ]);
  });

  it("joins one record's blocks into one piece", () => {
    expect(
      renderConversation([assistant([{ type: "text", text: "looking" }, { type: "tool_use", id: "t", name: "Read", input: {} }])]),
    ).toEqual(["Assistant:\nlooking\n\nAssistant called the tool Read (t) with:\n{}"]);
  });
});

describe("chunkConversation", () => {
  it("answers no parts for an empty conversation", () => {
    expect(chunkConversation([], 10)).toEqual([]);
  });

  it("joins pieces that fit into one part", () => {
    expect(chunkConversation(["ab", "cd"], 10)).toEqual(["ab\n\ncd"]);
  });

  it("starts a new part where the next piece would exceed the budget", () => {
    expect(chunkConversation(["abcd", "efgh"], 9)).toEqual(["abcd", "efgh"]);
  });

  it("fills a part exactly to its budget", () => {
    expect(chunkConversation(["abc", "def"], 8)).toEqual(["abc\n\ndef"]);
  });

  it("cuts a piece larger than the budget at the budget", () => {
    expect(chunkConversation(["abcdefgh"], 3)).toEqual(["abc", "def", "gh"]);
  });

  it("never lets a part exceed its budget", () => {
    const parts = chunkConversation(["a".repeat(7), "b".repeat(2), "c".repeat(11)], 5);

    expect(parts.every((part) => part.length <= 5)).toBe(true);
  });

  it("refuses a budget that is not positive", () => {
    expect(() => chunkConversation(["a"], 0)).toThrow("a part's budget must be positive, not 0");
  });

  it("reads at most SUMMARY_PART_CHARS by default", () => {
    expect(chunkConversation(["a".repeat(SUMMARY_PART_CHARS + 1)]).map((part) => part.length)).toEqual([SUMMARY_PART_CHARS, 1]);
  });
});

describe("summarizerPrompt", () => {
  const ALL = conversationv1.SessionCompactScope.ALL;

  it("asks a conversation read in one part with the compaction's own instruction, unchanged", () => {
    expect(summarizerPrompt({ text: "User:\nhi", index: 1, count: 1 }, ALL)).toBe(
      `The conversation to summarize:\n\n<conversation>\nUser:\nhi\n</conversation>\n\n${compactionPrompt(ALL)}`,
    );
  });

  it("asks the scope's own instruction", () => {
    const scope = conversationv1.SessionCompactScope.PROMPTS;

    expect(summarizerPrompt({ text: "x", index: 1, count: 1 }, scope).endsWith(compactionPrompt(scope))).toBe(true);
  });

  it("says which part of how many the first part is, with no summary before it", () => {
    const prompt = summarizerPrompt({ text: "x", index: 1, count: 3 }, ALL);

    expect([prompt.includes("arrives in 3 parts, oldest first. This is part 1."), prompt.includes("<summary_so_far>")]).toEqual([
      true,
      false,
    ]);
  });

  it("carries the summary so far into a later part, and asks for one folded summary", () => {
    const prompt = summarizerPrompt({ text: "x", index: 2, count: 3, summarySoFar: "so far" }, ALL);

    expect([
      prompt.includes("The summary of parts 1 to 1:\n\n<summary_so_far>\nso far\n</summary_so_far>"),
      prompt.endsWith(
        `${compactionPrompt(ALL)}\n\nFold the summary so far and this part into ONE summary of the conversation through this part.`,
      ),
    ]).toEqual([true, true]);
  });
});

describe("chainPermissionMode", () => {
  it("reads the last user record's mode", () => {
    expect(
      chainPermissionMode([
        { type: "user", permissionMode: "default" },
        { type: "assistant" },
        { type: "user", permissionMode: "acceptEdits" },
        { type: "assistant" },
      ]),
    ).toBe("acceptEdits");
  });

  it("skips a later user record that names none", () => {
    expect(chainPermissionMode([{ type: "user", permissionMode: "auto" }, { type: "user" }])).toBe("auto");
  });

  it("reads no mode off a record that is not a user's", () => {
    expect(chainPermissionMode([{ type: "system", permissionMode: "plan" }])).toBeUndefined();
  });

  it("answers absence for a chain that names none", () => {
    expect(chainPermissionMode([])).toBeUndefined();
  });
});
