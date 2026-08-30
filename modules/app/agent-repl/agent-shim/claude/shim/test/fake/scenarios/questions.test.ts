/**
 * The question family. A question is a gated tool call whose ANSWERS ride back
 * as the allow's `updatedInput`, so each test supplies a gate that answers.
 */
import { describe, expect, it } from "vitest";

import type { CanUseToolLike, PermissionResultLike } from "../../../src/sdk/types.js";
import { driveScenario, theResult, toolUseResults, toolUses } from "../harness.js";

/** A gate that answers every question with its first option's label. */
const answerFirst: CanUseToolLike = async (name, input) => {
  if (name !== "AskUserQuestion") return { behavior: "allow", updatedInput: input } as PermissionResultLike;
  const questions = (input as { questions: { question: string; options: { label: string }[] }[] }).questions;
  const answers: Record<string, string> = {};
  for (const q of questions) answers[q.question] = q.options[0]?.label ?? "";
  return { behavior: "allow", updatedInput: { ...input, answers } } as PermissionResultLike;
};

const declineToAnswer: CanUseToolLike = async () =>
  ({ behavior: "deny", message: "the question was never answered" }) as PermissionResultLike;

const questionInput = (driven: Awaited<ReturnType<typeof driveScenario>>): {
  questions: { question: string; multiSelect: boolean; options: unknown[] }[];
} => (toolUses(driven)[0] as { input: { questions: never[] } }).input as never;

describe("a single-select question", () => {
  it("asks through the gate with the question's own tool_use id", async () => {
    // Arrange
    let toolUseId = "";
    const spy: CanUseToolLike = async (name, input, options) => {
      toolUseId = options.toolUseID;
      return answerFirst(name, input, options);
    };

    // Act
    const driven = await driveScenario(["!ask-single"], { canUseTool: spy });

    // Assert. AgentQuestionId IS the ask's tool_use_id, verbatim.
    expect((toolUses(driven)[0] as { id: string }).id).toBe(toolUseId);
  });

  it("offers four options, which the vendor's own limit allows", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ask-single"], { canUseTool: answerFirst });

    // Assert
    expect(questionInput(driven).questions[0]?.options).toHaveLength(4);
  });

  it("answers with a map KEYED BY THE QUESTION TEXT, as the corpus does", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ask-single"], { canUseTool: answerFirst });
    const result = toolUseResults(driven.transcript())[0] as { answers: Record<string, string> };

    // Assert
    expect(result.answers).toEqual({
      "How do you want the new branch set up?": "New worktree off master",
    });
  });
});

describe("a multi-question batch", () => {
  it("asks a multi-select and a single-select in ONE call", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ask-multi"], { canUseTool: answerFirst });

    // Assert. The mixed batch is what makes positional answers impossible.
    expect(questionInput(driven).questions.map((q) => q.multiSelect)).toEqual([true, false]);
  });

  it("answers both questions in one map", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ask-multi"], { canUseTool: answerFirst });
    const result = toolUseResults(driven.transcript())[0] as { answers: Record<string, string> };

    // Assert
    expect(Object.keys(result.answers).sort()).toEqual(
      ["Run them now?", "Which suites should run?"].sort(),
    );
  });
});

describe("a free-text answer", () => {
  it("carries prose no listed option matches", async () => {
    // Arrange + Act. The gate declines to pick, so the scenario's own free text
    // stands in for the vendor's automatic "Other".
    const driven = await driveScenario(["!ask-free"], { canUseTool: declineToAnswer });
    const result = toolUseResults(driven.transcript())[0] as { answers: Record<string, string> };
    const labels = questionInput(driven).questions[0]?.options as { label: string }[];

    // Assert
    expect(labels.map((o) => o.label)).not.toContain(
      result.answers["Which model should the sweep use?"],
    );
  });

  it("still concludes the turn successfully", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ask-free"], { canUseTool: declineToAnswer });

    // Assert
    expect(theResult(driven).subtype).toBe("success");
  });
});

describe("an unanswered question", () => {
  it("answers with an EMPTY map rather than a missing one", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ask-unanswered"], { canUseTool: declineToAnswer });
    const result = toolUseResults(driven.transcript())[0] as { answers: Record<string, string> };

    // Assert. "Nobody answered" and "the answers are missing" are different facts.
    expect(result.answers).toEqual({});
  });

  it("marks the tool_result an error, which is how the model learns it was refused", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ask-unanswered"], { canUseTool: declineToAnswer });
    const block = (
      driven.transcript().find((l) => l.toolUseResult !== undefined)?.message as {
        content: { is_error: boolean }[];
      }
    ).content[0];

    // Assert
    expect(block?.is_error).toBe(true);
  });

  it("relays the gate's own refusal message", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!ask-unanswered"], { canUseTool: declineToAnswer });
    const block = (
      driven.transcript().find((l) => l.toolUseResult !== undefined)?.message as {
        content: { content: string }[];
      }
    ).content[0];

    // Assert
    expect(block?.content).toBe("Error: the question was never answered");
  });
});
