/**
 * test/engine/network-resume-prompt.test.ts — the prompt that asks the main
 * agent to continue the agents a network outage cut off.
 */
import { describe, expect, it } from "vitest";
import {
  NETWORK_RESUME_MARKER,
  isNetworkResumePrompt,
  networkResumePrompt,
  resumePromptTargets,
} from "../../src/engine/network-resume-prompt.js";

describe("the resume prompt", () => {
  it("names every agent it continues, in order, and reads them back", () => {
    // Arrange
    const targets = [
      { taskId: "a1111", description: "first" },
      { taskId: "a2222", description: "second `quoted`" },
    ];

    // Act
    const prompt = networkResumePrompt(targets);

    // Assert
    expect(resumePromptTargets(prompt)).toEqual(["a1111", "a2222"]);
  });

  it("opens with the marker", () => {
    // Act
    const prompt = networkResumePrompt([{ taskId: "a1", description: "d" }]);

    // Assert
    expect(prompt.startsWith(NETWORK_RESUME_MARKER)).toBe(true);
  });

  it("is recognized as the resume prompt", () => {
    // Act
    const prompt = networkResumePrompt([{ taskId: "a1", description: "d" }]);

    // Assert
    expect(isNetworkResumePrompt(prompt)).toBe(true);
  });

  it("an ordinary prompt is not a resume prompt", () => {
    // Act / Assert
    expect(isNetworkResumePrompt("please continue")).toBe(false);
  });

  it("an ordinary prompt names no agents", () => {
    // Act / Assert
    expect(resumePromptTargets("please continue\n- agent a1")).toEqual([]);
  });

  it("asks for SendMessage to the agent's own id", () => {
    // Act
    const prompt = networkResumePrompt([{ taskId: "a8586cc4fd87da466", description: "d" }]);

    // Assert
    expect(prompt).toContain("SendMessage");
    expect(prompt).toContain("`a8586cc4fd87da466`");
  });
});
