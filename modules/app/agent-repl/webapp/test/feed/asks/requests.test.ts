// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { AgentModelSchema } from "../../../../proto/gen/ts/conversation/v1/api_pb";
import { SessionCompactScope } from "../../../../proto/gen/ts/conversation/v1/session_pb";
import {
  buildAnswerColdGateRequest,
  buildAnswerPermissionRequest,
  buildAnswerQuestionRequest,
} from "../../../src/feed/asks/requests.js";
import { feedId, WORKSPACE } from "../harness.js";

const ROW = feedId("ask-1");
const MODEL = create(AgentModelSchema, { name: "claude-opus-5" });

describe("buildAnswerPermissionRequest", () => {
  it("echoes the workspace", () => {
    const req = buildAnswerPermissionRequest(WORKSPACE, ROW, { kind: "allowOnce" });
    expect(req.workspace).toEqual(WORKSPACE);
  });

  it("echoes the card's row, exactly as the feed served it", () => {
    const req = buildAnswerPermissionRequest(WORKSPACE, ROW, { kind: "allowOnce" });
    expect(req.permission).toEqual(ROW);
  });

  const arms = [
    { kind: "allowOnce", arm: "allowOnce" },
    { kind: "allowStanding", arm: "allowStanding" },
    { kind: "deny", arm: "deny" },
  ] as const;

  for (const c of arms) {
    it(`sets the ${c.arm} arm`, () => {
      const req = buildAnswerPermissionRequest(WORKSPACE, ROW, { kind: c.kind });
      expect(req.answer.case).toBe(c.arm);
    });
  }

  it("carries the deny reason when the user typed one", () => {
    const req = buildAnswerPermissionRequest(WORKSPACE, ROW, {
      kind: "deny",
      reason: "that path is not mine",
    });
    expect(req.answer.case === "deny" ? req.answer.value.reason?.text : null).toBe(
      "that path is not mine",
    );
  });

  it("leaves the deny reason unset when nothing was typed", () => {
    const req = buildAnswerPermissionRequest(WORKSPACE, ROW, { kind: "deny" });
    expect(req.answer.case === "deny" ? req.answer.value.reason : "set").toBeUndefined();
  });
});

describe("buildAnswerQuestionRequest", () => {
  it("echoes the card's row", () => {
    const req = buildAnswerQuestionRequest(WORKSPACE, ROW, []);
    expect(req.question).toEqual(ROW);
  });

  it("sends one answer per question, in the order collected", () => {
    const req = buildAnswerQuestionRequest(WORKSPACE, ROW, [
      { questionText: "first?", chosen: ["a"] },
      { questionText: "second?", chosen: ["b"] },
    ]);
    expect(req.answers.map((a) => a.questionText)).toEqual(["first?", "second?"]);
  });

  it("echoes each question's text verbatim", () => {
    const req = buildAnswerQuestionRequest(WORKSPACE, ROW, [
      { questionText: "Which  auth  method?", chosen: [] , otherText: "none" },
    ]);
    expect(req.answers[0]?.questionText).toBe("Which  auth  method?");
  });

  it("echoes the chosen labels verbatim", () => {
    const req = buildAnswerQuestionRequest(WORKSPACE, ROW, [
      { questionText: "q", chosen: ["OAuth 2.0", "API key"] },
    ]);
    expect(req.answers[0]?.chosen).toEqual(["OAuth 2.0", "API key"]);
  });

  it("sends no chosen labels for a free-text-only answer", () => {
    const req = buildAnswerQuestionRequest(WORKSPACE, ROW, [
      { questionText: "q", chosen: [], otherText: "something else" },
    ]);
    expect(req.answers[0]?.chosen).toEqual([]);
  });

  it("carries the free text when the user typed any", () => {
    const req = buildAnswerQuestionRequest(WORKSPACE, ROW, [
      { questionText: "q", chosen: [], otherText: "something else" },
    ]);
    expect(req.answers[0]?.otherText?.text).toBe("something else");
  });

  it("leaves the free text unset when nothing was typed", () => {
    const req = buildAnswerQuestionRequest(WORKSPACE, ROW, [
      { questionText: "q", chosen: ["a"] },
    ]);
    expect(req.answers[0]?.otherText).toBeUndefined();
  });
});

describe("buildAnswerColdGateRequest", () => {
  it("echoes the gate's row", () => {
    const req = buildAnswerColdGateRequest(WORKSPACE, ROW, { kind: "pay" });
    expect(req.gate).toEqual(ROW);
  });

  const arms = [
    { kind: "pay", arm: "pay" },
    { kind: "clear", arm: "clear" },
  ] as const;

  for (const c of arms) {
    it(`sets the ${c.arm} arm`, () => {
      const req = buildAnswerColdGateRequest(WORKSPACE, ROW, { kind: c.kind });
      expect(req.choice.case).toBe(c.arm);
    });
  }

  it("echoes the summarizer the menu served, whole", () => {
    const req = buildAnswerColdGateRequest(WORKSPACE, ROW, {
      kind: "compact",
      model: MODEL,
      scope: SessionCompactScope.PROMPTS,
    });
    expect(req.choice.case === "compact" ? req.choice.value.model : null).toEqual(MODEL);
  });

  it("echoes the scope the menu served", () => {
    const req = buildAnswerColdGateRequest(WORKSPACE, ROW, {
      kind: "compact",
      model: MODEL,
      scope: SessionCompactScope.RESPONSES,
    });
    expect(req.choice.case === "compact" ? req.choice.value.scope : null).toBe(
      SessionCompactScope.RESPONSES,
    );
  });
});
