// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { AgentModelSchema } from "../../../../proto/gen/ts/conversation/v1/api_pb";
import { SessionCompactScope } from "../../../../proto/gen/ts/conversation/v1/session_pb";
import {
  buildAnswerColdGateRequest,
  buildAnswerPermissionRequest,
  buildUpdateQuestionDraftRequest,
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

  it("leaves the deny reason unset: a reason is the editor's", () => {
    const req = buildAnswerPermissionRequest(WORKSPACE, ROW, { kind: "deny" });
    expect(req.answer.case === "deny" ? req.answer.value.reason : "set").toBeUndefined();
  });
});

describe("buildUpdateQuestionDraftRequest", () => {
  it("echoes the workspace and the card's row", () => {
    const req = buildUpdateQuestionDraftRequest(WORKSPACE, ROW, { kind: "show", questionText: "q?" });
    expect([req.workspace, req.question]).toEqual([WORKSPACE, ROW]);
  });

  it("sets the show arm, echoing the question text verbatim", () => {
    const req = buildUpdateQuestionDraftRequest(WORKSPACE, ROW, { kind: "show", questionText: "Which  auth?" });
    expect(req.edit).toMatchObject({ case: "show", value: { questionText: "Which  auth?" } });
  });

  it("sets the pick arm, echoing the question text and the label verbatim", () => {
    const req = buildUpdateQuestionDraftRequest(WORKSPACE, ROW, {
      kind: "pick",
      questionText: "Which auth?",
      label: "OAuth 2.0",
    });
    expect(req.edit).toMatchObject({ case: "pick", value: { questionText: "Which auth?", label: "OAuth 2.0" } });
  });

  it("sets the confirm arm, echoing the question text verbatim", () => {
    const req = buildUpdateQuestionDraftRequest(WORKSPACE, ROW, { kind: "confirm", questionText: "Which suites?" });
    expect(req.edit).toMatchObject({ case: "confirm", value: { questionText: "Which suites?" } });
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

  it("sets the compact_and_resume arm for compact and resume", () => {
    const req = buildAnswerColdGateRequest(WORKSPACE, ROW, {
      kind: "compactAndResume",
      model: MODEL,
      scope: SessionCompactScope.ALL,
    });
    expect(req.choice.case).toBe("compactAndResume");
  });

  it("echoes compact and resume's summarizer and scope as the menu served them", () => {
    const req = buildAnswerColdGateRequest(WORKSPACE, ROW, {
      kind: "compactAndResume",
      model: MODEL,
      scope: SessionCompactScope.PROMPTS,
    });
    const value = req.choice.case === "compactAndResume" ? req.choice.value : null;
    expect([value?.model, value?.scope]).toEqual([MODEL, SessionCompactScope.PROMPTS]);
  });
});
