/**
 * requests — the three ANSWER verbs the ask cards send, built in ONE place,
 * symmetrically with the drawing side's `draw<Message>` functions.
 *
 * WHY A BUILDER PER REQUEST. Every one of these verbs is an ECHO: the row's own
 * `FeedId`, the question's served text, an option's served label, the gate's
 * served `AgentModel` and `SessionCompactScope`. Nothing is parsed, nothing is
 * minted, and nothing is constructed from display text — the daemon (and the
 * shim behind it) reconstructs which ask is being answered from the values it
 * served, so a value this end "cleaned up" is a value it can no longer match.
 * A named builder per request is where that discipline is visible and testable
 * in one file, instead of being a rule reviewers have to re-check at every
 * click handler.
 *
 * ABSENCE IS ABSENCE. An unset deny reason and an unset free-text answer are
 * left unset rather than sent as an empty string: "the user typed nothing" and
 * "the user typed the empty string" are different claims, and the schema models
 * the first as the field not being there.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import type { WorkspaceRef } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FeedId } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  AnswerPermissionRequestSchema,
  type AnswerPermissionRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_permission_pb";
import {
  AnswerQuestionRequestSchema,
  type AnswerQuestionRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_question_pb";
import {
  AnswerColdGateRequestSchema,
  type AnswerColdGateRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_cold_gate_pb";
import type { AgentModel } from "../../../../proto/gen/ts/conversation/v1/api_pb";
import type { SessionCompactScope } from "../../../../proto/gen/ts/conversation/v1/session_pb";

/** Which button of the consent card was pressed. */
export type PermissionAnswer =
  | { kind: "allowOnce" }
  | { kind: "allowStanding" }
  /** The reason is absent unless the user actually typed one. */
  | { kind: "deny"; reason?: string };

/**
 * Answer one consent card.
 *
 * `allow_standing` is legal ONLY when the card carried `standing_offered`; that
 * is enforced where the button is drawn (it simply is not drawn otherwise), so
 * this builder states the answer it was given rather than second-guessing it.
 */
export function buildAnswerPermissionRequest(
  workspace: WorkspaceRef,
  permission: FeedId,
  answer: PermissionAnswer,
): AnswerPermissionRequest {
  return create(AnswerPermissionRequestSchema, {
    workspace,
    permission,
    answer: permissionArm(answer),
  });
}

/**
 * The request's oneof arm for ANSWER, as an INIT shape.
 *
 * The init shape rather than a built message, because the arm is handed to
 * `create` along with the rest of the request: building the arm separately would
 * mean naming each empty arm message's type by hand, which is exactly the kind
 * of hand-written wire detail the generated code exists to remove.
 */
function permissionArm(
  answer: PermissionAnswer,
): MessageInitShape<typeof AnswerPermissionRequestSchema>["answer"] {
  switch (answer.kind) {
    case "allowOnce":
      return { case: "allowOnce", value: {} };
    case "allowStanding":
      return { case: "allowStanding", value: {} };
    case "deny":
      return {
        case: "deny",
        value:
          answer.reason === undefined ? {} : { reason: { text: answer.reason } },
      };
  }
}

/** One question's answer, as the card collected it. */
export interface QuestionAnswer {
  /** The question's text, ECHOED verbatim — how the ask is reconstructed. */
  questionText: string;
  /** The chosen option labels, echoed verbatim. Empty for free text alone. */
  chosen: readonly string[];
  /** The free text, when the user typed any. */
  otherText?: string;
}

/**
 * Answer a whole question batch in ONE verb.
 *
 * The batch is one ask, so it is one submission: sending a verb per question
 * would let a batch be half-answered, which is a state neither the card nor the
 * shim has a meaning for.
 */
export function buildAnswerQuestionRequest(
  workspace: WorkspaceRef,
  question: FeedId,
  answers: readonly QuestionAnswer[],
): AnswerQuestionRequest {
  return create(AnswerQuestionRequestSchema, {
    workspace,
    question,
    answers: answers.map((answer) => ({
      questionText: answer.questionText,
      chosen: [...answer.chosen],
      ...(answer.otherText === undefined ? {} : { otherText: { text: answer.otherText } }),
    })),
  });
}

/** Which remediation the cold gate's reader chose. */
export type ColdGateChoice =
  | { kind: "pay" }
  | { kind: "clear" }
  /** Both values are echoed from the gate's own menu. */
  | { kind: "compact"; model: AgentModel; scope: SessionCompactScope };

/** Answer the cold-context gate. */
export function buildAnswerColdGateRequest(
  workspace: WorkspaceRef,
  gate: FeedId,
  choice: ColdGateChoice,
): AnswerColdGateRequest {
  return create(AnswerColdGateRequestSchema, {
    workspace,
    gate,
    choice:
      choice.kind === "compact"
        ? { case: "compact", value: { model: choice.model, scope: choice.scope } }
        : choice.kind === "pay"
          ? { case: "pay", value: {} }
          : { case: "clear", value: {} },
  });
}
