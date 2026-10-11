/**
 * requests — the verbs the ask cards send, built in ONE place,
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
 * NOTHING TYPED TRAVELS HERE. A deny's reason and a question's typed answer
 * are the editor's (a plain send made while the ask stands), so no builder in
 * this file carries free text.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import type { WorkspaceRef } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FeedId } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  AnswerPermissionRequestSchema,
  type AnswerPermissionRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_permission_pb";
import {
  UpdateQuestionDraftRequestSchema,
  type UpdateQuestionDraftRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_update_question_draft_pb";
import {
  AnswerColdGateRequestSchema,
  type AnswerColdGateRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_cold_gate_pb";
import type { AgentModel } from "../../../../proto/gen/ts/conversation/v1/api_pb";
import type { SessionCompactScope } from "../../../../proto/gen/ts/conversation/v1/session_pb";

/**
 * Which button of the consent card was pressed. A deny carries no reason: a
 * deny WITH a reason is typed in the editor's composer, never on the card.
 */
export type PermissionAnswer = { kind: "allowOnce" } | { kind: "allowStanding" } | { kind: "deny" };

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
      return { case: "deny", value: {} };
  }
}

/** One click on a standing question card, naming its question by served text. */
export type QuestionDraftEdit =
  /** A tab: show this question. */
  | { kind: "show"; questionText: string }
  /** An option: answer a single-select with it, or toggle a multi-select's tick. */
  | { kind: "pick"; questionText: string; label: string }
  /** A multi-select's confirm: its ticks are its answer. */
  | { kind: "confirm"; questionText: string };

/**
 * Edit a standing question card's daemon-owned draft with one click.
 *
 * There is no "answer the batch" verb on this end any more: the click that
 * answers the last question is the one the daemon delivers the batch on.
 */
export function buildUpdateQuestionDraftRequest(
  workspace: WorkspaceRef,
  question: FeedId,
  edit: QuestionDraftEdit,
): UpdateQuestionDraftRequest {
  return create(UpdateQuestionDraftRequestSchema, {
    workspace,
    question,
    edit: questionDraftArm(edit),
  });
}

/** The request's oneof arm for EDIT, as an INIT shape (see permissionArm). */
function questionDraftArm(
  edit: QuestionDraftEdit,
): MessageInitShape<typeof UpdateQuestionDraftRequestSchema>["edit"] {
  switch (edit.kind) {
    case "show":
      return { case: "show", value: { questionText: edit.questionText } };
    case "pick":
      return { case: "pick", value: { questionText: edit.questionText, label: edit.label } };
    case "confirm":
      return { case: "confirm", value: { questionText: edit.questionText } };
  }
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
