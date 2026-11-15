/**
 * convert/ask-settles.ts — the ONE shape of an ask's settle.
 *
 * Two producers write an ask's settle: the permission gate, for an ask it held
 * (answered, denied, stood down, withdrawn, abandoned), and the session-start
 * reconciliation, for an ask the record holds open that no gate holds
 * (store/reconcile.ts `closingAbandonedAsk`). Both write the same frame, so it
 * is built here once: a settled frame restates what it settles (a question its
 * batch, a consent ask the call it gated) and when, because a reader serves it
 * with no start beside it.
 */
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../proto.js";

/** The settle of the question ID: its batch restated, when, and how it concluded. */
export function questionSettle(
  id: conversationv1.AgentQuestionId,
  batch: conversationv1.AgentQuestionBatch | undefined,
  settledAt: conversationv1.AgentActivitySettledAt,
  outcome: conversationv1.AgentQuestionSuccess["outcome"],
): conversationv1.AgentUpdate["update"] {
  return {
    case: "question",
    value: create(conversationv1.AgentQuestionSchema, {
      id,
      result: { case: "success", value: create(conversationv1.AgentQuestionSuccessSchema, { batch, settledAt, outcome }) },
    }),
  };
}

/** The settle of the consent ask ID: the call it gated, when, and the decision. */
export function permissionSettle(
  id: conversationv1.AgentPermissionId,
  gatedCall: conversationv1.AgentActivityId | undefined,
  settledAt: conversationv1.AgentActivitySettledAt,
  decision: conversationv1.AgentPermissionSuccess["decision"],
): conversationv1.AgentUpdate["update"] {
  return {
    case: "permission",
    value: create(conversationv1.AgentPermissionSchema, {
      id,
      gatedCall,
      result: { case: "success", value: create(conversationv1.AgentPermissionSuccessSchema, { decision, settledAt }) },
    }),
  };
}

/** A question's ABANDONED outcome: the session that asked it is gone. */
export function abandonedQuestionOutcome(): conversationv1.AgentQuestionSuccess["outcome"] {
  return { case: "abandoned", value: create(conversationv1.AgentQuestionAbandonedSchema, {}) };
}

/** A consent ask's ABANDONED decision: the session that asked it is gone. */
export function abandonedPermissionDecision(): conversationv1.AgentPermissionSuccess["decision"] {
  return { case: "abandoned", value: create(conversationv1.AgentPermissionAbandonedSchema, {}) };
}
