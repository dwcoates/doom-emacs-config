import { describe, expect, it } from "vitest";
import { create, equals } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import {
  abandonedPermissionDecision,
  abandonedQuestionOutcome,
  permissionSettle,
  questionSettle,
} from "../../src/convert/ask-settles.js";
import { closingAbandonedAsk } from "../../src/store/reconcile.js";
import { settledAt } from "../../src/convert/entries.js";

const QID = create(conversationv1.AgentQuestionIdSchema, { value: "toolu_q" });
const PID = create(conversationv1.AgentPermissionIdSchema, { value: "toolu_p" });
const CALL = create(conversationv1.AgentActivityIdSchema, { value: "toolu_p" });
const BATCH = create(conversationv1.AgentQuestionBatchSchema, {
  questions: [create(conversationv1.AgentQuestionAskedSchema, { header: "Pick" })],
});
const AT = settledAt(5_000, 1_000);

describe("questionSettle", () => {
  it("restates the batch, the instant and the outcome under the ask's id", () => {
    // Act
    const update = questionSettle(QID, BATCH, AT, abandonedQuestionOutcome());

    // Assert
    const question = update.case === "question" ? update.value : undefined;
    const success = question?.result.case === "success" ? question.result.value : undefined;
    expect([question?.id?.value, success?.outcome.case, success?.settledAt?.atMs]).toEqual(["toolu_q", "abandoned", 5_000n]);
    expect(success?.batch).toEqual(BATCH);
  });
});

describe("permissionSettle", () => {
  it("names the gated call, the instant and the decision under the ask's id", () => {
    // Act
    const update = permissionSettle(PID, CALL, AT, abandonedPermissionDecision());

    // Assert
    const permission = update.case === "permission" ? update.value : undefined;
    const success = permission?.result.case === "success" ? permission.result.value : undefined;
    expect([permission?.id?.value, permission?.gatedCall?.value, success?.decision.case, success?.settledAt?.atMs]).toEqual([
      "toolu_p",
      "toolu_p",
      "abandoned",
      5_000n,
    ]);
  });
});

describe("the reconciliation's abandoned settle", () => {
  it("is exactly the shared question settle", () => {
    // Arrange
    const ask = create(conversationv1.SessionOpenAskSchema, {
      agent: { value: "toolu_child" },
      ask: {
        case: "question",
        value: create(conversationv1.AgentQuestionSchema, {
          id: QID,
          result: {
            case: "start",
            value: create(conversationv1.AgentQuestionStartSchema, { batch: BATCH, startedAt: { atMs: 1_000n } }),
          },
        }),
      },
    });

    // Act
    const entry = closingAbandonedAsk(ask, 5_000);

    // Assert
    const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
    const update = frame?.result.case === "update" ? frame.result.value : undefined;
    const want = create(conversationv1.AgentUpdateSchema, { update: questionSettle(QID, BATCH, AT, abandonedQuestionOutcome()) });
    expect(update !== undefined && equals(conversationv1.AgentUpdateSchema, update, want)).toBe(true);
  });
});
