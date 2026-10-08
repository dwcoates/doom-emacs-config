/**
 * HOLD TRAY — held prompts and daemon-posed offers at the feed's tail.
 *
 * A held prompt is a `UserSaid` the daemon has NOT yet delivered, so it is
 * never a feed row: it lives in its own whole-list-replaced region. The rules
 * with teeth here are the two rulings:
 *
 *   - [accept] exists ONLY on `hold_for_turn_end` entries and on no other
 *     classification, and
 *   - a hold delivers itself when it clears, so `accepted` is also drawn as
 *     pure state.
 *
 * The tray is also where prompt LOSS would happen, so release and drop are
 * asserted to echo the TurnId verbatim rather than any index or position.
 */
import { afterEach, describe, expect, it } from "vitest";

import {
  DaemonHoldItemSchema,
  HeldPromptSchema,
  HeldOfferSchema,
} from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import {
  UpdateHeldPromptRequestSchema,
  UpdateHeldPromptResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_held_prompt_pb";
import { AnswerHeldOfferRequestSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_held_offer_pb";
import { FoldHeldPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_fold_held_prompt_pb";
import { ClassifierRoute } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_classifier_prompt_pb";
import { create } from "@bufbuild/protobuf";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import {
  HELD_OFFER_ARMS,
  HELD_OFFER_HEADLINE,
  HELD_TURN_ID,
  HOLD_BADGES,
  HOLD_ACCEPTABLE_ARM,
  HOLD_ARMS,
  HOLD_CLASSIFICATION_ARMS,
  WORKSPACE_ID,
  assertCoversOneof,
  heldOfferItem,
  heldPromptItem,
  holdTray,
  type HoldClassificationArm,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** Boot with one tray already scripted. */
async function withTray(init: Parameters<typeof holdTray>[0]): Promise<Harness> {
  harness = await startHarness({ arrange: (fake) => fake.setTray(WORKSPACE_ID, holdTray(init)) });
  return harness;
}

describe("arm coverage", () => {
  it("covers both tray item kinds", () => {
    assertCoversOneof(DaemonHoldItemSchema, "item", ["prompt", "offer"]);
  });

  it("covers every hold classification arm", () => {
    assertCoversOneof(HeldPromptSchema, "classification", [...HOLD_CLASSIFICATION_ARMS]);
  });

  it("covers every hold arm", () => {
    assertCoversOneof(HeldPromptSchema, "hold", [...HOLD_ARMS]);
  });

  it("covers every offer arm", () => {
    assertCoversOneof(HeldOfferSchema, "offer", [...HELD_OFFER_ARMS]);
  });

  it("covers every UpdateHeldPrompt action", () => {
    assertCoversOneof(UpdateHeldPromptRequestSchema, "action", ["release", "drop", "accept"]);
  });
});

describe("the heading", () => {
  it("draws no counter over the cards, the heading being retired from the wire", async () => {
    // Arrange / Act
    await withTray({});
    // Assert
    expect(harness.$('[data-component="hold-tray"]')?.textContent).not.toContain("held (");
  });
});

describe.each(HOLD_CLASSIFICATION_ARMS)("a %s hold", (classification) => {
  it("carries its classification arm", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem({ classification })] });
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.dataset.arm).toBe(classification);
  });

  it("draws the held prompt's own text verbatim", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem({ classification, text: "also fix the topbar" })] });
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.textContent).toContain(
      "also fix the topbar",
    );
  });
});

/** The words each classification puts on screen, so they draw DISTINCTLY. */
const CLASSIFICATION_TEXT: Record<HoldClassificationArm, string> = {
  classifying: "",
  interject: "it changes the current work",
  afterToolCall: "it adds to the running work",
  holdForTurnEnd: "it is a follow-up",
  uninterruptibleTurn: "",
  classificationError: "the classifier timed out",
  daemonHeld: "",
};

describe("classification detail", () => {
  it.each(
    HOLD_CLASSIFICATION_ARMS.filter((arm) => CLASSIFICATION_TEXT[arm] !== ""),
  )("draws the %s rationale verbatim", async (classification) => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem({ classification })] });
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.textContent).toContain(
      CLASSIFICATION_TEXT[classification],
    );
  });

  it("draws the uninterruptible turn's daemon-named note verbatim under the hold's badge", async () => {
    // Arrange / Act: the fixture's entry is held, so the hold claims the one
    // badge and the verdict is a note.
    await withTray({ items: [heldPromptItem({ classification: "uninterruptibleTurn" })] });
    // Assert
    expect(
      harness.$(`[data-held-turn="${HELD_TURN_ID}"] .queued-status-note`)?.textContent,
    ).toBe(HOLD_BADGES.uninterruptibleTurn?.detail);
  });

  // This set assertion boots one real-socket harness per classification.
  it("draws every classification with a distinct rendering", async () => {
    // Arrange: ONE page, each arm pushed live over its open tray stream.
    // Booting a daemon and a page per arm made this test's cost scale with
    // the arm count and overran its budget under concurrent load.
    await withTray({ items: [] });
    const rendered: string[] = [];
    for (const classification of HOLD_CLASSIFICATION_ARMS) {
      // Act
      harness.fake.setTray(WORKSPACE_ID, holdTray({ items: [heldPromptItem({ classification })] }));
      await harness.settle();
      rendered.push(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.innerHTML ?? "");
    }
    // Assert
    expect([new Set(rendered).size, rendered.includes("")]).toEqual([HOLD_CLASSIFICATION_ARMS.length, false]);
  });
});

describe.each(HOLD_ARMS)("a %s hold reason", (hold) => {
  it("carries its hold arm", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem({ hold })] });
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.dataset.hold).toBe(hold);
  });
});

describe("hold reasons draw distinctly", () => {
  it("gives every hold arm its own rendering", async () => {
    // Arrange: one page, each arm pushed live (see the classification test).
    await withTray({ items: [] });
    const rendered: string[] = [];
    for (const hold of HOLD_ARMS) {
      // Act
      harness.fake.setTray(WORKSPACE_ID, holdTray({ items: [heldPromptItem({ hold })] }));
      await harness.settle();
      rendered.push(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.innerHTML ?? "");
    }
    // Assert
    expect([new Set(rendered).size, rendered.includes("")]).toEqual([HOLD_ARMS.length, false]);
  });

  it("draws the shutdown hold's own schedule id", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem({ hold: "shutdown" })] });
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.textContent).toContain("sched-1");
  }, 1_500);
});

describe("the queued-at clock", () => {
  it("ages the held prompt as time passes", async () => {
    // Arrange
    await withTray({});
    const before = harness.$(`[data-held-turn="${HELD_TURN_ID}"] [data-queued]`)?.textContent;
    // Act
    await harness.tick(60_000);
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"] [data-queued]`)?.textContent).not.toBe(
      before,
    );
  });
});

describe("release and drop", () => {
  it("calls UpdateHeldPrompt with the release arm", async () => {
    // Arrange
    await withTray({});
    // Act
    await harness.click('[data-held-action="release"]');
    // Assert
    const [request] = harness.fake.calls<{ action: { case?: string } }>("updateHeldPrompt");
    expect(request.action.case).toBe("release");
  });

  it("calls UpdateHeldPrompt with the drop arm", async () => {
    // Arrange
    await withTray({});
    // Act
    await harness.click('[data-held-action="drop"]');
    // Assert
    const [request] = harness.fake.calls<{ action: { case?: string } }>("updateHeldPrompt");
    expect(request.action.case).toBe("drop");
  });

  it("echoes the held prompt's own TurnId", async () => {
    // Arrange
    await withTray({ items: [heldPromptItem({ turn: "turn-xyz" })] });
    // Act
    await harness.click('[data-held-turn="turn-xyz"] [data-held-action="release"]');
    // Assert
    const [request] = harness.fake.calls<{ turn?: { value: string } }>("updateHeldPrompt");
    expect(request.turn?.value).toBe("turn-xyz");
  });

  it("echoes the workspace", async () => {
    // Arrange
    await withTray({});
    // Act
    await harness.click('[data-held-action="release"]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("updateHeldPrompt");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("draws a refusal at the tray row rather than losing the prompt", async () => {
    // Arrange
    await withTray({});
    harness.fake.answer(
      "updateHeldPrompt",
      // Landing 4 TYPED every error: the cause oneof is set, and an unset one
      // is a malformed view rather than a refusal.
      create(UpdateHeldPromptResponseSchema, {
        result: { case: "error", value: { cause: { case: "noSuchHold", value: {} } } },
      }),
    );
    // Act
    await harness.click('[data-held-action="drop"]');
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"] .refusal`)).not.toBeNull();
  });

  it("leaves the row drawn when the action is refused", async () => {
    // Arrange
    await withTray({});
    harness.fake.answer(
      "updateHeldPrompt",
      // Landing 4 TYPED every error: the cause oneof is set, and an unset one
      // is a malformed view rather than a refusal.
      create(UpdateHeldPromptResponseSchema, {
        result: { case: "error", value: { cause: { case: "noSuchHold", value: {} } } },
      }),
    );
    // Act
    await harness.click('[data-held-action="drop"]');
    // Assert: the daemon still holds it, so the tray must still show it.
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)).not.toBeNull();
  });
});

describe("the fold above button", () => {
  /** Two held prompts, the second offered a fold into the first. */
  const foldable = (): Parameters<typeof holdTray>[0] => ({
    items: [
      heldPromptItem({ turn: "turn-ahead", text: "first words" }),
      heldPromptItem({ turn: "turn-behind", text: "second words", foldAbove: "turn-ahead" }),
    ],
  });

  it("is drawn on the entry the daemon offers a fold", async () => {
    // Arrange / Act
    await withTray(foldable());
    // Assert
    const button = harness.$('[data-held-turn="turn-behind"] [data-held-action="fold"]');
    expect(button?.textContent).toBe("fold above");
  });

  it("is not drawn on an entry the daemon offers no fold", async () => {
    // Arrange / Act
    await withTray(foldable());
    // Assert
    expect(harness.$('[data-held-turn="turn-ahead"] [data-held-action="fold"]')).toBeNull();
  });

  it("calls FoldHeldPrompt echoing the entry's own TurnId and the served entry ahead", async () => {
    // Arrange
    await withTray(foldable());
    // Act
    await harness.click('[data-held-turn="turn-behind"] [data-held-action="fold"]');
    // Assert
    const [request] = harness.fake.calls<{ turn?: { value: string }; above?: { value: string }; workspace?: { id: string } }>(
      "foldHeldPrompt",
    );
    expect([request.turn?.value, request.above?.value, request.workspace?.id]).toEqual([
      "turn-behind",
      "turn-ahead",
      WORKSPACE_ID,
    ]);
  });

  it("draws a refusal at the tray row, naming the arm", async () => {
    // Arrange
    await withTray(foldable());
    harness.fake.answer(
      "foldHeldPrompt",
      create(FoldHeldPromptResponseSchema, {
        result: { case: "error", value: { cause: { case: "aboveMoved", value: {} } } },
      }),
    );
    // Act
    await harness.click('[data-held-turn="turn-behind"] [data-held-action="fold"]');
    // Assert
    expect(harness.$('[data-held-turn="turn-behind"] .refusal')?.getAttribute("data-arm")).toBe("aboveMoved");
  });
});

describe("the update classifier button", () => {
  const classified = (): Parameters<typeof holdTray>[0] => ({
    items: [heldPromptItem({ turn: "turn-held", text: "after the tests pass, bump the version", classification: "holdForTurnEnd" })],
  });

  it("reveals the form and sends the typed change with the card's example", async () => {
    // Arrange
    await withTray(classified());
    await harness.click('[data-held-turn="turn-held"] [data-held-action="update-classifier"]');
    const input = harness.$('[data-held-turn="turn-held"] .classifier-update textarea') as HTMLTextAreaElement;
    input.value = "interrupt whenever something must happen after something else";
    input.dispatchEvent(new Event("input"));
    // Act
    await harness.click('[data-held-turn="turn-held"] [data-classifier-action="apply"]');
    // Assert
    const [request] = harness.fake.calls<{ instruction: string; example?: { text: string; route: number } }>(
      "updateClassifierPrompt",
    );
    expect([request.instruction, request.example?.text, request.example?.route]).toEqual([
      "interrupt whenever something must happen after something else",
      "after the tests pass, bump the version",
      ClassifierRoute.HOLD_FOR_TURN_END,
    ]);
  });

  it("says the daemon's commit at the form", async () => {
    // Arrange
    await withTray(classified());
    await harness.click('[data-held-turn="turn-held"] [data-held-action="update-classifier"]');
    const input = harness.$('[data-held-turn="turn-held"] .classifier-update textarea') as HTMLTextAreaElement;
    input.value = "interrupt for 'after'";
    input.dispatchEvent(new Event("input"));
    // Act
    await harness.click('[data-held-turn="turn-held"] [data-classifier-action="apply"]');
    // Assert
    expect(harness.text('[data-held-turn="turn-held"] .classifier-update-status')).toBe(
      "classifier updated (commit 012345678)",
    );
  });
});

describe("the accept button", () => {
  it("exists on a hold_for_turn_end entry", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem({ classification: HOLD_ACCEPTABLE_ARM })] });
    // Assert
    expect(harness.$('[data-held-action="accept"]')).not.toBeNull();
  });

  it("calls UpdateHeldPrompt with the accept arm", async () => {
    // Arrange
    await withTray({ items: [heldPromptItem({ classification: HOLD_ACCEPTABLE_ARM })] });
    // Act
    await harness.click('[data-held-action="accept"]');
    // Assert
    const [request] = harness.fake.calls<{ action: { case?: string } }>("updateHeldPrompt");
    expect(request.action.case).toBe("accept");
  });

  it("echoes the TurnId on an accept", async () => {
    // Arrange
    await withTray({
      items: [heldPromptItem({ classification: HOLD_ACCEPTABLE_ARM, turn: "turn-accept" })],
    });
    // Act
    await harness.click('[data-held-turn="turn-accept"] [data-held-action="accept"]');
    // Assert
    const [request] = harness.fake.calls<{ turn?: { value: string } }>("updateHeldPrompt");
    expect(request.turn?.value).toBe("turn-accept");
  });

  it.each(HOLD_CLASSIFICATION_ARMS.filter((arm) => arm !== HOLD_ACCEPTABLE_ARM))(
    "does not exist on a %s entry",
    async (classification) => {
      // Arrange / Act
      await withTray({ items: [heldPromptItem({ classification })] });
      // Assert
      expect(harness.$('[data-held-action="accept"]')).toBeNull();
    },
  );

  it("draws the accepted marker as state", async () => {
    // Arrange / Act
    await withTray({
      items: [heldPromptItem({ classification: HOLD_ACCEPTABLE_ARM, accepted: true })],
    });
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.dataset.accepted).toBe("true");
  });

  it("draws the un-accepted marker as state", async () => {
    // Arrange / Act
    await withTray({
      items: [heldPromptItem({ classification: HOLD_ACCEPTABLE_ARM, accepted: false })],
    });
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.dataset.accepted).toBe("false");
  });
});

describe("the merge-dequeue offer", () => {
  it("carries its offer arm", async () => {
    // Arrange / Act
    await withTray({ items: [heldOfferItem()] });
    // Assert
    expect(harness.$("[data-offer]")?.dataset.offer).toBe("mergeDequeue");
  });

  it("draws the composed headline verbatim", async () => {
    // Arrange / Act
    await withTray({ items: [heldOfferItem()] });
    // Assert
    expect(harness.$("[data-offer]")?.textContent).toContain(HELD_OFFER_HEADLINE);
  });

  it("calls AnswerHeldOffer with the keep decision", async () => {
    // Arrange
    await withTray({ items: [heldOfferItem()] });
    // Act
    await harness.click('[data-offer-decision="keep"]');
    // Assert
    const [request] = harness.fake.calls<{
      answer: { value?: { decision: { case?: string } } };
    }>("answerHeldOffer");
    expect(request.answer.value?.decision.case).toBe("keep");
  });

  it("calls AnswerHeldOffer with the release decision", async () => {
    // Arrange
    await withTray({ items: [heldOfferItem()] });
    // Act
    await harness.click('[data-offer-decision="release"]');
    // Assert
    const [request] = harness.fake.calls<{
      answer: { value?: { decision: { case?: string } } };
    }>("answerHeldOffer");
    expect(request.answer.value?.decision.case).toBe("release");
  });

  it("sends the merge-dequeue answer arm", async () => {
    // Arrange
    await withTray({ items: [heldOfferItem()] });
    // Act
    await harness.click('[data-offer-decision="keep"]');
    // Assert
    const [request] = harness.fake.calls<{ answer: { case?: string } }>("answerHeldOffer");
    expect(request.answer.case).toBe("mergeDequeue");
  });

  it("echoes the workspace on the answer", async () => {
    // Arrange
    await withTray({ items: [heldOfferItem()] });
    // Act
    await harness.click('[data-offer-decision="keep"]');
    // Assert
    const [request] = harness.fake.calls<{ workspace?: { id: string } }>("answerHeldOffer");
    expect(request.workspace?.id).toBe(WORKSPACE_ID);
  });

  it("covers every arm of the AnswerHeldOffer request", () => {
    assertCoversOneof(AnswerHeldOfferRequestSchema, "answer", [...HELD_OFFER_ARMS]);
  });
});

describe("an empty tray", () => {
  it("draws nothing at all, so the region collapses", async () => {
    // Arrange / Act
    await withTray({ items: [] });
    // Assert
    expect(harness.$('[data-component="hold-tray"]')?.childElementCount).toBe(0);
  });

  it("draws no held rows", async () => {
    // Arrange / Act
    await withTray({ items: [] });
    // Assert
    expect(harness.$$("[data-held-turn]")).toHaveLength(0);
  });

  it("brings the region back when an item arrives", async () => {
    // Arrange
    await withTray({ items: [] });
    // Act
    harness.fake.setTray(WORKSPACE_ID, holdTray({ items: [heldPromptItem({ turn: "turn-a" })] }));
    await harness.settle();
    // Assert
    expect(harness.$('[data-held-turn="turn-a"]')).not.toBeNull();
  });
});

describe("whole-list replacement", () => {
  it("drops an item the next push omits", async () => {
    // Arrange
    await withTray({ items: [heldPromptItem({ turn: "turn-a" }), heldPromptItem({ turn: "turn-b" })] });
    // Act
    harness.fake.setTray(WORKSPACE_ID, holdTray({ items: [heldPromptItem({ turn: "turn-a" })] }));
    await harness.settle();
    // Assert
    expect(harness.$('[data-held-turn="turn-b"]')).toBeNull();
  });

  it("draws both a prompt and an offer together", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem(), heldOfferItem()] });
    // Assert
    expect(harness.$$("[data-held-turn], [data-offer]")).toHaveLength(2);
  });
});
