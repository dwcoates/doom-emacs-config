/**
 * HOLD TRAY — held prompts at the feed's tail.
 *
 * A held prompt is a `UserSaid` the daemon has NOT yet delivered, so it is
 * never a feed row: it lives in its own whole-list-replaced region. A hold
 * delivers itself when it clears, so its only answers are release and drop.
 *
 * The tray is also where prompt LOSS would happen, so release and drop are
 * asserted to echo the TurnId verbatim rather than any index or position.
 */
import { afterEach, describe, expect, it } from "vitest";

import {
  FeedHeldItemSchema,
  HeldIngressPromptBadgeSchema,
  HeldPromptSchema,
} from "../../../proto/gen/ts/frontend/v1/feed_held_pb";
import {
  UpdateHeldPromptRequestSchema,
  UpdateHeldPromptResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_held_prompt_pb";
import { FoldHeldPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_fold_held_prompt_pb";
import { ClassifierRoute } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_classifier_prompt_pb";
import { create } from "@bufbuild/protobuf";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import {
  HELD_TURN_ID,
  HOLD_BADGES,
  HOLD_ARMS,
  HOLD_CLASSIFICATION_ARMS,
  INGRESS_BADGE_ARMS,
  WORKSPACE_ID,
  assertCoversOneof,
  heldIngressItem,
  heldPromptItem,
  heldSection,
  type HoldClassificationArm,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** Boot with one tray already scripted. */
async function withTray(init: Parameters<typeof heldSection>[0]): Promise<Harness> {
  harness = await startHarness({ arrange: (fake) => fake.setHeld(WORKSPACE_ID, heldSection(init)) });
  return harness;
}

describe("arm coverage", () => {
  it("covers every held item kind", () => {
    assertCoversOneof(FeedHeldItemSchema, "item", ["prompt", "ingress"]);
  });

  it("covers every ingress badge arm", () => {
    assertCoversOneof(HeldIngressPromptBadgeSchema, "standsFor", [...INGRESS_BADGE_ARMS]);
  });

  it("covers every hold classification arm", () => {
    assertCoversOneof(HeldPromptSchema, "classification", [...HOLD_CLASSIFICATION_ARMS]);
  });

  it("covers every hold arm", () => {
    assertCoversOneof(HeldPromptSchema, "hold", [...HOLD_ARMS]);
  });

  it("covers every UpdateHeldPrompt action", () => {
    assertCoversOneof(UpdateHeldPromptRequestSchema, "action", ["release", "drop"]);
  });
});

describe.each(INGRESS_BADGE_ARMS)("an ingress prompt waiting on %s", (arm) => {
  it("draws its badge's words under its arm", async () => {
    // Arrange / Act
    await withTray({ items: [heldIngressItem({ arm })] });
    // Assert
    const pill = harness.$(`[data-held-ingress="daemon"] [data-held-status="${arm}"]`);
    expect(pill?.textContent).toBe(`badge ${arm}`);
  });
});

describe("an ingress prompt", () => {
  it("draws what the user said, and no control", async () => {
    // Arrange / Act
    await withTray({ items: [heldIngressItem({ text: "what's dmitry's comment" })] });
    // Assert
    const card = harness.$('[data-held-ingress="daemon"]');
    expect([card?.textContent?.includes("what's dmitry's comment"), card?.querySelector("button") ?? null]).toEqual([true, null]);
  });

  it("is drawn after the held prompts", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem(), heldIngressItem()] });
    // Assert
    const cards = harness.$$('[data-held-item]').map((card) => card.getAttribute("data-held-item"));
    expect(cards).toEqual([`held:prompt:${HELD_TURN_ID}`, "held:ingress:k-1"]);
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
  awaitingDelivery: "",
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
      harness.fake.setHeld(WORKSPACE_ID, heldSection({ items: [heldPromptItem({ classification })] }));
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
      harness.fake.setHeld(WORKSPACE_ID, heldSection({ items: [heldPromptItem({ hold })] }));
      await harness.settle();
      rendered.push(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.innerHTML ?? "");
    }
    // Assert
    expect([new Set(rendered).size, rendered.includes("")]).toEqual([HOLD_ARMS.length, false]);
  });

  it("titles the shutdown hold's badge with its own schedule id", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem({ hold: "shutdown" })] });
    // Assert — the badge's detail sentence is its hover title.
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"] .held-badge`)?.title).toContain("sched-1");
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
  const foldable = (): Parameters<typeof heldSection>[0] => ({
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
    expect(button?.textContent).toBe("Fold Above");
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
  const classified = (): Parameters<typeof heldSection>[0] => ({
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

describe("a hold_for_turn_end entry's controls", () => {
  it("offers no Accept control", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem({ classification: "holdForTurnEnd" })] });
    // Assert
    expect(harness.$('[data-held-action="accept"]')).toBeNull();
  });

  it("carries no acceptance state on the card", async () => {
    // Arrange / Act
    await withTray({ items: [heldPromptItem({ classification: "holdForTurnEnd" })] });
    // Assert
    expect(harness.$(`[data-held-turn="${HELD_TURN_ID}"]`)?.hasAttribute("data-accepted")).toBe(false);
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
    harness.fake.setHeld(WORKSPACE_ID, heldSection({ items: [heldPromptItem({ turn: "turn-a" })] }));
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
    harness.fake.setHeld(WORKSPACE_ID, heldSection({ items: [heldPromptItem({ turn: "turn-a" })] }));
    await harness.settle();
    // Assert
    expect(harness.$('[data-held-turn="turn-b"]')).toBeNull();
  });
});
