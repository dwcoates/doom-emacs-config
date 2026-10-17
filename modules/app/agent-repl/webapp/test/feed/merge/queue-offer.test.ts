// @vitest-environment jsdom
import { type Control } from "../../../src/control.js";
import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  AnswerMergeQueueOfferErrorSchema,
  AnswerMergeQueueOfferResponseSchema,
  AnswerMergeQueueOfferRequestSchema,
  type AnswerMergeQueueOfferRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_merge_queue_offer_pb";
import { oneofArms } from "../../arms.js";
import {
  FeedMergeQueueOfferSchema,
  FeedMergeSchema,
  FeedRowSchema,
  type FeedMerge,
  type FeedMergeQueueOffer,
  type FeedRow,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FailureSink } from "../../../src/failure/sink.js";
import { log } from "../../../src/log.js";
import type { AppContext } from "../../../src/rpc/context.js";
import { scriptedAppContext } from "../../rpc/app-context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import type { OverlayControls } from "../../../src/feed/bubble.js";
import {
  answerMergeQueueOfferRefusal,
  drawMergeQueueOffer,
  mergeQueueOfferOverlay,
} from "../../../src/feed/merge/queue-offer.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const SINK: FailureSink = { report: () => undefined, retract: () => undefined };
const HEADLINE = "this merge is in the queue: evict it, run it next, or leave it be?";

const successResponse = () =>
  create(AnswerMergeQueueOfferResponseSchema, { result: { case: "success", value: {} } });
/** A refusal carrying ARM, with the payload that arm's sentence needs. */
const refusalResponse = (arm: string) => () =>
  create(AnswerMergeQueueOfferResponseSchema, {
    result: { case: "error", value: { cause: { case: arm, value: CAUSE_FILL[arm] ?? {} } } },
  } as never);
const errorResponse = refusalResponse("noOfferStanding");
const unreachable = () => {
  throw new Error("the socket went away");
};

/** What each fact-carrying arm must carry for its sentence to be complete. */
const CAUSE_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  workspaceRefMismatch: { registryDir: "/w/registry" },
  transferringAway: { address: "127.0.0.1:7777" },
};

function context(
  answer: () => ReturnType<typeof successResponse> = successResponse,
  seen: AnswerMergeQueueOfferRequest[] = [],
): AppContext {
  return scriptedAppContext(
    {
      answerMergeQueueOffer: (request) => {
        seen.push(request);
        return answer();
      },
    },
    { workspace: WORKSPACE, tickMs: 60_000, failures: SINK },
  );
}

/** Overlay controls that record what the overlay asked of its bubble. */
function recordingControls(): OverlayControls & { calls: string[] } {
  const calls: string[] = [];
  return { calls, dismiss: () => calls.push("dismiss"), restore: () => calls.push("restore") };
}

const offer = (): FeedMergeQueueOffer =>
  create(FeedMergeQueueOfferSchema, { headline: { text: HEADLINE } });

const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

const click = (el: HTMLElement, decision: string): void =>
  el.querySelector<Control>(`[data-offer-decision="${decision}"]`)?.click();

describe("drawMergeQueueOffer", () => {
  it("draws the daemon's composed headline verbatim", () => {
    const el = drawMergeQueueOffer(offer(), context(), recordingControls());
    expect(el.querySelector(".merge-queue-offer-head")?.textContent).toBe(HEADLINE);
  });

  it("offers exactly the three answers the request's arms name, in order", () => {
    const el = drawMergeQueueOffer(offer(), context(), recordingControls());
    const decisions = [...el.querySelectorAll("[data-offer-decision]")].map((node) =>
      node.getAttribute("data-offer-decision"),
    );
    expect(decisions).toEqual(["evict", "prioritize", "cancel"]);
  });

  it("covers every decision arm of the request", () => {
    expect([...oneofArms(AnswerMergeQueueOfferRequestSchema, "decision")].sort()).toEqual(
      ["cancel", "evict", "prioritize"],
    );
  });

  it("marks Cancel as the answer focus lands on", () => {
    const el = drawMergeQueueOffer(offer(), context(), recordingControls());
    expect(el.querySelector("[data-autofocus]")?.getAttribute("data-offer-decision")).toBe("cancel");
  });

  it("refuses a question with no headline", () => {
    const bare = create(FeedMergeQueueOfferSchema, {});
    expect(() => drawMergeQueueOffer(bare, context(), recordingControls())).toThrow(MalformedView);
  });
});

describe("answering the question", () => {
  it.each(["evict", "prioritize", "cancel"])("echoes the %s decision", async (decision) => {
    const seen: AnswerMergeQueueOfferRequest[] = [];
    const el = drawMergeQueueOffer(offer(), context(successResponse, seen), recordingControls());
    click(el, decision);
    await settle();
    expect(seen[0]?.decision.case).toBe(decision);
  });

  it("echoes the workspace on every answer", async () => {
    const seen: AnswerMergeQueueOfferRequest[] = [];
    const el = drawMergeQueueOffer(offer(), context(successResponse, seen), recordingControls());
    click(el, "cancel");
    await settle();
    expect(seen[0]?.workspace?.id).toBe("ws-1");
  });

  it("closes the overlay at once on any answer, and keeps it closed on success", async () => {
    const controls = recordingControls();
    const el = drawMergeQueueOffer(offer(), context(), controls);
    click(el, "evict");
    await settle();
    expect(controls.calls).toEqual(["dismiss"]);
  });

  it("brings the overlay back with the refusal when the daemon refuses", async () => {
    const controls = recordingControls();
    const el = drawMergeQueueOffer(offer(), context(errorResponse), controls);
    click(el, "prioritize");
    await settle();
    expect([controls.calls, el.querySelector(".offer-refusal")?.getAttribute("data-arm")]).toEqual([
      ["dismiss", "restore"],
      "noOfferStanding",
    ]);
  });

  it("logs a refusal as a warning naming the decision and the arm", async () => {
    const warn = vi.spyOn(log, "warn");
    const el = drawMergeQueueOffer(offer(), context(errorResponse), recordingControls());
    click(el, "cancel");
    await settle();
    expect(warn).toHaveBeenCalledWith(
      "AnswerMergeQueueOffer refused a cancel",
      expect.objectContaining({
        operation: "merge.queue-offer.refused",
        context: expect.objectContaining({ decision: "cancel", arm: "noOfferStanding" }) as unknown,
      }),
    );
    warn.mockRestore();
  });

  it.each(oneofArms(AnswerMergeQueueOfferErrorSchema, "cause"))(
    "labels the %s arm and says something about it",
    async (arm) => {
      const el = drawMergeQueueOffer(offer(), context(refusalResponse(arm)), recordingControls());
      click(el, "evict");
      await settle();
      const refusal = el.querySelector(".offer-refusal");
      expect([refusal?.getAttribute("data-arm"), refusal?.textContent === ""]).toEqual([arm, false]);
    },
  );

  it("re-enables the answers after a refusal", async () => {
    const el = drawMergeQueueOffer(offer(), context(errorResponse), recordingControls());
    click(el, "cancel");
    await settle();
    const enabled = [...el.querySelectorAll<Control>("ar-button")].every((b) => !b.disabled);
    expect(enabled).toBe(true);
  });

  it("disables every answer while one is in flight", () => {
    const el = drawMergeQueueOffer(
      offer(),
      context(() => new Promise<never>(() => undefined) as never),
      recordingControls(),
    );
    click(el, "cancel");
    const disabled = [...el.querySelectorAll<Control>("ar-button")].every((b) => b.disabled);
    expect(disabled).toBe(true);
  });
});

describe("the daemon could not be reached", () => {
  it("states the unreachable daemon at the overlay rather than as a refusal arm", async () => {
    const el = drawMergeQueueOffer(offer(), context(unreachable), recordingControls());
    click(el, "evict");
    await settle();
    expect([
      el.querySelector(".offer-refusal")?.getAttribute("data-arm"),
      el.querySelector(".offer-refusal")?.textContent,
    ]).toEqual(["error", "the daemon could not be reached"]);
  });

  it("brings the overlay back so the question can be answered again", async () => {
    const controls = recordingControls();
    const el = drawMergeQueueOffer(offer(), context(unreachable), controls);
    click(el, "evict");
    await settle();
    expect(controls.calls).toEqual(["dismiss", "restore"]);
  });

  it("logs the failure as an error naming the decision", async () => {
    const error = vi.spyOn(log, "error");
    const el = drawMergeQueueOffer(offer(), context(unreachable), recordingControls());
    click(el, "prioritize");
    await settle();
    expect(error).toHaveBeenCalledWith(
      expect.stringContaining("AnswerMergeQueueOffer failed for a prioritize"),
      expect.objectContaining({ operation: "merge.queue-offer.failed" }),
    );
    error.mockRestore();
  });
});

describe("answerMergeQueueOfferRefusal", () => {
  it("refuses an arm a newer daemon added rather than wording it as a shrug", () => {
    expect(() => answerMergeQueueOfferRefusal({ case: "offerExpired", value: {} } as never)).toThrow(
      MalformedView,
    );
  });
});

describe("mergeQueueOfferOverlay", () => {
  const rowOf = (merge: FeedMerge): FeedRow =>
    create(FeedRowSchema, { id: { value: "head" }, row: { case: "activity", value: { unit: { case: "merge", value: merge } } } });
  const mergeOf = (row: FeedRow): FeedMerge => (row.row.value as { unit: { value: FeedMerge } }).unit.value;

  it("keys a head carrying the question by its sentence", () => {
    const overlay = mergeQueueOfferOverlay(context(), mergeOf);
    const row = rowOf(create(FeedMergeSchema, { queueOffer: offer() }));
    expect(overlay.keyOf(row)).toBe(HEADLINE);
  });

  it("answers no key for a head with no question", () => {
    const overlay = mergeQueueOfferOverlay(context(), mergeOf);
    expect(overlay.keyOf(rowOf(create(FeedMergeSchema, {})))).toBeUndefined();
  });

  it("refuses a question with no headline", () => {
    const overlay = mergeQueueOfferOverlay(context(), mergeOf);
    const row = rowOf(create(FeedMergeSchema, { queueOffer: create(FeedMergeQueueOfferSchema, {}) }));
    expect(() => overlay.keyOf(row)).toThrow(MalformedView);
  });
});
