// @vitest-environment jsdom
import { type Control } from "../../src/control.js";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  AnswerHeldOfferErrorSchema,
  AnswerHeldOfferResponseSchema,
  type AnswerHeldOfferRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_held_offer_pb";
import { oneofArms } from "../arms.js";
import { HeldOfferSchema, type HeldOffer } from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { testAppContext } from "../rpc/app-context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import type { TrayContext } from "../../src/tray/context.js";
import { answerHeldOfferRefusal, drawHeldOffer } from "../../src/tray/held-offer.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const SINK: FailureSink = { report: () => undefined, retract: () => undefined };

const successResponse = () =>
  create(AnswerHeldOfferResponseSchema, { result: { case: "success", value: {} } });
/** A refusal carrying ARM, with the payload that arm's sentence needs. */
const refusalResponse = (arm: string) => () =>
  create(AnswerHeldOfferResponseSchema, {
    result: { case: "error", value: { cause: { case: arm, value: CAUSE_FILL[arm] ?? {} } } },
  } as never);
const errorResponse = refusalResponse("noOfferStanding");

/** What each fact-carrying arm must carry for its sentence to be complete. */
const CAUSE_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  workspaceRefMismatch: { registryDir: "/w/registry" },
  transferringAway: { address: "127.0.0.1:7777" },
};

function trayContext(
  answer: () => ReturnType<typeof successResponse> = successResponse,
  seen: AnswerHeldOfferRequest[] = [],
): { tc: TrayContext; seen: AnswerHeldOfferRequest[] } {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      answerHeldOffer: (request) => {
        seen.push(request);
        return answer();
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: createTicker(60_000),
    failures: SINK,
    composerEnabled: false,
  });
  return { tc: { ctx, onDispose: () => undefined }, seen };
}

const offer = (): HeldOffer =>
  create(HeldOfferSchema, {
    offer: {
      case: "mergeDequeue",
      value: { headline: { text: "interrupting — keep your merge's queue slot, or release it?" } },
    },
  });

const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

describe("drawHeldOffer", () => {
  it("marks the offer's arm on the card", () => {
    const { tc } = trayContext();
    expect(drawHeldOffer(offer(), tc).getAttribute("data-offer")).toBe("mergeDequeue");
  });

  it("draws the daemon's composed headline verbatim", () => {
    const { tc } = trayContext();
    const card = drawHeldOffer(offer(), tc);
    expect(card.querySelector(".merge-dequeue-head")?.textContent).toBe(
      "interrupting — keep your merge's queue slot, or release it?",
    );
  });

  it("wears the alarm frame the destructive answer earns", () => {
    const { tc } = trayContext();
    expect(drawHeldOffer(offer(), tc).classList.contains("merge-dequeue")).toBe(true);
  });

  it("offers exactly the two answers the request's arms name", () => {
    const { tc } = trayContext();
    const card = drawHeldOffer(offer(), tc);
    const decisions = [...card.querySelectorAll("[data-offer-decision]")].map((node) =>
      node.getAttribute("data-offer-decision"),
    );
    expect(decisions).toEqual(["keep", "release"]);
  });

  it("refuses an offer whose oneof is unset", () => {
    const { tc } = trayContext();
    expect(() => drawHeldOffer(create(HeldOfferSchema, {}), tc)).toThrow(MalformedView);
  });

  it("refuses a merge-dequeue offer with no headline", () => {
    const { tc } = trayContext();
    const bare = create(HeldOfferSchema, { offer: { case: "mergeDequeue", value: {} } });
    expect(() => drawHeldOffer(bare, tc)).toThrow(MalformedView);
  });
});

describe("answering an offer", () => {
  it("echoes the keep decision", async () => {
    const seen: AnswerHeldOfferRequest[] = [];
    const { tc } = trayContext(successResponse, seen);
    const card = drawHeldOffer(offer(), tc);
    card.querySelector<Control>('[data-offer-decision="keep"]')?.click();
    await settle();
    expect(seen[0]?.answer.case).toBe("mergeDequeue");
    expect(seen[0]?.answer.value?.decision.case).toBe("keep");
  });

  it("echoes the release decision", async () => {
    const seen: AnswerHeldOfferRequest[] = [];
    const { tc } = trayContext(successResponse, seen);
    const card = drawHeldOffer(offer(), tc);
    card.querySelector<Control>('[data-offer-decision="release"]')?.click();
    await settle();
    expect(seen[0]?.answer.value?.decision.case).toBe("release");
  });

  it("echoes the workspace on every answer", async () => {
    const seen: AnswerHeldOfferRequest[] = [];
    const { tc } = trayContext(successResponse, seen);
    const card = drawHeldOffer(offer(), tc);
    card.querySelector<Control>('[data-offer-decision="keep"]')?.click();
    await settle();
    expect(seen[0]?.workspace?.id).toBe("ws-1");
  });

  it("draws the refusal at the card when the daemon refuses", async () => {
    const { tc } = trayContext(errorResponse);
    const card = drawHeldOffer(offer(), tc);
    card.querySelector<Control>('[data-offer-decision="release"]')?.click();
    await settle();
    expect(card.querySelector(".offer-refusal")?.getAttribute("data-arm")).toBe(
      "noOfferStanding",
    );
  });

  it.each(oneofArms(AnswerHeldOfferErrorSchema, "cause"))(
    "labels the %s arm and says something about it",
    async (arm) => {
      const { tc } = trayContext(refusalResponse(arm));
      const card = drawHeldOffer(offer(), tc);
      card.querySelector<Control>('[data-offer-decision="release"]')?.click();
      await settle();
      const refusal = card.querySelector(".offer-refusal");
      expect([refusal?.getAttribute("data-arm"), refusal?.textContent === ""]).toEqual([arm, false]);
    },
  );

  it("names the successor daemon on a transfer, from the one shared wording", async () => {
    const { tc } = trayContext(refusalResponse("transferringAway"));
    const card = drawHeldOffer(offer(), tc);
    card.querySelector<Control>('[data-offer-decision="release"]')?.click();
    await settle();
    expect(card.querySelector(".offer-refusal")?.textContent).toContain("127.0.0.1:7777");
  });

  it("says a superseded offer is superseded", async () => {
    const { tc } = trayContext(refusalResponse("offerSuperseded"));
    const card = drawHeldOffer(offer(), tc);
    card.querySelector<Control>('[data-offer-decision="release"]')?.click();
    await settle();
    expect(card.querySelector(".offer-refusal")?.textContent).toContain("superseded");
  });

  it("re-enables the answers after a refusal", async () => {
    const { tc } = trayContext(errorResponse);
    const card = drawHeldOffer(offer(), tc);
    const keep = card.querySelector<Control>('[data-offer-decision="keep"]');
    keep?.click();
    await settle();
    expect(keep?.disabled).toBe(false);
  });

  it("disables both answers while one is in flight", () => {
    const { tc } = trayContext(() => new Promise<never>(() => undefined) as never);
    const card = drawHeldOffer(offer(), tc);
    card.querySelector<Control>('[data-offer-decision="keep"]')?.click();
    const disabled = [...card.querySelectorAll<Control>("ar-button")].every((b) => b.disabled);
    expect(disabled).toBe(true);
  });
});

describe("the daemon could not be reached", () => {
  it("states the unreachable daemon at the card rather than as a refusal arm", async () => {
    // ARRANGE: the verb never answers at all — a transport failure, which is
    // not one of the endpoint's own arms.
    const { tc } = trayContext(() => {
      throw new Error("the socket went away");
    });
    const card = drawHeldOffer(offer(), tc);
    // ACT
    card.querySelector<Control>('[data-offer-decision="keep"]')?.click();
    await settle();
    // ASSERT
    expect(card.querySelector(".offer-refusal")?.getAttribute("data-arm")).toBe("error");
  });

  it("words an unreachable daemon without inventing one of the endpoint's arms", async () => {
    // ARRANGE
    const { tc } = trayContext(() => {
      throw new Error("the socket went away");
    });
    const card = drawHeldOffer(offer(), tc);
    // ACT
    card.querySelector<Control>('[data-offer-decision="release"]')?.click();
    await settle();
    // ASSERT
    expect(card.querySelector(".offer-refusal")?.textContent).toBe(
      "the daemon could not be reached",
    );
  });

  it("re-enables the answers once the unreachable daemon is stated", async () => {
    // ARRANGE
    const { tc } = trayContext(() => {
      throw new Error("the socket went away");
    });
    const card = drawHeldOffer(offer(), tc);
    const keep = card.querySelector<Control>('[data-offer-decision="keep"]');
    // ACT
    keep?.click();
    await settle();
    // ASSERT
    expect(keep?.disabled).toBe(false);
  });
});

describe("answerHeldOfferRefusal", () => {
  it("refuses an arm a newer daemon added rather than wording it as a shrug", () => {
    // ARRANGE / ACT / ASSERT
    expect(() =>
      answerHeldOfferRefusal({ case: "offerExpired", value: {} } as never),
    ).toThrow(MalformedView);
  });

  it("quotes the cause's own path in the refusal it raises", () => {
    // ARRANGE
    let raised: unknown;
    // ACT
    try {
      answerHeldOfferRefusal({ case: "offerExpired", value: {} } as never);
    } catch (err) {
      raised = err;
    }
    // ASSERT
    expect((raised as MalformedView).path).toBe("AnswerHeldOfferError.cause");
  });
});
