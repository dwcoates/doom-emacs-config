// @vitest-environment jsdom
import { type Control } from "../../src/control.js";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { oneofArms } from "../arms.js";
import {
  RequestCommandSupportErrorSchema,
  RequestCommandSupportResponseSchema,
  type RequestCommandSupportRequest,
  type RequestCommandSupportResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_request_command_support_pb";
import {
  FeedCommandRefusedSchema,
  FeedRowSchema,
  type FeedCommandRefused,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import type { RowContext } from "../../src/feed/cards/context.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { testAppContext } from "../rpc/app-context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  drawFeedCommandRefused,
  requestCommandSupportRefusal,
} from "../../src/panels/refused.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const SINK: FailureSink = { report: () => undefined, retract: () => undefined };

const successResponse = (): RequestCommandSupportResponse =>
  create(RequestCommandSupportResponseSchema, {
    result: { case: "success", value: { workspace: { id: "ws-support", dir: "/s" } } },
  });
/** What each fact-carrying arm must carry for its sentence to be complete. */
const CAUSE_FILL: Readonly<Record<string, Record<string, unknown>>> = {
  workspaceRefMismatch: { registryDir: "/w/registry" },
  transferringAway: { address: "127.0.0.1:7777" },
  briefMissing: { name: "ship-the-rail" },
};

/** A refusal carrying ARM, with the payload that arm's sentence needs. */
const refusalResponse = (arm: string) => (): RequestCommandSupportResponse =>
  create(RequestCommandSupportResponseSchema, {
    result: { case: "error", value: { cause: { case: arm, value: CAUSE_FILL[arm] ?? {} } } },
  } as never);
const errorResponse = refusalResponse("blankCommand");

function rowContext(
  answer: () => RequestCommandSupportResponse = successResponse,
  seen: RequestCommandSupportRequest[] = [],
  throws?: Error,
): { rc: RowContext; seen: RequestCommandSupportRequest[] } {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      requestCommandSupport: (request) => {
        seen.push(request);
        if (throws !== undefined) throw throws;
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
  return {
    rc: { ctx, feed: "root", row: create(FeedRowSchema, {}), revealRow: async () => true },
    seen,
  };
}

const refused = (withOffer: boolean): FeedCommandRefused =>
  create(FeedCommandRefusedSchema, {
    command: { text: "/agents" },
    reason: { text: "/agents is not supported here" },
    ...(withOffer ? { addSupport: {} } : {}),
  });

const settle = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

describe("drawFeedCommandRefused", () => {
  it("draws the command verbatim", () => {
    const { rc } = rowContext();
    expect(drawFeedCommandRefused(refused(false), rc).querySelector("[data-command]")?.textContent).toBe(
      "/agents",
    );
  });

  it("draws the daemon's composed reason verbatim", () => {
    const { rc } = rowContext();
    const card = drawFeedCommandRefused(refused(false), rc);
    expect(card.querySelector(".command-refused-reason")?.textContent).toBe(
      "/agents is not supported here",
    );
  });

  it("draws no offer button when the daemon offered none", () => {
    const { rc } = rowContext();
    expect(drawFeedCommandRefused(refused(false), rc).querySelector("[data-add-support]")).toBeNull();
  });

  it("draws the offer button when the daemon offered it", () => {
    const { rc } = rowContext();
    expect(
      drawFeedCommandRefused(refused(true), rc).querySelector("[data-add-support]"),
    ).not.toBeNull();
  });

  it("refuses a card with no command", () => {
    const { rc } = rowContext();
    const bare = create(FeedCommandRefusedSchema, { reason: { text: "no" } });
    expect(() => drawFeedCommandRefused(bare, rc)).toThrow(MalformedView);
  });

  it("refuses a card with no reason", () => {
    const { rc } = rowContext();
    const bare = create(FeedCommandRefusedSchema, { command: { text: "/agents" } });
    expect(() => drawFeedCommandRefused(bare, rc)).toThrow(MalformedView);
  });
});

describe("the add-support offer", () => {
  it("echoes the card's command", async () => {
    const seen: RequestCommandSupportRequest[] = [];
    const { rc } = rowContext(successResponse, seen);
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    expect(seen[0]?.command).toBe("/agents");
  });

  it("echoes the workspace whose feed carried the card", async () => {
    const seen: RequestCommandSupportRequest[] = [];
    const { rc } = rowContext(successResponse, seen);
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    expect(seen[0]?.workspace?.id).toBe("ws-1");
  });

  it("leaves a brief note on success", async () => {
    const { rc } = rowContext();
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    expect(card.querySelector("[data-support-note]")?.textContent).toBe(
      "support workspace created",
    );
  });

  it("names no workspace in the note: the roster shows it", async () => {
    const { rc } = rowContext();
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    expect(card.textContent).not.toContain("ws-support");
  });

  it("keeps the button disabled after the ask landed", async () => {
    const { rc } = rowContext();
    const card = drawFeedCommandRefused(refused(true), rc);
    const button = card.querySelector<Control>("[data-add-support]");
    button?.click();
    await settle();
    expect(button?.disabled).toBe(true);
  });

  it("draws the refusal at the button", async () => {
    const { rc } = rowContext(errorResponse);
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    expect(card.querySelector(".command-refused-refusal")?.getAttribute("data-arm")).toBe(
      "blankCommand",
    );
  });

  it.each(oneofArms(RequestCommandSupportErrorSchema, "cause"))(
    "labels the %s arm and says something about it",
    async (arm) => {
      const { rc } = rowContext(refusalResponse(arm));
      const card = drawFeedCommandRefused(refused(true), rc);
      card.querySelector<Control>("[data-add-support]")?.click();
      await settle();
      const refusal = card.querySelector(".command-refused-refusal");
      expect([refusal?.getAttribute("data-arm"), refusal?.textContent === ""]).toEqual([arm, false]);
    },
  );

  it("names the registry's directory on a mismatch, from the one shared wording", async () => {
    const { rc } = rowContext(refusalResponse("workspaceRefMismatch"));
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    expect(card.querySelector(".command-refused-refusal")?.textContent).toContain("/w/registry");
  });

  it("names the workspace whose brief the daemon went looking for", async () => {
    const { rc } = rowContext(refusalResponse("briefMissing"));
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    expect(card.querySelector(".command-refused-refusal")?.textContent).toContain("ship-the-rail");
  });

  it("re-enables the button after a refusal", async () => {
    const { rc } = rowContext(errorResponse);
    const card = drawFeedCommandRefused(refused(true), rc);
    const button = card.querySelector<Control>("[data-add-support]");
    button?.click();
    await settle();
    expect(button?.disabled).toBe(false);
  });

  it("says the daemon could not be reached on a transport failure", async () => {
    const { rc } = rowContext(successResponse, [], new Error("no daemon"));
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    expect(card.querySelector(".command-refused-refusal")?.textContent).toBe(
      "the daemon could not be reached",
    );
  });

  it("disables the button while the ask is in flight", () => {
    const { rc } = rowContext(() => new Promise<never>(() => undefined) as never);
    const card = drawFeedCommandRefused(refused(true), rc);
    const button = card.querySelector<Control>("[data-add-support]");
    button?.click();
    expect(button?.disabled).toBe(true);
  });
});

describe("an answer this build cannot read", () => {
  /** A recording sink, so the guard's filed card is observable. */
  function recordingContext(answer: () => RequestCommandSupportResponse): {
    rc: RowContext;
    reported: string[];
  } {
    const reported: string[] = [];
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, { requestCommandSupport: () => answer() });
    });
    const ctx = testAppContext({
      client: createAgentReplClient(transport),
      workspace: WORKSPACE,
      ticker: createTicker(60_000),
      failures: {
        report: (kind) => reported.push(kind.kind.case ?? "unset"),
        retract: () => undefined,
      },
      composerEnabled: false,
    });
    return {
      rc: { ctx, feed: "root", row: create(FeedRowSchema, {}), revealRow: async () => true },
      reported,
    };
  }

  /** The daemon answered with a `result` oneof that sets no arm. */
  const unreadable = (): RequestCommandSupportResponse =>
    create(RequestCommandSupportResponseSchema, {});

  it("files the unreadable answer as frame_undecodable rather than losing it", async () => {
    // ARRANGE
    const { rc, reported } = recordingContext(unreadable);
    const card = drawFeedCommandRefused(refused(true), rc);
    // ACT
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    // ASSERT
    expect(reported).toEqual(["frameUndecodable"]);
  });

  it("draws no transport sentence for an unreadable answer, which is not the link failing", async () => {
    const { rc } = recordingContext(unreadable);
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<Control>("[data-add-support]")?.click();
    await settle();
    expect(card.querySelector(".command-refused-refusal")).toBeNull();
  });

  it("refuses a support-refusal cause arm the bundle cannot name", () => {
    expect(() =>
      requestCommandSupportRefusal({ case: "quotaSpent", value: {} } as never),
    ).toThrow(MalformedView);
  });
});
