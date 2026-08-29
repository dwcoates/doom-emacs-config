// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
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
import { createAppContext } from "../../src/rpc/context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { drawFeedCommandRefused } from "../../src/panels/refused.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const SINK: FailureSink = { report: () => undefined, retract: () => undefined };

const successResponse = (): RequestCommandSupportResponse =>
  create(RequestCommandSupportResponseSchema, {
    result: { case: "success", value: { workspace: { id: "ws-support", dir: "/s" } } },
  });
const errorResponse = (): RequestCommandSupportResponse =>
  create(RequestCommandSupportResponseSchema, { result: { case: "error", value: {} } });

function rowContext(
  answer: () => RequestCommandSupportResponse = successResponse,
  seen: RequestCommandSupportRequest[] = [],
  throws?: unknown,
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
  const ctx = createAppContext({
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
    card.querySelector<HTMLButtonElement>("[data-add-support]")?.click();
    await settle();
    expect(seen[0]?.command).toBe("/agents");
  });

  it("echoes the workspace whose feed carried the card", async () => {
    const seen: RequestCommandSupportRequest[] = [];
    const { rc } = rowContext(successResponse, seen);
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<HTMLButtonElement>("[data-add-support]")?.click();
    await settle();
    expect(seen[0]?.workspace?.id).toBe("ws-1");
  });

  it("leaves a brief note on success", async () => {
    const { rc } = rowContext();
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<HTMLButtonElement>("[data-add-support]")?.click();
    await settle();
    expect(card.querySelector("[data-support-note]")?.textContent).toBe(
      "support workspace created",
    );
  });

  it("names no workspace in the note: the roster shows it", async () => {
    const { rc } = rowContext();
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<HTMLButtonElement>("[data-add-support]")?.click();
    await settle();
    expect(card.textContent).not.toContain("ws-support");
  });

  it("keeps the button disabled after the ask landed", async () => {
    const { rc } = rowContext();
    const card = drawFeedCommandRefused(refused(true), rc);
    const button = card.querySelector<HTMLButtonElement>("[data-add-support]");
    button?.click();
    await settle();
    expect(button?.disabled).toBe(true);
  });

  it("draws the refusal at the button", async () => {
    const { rc } = rowContext(errorResponse);
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<HTMLButtonElement>("[data-add-support]")?.click();
    await settle();
    expect(card.querySelector(".command-refused-refusal")?.getAttribute("data-arm")).toBe("error");
  });

  it("re-enables the button after a refusal", async () => {
    const { rc } = rowContext(errorResponse);
    const card = drawFeedCommandRefused(refused(true), rc);
    const button = card.querySelector<HTMLButtonElement>("[data-add-support]");
    button?.click();
    await settle();
    expect(button?.disabled).toBe(false);
  });

  it("says the daemon could not be reached on a transport failure", async () => {
    const { rc } = rowContext(successResponse, [], new Error("no daemon"));
    const card = drawFeedCommandRefused(refused(true), rc);
    card.querySelector<HTMLButtonElement>("[data-add-support]")?.click();
    await settle();
    expect(card.querySelector(".command-refused-refusal")?.textContent).toBe(
      "the daemon could not be reached",
    );
  });

  it("disables the button while the ask is in flight", () => {
    const { rc } = rowContext(() => new Promise<never>(() => undefined) as never);
    const card = drawFeedCommandRefused(refused(true), rc);
    const button = card.querySelector<HTMLButtonElement>("[data-add-support]");
    button?.click();
    expect(button?.disabled).toBe(true);
  });
});
