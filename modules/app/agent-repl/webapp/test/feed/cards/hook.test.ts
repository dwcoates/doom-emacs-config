// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  FeedHookBlockedSchema,
  FeedHookFailedSchema,
  FeedHookSchema,
  FeedIdSchema,
  FeedRowSchema,
  type FeedHook,
  type FeedId,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import type { RowContext } from "../../../src/feed/cards/context.js";
import { drawFeedHook } from "../../../src/feed/cards/hook.js";

const SINK: FailureSink = { report: () => {}, retract: () => {} };

interface Harness {
  rc: RowContext;
  revealed: FeedId[];
}

/** A row context recording every reveal, answering REACHED. */
function harness(reached = true): Harness {
  const revealed: FeedId[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {});
  });
  return {
    revealed,
    rc: {
      ctx: testAppContext({
        client: createAgentReplClient(transport),
        workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
        ticker: createTicker(1000),
        failures: SINK,
        composerEnabled: false,
      }),
      feed: "root",
      row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: "row-1" }) }),
      revealRow: async (id) => {
        revealed.push(id);
        return reached;
      },
    },
  };
}

const HEADLINE = "hook blocked: protect-master (PreToolUse)";

/** The blocked arm, carrying the hook's own refusal text. */
function blocked(reason: string): FeedHook["outcome"] {
  return { case: "blocked", value: create(FeedHookBlockedSchema, { reason }) };
}

/** The failed arm, carrying the exit code and the output when there was any. */
function failed(exitCode: number, output?: string): FeedHook["outcome"] {
  return {
    case: "failed",
    value: create(FeedHookFailedSchema, {
      exitCode,
      output: output === undefined ? undefined : { text: output },
    }),
  };
}

/** A hook card with the composed headline, an OUTCOME and an optional GATED. */
function hook(outcome: FeedHook["outcome"], gated?: FeedId): FeedHook {
  return create(FeedHookSchema, {
    headline: { text: HEADLINE },
    gatedCall: gated === undefined ? undefined : { row: gated },
    outcome,
  });
}

/** Let the reveal's promise settle. */
async function settle(): Promise<void> {
  for (let i = 0; i < 10; i += 1) await vi.advanceTimersByTimeAsync(0);
}

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

describe("the hook card", () => {
  it("draws the composed headline verbatim", () => {
    const el = drawFeedHook(
      hook(blocked("master is protected")),
      harness().rc,
    );
    expect(el.querySelector(".tool-name")?.textContent).toBe(
      "hook blocked: protect-master (PreToolUse)",
    );
  });

  it("carries the outcome arm as the card's state", () => {
    const el = drawFeedHook(
      hook(blocked("nope")),
      harness().rc,
    );
    expect(el.getAttribute("data-state")).toBe("blocked");
  });

  it("draws no gated-call link for a call-less firing", () => {
    const el = drawFeedHook(
      hook(blocked("nope")),
      harness().rc,
    );
    expect(el.querySelector(".hook-gated")).toBeNull();
  });
});

describe("the blocked outcome", () => {
  it("wears the loud treatment", () => {
    const el = drawFeedHook(
      hook(blocked("nope")),
      harness().rc,
    );
    expect(el.classList.contains("tool-hook-blocked")).toBe(true);
  });

  it("draws the hook's own reason verbatim", () => {
    const el = drawFeedHook(
      hook(blocked("master is protected")),
      harness().rc,
    );
    expect(el.querySelector(".hook-reason")?.textContent).toBe("master is protected");
  });
});

describe("the failed outcome", () => {
  it("draws the exit chip in the error hue for a non-zero code", () => {
    const el = drawFeedHook(hook(failed(1)), harness().rc);
    const chip = el.querySelector(".hook-exit");
    expect([chip?.textContent, chip?.classList.contains("err")]).toEqual(["exit 1", true]);
  });

  it("draws the exit chip plainly for a zero code", () => {
    const el = drawFeedHook(hook(failed(0)), harness().rc);
    const chip = el.querySelector(".hook-exit");
    expect([chip?.textContent, chip?.classList.contains("err")]).toEqual(["exit 0", false]);
  });

  it("draws the capped output when the firing produced any", () => {
    const el = drawFeedHook(
      hook(failed(2, "traceback…")),
      harness().rc,
    );
    expect(el.querySelector(".tool-output")?.textContent).toBe("traceback…");
  });

  it("draws no output box when the firing produced none", () => {
    const el = drawFeedHook(hook(failed(2)), harness().rc);
    expect(el.querySelector(".tool-output")).toBeNull();
  });

  it("takes no loud treatment", () => {
    const el = drawFeedHook(hook(failed(2)), harness().rc);
    expect(el.classList.contains("tool-hook-blocked")).toBe(false);
  });
});

describe("the gated-call link", () => {
  const GATED = create(FeedIdSchema, { value: "row-42" });

  it("draws the link when the firing gated a call", () => {
    const el = drawFeedHook(
      hook(blocked("nope"), GATED),
      harness().rc,
    );
    expect(el.querySelector(".hook-gated")?.getAttribute("data-gated-row")).toBe("row-42");
  });

  it("hands the served row id back whole on a click", async () => {
    const h = harness();
    const el = drawFeedHook(
      hook(blocked("nope"), GATED),
      h.rc,
    );
    el.querySelector<HTMLElement>(".hook-gated")?.click();
    await settle();
    expect(h.revealed).toEqual([GATED]);
  });

  it("marks the link when the row could not be reached", async () => {
    const h = harness(false);
    const el = drawFeedHook(
      hook(blocked("nope"), GATED),
      h.rc,
    );
    el.querySelector<HTMLElement>(".hook-gated")?.click();
    await settle();
    expect(el.querySelector(".hook-gated")?.getAttribute("data-unreachable")).toBe("true");
  });

  it("leaves the link unmarked when the row was reached", async () => {
    const h = harness();
    const el = drawFeedHook(
      hook(blocked("nope"), GATED),
      h.rc,
    );
    el.querySelector<HTMLElement>(".hook-gated")?.click();
    await settle();
    expect(el.querySelector(".hook-gated")?.hasAttribute("data-unreachable")).toBe(false);
  });
});

describe("a malformed hook card", () => {
  it("refuses an unset outcome", () => {
    expect(() => drawFeedHook(hook({ case: undefined }), harness().rc)).toThrow(MalformedView);
  });

  it("refuses an unset headline", () => {
    const u = create(FeedHookSchema, { outcome: failed(1) });
    expect(() => drawFeedHook(u, harness().rc)).toThrow(MalformedView);
  });

  it("refuses a gated call with no row", () => {
    // A gated-call element whose row the producer left unset.
    const u = hook(blocked("nope"));
    (u as { gatedCall: unknown }).gatedCall = { $typeName: "frontend.v1.FeedHookGatedCall" };
    expect(() => drawFeedHook(u, harness().rc)).toThrow(MalformedView);
  });

  it("refuses an arm this build has no case for", () => {
    const u = hook({ case: undefined });
    (u as { outcome: unknown }).outcome = { case: "teleported", value: {} };
    expect(() => drawFeedHook(u, harness().rc)).toThrow(MalformedView);
  });
});
