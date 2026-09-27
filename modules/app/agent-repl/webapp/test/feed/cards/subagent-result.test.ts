// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import {
  FeedRowSchema,
  FeedSubagentResultSchema,
  type FeedSubagentResult,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { BUBBLE_CAP_ATTRIBUTE, BUBBLE_ROLE_ATTRIBUTE, BUBBLE_UNCAPPED } from "../../../src/bubble/draw.js";
import {
  SUBAGENT_RESULT_CLASS,
  SUBAGENT_RESULT_REASON_CLASS,
  SUBAGENT_RESULT_REPORT_CLASS,
  SUBAGENT_RESULT_STATE_ARMS,
  drawFeedSubagentResult,
} from "../../../src/feed/cards/subagent-result.js";
import type { RowContext } from "../../../src/feed/renderers.js";

type InitOfState = MessageInitShape<typeof FeedSubagentResultSchema>["state"];

const SINK: FailureSink = { report: () => {}, retract: () => {} };

/** A row context no call is ever made through. */
function rc(): RowContext {
  return {
    ctx: testAppContext({
      client: createAgentReplClient(createRouterTransport(() => {})),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker: createTicker(1000),
      failures: SINK,
      composerEnabled: false,
    }),
    feed: "root",
    row: create(FeedRowSchema, {}),
    revealRow: async () => false,
  };
}

/** A result carrying REPORT in STATE. */
function result(state: InitOfState, report = "the report"): FeedSubagentResult {
  return create(FeedSubagentResultSchema, { report: { text: report }, state });
}

const DELIVERED: InitOfState = { case: "delivered", value: {} };

describe("drawFeedSubagentResult: its spec", () => {
  it("is a response-role bubble", () => {
    expect(drawFeedSubagentResult(result(DELIVERED), rc()).getAttribute(BUBBLE_ROLE_ATTRIBUTE)).toBe("response");
  });

  it("wears the subagent-result hook", () => {
    expect(drawFeedSubagentResult(result(DELIVERED), rc()).classList.contains(SUBAGENT_RESULT_CLASS)).toBe(true);
  });

  it("is uncapped, like a final answer", () => {
    expect(drawFeedSubagentResult(result(DELIVERED), rc()).getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe(BUBBLE_UNCAPPED);
  });

  it("paints the report through the markdown pipeline", () => {
    const el = drawFeedSubagentResult(result(DELIVERED, "**all four items done**"), rc());
    expect(el.querySelector(`.${SUBAGENT_RESULT_REPORT_CLASS} strong`)?.textContent).toBe("all four items done");
  });

  it("states a delivering result on data-state", () => {
    const el = drawFeedSubagentResult(result({ case: "delivering", value: {} }), rc());
    expect(el.getAttribute("data-state")).toBe("delivering");
  });

  it("states a delivered result on data-state", () => {
    expect(drawFeedSubagentResult(result(DELIVERED), rc()).getAttribute("data-state")).toBe("delivered");
  });

  it("states an undelivered result on data-state", () => {
    const el = drawFeedSubagentResult(result({ case: "undelivered", value: {} }), rc());
    expect(el.getAttribute("data-state")).toBe("undelivered");
  });

  it("draws an undelivered result's reason verbatim in the quiet line", () => {
    const el = drawFeedSubagentResult(
      result({ case: "undelivered", value: { reason: { text: "the parent is gone" } } }),
      rc(),
    );
    expect(el.querySelector(`.${SUBAGENT_RESULT_REASON_CLASS}`)?.textContent).toBe("the parent is gone");
  });

  it("draws no reason line when the vendor stated none", () => {
    const el = drawFeedSubagentResult(result({ case: "undelivered", value: {} }), rc());
    expect(el.querySelector(`.${SUBAGENT_RESULT_REASON_CLASS}`)).toBeNull();
  });

  it("draws every state arm the schema declares", () => {
    expect([...SUBAGENT_RESULT_STATE_ARMS].sort()).toEqual(
      FeedSubagentResultSchema.oneofs.find((o) => o.name === "state")?.fields.map((f) => f.localName).sort(),
    );
  });
});

describe("drawFeedSubagentResult malformed input", () => {
  it("refuses a result whose state is unset", () => {
    expect(() => drawFeedSubagentResult(create(FeedSubagentResultSchema, { report: { text: "r" } }), rc())).toThrow(
      MalformedView,
    );
  });

  it("refuses a result with no report", () => {
    expect(() => drawFeedSubagentResult(create(FeedSubagentResultSchema, { state: DELIVERED }), rc())).toThrow(
      MalformedView,
    );
  });

  it("refuses a state arm this build does not know", () => {
    const u = result(DELIVERED);
    (u as unknown as { state: { case: string; value: unknown } }).state = { case: "abandoned", value: {} };
    expect(() => drawFeedSubagentResult(u, rc())).toThrow(MalformedView);
  });
});
