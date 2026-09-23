// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  OpenExternalResponseSchema,
  type OpenExternalRequest,
  type OpenExternalResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import {
  FeedArtifactSchema,
  FeedRowSchema,
  type FeedArtifact,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { type AppContext } from "../../../src/rpc/context.js";
import { testAppContext } from "../../rpc/app-context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { ARTIFACT_STATE_ARMS, drawFeedArtifact } from "../../../src/feed/cards/artifact.js";
import type { RowContext } from "../../../src/feed/renderers.js";

/**
 * The oneof as an INIT shape rather than a built message: the fixtures below
 * hand plain object literals to `create`, which is what protobuf-es accepts,
 * while the built message type would demand a `$typeName` on every arm.
 */
type InitOfFeedArtifact = MessageInitShape<typeof FeedArtifactSchema>["state"];

const SINK: FailureSink = { report: () => {}, retract: () => {} };
const HEADING = "📊 Merge Queue Report";
const URL = "https://claude.ai/code/artifacts/abc";

interface Harness {
  rc: RowContext;
  opened: OpenExternalRequest[];
}

/** A row context whose OpenExternal answers with ANSWER. */
function harness(answer?: OpenExternalResponse): Harness {
  const opened: OpenExternalRequest[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      openExternal: (req) => {
        opened.push(req);
        return (
          answer ??
          create(OpenExternalResponseSchema, { result: { case: "success", value: {} } })
        );
      },
    });
  });
  const ctx: AppContext = testAppContext({
    client: createAgentReplClient(transport),
    workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
    ticker: createTicker(1000),
    failures: SINK,
    composerEnabled: false,
  });
  return {
    opened,
    rc: {
      ctx,
      feed: "root",
      row: create(FeedRowSchema, {}),
      revealRow: async () => false,
    },
  };
}

/** An artifact bubble with the composed heading and one STATE. */
function artifact(state: InitOfFeedArtifact): FeedArtifact {
  return create(FeedArtifactSchema, { heading: { text: HEADING }, state });
}

/** Let the scripted answer settle; the router hands it back on a timer. */
async function settle(): Promise<void> {
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
}

beforeEach(() => {
  vi.useFakeTimers();
});

// The fake clock is this file's own; hand the real one back so a
// later file sharing this worker never inherits a frozen timer.
afterEach(() => {
  vi.useRealTimers();
});

describe("drawFeedArtifact", () => {
  it("is the purple response-styled bubble", () => {
    const el = drawFeedArtifact(artifact({ case: "publishing", value: {} }), harness().rc);
    expect(el.className).toBe("bubble md assistant agentic");
  });

  it("draws the heading verbatim, the daemon's favicon emoji included", () => {
    const el = drawFeedArtifact(artifact({ case: "publishing", value: {} }), harness().rc);
    expect(el.querySelector(".agentic-heading")?.textContent).toBe(HEADING);
  });

  const states = [
    { arm: "publishing", state: { case: "publishing", value: {} }, badge: "publishing" },
    { arm: "published", state: { case: "published", value: { url: { url: URL } } }, badge: "published" },
    { arm: "failed", state: { case: "failed", value: { text: "quota exceeded" } }, badge: "failed" },
  ] as const;

  for (const c of states) {
    it(`carries ${c.arm} as the bubble's state`, () => {
      const el = drawFeedArtifact(artifact(c.state), harness().rc);
      expect(el.getAttribute("data-state")).toBe(c.arm);
    });

    it(`badges the ${c.arm} arm`, () => {
      const el = drawFeedArtifact(artifact(c.state), harness().rc);
      expect(el.querySelector(".badge")?.textContent).toBe(c.badge);
    });
  }

  it("draws every state the schema carries", () => {
    expect([...ARTIFACT_STATE_ARMS].sort()).toEqual(
      ["publishing", "published", "failed"].sort(),
    );
  });

  it("draws no url line while the publish is in flight", () => {
    const el = drawFeedArtifact(artifact({ case: "publishing", value: {} }), harness().rc);
    expect(el.querySelector(".artifact-url")).toBeNull();
  });

  it("draws the published url verbatim", () => {
    const el = drawFeedArtifact(
      artifact({ case: "published", value: { url: { url: URL } } }),
      harness().rc,
    );
    expect(el.querySelector(".artifact-url a")?.textContent).toBe(URL);
  });

  it("links the url through the shared external component", async () => {
    const h = harness();
    const el = drawFeedArtifact(
      artifact({ case: "published", value: { url: { url: URL } } }),
      h.rc,
    );
    el.querySelector<HTMLAnchorElement>(".artifact-url a")?.click();
    await settle();
    expect(h.opened.map((r) => r.url)).toEqual([URL]);
  });

  it("draws a refusal at the link when the daemon would not open it", async () => {
    const h = harness(
      create(OpenExternalResponseSchema, {
        result: { case: "error", value: { cause: { case: "invalidUrl", value: {} } } },
      }),
    );
    const el = drawFeedArtifact(
      artifact({ case: "published", value: { url: { url: URL } } }),
      h.rc,
    );
    const anchor = el.querySelector<HTMLAnchorElement>(".artifact-url a");
    anchor?.click();
    await settle();
    // The refusal is the shared `.refusal[data-arm]` ELEMENT beside the link,
    // not a mark on the anchor: the arm's own facts need somewhere to be read.
    expect(
      anchor?.parentElement?.querySelector(".refusal")?.getAttribute("data-arm"),
    ).toBe("invalidUrl");
  });

  it("draws the failed reason where the url would have been", () => {
    const el = drawFeedArtifact(
      artifact({ case: "failed", value: { text: "quota exceeded" } }),
      harness().rc,
    );
    expect(el.querySelector(".artifact-failed")?.textContent).toBe("quota exceeded");
  });

  it("draws no url line on a failed publish", () => {
    const el = drawFeedArtifact(
      artifact({ case: "failed", value: { text: "quota exceeded" } }),
      harness().rc,
    );
    expect(el.querySelector(".artifact-url")).toBeNull();
  });
});

describe("drawFeedArtifact malformed input", () => {
  it("refuses a bubble whose state oneof is unset", () => {
    const u = create(FeedArtifactSchema, { heading: { text: HEADING } });
    expect(() => drawFeedArtifact(u, harness().rc)).toThrow(MalformedView);
  });

  it("refuses a bubble with no heading", () => {
    const u = create(FeedArtifactSchema, { state: { case: "publishing", value: {} } });
    expect(() => drawFeedArtifact(u, harness().rc)).toThrow(MalformedView);
  });

  it("refuses a published bubble with no url", () => {
    const u = artifact({ case: "published", value: {} });
    expect(() => drawFeedArtifact(u, harness().rc)).toThrow(MalformedView);
  });

  it("refuses a state arm this build does not know", () => {
    const u = artifact({ case: "publishing", value: {} });
    (u as unknown as { state: { case: string; value: unknown } }).state = {
      case: "withdrawn",
      value: {},
    };
    expect(() => drawFeedArtifact(u, harness().rc)).toThrow(MalformedView);
  });
});
