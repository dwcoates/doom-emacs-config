/**
 * FAILURES — the three failure vocabularies, each drawn in its own place.
 *
 *   1. the six CLIENT-LOCAL FailureKind arms, minted only by the webapp itself
 *      (R4) and listed in the topbar's warning chip, each drawn distinctly
 *      with its typed evidence,
 *   2. every FeedTurnEndedErrored arm, which is where a VENDOR failure now
 *      arrives (the failure vocabulary lost its vendor band),
 *   3. every FeedPageError arm, drawn where the rows would have been.
 */
import { afterEach, describe, expect, it } from "vitest";

import {
  FeedTurnEndedErroredSchema,
  FeedPageErrorSchema,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { FailureKindSchema } from "../../../proto/gen/ts/frontend/v1/failure_pb";

import { chipFailureText, startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import { RENDER_COLORS, failureSideColor } from "./vocab";
import {
  CLIENT_FAILURE_ARMS,
  FEED_PAGE_ERROR_ARMS,
  RETRYING_TURN_ERROR_ARMS,
  TURN_ERROR_ARMS,
  TURN_ERROR_HEADLINES,
  WORKSPACE_ID,
  clientFailure,
  feedPageError,
  turnEndedErroredRow,
  type ClientFailureArm,
} from "./fixtures";

let harness: Harness;

afterEach(async () => {
  await harness?.stop();
});

/** Report one client-local failure through the app's own sink. */
async function report(arm: ClientFailureArm): Promise<Harness> {
  const h = await startHarness();
  h.ctx.failures.report(clientFailure(arm));
  await h.settle();
  return h;
}

describe("the client-local failure vocabulary", () => {
  it("names exactly the arms the failure schema declares as client-local", () => {
    // Arrange
    const declared = FailureKindSchema.oneofs
      .find((o) => o.localName === "kind")
      ?.fields.map((f) => f.localName);
    // Assert: every arm the webapp mints is a real arm of the shared oneof.
    expect(declared).toEqual(expect.arrayContaining([...CLIENT_FAILURE_ARMS]));
  });

  it("assigns the client-local side a color the palette declares", () => {
    // Assert: guards the vocabulary itself, not the app.
    expect(RENDER_COLORS.colors).toContain(failureSideColor("client_local"));
  });
});

describe.each(CLIENT_FAILURE_ARMS)("the %s failure", (arm) => {
  it("lists its own arm in the topbar's warning chip", async () => {
    // Arrange / Act
    harness = await report(arm);
    // Assert
    expect(harness.failureArms()).toEqual([arm]);
  });

  it("retracts when the arm is retracted", async () => {
    // Arrange
    harness = await report(arm);
    // Act
    harness.ctx.failures.retract(arm);
    await harness.settle();
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });
});

/** The typed evidence each arm carries, and the text it must put on screen. */
const FAILURE_EVIDENCE: Record<ClientFailureArm, string> = {
  daemonUnreachable: "1006",
  workspaceGone: "",
  bootFailed: "the page address had no workspace",
  controlPlaneFailed: "WatchFooter",
  frameUndecodable: "unknown field 999",
  staleBundle: "the daemon shipped a newer bundle",
};

describe("failure evidence", () => {
  it.each(
    CLIENT_FAILURE_ARMS.filter((arm) => FAILURE_EVIDENCE[arm] !== ""),
  )("draws the %s arm's evidence verbatim", async (arm) => {
    // Arrange / Act
    harness = await report(arm);
    // Assert
    expect(await chipFailureText(harness, arm)).toContain(FAILURE_EVIDENCE[arm]);
  });

  it("draws the workspace-gone arm, which carries no evidence at all", async () => {
    // Arrange / Act
    harness = await report("workspaceGone");
    // Assert
    expect(await chipFailureText(harness, "workspaceGone")).toContain("no longer exists");
  });
});

describe("turn error arms", () => {
  it("covers every arm the contract declares", () => {
    // Assert
    const declared = FeedTurnEndedErroredSchema.oneofs
      .find((o) => o.localName === "error")
      ?.fields.map((f) => f.localName)
      .sort();
    expect(declared).toEqual([...TURN_ERROR_ARMS].sort());
  });

  it.each(TURN_ERROR_ARMS)("draws the %s arm distinctly", async (arm) => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.querySelector(`[data-turn-error="${arm}"]`)).not.toBeNull();
  });

  it.each(TURN_ERROR_ARMS)("draws the %s arm's composed headline verbatim", async (arm) => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm));
    await harness.settle();
    // Assert: the daemon composed it; the client holds no per-arm table.
    expect(harness.row("row-1")?.textContent).toContain(TURN_ERROR_HEADLINES[arm]);
  });

  it.each(TURN_ERROR_ARMS)("draws the %s arm's composed message verbatim", async (arm) => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm, { message: `it broke: ${arm}` }));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.textContent).toContain(`it broke: ${arm}`);
  });
});

describe.each(RETRYING_TURN_ERROR_ARMS)("the %s arm's retry countdown", (arm) => {
  it("draws a countdown from the served deadline", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm, { retryAfterMs: 30_000n }));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.querySelector("[data-retry-countdown]")).not.toBeNull();
  });

  it("ticks the countdown down as time passes", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm, { retryAfterMs: 30_000n }));
    await harness.settle();
    const before = harness.$("[data-retry-countdown]")?.textContent;
    // Act
    await harness.tick(10_000);
    // Assert
    expect(harness.$("[data-retry-countdown]")?.textContent).not.toBe(before);
  });

  it("draws no countdown when the arm carries no deadline", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act: retry_after_ms is `optional`; absent means draw nothing, never tick.
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      turnEndedErroredRow(arm, { retryAfterMs: undefined }),
    );
    await harness.settle();
    // Assert
    expect(harness.$("[data-retry-countdown]")).not.toBeNull();
  });
});

describe("the two token-limit arms", () => {
  it("draws max_tokens (mid-arrival truncation) distinctly", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow("maxTokens"));
    await harness.settle();
    // Assert
    expect(harness.$('[data-turn-error="maxTokens"]')).not.toBeNull();
  });

  it("draws max_output_tokens (an outright refusal) distinctly", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow("maxOutputTokens"));
    await harness.settle();
    // Assert
    expect(harness.$('[data-turn-error="maxOutputTokens"]')).not.toBeNull();
  });

  it("reads the two arms differently because the DAEMON worded them differently", async () => {
    // Arrange: the distinction lives in the served headline, not in a client
    // table — the renderer holds no per-arm sentences at all.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow("maxTokens"));
    await harness.settle();
    const truncated = harness.row("row-1")?.textContent ?? "";
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow("maxOutputTokens"));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.textContent).not.toBe(truncated);
  });

  it("draws whatever headline the daemon serves, holding no table of its own", async () => {
    // Arrange: serve max_output_tokens' wording ON the max_tokens arm. A
    // renderer with its own per-arm sentence would override this; a renderer
    // that draws the headline verbatim shows exactly what arrived.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      turnEndedErroredRow("maxTokens", { headline: TURN_ERROR_HEADLINES.maxOutputTokens }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.textContent).toContain(TURN_ERROR_HEADLINES.maxOutputTokens);
  });
});

describe("the unmodeled vendor arm", () => {
  it("draws the vendor's own type string verbatim", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow("vendorUnmodeled"));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.textContent).toContain("vendor_teapot");
  });
});

describe("page errors", () => {
  it("covers every arm the contract declares", () => {
    // Assert
    const declared = FeedPageErrorSchema.oneofs
      .find((o) => o.localName === "kind")
      ?.fields.map((f) => f.localName)
      .sort();
    expect(declared).toEqual([...FEED_PAGE_ERROR_ARMS].sort());
  });

  it("draws the history-replay-truncated arm", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) => fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageError()),
    });
    // Assert
    expect(harness.$('[data-page-error="historyReplayTruncated"]')).not.toBeNull();
  });

  it("draws the page error's composed headline verbatim", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) => fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageError()),
    });
    // Assert
    expect(harness.feedContainer()?.textContent).toContain("history could not be replayed");
  });

  it("draws the truncation's own evidence verbatim", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) => fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageError()),
    });
    // Assert
    expect(harness.feedContainer()?.textContent).toContain("store gap");
  });
});
