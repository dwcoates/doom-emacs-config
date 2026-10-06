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

import { bootColdOnce, chipFailureText, startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import { RENDER_COLORS, failureSideColor } from "./vocab";
import {
  CLIENT_FAILURE_ARMS,
  FEED_PAGE_ERROR_ARMS,
  RETRYING_TURN_ERROR_ARMS,
  TURN_ERROR_ARMS,
  WORKSPACE_ID,
  clientFailure,
  feedId,
  feedPageError,
  turnEndedConcludedRow,
  turnEndedErroredRow,
  turnEndedInterruptedRow,
  turnErrorMarker,
  type ClientFailureArm,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

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

  it.each(TURN_ERROR_ARMS)("draws the %s arm as the outcome marker the daemon composed", async (arm) => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm));
    await harness.settle();
    // Assert: the daemon composed it; the client holds no per-arm table.
    const marker = turnErrorMarker(arm);
    const words = marker.detail === undefined ? marker.label?.text : `${marker.label?.text ?? ""} · ${marker.detail.text}`;
    expect(harness.row("row-1")?.querySelector(".outcome-marker-text")?.textContent).toBe(words);
  });

  it.each(TURN_ERROR_ARMS.filter((arm) => turnErrorMarker(arm).family?.case === "vendorFault"))(
    "carries the %s arm's vendor message verbatim in the marker's expansion",
    async (arm) => {
      // Arrange
      harness = await startHarness();
      await harness.fake.awaitStream("watchFeed");
      // Act
      harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm, { message: `it broke: ${arm}` }));
      await harness.settle();
      // Assert
      expect(harness.row("row-1")?.querySelector('[data-line="message"] .outcome-marker-value')?.textContent).toBe(
        `it broke: ${arm}`,
      );
    },
  );
});

describe("the outcome marker replaces the ended-turn bubble (owner ruling 2026-10-06)", () => {
  it.each(TURN_ERROR_ARMS)("draws no bubble for the %s ending", async (arm) => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.querySelector(".bubble")).toBeNull();
  });

  it.each(TURN_ERROR_ARMS)("paints the %s ending's marker with its family", async (arm) => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.querySelector(".outcome-marker")?.getAttribute("data-family")).toBe(
      turnErrorMarker(arm).family?.case,
    );
  });

  it("draws the neutral interrupted marker for an interrupted turn", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedInterruptedRow());
    await harness.settle();
    // Assert
    const marker = harness.row("row-1")?.querySelector(".outcome-marker");
    expect([marker?.getAttribute("data-family"), marker?.querySelector(".outcome-marker-label")?.textContent]).toEqual([
      "neutral",
      "interrupted",
    ]);
  });

  it("draws nothing for a normal completed turn", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedConcludedRow(feedId("r1")));
    await harness.settle();
    // Assert
    expect([harness.row("row-1")?.querySelector(".bubble"), harness.row("row-1")?.querySelector(".outcome-marker")]).toEqual([null, null]);
  });

  it("opens a fault's expansion on a click, inside the row", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow("internal"));
    await harness.settle();
    const pill = harness.row("row-1")?.querySelector<HTMLElement>(".outcome-marker-pill");
    // Act
    pill?.click();
    await harness.settle();
    // Assert
    expect((harness.row("row-1")?.querySelector(".outcome-marker-expansion") as HTMLElement | null)?.hidden).toBe(false);
  });
});

describe.each(RETRYING_TURN_ERROR_ARMS)("the %s arm's retry countdown", (arm) => {
  it("counts down to the served instant in the marker's expansion", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm, { retryAfterMs: 30_000n }));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.querySelector(".outcome-marker-retry")).not.toBeNull();
  });

  it("ticks the countdown down as time passes", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm, { retryAfterMs: 30_000n }));
    await harness.settle();
    const before = harness.$(".outcome-marker-retry")?.textContent;
    // Act
    await harness.tick(10_000);
    // Assert
    expect(harness.$(".outcome-marker-retry")?.textContent).not.toBe(before);
  });

  it("draws no countdown when the vendor stated no wait", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow(arm, { retryAfterMs: undefined }));
    await harness.settle();
    // Assert
    expect(harness.$(".outcome-marker-retry")).toBeNull();
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
    // Arrange: the distinction lives in the served marker, not in a client
    // table — the renderer holds no per-arm words at all.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow("maxTokens"));
    await harness.settle();
    const truncated = harness.row("row-1")?.querySelector(".outcome-marker-text")?.textContent ?? "";
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedErroredRow("maxOutputTokens"));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.querySelector(".outcome-marker-text")?.textContent).not.toBe(truncated);
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
    expect(harness.row("row-1")?.querySelector(".outcome-marker-detail")?.textContent).toContain("vendor_teapot");
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
