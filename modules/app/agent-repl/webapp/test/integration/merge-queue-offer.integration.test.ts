/**
 * THE MERGE-QUEUE QUESTION — drawn in the pinned merge bubble, over its tabs.
 *
 * An interrupt of an idle workspace whose sync merge is queued raises the
 * question on the merge's pinned head (FeedMerge.queue_offer). The bubble
 * draws it in place of its tabs, focused; only its answer acts on the queue,
 * and any answer closes it. A pinned bubble cannot be folded.
 */
import { afterEach, describe, expect, it } from "vitest";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import { MERGE_QUEUE_OFFER_HEADLINE, WORKSPACE_ID, mergeQueueOfferRow } from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** Boot, then push the pinned head carrying the question. */
async function withQuestion(): Promise<Harness> {
  harness = await startHarness();
  harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, mergeQueueOfferRow());
  await harness.settle();
  return harness;
}

const overlay = (): HTMLElement | null => harness.$("[data-merge-queue-offer]");
const overlaySlot = (): HTMLElement | null => harness.row("merge-q")?.querySelector(".bubble-overlay") ?? null;

describe("the merge-queue question in the pinned bubble", () => {
  it("draws the daemon's sentence inside the merge bubble", async () => {
    await withQuestion();
    expect(harness.row("merge-q")?.querySelector("[data-merge-queue-offer]")?.textContent).toContain(
      MERGE_QUEUE_OFFER_HEADLINE,
    );
  });

  it("hides the bubble's tabs while the question stands", async () => {
    await withQuestion();
    expect(harness.row("merge-q")?.querySelector<HTMLElement>("[data-subfeed]")?.hidden).toBe(true);
  });

  it("draws no card in the held section", async () => {
    await withQuestion();
    expect(harness.$('[data-component="hold-tray"]')?.childElementCount ?? 0).toBe(0);
  });

  it("focuses Cancel", async () => {
    await withQuestion();
    expect(overlay()?.ownerDocument.activeElement?.getAttribute("data-offer-decision")).toBe("cancel");
  });

  it.each(["evict", "prioritize", "cancel"])("sends the %s answer", async (decision) => {
    await withQuestion();
    await harness.click(`[data-offer-decision="${decision}"]`);
    const [request] = harness.fake.calls<{ decision: { case?: string }; workspace?: { id: string } }>(
      "answerMergeQueueOffer",
    );
    expect([request.decision.case, request.workspace?.id]).toEqual([decision, WORKSPACE_ID]);
  });

  it("closes the question once answered", async () => {
    await withQuestion();
    await harness.click('[data-offer-decision="evict"]');
    expect(overlaySlot()?.hidden).toBe(true);
  });

  it("keeps the pinned bubble open on a head click", async () => {
    await withQuestion();
    await harness.click('[data-feed-row="merge-q"] [data-expand]');
    expect(harness.row("merge-q")?.dataset.expanded).toBe("true");
  });
});
