/**
 * §F7 — THE MERGE BUBBLE AND ITS TAB STRIP, against the real chain.
 *
 * The merge bubble is the one arm the agent does not author: the daemon
 * orchestrates it, and from the user's perspective the turn has not concluded
 * while it runs — so it is ACTIVITY, drawn as a bubble whose body is a TAB
 * STRIP over `FeedMergeTab` rows.
 *
 * THE PARITY INVARIANT IS THE POINT: the merge bubble uses the SAME sub-feed
 * plumbing as a subagent bubble (expand -> `OpenFeed` on the bubble's own
 * FeedId -> `WatchFeed`; collapse -> abandon the token). A merge-specific
 * nested-content loader is a DEFECT, and only the webapp can show that the
 * same plumbing is what ran.
 *
 * THE WORLD IS THIS AREA'S OWN: the Go driver (`TestWebappLayerMergeTabs`)
 * registers a repository backed by the scripted fake git, creates a CHILD
 * workspace, and points this page at the child — because a merge needs
 * something to merge. Nothing here touches real git.
 *
 * ONE MERGE, ENQUEUED ONCE, IN `beforeAll`. A merge is not a step the page can
 * repeat: `MergeWorkspace` enqueues, and "life thereafter is the feed's merge
 * bubble" — a second enqueue mid-merge is a different subject (the merge
 * queue's, which the Go area owns). It is also LIVE while it runs, which keeps
 * the page redrawing, so every wait in this file is done once, up front,
 * rather than per test.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import { BOOT_BUDGET_MS, TURN_TEST_MS, awaitDrawn, bootLayer, rows, textOf } from "./drive";

let app: MountedApp;
/** The merge bubble's own FeedId, which is also its sub-feed's address. */
let bubbleId: string;

/** The merge bubble rows currently drawn. */
function mergeRows(): HTMLElement[] {
  return rows(app, "activity", "merge");
}

/** The merge bubble's row element, re-read each time (rows upsert whole). */
function bubble(): HTMLElement | null {
  return app.$(`[data-feed-row="${bubbleId}"]`);
}

beforeAll(async () => {
  app = await bootLayer();

  // Enqueue the merge through the app's OWN verb, then wait for the bubble the
  // daemon draws for it and for the tab strip inside that bubble.
  const response = await app.ctx.client.mergeWorkspace({ workspace: app.ctx.workspace });
  expect(response.result.case, `MergeWorkspace answered ${response.result.case}`).toBe("success");
  await awaitDrawn(app, "the merge bubble", () => mergeRows().length > 0);
  const drawn = mergeRows();
  const id = (drawn[drawn.length - 1] as HTMLElement).dataset.feedRow;
  if (id === undefined || id === "") throw new Error("the merge bubble carries no FeedId");
  bubbleId = id;
  await awaitDrawn(
    app,
    "the merge bubble's tab strip",
    () => app.$$(`[data-feed-row="${bubbleId}"] [data-merge-tab]`).length > 0,
  );
  // BUDGET: the ordinary boot + one-turn budget, with NO new bound minted.
  // MEASURED against this chain: the merge bubble is drawn 19-57ms after the
  // enqueue and the tab strip 6-8ms after that (three runs), because the
  // scripted conflict parks the merge early — the daemon's own repair turn
  // runs on behind it and nothing here waits for it. An earlier draft carried
  // a 15s "merge chain" bound; that was covering a HARNESS FAULT (a missing
  // AGENT_REPL_TEST_ALL_SCRIPT made the gate exit 127 so the merge never
  // reached a terminal), not a slow merge, and it went away with the fault.
}, TURN_TEST_MS + BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

// §F7 #30.
it("draws the merge as ACTIVITY rather than as a turn of its own", () => {
  // Assert — the daemon orchestrates the merge, but the user sees a turn still
  // in progress, so the row is an activity unit.
  const row = bubble();
  expect(row).not.toBeNull();
  expect(row?.dataset.rowKind).toBe("activity");
  expect(row?.dataset.unit).toBe("merge");
  expect(textOf(row)).not.toBe("");
});

// §F7 #30 (the strip) — every tab is conditional on its work having begun, so
// what is pinned is that the tabs drawn are named ones, not that a particular
// tab exists.
it("draws a tab strip whose every tab names which tab it is", () => {
  // Assert
  const tabs = app.$$(`[data-feed-row="${bubbleId}"] [data-merge-tab]`);
  expect(tabs.length).toBeGreaterThan(0);
  for (const tab of tabs) expect(tab.getAttribute("data-merge-tab")).not.toBe("");
});

// §F7 #31 — a RESOLVED tab draws the row's own content.
it("draws the open tab's own resolved content rather than an empty shell", () => {
  // Assert — the bubble's body carries text the DAEMON resolved (the queue
  // snapshot, the merge narration, the suites), never a placeholder the client
  // invented.
  expect(textOf(bubble())).not.toBe("");
  expect(app.$$(`[data-feed-row="${bubbleId}"] [data-merge-tab]`).length).toBeGreaterThan(0);
});

// §F7 #31 (the paint rule) — test output is PAINT-CLASS SPANS; the client
// paints classes and never parses ANSI.
it("draws no raw ANSI escape anywhere in the merge bubble", () => {
  // Assert — whatever the scripted test-all wrote, the page holds no escape
  // sequence: colour arrives as classes the daemon named.
  // eslint-disable-next-line no-control-regex
  expect(bubble()?.innerHTML ?? "").not.toMatch(/\[/);
});

// §F7 #32 — THE PARITY INVARIANT: the bubble's body is a FEED at the bubble's
// own FeedId, the same address a subagent bubble's sub-feed lives at. A
// merge-specific nested-content loader would draw no such container.
// This is the one test in the file that ACTS on the live merge (it clicks the
// expand control and waits for the OpenFeed round trip), so it takes the turn
// budget rather than the 900ms global — the same per-site discipline the
// integration config states, never a raised global.
it(
  "holds the merge body in a sub-feed at the bubble's own address",
  async () => {
    // Act — open the bubble if the daemon drew it collapsed (a merge bubble
    // starts where the daemon says, unlike a subagent's).
    if (bubble()?.dataset.expanded !== "true") {
      const expand = app.$(`[data-feed-row="${bubbleId}"] [data-expand]`);
      if (expand !== null) await app.clickElement(expand);
    }

    // Assert
    await awaitDrawn(
      app,
      `the merge sub-feed at ${bubbleId}`,
      () => app.feedContainer(bubbleId) !== null,
    );
    expect(app.feedContainer(bubbleId)).not.toBeNull();
  },
  TURN_TEST_MS,
);
