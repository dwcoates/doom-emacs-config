/**
 * FEED ROUTING — identity, paging, and the sub-feed lifecycle.
 *
 * The feed is the one SELF-SIMILAR component: a bubble IS a feed. Everything
 * here is about that — that a row is addressed by its `FeedId` and replaced
 * whole, that paging asks first/next and never carries a cursor, and that a
 * bubble's expansion is the SAME plumbing whether the bubble is a subagent or
 * a merge. The parity assertion is the load-bearing one: a merge-specific
 * nested-content loader is a defect by ruling, and the way to catch it is to
 * compare the two rpc sequences directly.
 */
import { afterEach, describe, expect, it } from "vitest";

import { bootColdOnce, startHarness, type Harness } from "./harness";
import { installStylesheet } from "../stylesheet.js";
import { ROOT_FEED } from "./fake-daemon";
import {
  WORKSPACE_ID,
  activityRow,
  detachedSubagentRow,
  feedId,
  feedPageError,
  feedPageSuccess,
  mergeUnit,
  type MergeResult,
  responseRow,
  skillUnit,
  subagentUnit,
  turnEndedConcludedRow,
  userPromptRow,
} from "./fixtures";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

describe("upsert by FeedId", () => {
  it("replaces a row in place when its id is pushed again", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("update", "half an answer"));
    await harness.settle();
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "the whole answer"));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.textContent).toContain("the whole answer");
  });

  it("does not duplicate the row on a re-push", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("update"));
    await harness.settle();
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success"));
    await harness.settle();
    // Assert
    expect(harness.rowIds()).toEqual(["row-1"]);
  });

  it("preserves ordering when an earlier row is re-pushed", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, userPromptRow("first", { id: feedId("a") }));
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, userPromptRow("second", { id: feedId("b") }));
    await harness.settle();
    // Act: re-push the FIRST row; it must stay first.
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, userPromptRow("first, grown", { id: feedId("a") }));
    await harness.settle();
    // Assert
    expect(harness.rowIds()).toEqual(["a", "b"]);
  });

  it("draws late rows at their keys, not below the rows that arrived before them", async () => {
    // Arrange — a turn's answer and its end are already drawn.
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, userPromptRow("why", { id: feedId("prompt"), order: { key: "k1" } }));
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "because", { id: feedId("answer"), order: { key: "k5" } }));
    await harness.settle();
    // Act — two rows whose keys sort between them arrive last.
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "one", { id: feedId("late-1"), order: { key: "k3" } }));
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "two", { id: feedId("late-2"), order: { key: "k4" } }));
    await harness.settle();
    // Assert
    expect(harness.rowIds()).toEqual(["prompt", "late-1", "late-2", "answer"]);
  });

  it("refuses a pushed row without an order key as an undecodable frame", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "unplaced", { order: undefined }));
    await harness.settle();
    // Assert
    expect({ rows: harness.rowIds(), failures: harness.failureArms() }).toEqual({
      rows: [],
      failures: expect.arrayContaining(["frameUndecodable"]) as unknown,
    });
  });

  it("drops the old text when a row is replaced whole", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("update", "the draft"));
    await harness.settle();
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "the final"));
    await harness.settle();
    // Assert
    expect(harness.row("row-1")?.textContent).not.toContain("the draft");
  });
});

describe("presentation nesting", () => {
  it("draws a row with a parent inside its container", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, activityRow(subagentUnit("live"), { id: feedId("head") }));
    await harness.settle();
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      responseRow("success", "nested work", { id: feedId("child"), parent: { row: feedId("head") } }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("head")?.contains(harness.row("child"))).toBe(true);
  });

  it("draws a row with no parent at the top level", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, activityRow(subagentUnit("live"), { id: feedId("head") }));
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "loose", { id: feedId("loose") }));
    await harness.settle();
    // Assert
    expect(harness.row("head")?.contains(harness.row("loose"))).toBe(false);
  });
});

describe("paging", () => {
  it("asks GetFeedPage for `next`, never for a cursor", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([userPromptRow("newest")], { edge: "hasMore" }));
        fake.setNextPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([userPromptRow("older")]));
      },
    });
    // Act
    await harness.click("[data-load-more]");
    // Assert
    const [request] = harness.fake.calls<{ page: { case?: string } }>("getFeedPage");
    expect(request.page.case).toBe("next");
  });

  it("prepends the older page above the newest rows", async () => {
    // Arrange
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([userPromptRow("newest", { id: feedId("new") })], { edge: "hasMore" }),
        );
        fake.setNextPage(
          WORKSPACE_ID,
          ROOT_FEED,
          // The older row's key sorts before the newest page's: the walk serves history.
          feedPageSuccess([userPromptRow("older", { id: feedId("old"), order: { key: "a" } })]),
        );
      },
    });
    // Act
    await harness.click("[data-load-more]");
    // Assert
    expect(harness.rowIds()).toEqual(["old", "new"]);
  });

  it("hides the load-more control once the page reports at_start", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) =>
        fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([userPromptRow("all of it")], { edge: "atStart" })),
    });
    // Assert
    expect(harness.$("[data-load-more]")).toBeNull();
  });

  it("shows the load-more control while the page reports has_more", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) =>
        fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([userPromptRow("newest")], { edge: "hasMore" })),
    });
    // Assert
    expect(harness.$("[data-load-more]")).not.toBeNull();
  });

  it("draws a page error where the rows would have gone", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) => fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageError()),
    });
    // Assert
    expect(harness.feedContainer()?.textContent).toContain("history could not be replayed");
  });

  it("draws no rows when the page is an error", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) => fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageError()),
    });
    // Assert
    expect(harness.rowIds()).toEqual([]);
  });
});

describe("breadcrumbs", () => {
  it("draws the crumbs a page carries", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) =>
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([userPromptRow("hi")], {
            breadcrumbs: [{ label: "root", target: "root-id" }, { label: "reviewer", target: "bubble" }],
          }),
        ),
    });
    // Assert
    expect(harness.$("[data-breadcrumbs]")?.textContent).toContain("reviewer");
  });

  it("draws no breadcrumb line when the list is empty", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) => fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([userPromptRow("hi")])),
    });
    // Assert
    expect(harness.$("[data-breadcrumbs]")).toBeNull();
  });
});

/**
 * The bubble lifecycle, run identically for a subagent and for a merge. The
 * merge is a LANDED one: a success is the one merge state the daemon draws
 * folded (owner ruling, 2026-10-08), so the reader's expand drives the
 * plumbing exactly as it does for a subagent.
 */
const BUBBLE_CASES = [
  { name: "a subagent bubble", unit: () => subagentUnit("live") },
  { name: "a merge bubble", unit: () => mergeUnit("success") },
] as const;

describe.each(BUBBLE_CASES)("$name", ({ unit }) => {
  const openBubble = async (): Promise<Harness> => {
    const h = await startHarness({
      arrange: (fake) => {
        fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([activityRow(unit(), { id: feedId("bubble") })]));
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    return h;
  };

  it("calls OpenFeed with the bubble's own FeedId when expanded", async () => {
    // Arrange
    harness = await openBubble();
    harness.fake.clearCalls();
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert
    const [request] = harness.fake.calls<{ feed?: { value: string } }>("openFeed");
    expect(request.feed?.value).toBe("bubble");
  });

  it("watches the sub-feed with the token OpenFeed minted", async () => {
    // Arrange
    harness = await openBubble();
    harness.fake.clearCalls();
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed");
    // Assert
    const watches = harness.fake.calls<{ watch?: { value: string } }>("watchFeed");
    const minted = harness.fake.mintedTokens(WORKSPACE_ID, "bubble");
    expect(watches.at(-1)?.watch?.value).toBe(minted.at(-1));
  });

  it("marks the bubble expanded", async () => {
    // Arrange
    harness = await openBubble();
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert
    expect(harness.row("bubble")?.dataset.expanded).toBe("true");
  });

  it("cancels the sub-feed watch on collapse", async () => {
    // Arrange
    harness = await openBubble();
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed", 2);
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.settle();
    // Assert
    expect(harness.fake.liveStreams("watchFeed", WORKSPACE_ID, "bubble")).toBe(0);
  });

  it("re-opens the sub-feed on a second expand", async () => {
    // Arrange
    harness = await openBubble();
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    const opensBefore = harness.fake.calls("openFeed").length;
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert
    expect(harness.fake.calls("openFeed").length).toBe(opensBefore + 1);
  });

  // A SUB-FEED HANGS BENEATH ITS HEAD. The bubble is a `.tool-card` now (owner
  // ruling, 2026-09-14), and `.tool-card.bubble-fold` states `display: flex;
  // flex-direction: column` explicitly, so the head line and the whole sub-feed
  // stack rather than falling to a side-by-side row. The old `.bubble` flex-ROW
  // failure this replaces (head and sub-feed side by side, half the bubble empty
  // under the head) was photographed by the G49 playbook. The cascade is
  // installed here because a class assertion alone passed the entire time.
  // Real stylesheet installation plus a socket-backed expansion is the bound;
  // it reached 1635ms under concurrent integration load.
  it("stacks its sub-feed under its head, under the real stylesheet", async () => {
    // Arrange
    harness = await openBubble();
    const remove = installStylesheet();
    try {
      await harness.click('[data-feed-row="bubble"] [data-expand]');
      const bubble = harness.row("bubble")?.querySelector(".bubble-fold");
      // Act / Assert
      expect(bubble).not.toBeNull();
      expect(window.getComputedStyle(bubble as Element).flexDirection).toBe("column");
    } finally {
      remove();
    }
  }, 2_500);

  it("hosts the sub-feed inside the bubble rather than navigating to it", async () => {
    // Arrange
    harness = await openBubble();
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert
    expect(harness.row("bubble")?.querySelector("[data-subfeed]")).not.toBeNull();
  });

  it("draws a sub-feed row pushed on the bubble's tail", async () => {
    // Arrange
    harness = await openBubble();
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed", 2);
    // Act
    harness.fake.pushRow(WORKSPACE_ID, "bubble", responseRow("success", "inner work", { id: feedId("inner") }));
    await harness.settle();
    // Assert
    expect(harness.row("bubble")?.textContent).toContain("inner work");
  });

  // THE FOLD'S OTHER HALF. `data-expanded` says the bubble is shut; the panel's
  // own `hidden` is what makes it LOOK shut, and the two came apart in the real
  // webview -- the sheet's `.agent-panel { display: flex }` outranked the
  // user-agent `[hidden]` rule, so a collapsed sub-feed stayed fully drawn
  // under a caret that said it was closed. The sheet now carries the guard
  // (pinned in test/feed/bubble.test.ts, and asserted against a real browser in
  // the G49 playbook); what is asserted here is that the shell's own caret
  // reaches the attribute that guard keys on.
  it("hides the sub-feed panel when the caret folds it", async () => {
    // Arrange
    harness = await openBubble();
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed", 2);
    harness.fake.pushRow(WORKSPACE_ID, "bubble", responseRow("success", "inner work", { id: feedId("inner") }));
    await harness.settle();
    const panel = harness.row("bubble")?.querySelector<HTMLElement>("[data-subfeed]");
    expect(panel?.hidden).toBe(false);
    // Act
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.settle();
    // Assert
    expect(panel?.hidden).toBe(true);
    expect(harness.row("bubble")?.dataset.expanded).toBe("false");
  });

  it("does not draw a sub-feed row on the root feed", async () => {
    // Arrange
    harness = await openBubble();
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed", 2);
    // Act
    harness.fake.pushRow(WORKSPACE_ID, "bubble", responseRow("success", "inner", { id: feedId("inner") }));
    await harness.settle();
    // Assert: the inner row is inside the bubble, not a sibling of it.
    expect(harness.feedContainer()?.children).not.toContain(harness.row("inner"));
  });
});

// A BACKGROUND SPAWN CHANGES PLACEMENT UNDER ITS OWN FeedId. The daemon
// announces it as a synchronous `subagent` unit and re-pushes the SAME row as
// `detached_subagent` once the vendor answers `async_launched`. The bubble is
// kept across that push so an open sub-feed survives it, which means the head
// must be chosen from the row in hand -- it was chosen once at mount, and every
// detached spawn was then refused as an unreadable frame and froze on the state
// it was announced with. Caught by the G51 playbook, whose `!cancel-all`
// launches two of them.
describe("a spawn that moves to its detached placement", () => {
  const startSync = async (): Promise<Harness> =>
    startHarness({
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });

  it("reads the re-pushed row rather than refusing it", async () => {
    // Arrange
    harness = await startSync();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, detachedSubagentRow("succeeded", { id: feedId("bubble") }));
    await harness.settle();
    // Assert
    expect(harness.failureArms()).toEqual([]);
  });

  it("redraws the head in its new placement", async () => {
    // Arrange
    harness = await startSync();
    await harness.fake.awaitStream("watchFeed");
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, detachedSubagentRow("succeeded", { id: feedId("bubble") }));
    await harness.settle();
    // Assert
    expect(
      harness.row("bubble")?.querySelector(".subagent-head")?.getAttribute("data-state"),
    ).toBe("succeeded");
  });

  it("keeps the reader's open sub-feed across the move", async () => {
    // Arrange
    harness = await startSync();
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed", 2);
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, detachedSubagentRow("live", { id: feedId("bubble") }));
    await harness.settle();
    // Assert
    expect(harness.row("bubble")?.dataset.expanded).toBe("true");
  });
});

describe("the merge bubble's parity with a subagent bubble", () => {
  /** Expand one bubble and return the rpc names it called, in order. */
  const sequenceFor = async (unit: () => ReturnType<typeof subagentUnit>): Promise<string[]> => {
    const h = await startHarness({
      arrange: (fake) => {
        fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([activityRow(unit(), { id: feedId("bubble") })]));
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    h.fake.clearCalls();
    await h.click('[data-feed-row="bubble"] [data-expand]');
    await h.fake.awaitStream("watchFeed", 2);
    const sequence = h.fake.log().map((c) => c.rpc);
    await h.stop();
    return sequence;
  };

  it("expands through the same rpc sequence for both bubble kinds", async () => {
    // Arrange / Act
    const subagentSequence = await sequenceFor(() => subagentUnit("live"));
    // A landed merge: the one merge state that arrives folded.
    const mergeSequence = await sequenceFor(() => mergeUnit("success"));
    // Assert: a merge-specific nested-content loader would show up here. The
    // one merge-only call is the reader's fold being RECORDED
    // (`FoldMergeBubble`, owner ruling 2026-10-08), which loads nothing.
    expect(mergeSequence.filter((rpc) => rpc !== "foldMergeBubble")).toEqual(subagentSequence);
    harness = await startHarness();
  });

  it("expands through OpenFeed then a SUBSCRIPTION on the page's stream, and nothing else", async () => {
    // Arrange / Act
    // A landed merge: the one merge state that arrives folded.
    const sequence = await sequenceFor(() => mergeUnit("success"));
    // Assert: the bubble's tail rides the stream the page already holds. The
    // absence of `watchPage` here is the load-bearing half — the calls were
    // cleared after boot, so a second connection for this tail would appear.
    // `watchFeed` still follows because the subscription IS that watch: the
    // fake serves it from the very source the dedicated rpc serves.
    // The reader's click is recorded too (`FoldMergeBubble`), once the
    // bubble has opened; it loads nothing.
    expect(sequence).toEqual(["openFeed", "subscribePage", "watchFeed", "foldMergeBubble"]);
    harness = await startHarness();
  });
});

describe("the final answer", () => {
  it("marks exactly the response row that turn_ended names as the answer", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "first", { id: feedId("r1") }));
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "final", { id: feedId("r2") }));
    await harness.settle();
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      ROOT_FEED,
      turnEndedConcludedRow(feedId("r2"), { id: feedId("end") }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("r2")?.dataset.finalAnswer).toBe("true");
  });

  it("leaves the earlier response rows unmarked", async () => {
    // Arrange
    harness = await startHarness();
    await harness.fake.awaitStream("watchFeed");
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "first", { id: feedId("r1") }));
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, responseRow("success", "final", { id: feedId("r2") }));
    await harness.settle();
    // Act
    harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, turnEndedConcludedRow(feedId("r2"), { id: feedId("end") }));
    await harness.settle();
    // Assert
    expect(harness.row("r1")?.dataset.finalAnswer).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// SUB-FEED PAGING (audit 1, item 3)
//
// A bubble IS a feed, so its history is paged by the SAME verb with the
// bubble's own FeedId — the self-similarity the whole feed mechanism rests on.
// A `GetFeedPage` that dropped the address would page the ROOT into the
// bubble, which is exactly the defect these assertions catch.
// ---------------------------------------------------------------------------

describe("paging a sub-feed", () => {
  /** A bubble whose sub-feed has one row and more history behind it. */
  const openPagedBubble = async (): Promise<void> => {
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(
          WORKSPACE_ID,
          "bubble",
          feedPageSuccess([responseRow("success", "newest inner", { id: feedId("inner-new") })], {
            edge: "hasMore",
          }),
        );
        fake.setNextPage(
          WORKSPACE_ID,
          "bubble",
          // The older row's key sorts before the newest page's: the walk serves history.
          feedPageSuccess([
            responseRow("success", "older inner", { id: feedId("inner-old"), order: { key: "a" } }),
          ]),
        );
      },
    });
    await harness.click('[data-feed-row="bubble"] [data-expand]');
  };

  it("draws a load-more control inside the bubble", async () => {
    // Arrange / Act
    await openPagedBubble();
    // Assert
    expect(harness.$('[data-feed-row="bubble"] [data-subfeed] [data-load-more]')).not.toBeNull();
  });

  it("addresses GetFeedPage to the bubble's own FeedId", async () => {
    // Arrange
    await openPagedBubble();
    // Act
    await harness.click('[data-feed-row="bubble"] [data-subfeed] [data-load-more]');
    // Assert
    const [request] = harness.fake.calls<{ feed?: { value: string } }>("getFeedPage");
    expect(request.feed?.value).toBe("bubble");
  });

  it("asks for `next`, never for a cursor", async () => {
    // Arrange
    await openPagedBubble();
    // Act
    await harness.click('[data-feed-row="bubble"] [data-subfeed] [data-load-more]');
    // Assert
    const [request] = harness.fake.calls<{ page: { case?: string } }>("getFeedPage");
    expect(request.page.case).toBe("next");
  });

  it("prepends the older rows inside the bubble", async () => {
    // Arrange
    await openPagedBubble();
    // Act
    await harness.click('[data-feed-row="bubble"] [data-subfeed] [data-load-more]');
    // Assert
    const subfeed = harness.$('[data-feed-row="bubble"] [data-subfeed]');
    expect(harness.rowIds(subfeed ?? undefined)).toEqual(["inner-old", "inner-new"]);
  });

  it("draws no older row on the root feed", async () => {
    // Arrange
    await openPagedBubble();
    // Act
    await harness.click('[data-feed-row="bubble"] [data-subfeed] [data-load-more]');
    // Assert: the page landed in the feed it was asked for.
    expect(harness.row("bubble")?.contains(harness.row("inner-old"))).toBe(true);
  });

  it("draws no load-more inside a bubble whose page is at_start", async () => {
    // Arrange / Act
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(
          WORKSPACE_ID,
          "bubble",
          feedPageSuccess([responseRow("success", "all of it", { id: feedId("inner") })], {
            edge: "atStart",
          }),
        );
      },
    });
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    // Assert
    expect(harness.$('[data-feed-row="bubble"] [data-subfeed] [data-load-more]')).toBeNull();
  });
});

/**
 * A SUB-FEED'S TAIL DIES.
 *
 * A bubble's tail is standing, so an end it did not ask for is a transport
 * failure — and the reopen must go back through `OpenFeed` rather than
 * re-echoing the token the dead tail was pinned to (feed_token.proto: a token
 * pins the tail to begin exactly after the page its open answered with). The
 * root feed already does this; a bubble that did not would resume against a
 * page painted before the outage.
 */
describe("a sub-feed's tail after a transport death", () => {
  const openBubble = async (): Promise<void> => {
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(subagentUnit("live"), { id: feedId("bubble") })]),
        );
        fake.setPage(WORKSPACE_ID, "bubble", feedPageSuccess([]));
      },
    });
    await harness.click('[data-feed-row="bubble"] [data-expand]');
    await harness.fake.awaitStream("watchFeed", 2);
  };

  it("re-opens the sub-feed rather than reusing the dead token", async () => {
    // Arrange
    await openBubble();
    const opensBefore = harness.fake.calls("openFeed").length;
    // Act
    harness.fake.endStream("watchFeed", WORKSPACE_ID, "bubble");
    await harness.tick(5_000);
    // Assert
    expect(harness.fake.calls("openFeed").length).toBeGreaterThan(opensBefore);
  });

  it("tails the token the fresh open minted", async () => {
    // Arrange
    await openBubble();
    // Act
    harness.fake.endStream("watchFeed", WORKSPACE_ID, "bubble");
    await harness.tick(5_000);
    // Assert
    const watches = harness.fake.calls<{ watch?: { value: string } }>("watchFeed");
    const minted = harness.fake.mintedTokens(WORKSPACE_ID, "bubble");
    expect(watches.at(-1)?.watch?.value).toBe(minted.at(-1));
  });

  it("draws a row pushed on the reopened tail", async () => {
    // Arrange
    await openBubble();
    harness.fake.endStream("watchFeed", WORKSPACE_ID, "bubble");
    await harness.tick(5_000);
    // Act
    harness.fake.pushRow(
      WORKSPACE_ID,
      "bubble",
      responseRow("success", "after the death", { id: feedId("inner-after") }),
    );
    await harness.settle();
    // Assert
    expect(harness.row("bubble")?.textContent).toContain("after the death");
  });

  it("paints the fresh page over the rows the dead tail had left", async () => {
    // Arrange
    await openBubble();
    harness.fake.pushRow(
      WORKSPACE_ID,
      "bubble",
      responseRow("success", "live row", { id: feedId("inner-live") }),
    );
    await harness.settle();
    // Act: the fresh page (still empty) REPLACES the sub-feed's rows.
    harness.fake.endStream("watchFeed", WORKSPACE_ID, "bubble");
    await harness.tick(5_000);
    // Assert
    expect(harness.row("inner-live")).toBeNull();
  });
});

/**
 * A FOLD SURVIVES A FULL PAGE REPLACE (owner ruling, 2026-09-18: "a redraw
 * never un-toggles, whatever its shape"). A re-push of one row has always kept
 * the reader's toggle; a REPLACE — the reconnect, the reload, the compaction
 * replay — tears every row down and rebuilds it, and the folds ride across that
 * gap on the row's own identity.
 */
describe("the reader's folds across a page replace", () => {
  /** Boot on a page holding one skill card, and serve that same page again. */
  const bootWithSkill = async (): Promise<void> => {
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(
          WORKSPACE_ID,
          ROOT_FEED,
          feedPageSuccess([activityRow(skillUnit("loaded"), { id: feedId("skill-1") })]),
        );
      },
    });
  };

  it("keeps a card the reader opened open across the replace", async () => {
    // Arrange
    await bootWithSkill();
    await harness.click('[data-feed-row="skill-1"] .tool-skill');
    // Act: the tail dies, the app re-opens the feed and REPLACES every row.
    harness.fake.endStream("watchFeed", WORKSPACE_ID, ROOT_FEED);
    await harness.tick(5_000);
    // Assert
    expect(
      harness.$('[data-feed-row="skill-1"] .tool-skill')?.classList.contains("expanded"),
    ).toBe(true);
  });

  it("leaves a card the reader never opened closed across the replace", async () => {
    // Arrange
    await bootWithSkill();
    // Act
    harness.fake.endStream("watchFeed", WORKSPACE_ID, ROOT_FEED);
    await harness.tick(5_000);
    // Assert
    expect(
      harness.$('[data-feed-row="skill-1"] .tool-skill')?.classList.contains("expanded"),
    ).toBe(false);
  });
});

/**
 * A MERGE BUBBLE ACROSS A PAGE REPLACE (owner ruling, 2026-10-08): the replace
 * draws the daemon's fold, open save a success, and a bubble the READER left
 * open on this page is opened again, the reader's fold always winning.
 */
describe("a merge bubble across a page replace", () => {
  /** Boot on a page holding one merge in RESULT. */
  const bootWithMerge = async (result: MergeResult): Promise<void> => {
    harness = await startHarness({
      arrange: (fake) => {
        fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([activityRow(mergeUnit(result), { id: feedId("merge-1") })]));
      },
    });
  };
  /** Kill the tail so the app re-opens the feed onto a page holding a merge in RESULT. */
  const replaceWith = async (result: MergeResult): Promise<void> => {
    harness.fake.setPage(WORKSPACE_ID, ROOT_FEED, feedPageSuccess([activityRow(mergeUnit(result), { id: feedId("merge-1") })]));
    harness.fake.endStream("watchFeed", WORKSPACE_ID, ROOT_FEED);
    await harness.tick(5_000);
  };
  const expanded = () => harness.row("merge-1")?.dataset.expanded;

  it("leaves a running merge's bubble open across the replace", async () => {
    // Arrange
    await bootWithMerge("update");
    // Act
    await replaceWith("update");
    // Assert
    expect(expanded()).toBe("true");
  });

  it("draws the bubble folded when the merge succeeded across the replace", async () => {
    // Arrange
    await bootWithMerge("update");
    // Act
    await replaceWith("success");
    // Assert
    expect(expanded()).toBe("false");
  });

  it("opens again a landed merge's bubble the reader opened, across the replace", async () => {
    // Arrange
    await bootWithMerge("success");
    await harness.click('[data-feed-row="merge-1"] [data-expand]');
    // Act
    await replaceWith("success");
    // Assert
    expect(expanded()).toBe("true");
  });
});
