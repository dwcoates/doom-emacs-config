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

import { startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import {
  WORKSPACE_ID,
  activityRow,
  feedId,
  feedPageError,
  feedPageSuccess,
  mergeUnit,
  responseRow,
  subagentUnit,
  turnEndedConcludedRow,
  userPromptRow,
} from "./fixtures";

let harness: Harness;

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
          feedPageSuccess([userPromptRow("older", { id: feedId("old") })]),
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

/** The bubble lifecycle, run identically for a subagent and for a merge. */
const BUBBLE_CASES = [
  { name: "a subagent bubble", unit: () => subagentUnit("live") },
  { name: "a merge bubble", unit: () => mergeUnit("update") },
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
    const mergeSequence = await sequenceFor(() => mergeUnit("update"));
    // Assert: a merge-specific nested-content loader would show up here.
    expect(mergeSequence).toEqual(subagentSequence);
    harness = await startHarness();
  });

  it("expands through OpenFeed then WatchFeed and nothing else", async () => {
    // Arrange / Act
    const sequence = await sequenceFor(() => mergeUnit("update"));
    // Assert
    expect(sequence).toEqual(["openFeed", "watchFeed"]);
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
