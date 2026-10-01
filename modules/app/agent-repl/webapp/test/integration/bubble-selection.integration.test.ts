/**
 * BUBBLE SELECTION — a click on a root-feed prompt or response bubble asks the
 * daemon to select it, and the daemon's selection push is what marks it blue
 * and expands it (owner ruling, 2026-10-01). The fake daemon answers
 * SelectFeedRow and never pushes on its own; each test states the push.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedSelectionSchema, type FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { bootColdOnce, startHarness, type Harness } from "./harness";
import { ROOT_FEED } from "./fake-daemon";
import { WORKSPACE_ID, feedId, responseRow, userPromptRow } from "./fixtures";
import { selectable } from "../selectable.js";

let harness: Harness;

bootColdOnce();

afterEach(async () => {
  await harness?.stop();
});

/** Boot with ROWS on the root feed's tail. */
async function withRows(...rows: FeedRow[]): Promise<void> {
  harness = await startHarness();
  await harness.fake.awaitStream("watchFeed");
  for (const row of rows) harness.fake.pushRow(WORKSPACE_ID, ROOT_FEED, row);
  await harness.settle();
}

/** The daemon's selection push naming ID as a bubble, or none. */
async function pushSelection(id?: string): Promise<void> {
  harness.fake.pushSelection(
    WORKSPACE_ID,
    create(FeedSelectionSchema, {
      selection:
        id === undefined
          ? { case: "none", value: { viewport: { case: "returnToTail", value: {} } } }
          : { case: "bubble", value: { row: feedId(id) } },
    }),
  );
  await harness.settle();
}

/** The SelectFeedRow moves the page sent, as `bubble:<row>` or the move's case. */
function moves(): string[] {
  return harness.fake
    .calls<{ move: { case?: string; value?: unknown } }>("selectFeedRow")
    .map(({ move }) =>
      move.case === "bubble" ? `bubble:${(move.value as { row?: { value: string } }).row?.value ?? ""}` : String(move.case),
    );
}

describe("a click on a bubble", () => {
  it("asks the daemon to select a landed response", async () => {
    // Arrange
    await withRows(selectable(responseRow("success", "the answer", { id: feedId("r1") })));
    // Act
    await harness.click('[data-feed-row="r1"] .bubble-body');
    // Assert
    expect(moves()).toEqual(["bubble:r1"]);
  });

  it("asks the daemon to select a user prompt", async () => {
    // Arrange
    await withRows(selectable(userPromptRow("what is the capital?", { id: feedId("p1") })));
    // Act
    await harness.click('[data-feed-row="p1"] .bubble-body');
    // Assert
    expect(moves()).toEqual(["bubble:p1"]);
  });

  it("asks nothing for a response still arriving", async () => {
    // Arrange
    await withRows(responseRow("update", "partial", { id: feedId("r1") }));
    // Act
    await harness.click('[data-feed-row="r1"] .bubble-body');
    // Assert
    expect(moves()).toEqual([]);
  });

  it("asks the daemon to clear when the selected bubble is clicked", async () => {
    // Arrange
    await withRows(selectable(responseRow("success", "the answer", { id: feedId("r1") })));
    await pushSelection("r1");
    // Act
    await harness.click('[data-feed-row="r1"] .bubble-body');
    // Assert
    expect(moves()).toEqual(["clear"]);
  });
});

describe("the daemon's selection push", () => {
  it("marks the selected bubble of any kind blue", async () => {
    // Arrange
    await withRows(selectable(userPromptRow("what is the capital?", { id: feedId("p1") })));
    // Act
    await pushSelection("p1");
    // Assert
    expect(harness.$('[data-feed-row="p1"] .bubble')?.classList.contains("entry-selected")).toBe(true);
  });

  it("moves the mark off a bubble when another is selected", async () => {
    // Arrange
    await withRows(
      selectable(userPromptRow("first", { id: feedId("p1") })),
      selectable(userPromptRow("second", { id: feedId("p2") })),
    );
    await pushSelection("p1");
    // Act
    await pushSelection("p2");
    // Assert
    expect(
      ["p1", "p2"].map((id) => harness.$(`[data-feed-row="${id}"] .bubble`)?.classList.contains("entry-selected")),
    ).toEqual([false, true]);
  });

  it("expands the selected bubble and collapses it when the selection clears", async () => {
    // Arrange
    await withRows(selectable(userPromptRow("what is the capital?", { id: feedId("p1") })));
    const box = (): boolean | undefined =>
      harness.$('[data-feed-row="p1"] .bubble > .bubble-scroll')?.classList.contains("expanded");
    // Act
    await pushSelection("p1");
    const opened = box();
    await pushSelection();
    // Assert
    expect([opened, box()]).toEqual([true, false]);
  });
});
