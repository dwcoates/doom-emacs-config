// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  DOCKED_CARD_CLASS,
  DOCKED_ROW_CLASS,
  installGateDock,
  type GateDock,
} from "../../src/feed/gate-dock.js";

/** MutationObserver delivers on a microtask; this lets it run. */
async function settle(): Promise<void> {
  await Promise.resolve();
}

/** A root feed in the document. */
function rootFeed(): HTMLElement {
  const feed = document.createElement("main");
  feed.setAttribute("data-feed", "root");
  document.body.replaceChildren(feed);
  return feed;
}

/** A root row holding a cold-gate card in STATE. */
function gateRow(state: string): { row: HTMLElement; card: HTMLElement } {
  const row = document.createElement("div");
  row.className = "feed-item";
  const card = document.createElement("div");
  card.className = "cold-gate";
  card.setAttribute("data-state", state);
  row.append(card);
  return { row, card };
}

let dock: GateDock | null = null;
afterEach(() => {
  dock?.dispose();
  dock = null;
});

describe("installGateDock", () => {
  it("docks a standing gate on the root feed", async () => {
    // Arrange
    const feed = rootFeed();
    dock = installGateDock(feed);
    const { row, card } = gateRow("standing");

    // Act
    feed.append(row);
    await settle();

    // Assert
    expect([row.classList.contains(DOCKED_ROW_CLASS), card.classList.contains(DOCKED_CARD_CLASS)]).toEqual([true, true]);
  });

  it("docks a gate already standing when it installs", () => {
    // Arrange
    const feed = rootFeed();
    const { row } = gateRow("standing");
    feed.append(row);

    // Act
    dock = installGateDock(feed);

    // Assert
    expect(row.classList.contains(DOCKED_ROW_CLASS)).toBe(true);
  });

  it("undocks when the gate's row is retired", async () => {
    // Arrange
    const feed = rootFeed();
    const { row, card } = gateRow("standing");
    feed.append(row);
    dock = installGateDock(feed);

    // Act
    row.remove();
    await settle();

    // Assert
    expect([row.classList.contains(DOCKED_ROW_CLASS), card.classList.contains(DOCKED_CARD_CLASS)]).toEqual([false, false]);
  });

  it("undocks when the gate is resolved in place", async () => {
    // Arrange
    const feed = rootFeed();
    const { row, card } = gateRow("standing");
    feed.append(row);
    dock = installGateDock(feed);

    // Act
    card.setAttribute("data-state", "resolved");
    await settle();

    // Assert
    expect(row.classList.contains(DOCKED_ROW_CLASS)).toBe(false);
  });

  it("never docks a gate on a sub-feed", async () => {
    // Arrange
    const feed = rootFeed();
    dock = installGateDock(feed);
    const bubble = document.createElement("div");
    const sub = document.createElement("div");
    sub.setAttribute("data-feed", "sub");
    const { row } = gateRow("standing");
    sub.append(row);
    bubble.append(sub);

    // Act
    feed.append(bubble);
    await settle();

    // Assert
    expect([bubble.classList.contains(DOCKED_ROW_CLASS), row.classList.contains(DOCKED_ROW_CLASS)]).toEqual([false, false]);
  });

  it("leaves a docked gate as it is when another row lands", async () => {
    // Arrange
    const feed = rootFeed();
    const { row } = gateRow("standing");
    feed.append(row);
    dock = installGateDock(feed);
    const other = document.createElement("div");

    // Act
    feed.append(other);
    await settle();

    // Assert
    expect([row.classList.contains(DOCKED_ROW_CLASS), other.classList.contains(DOCKED_ROW_CLASS)]).toEqual([true, false]);
  });

  it("undocks on dispose", () => {
    // Arrange
    const feed = rootFeed();
    const { row } = gateRow("standing");
    feed.append(row);
    dock = installGateDock(feed);

    // Act
    dock.dispose();
    dock = null;

    // Assert
    expect(row.classList.contains(DOCKED_ROW_CLASS)).toBe(false);
  });
});
