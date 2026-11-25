// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  DOCKED_CARD_CLASS,
  DOCKED_ROW_CLASS,
  installGateDock,
  installPageGate,
  WEBHOST_GATE_CLASS,
  type GateDock,
} from "../../src/feed/gate-dock.js";
import { bodiedFeed, gateRow, page, settle } from "./gate-dock-page.js";

let installed: GateDock | null = null;
afterEach(() => {
  installed?.dispose();
  installed = null;
  document.documentElement.classList.remove(WEBHOST_GATE_CLASS);
});

describe("installGateDock on the feed's real shape", () => {
  it("hides only the gate's own row, never the body holding every row", async () => {
    // Arrange
    const { feed, dock } = page();
    const other = document.createElement("div");
    other.className = "feed-item";
    other.textContent = "the conversation";
    const { row } = gateRow("standing");
    const body = bodiedFeed(feed, other, row);
    installed = installGateDock(feed, dock);

    // Act
    await settle();

    // Assert
    expect([
      body.classList.contains(DOCKED_ROW_CLASS),
      other.classList.contains(DOCKED_ROW_CLASS),
      row.classList.contains(DOCKED_ROW_CLASS),
      dock.hidden,
    ]).toEqual([false, false, true, false]);
  });

  it("undocks when the gate's row leaves the body", async () => {
    // Arrange
    const { feed, dock } = page();
    const { row } = gateRow("standing");
    bodiedFeed(feed, row);
    installed = installGateDock(feed, dock);
    await settle();

    // Act
    row.remove();
    await settle();

    // Assert
    expect(dock.hidden).toBe(true);
  });
});

describe("installGateDock", () => {
  it("draws the dock below the footer, outside the main column, beside nothing", () => {
    // Arrange / Act
    const { dock, footer } = page();

    // Assert: a child of the page itself, after the main column, so it can
    // span the rail's column as well as the main one.
    expect([
      footer.compareDocumentPosition(dock) & Node.DOCUMENT_POSITION_FOLLOWING,
      dock.parentElement?.tagName ?? null,
      dock.previousElementSibling?.id ?? null,
    ]).toEqual([Node.DOCUMENT_POSITION_FOLLOWING, "BODY", "main-col"]);
  });

  it("moves a standing gate's card into the dock and shows it", async () => {
    // Arrange
    const { feed, dock } = page();
    installed = installGateDock(feed, dock);
    const { row, card } = gateRow("standing");

    // Act
    feed.append(row);
    await settle();

    // Assert
    expect([card.parentElement, dock.hidden, card.classList.contains(DOCKED_CARD_CLASS)]).toEqual([dock, false, true]);
  });

  it("draws no inline gate in the feed while docked", async () => {
    // Arrange
    const { feed, dock } = page();
    installed = installGateDock(feed, dock);
    const { row } = gateRow("standing");

    // Act
    feed.append(row);
    await settle();

    // Assert
    expect([feed.querySelector(".cold-gate"), row.classList.contains(DOCKED_ROW_CLASS)]).toEqual([null, true]);
  });

  it("docks a gate already standing when it installs", () => {
    // Arrange
    const { feed, dock } = page();
    const { row, card } = gateRow("standing");
    feed.append(row);

    // Act
    installed = installGateDock(feed, dock);

    // Assert
    expect(card.parentElement).toBe(dock);
  });

  it("undocks and hides the dock when the gate is answered (its row retired)", async () => {
    // Arrange
    const { feed, dock } = page();
    const { row } = gateRow("standing");
    feed.append(row);
    installed = installGateDock(feed, dock);

    // Act
    row.remove();
    await settle();

    // Assert
    expect([dock.hidden, dock.childElementCount]).toEqual([true, 0]);
  });

  it("returns a gate resolved in place to its row", async () => {
    // Arrange
    const { feed, dock } = page();
    const { row, card } = gateRow("standing");
    feed.append(row);
    installed = installGateDock(feed, dock);

    // Act
    card.setAttribute("data-state", "resolved");
    await settle();

    // Assert
    expect([card.parentElement, row.classList.contains(DOCKED_ROW_CLASS), dock.hidden]).toEqual([row, false, true]);
  });

  it("swaps in the card of a redrawn gate row", async () => {
    // Arrange
    const { feed, dock } = page();
    const first = gateRow("standing");
    feed.append(first.row);
    installed = installGateDock(feed, dock);
    const second = gateRow("standing");

    // Act
    first.row.replaceWith(second.row);
    await settle();

    // Assert
    expect([...dock.children]).toEqual([second.card]);
  });

  it("never docks a gate on a sub-feed", async () => {
    // Arrange
    const { feed, dock } = page();
    installed = installGateDock(feed, dock);
    const bubble = document.createElement("div");
    const sub = document.createElement("div");
    sub.setAttribute("data-feed", "sub");
    const { row, card } = gateRow("standing");
    sub.append(row);
    bubble.append(sub);

    // Act
    feed.append(bubble);
    await settle();

    // Assert
    expect([card.parentElement, dock.hidden]).toEqual([row, true]);
  });

  it("leaves a docked gate as it is when another row lands", async () => {
    // Arrange
    const { feed, dock } = page();
    const { row, card } = gateRow("standing");
    feed.append(row);
    installed = installGateDock(feed, dock);
    const other = document.createElement("div");

    // Act
    feed.append(other);
    await settle();

    // Assert
    expect([card.parentElement, other.classList.contains(DOCKED_ROW_CLASS)]).toEqual([dock, false]);
  });

});

describe("installPageGate", () => {
  it("in the webhost, marks the page as one whose gate the webhost draws", () => {
    // Arrange
    const { feed, dock } = page();

    // Act
    installed = installPageGate(document, true, feed, dock);

    // Assert
    expect([installed, document.documentElement.classList.contains(WEBHOST_GATE_CLASS)]).toEqual([null, true]);
  });

  it("in the webhost, docks no standing gate in the page", async () => {
    // Arrange
    const { feed, dock } = page();
    installed = installPageGate(document, true, feed, dock);
    const { row, card } = gateRow("standing");

    // Act
    feed.append(row);
    await settle();

    // Assert
    expect([card.parentElement, dock.hidden]).toEqual([row, true]);
  });

  it("on its own, docks the page's standing gate", async () => {
    // Arrange
    const { feed, dock } = page();
    installed = installPageGate(document, false, feed, dock);
    const { row, card } = gateRow("standing");

    // Act
    feed.append(row);
    await settle();

    // Assert
    expect([card.parentElement, document.documentElement.classList.contains(WEBHOST_GATE_CLASS)]).toEqual([dock, false]);
  });
});
