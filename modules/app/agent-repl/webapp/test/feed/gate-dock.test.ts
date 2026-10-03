// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  DOCK_HEIGHT_PROPERTY,
  toldDockHeight,
  DOCKED_CARD_CLASS,
  DOCKED_ROW_CLASS,
  installGateDock,
  type GateDock,
} from "../../src/feed/gate-dock.js";
import { shellElements } from "../../src/shell.js";
import { shellHTML } from "../shell-html.js";

/** MutationObserver delivers on a microtask; this lets it run. */
async function settle(): Promise<void> {
  await Promise.resolve();
  await Promise.resolve();
}

/** The real page shell, from index.html. */
function page(): { feed: HTMLElement; dock: HTMLElement; footer: HTMLElement } {
  document.body.innerHTML = shellHTML();
  const shell = shellElements(document);
  return { feed: shell.feed, dock: shell.gateDock, footer: shell.footer };
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

let installed: GateDock | null = null;
afterEach(() => {
  installed?.dispose();
  installed = null;
  document.documentElement.style.removeProperty(DOCK_HEIGHT_PROPERTY);
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

  it("spans every column of the page's second row", async () => {
    // Arrange
    const css = (await import("../../src/styles.css?raw")).default;

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect([rule.includes("grid-column: 1 / -1"), rule.includes("grid-row: 2")]).toEqual([true, true]);
  });

  it("keeps the rail in the first row, so it never reaches down beside the dock", async () => {
    // Arrange
    const css = (await import("../../src/styles.css?raw")).default;

    // Act
    const rail = /#ws-sidebar\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    const main = /#main-col\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect([rail.includes("grid-row: 1"), main.includes("grid-row: 1")]).toEqual([true, true]);
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

  it("takes the height Emacs told it", () => {
    // Arrange
    document.documentElement.style.setProperty(DOCK_HEIGHT_PROPERTY, "137px");

    // Act / Assert
    expect(toldDockHeight(document)).toBe("137px");
  });

  it("falls back when Emacs never told a height", () => {
    // Arrange / Act / Assert
    expect(toldDockHeight(document)).toBeNull();
  });

  it("sizes the dock from the told property, the fraction only as fallback", async () => {
    // Arrange
    const css = (await import("../../src/styles.css?raw")).default;

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect(rule).toContain("height: var(--gate-dock-height, 18.4vh)");
  });
});
