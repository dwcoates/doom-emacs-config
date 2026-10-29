// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  DOCK_HEIGHT_PROPERTY,
  toldDockHeight,
  DOCKED_CARD_CLASS,
  DOCKED_ROW_CLASS,
  installGateDock,
  installPageGate,
  WEBHOST_GATE_CLASS,
  type GateDock,
} from "../../src/feed/gate-dock.js";
import { shellElements } from "../../src/shell.js";
import { shellHTML } from "../shell-html.js";

// Imported at collection, not inside a test, so the transform never counts
// against a test's timeout.
import css from "../../src/styles.css?raw";

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
  document.documentElement.classList.remove(WEBHOST_GATE_CLASS);
});

/** The feed as feed-view.ts builds it: every row inside one body. */
function bodiedFeed(feed: HTMLElement, ...rows: HTMLElement[]): HTMLElement {
  const body = document.createElement("div");
  body.className = "feed-body";
  body.append(...rows);
  feed.append(body);
  return body;
}

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

  it("spans every column of the page's second row", () => {
    // Arrange

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect([rule.includes("grid-column: 1 / -1"), rule.includes("grid-row: 2")]).toEqual([true, true]);
  });

  it("keeps the rail in the first row, so it never reaches down beside the dock", () => {
    // Arrange

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

  it("floors the dock at exactly the told property (the fraction only as fallback)", () => {
    // Arrange

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect(rule).toMatch(/(?<![-\w])min-height: var\(--gate-dock-height, 18\.4vh\)/);
  });

  it("sets no fixed or maximum height, so a taller gate grows the dock (owner request, 2026-10-29)", () => {
    // Arrange

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect(rule).not.toMatch(/(?<![-\w])(?:max-)?height:/);
  });

  it("never scrolls the dock", () => {
    // Arrange

    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";

    // Assert
    expect(rule).not.toMatch(/overflow/);
  });
});

describe("the gate's background (owner request, 2026-10-28)", () => {
  it.each([
    ["the docked card", /#gate-dock > \.cold-gate\.cold-gate-docked[^{]*\{[^}]*\}/],
    ["the webhost's card", /#webhost-gate > \.cold-gate\.cold-gate-docked[^{]*\{[^}]*\}/],
    ["the undocked card", /\n\.cold-gate\s*\{[^}]*\}/],
  ])("paints %s the feed's own ground, never a told color or the revival yellow", (_name, selector) => {
    // Arrange
    // Act
    const rule = selector.exec(css)?.[0] ?? "";
    // Assert
    expect([rule.includes("background: var(--bg);"), rule.includes("--input-bg"), rule.includes("revival-gate-bg")]).toEqual([true, false, false]);
  });

  it.each([
    ["the page's dock", /#gate-dock > \.cold-gate\.cold-gate-docked[^{]*\{[^}]*\}/],
    ["the webhost's gate", /#webhost-gate > \.cold-gate\.cold-gate-docked[^{]*\{[^}]*\}/],
  ])("frames the docked gate on every side, not only above it, in %s", (_name, selector) => {
    // Arrange
    // Act
    const rule = selector.exec(css)?.[0] ?? "";
    // Assert
    expect([rule.includes("border-width: 1px;"), rule.includes("border-width: 1px 0 0")]).toEqual([true, false]);
  });

  it("lifts the docked gate off the page's last row so its bottom edge is whole", () => {
    // Arrange
    // Act
    const rule = /#gate-dock\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    // Assert
    expect(rule.includes("padding-bottom: 1px;")).toBe(true);
  });

  it("is told by no property any more", () => {
    // Arrange / Act / Assert
    expect(css.includes("--input-bg")).toBe(false);
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
