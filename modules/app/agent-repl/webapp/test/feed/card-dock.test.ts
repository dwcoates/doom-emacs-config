// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { installCardDock, type CardDock, type CardDockSpec } from "../../src/feed/card-dock.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

// The docks built on the helper, read as text so the suite can hold them to it.
import gateDockSource from "../../src/feed/gate-dock.ts?raw";
import askTraySource from "../../src/feed/ask-tray.ts?raw";

/** MutationObserver delivers on a microtask; this lets it run. */
async function settle(): Promise<void> {
  await Promise.resolve();
  await Promise.resolve();
}

const CARD_CLASS = "docked-card";
const ROW_CLASS = "docked-row";

/** A spec docking `.card[data-state="open"]`, recording each docking in DOCKED. */
function spec(docked: HTMLElement[], slot?: CardDockSpec["slot"]): CardDockSpec {
  return {
    operation: "test.card-dock",
    noun: "test card",
    standing: '.card[data-state="open"]',
    stateAttributes: ["data-state"],
    cardClass: CARD_CLASS,
    rowClass: ROW_CLASS,
    slot,
    docked: (card) => docked.push(card),
  };
}

/** A root feed and a dock, as the page lays them out. */
function page(): { feed: HTMLElement; body: HTMLElement; dock: HTMLElement } {
  document.body.innerHTML = "";
  const feed = document.createElement("main");
  feed.setAttribute("data-feed", "root");
  const body = document.createElement("div");
  body.className = "feed-body";
  feed.append(body);
  const dock = document.createElement("section");
  document.body.append(feed, dock);
  return { feed, body, dock };
}

/** A root row holding a card in STATE. */
function cardRow(state: string, text = ""): { row: HTMLElement; card: HTMLElement } {
  const row = document.createElement("article");
  row.className = "feed-item";
  const card = document.createElement("div");
  card.className = "card";
  card.setAttribute("data-state", state);
  card.textContent = text;
  row.append(card);
  return { row, card };
}

/** A slot wrapping each docked card. */
function wrap(): HTMLElement {
  const slot = document.createElement("div");
  slot.className = "slot";
  return slot;
}

let installed: CardDock | null = null;
afterEach(() => {
  installed?.dispose();
  installed = null;
});

describe("installCardDock", () => {
  it("docks every standing root card, in its rows' feed order", async () => {
    // Arrange
    const { feed, body, dock } = page();
    installed = installCardDock(feed, dock, spec([]));
    const first = cardRow("open", "first");
    const second = cardRow("open", "second");

    // Act: the later row lands first.
    body.append(second.row);
    await settle();
    body.prepend(first.row);
    await settle();

    // Assert
    expect([...dock.children]).toEqual([first.card, second.card]);
  });

  it("leaves a card that is not standing in its row", async () => {
    // Arrange
    const { feed, body, dock } = page();
    installed = installCardDock(feed, dock, spec([]));
    const { row, card } = cardRow("answered");

    // Act
    body.append(row);
    await settle();

    // Assert
    expect([card.parentElement, dock.hidden]).toEqual([row, true]);
  });

  it("marks the docked card and its home row", () => {
    // Arrange
    const { feed, body, dock } = page();
    const { row, card } = cardRow("open");
    body.append(row);

    // Act
    installed = installCardDock(feed, dock, spec([]));

    // Assert
    expect([card.classList.contains(CARD_CLASS), row.classList.contains(ROW_CLASS), dock.hidden]).toEqual([
      true,
      true,
      false,
    ]);
  });

  it("draws the card inside the slot the spec makes", () => {
    // Arrange
    const { feed, body, dock } = page();
    const { row, card } = cardRow("open");
    body.append(row);

    // Act
    installed = installCardDock(feed, dock, spec([], wrap));

    // Assert
    expect([card.parentElement?.className, card.parentElement?.parentElement]).toEqual(["slot", dock]);
  });

  it("hands each docked card to the spec's docking record once", async () => {
    // Arrange
    const docked: HTMLElement[] = [];
    const { feed, body, dock } = page();
    const { row, card } = cardRow("open");
    body.append(row);
    installed = installCardDock(feed, dock, spec(docked));

    // Act: an unrelated row lands.
    body.append(document.createElement("article"));
    await settle();

    // Assert
    expect(docked).toEqual([card]);
  });

  it("drops a card the feed removed on a redraw, and docks the redrawn one", async () => {
    // Arrange
    const { feed, body, dock } = page();
    const { row, card } = cardRow("open", "stale");
    body.append(row);
    installed = installCardDock(feed, dock, spec([], wrap));
    const fresh = cardRow("open", "fresh").card;

    // Act: the feed view removes the card it holds and draws the fresh one.
    card.remove();
    row.prepend(fresh);
    await settle();

    // Assert
    expect([dock.childElementCount, fresh.parentElement?.parentElement, card.classList.contains(CARD_CLASS)]).toEqual([
      1,
      dock,
      false,
    ]);
  });

  it("shows the home row again when its redraw is no longer standing", async () => {
    // Arrange
    const { feed, body, dock } = page();
    const { row, card } = cardRow("open");
    body.append(row);
    installed = installCardDock(feed, dock, spec([], wrap));
    const answered = cardRow("answered").card;

    // Act
    card.remove();
    row.prepend(answered);
    await settle();

    // Assert
    expect([answered.parentElement, row.classList.contains(ROW_CLASS), dock.hidden, dock.childElementCount]).toEqual([
      row,
      false,
      true,
      0,
    ]);
  });

  it("takes a retired row's card out of the dock", async () => {
    // Arrange
    const { feed, body, dock } = page();
    const { row, card } = cardRow("open");
    body.append(row);
    installed = installCardDock(feed, dock, spec([], wrap));

    // Act
    row.remove();
    await settle();

    // Assert
    expect([card.isConnected, dock.childElementCount, dock.hidden]).toEqual([false, 0, true]);
  });

  it("returns a card resolved in place to where it sat in its row", async () => {
    // Arrange
    const { feed, body, dock } = page();
    const { row, card } = cardRow("open");
    const after = document.createElement("span");
    row.append(after);
    body.append(row);
    installed = installCardDock(feed, dock, spec([], wrap));

    // Act
    card.setAttribute("data-state", "answered");
    await settle();

    // Assert
    expect([card.parentElement, card.nextSibling, dock.childElementCount]).toEqual([row, after, 0]);
  });

  it("never docks a card on a sub-feed", async () => {
    // Arrange
    const { feed, body, dock } = page();
    installed = installCardDock(feed, dock, spec([]));
    const sub = document.createElement("div");
    sub.setAttribute("data-feed", "sub");
    const { row, card } = cardRow("open");
    sub.append(row);

    // Act
    body.append(sub);
    await settle();

    // Assert
    expect(card.parentElement).toBe(row);
  });

  it("returns every docked card to its row when disposed", () => {
    // Arrange
    const { feed, body, dock } = page();
    const first = cardRow("open");
    const second = cardRow("open");
    body.append(first.row, second.row);
    const dockOf = installCardDock(feed, dock, spec([], wrap));

    // Act
    dockOf.dispose();

    // Assert
    expect([first.card.parentElement, second.card.parentElement, dock.childElementCount, dock.hidden]).toEqual([
      first.row,
      second.row,
      0,
      true,
    ]);
  });

  it("records each undocking under the spec's operation, with why", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { feed, body, dock } = page();
    const { row } = cardRow("open");
    body.append(row);
    installed = installCardDock(feed, dock, spec([]));

    // Act
    row.remove();
    await settle();

    // Assert
    expect((await forwardedRecord(capture, "test.card-dock.undocked")).context).toMatchObject({ why: "retired" });
  });
});

describe("the docks built on installCardDock", () => {
  it.each([
    ["gate-dock.ts", gateDockSource],
    ["ask-tray.ts", askTraySource],
  ])("%s docks through the shared helper and moves no card itself", (_name, source) => {
    // Arrange / Act
    const usesHelper = source.includes("installCardDock(");
    const movesByHand = /\.(replaceChildren|insertBefore)\(/.test(source) || /pageMutationObserver/.test(source);

    // Assert
    expect([usesHelper, movesByHand]).toEqual([true, false]);
  });
});
