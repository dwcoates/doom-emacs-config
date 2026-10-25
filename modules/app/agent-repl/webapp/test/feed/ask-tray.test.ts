// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  ASK_SLOT_CLASS,
  DOCKED_ASK_CLASS,
  DOCKED_ASK_ROW_CLASS,
  installAskTray,
} from "../../src/feed/ask-tray.js";
import type { CardDock } from "../../src/feed/card-dock.js";
import { shellElements } from "../../src/shell.js";
import { shellHTML } from "../shell-html.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

// Imported at collection, not inside a test, so the transform never counts
// against a test's timeout.
import css from "../../src/styles.css?raw";

/** MutationObserver delivers on a microtask; this lets it run. */
async function settle(): Promise<void> {
  await Promise.resolve();
  await Promise.resolve();
}

/** The real page shell, from index.html, with the feed's body. */
function page(): { feed: HTMLElement; body: HTMLElement; holdTray: HTMLElement; askTray: HTMLElement } {
  document.body.innerHTML = shellHTML();
  const shell = shellElements(document);
  const body = document.createElement("div");
  body.className = "feed-body";
  shell.feed.append(body);
  return { feed: shell.feed, body, holdTray: shell.holdTray, askTray: shell.askTray };
}

/** A root row holding an ask card of KIND in STATE, as asks/*.ts draw it. */
function askRow(kind: "question" | "permission", state: string): { row: HTMLElement; card: HTMLElement } {
  const row = document.createElement("article");
  row.className = "feed-item";
  const card = document.createElement("div");
  card.className = `tool-card ask-card ${kind} ${state === "open" ? "pending" : "resolved"}`;
  card.setAttribute("data-state", state);
  row.append(card);
  return { row, card };
}

let installed: CardDock | null = null;
afterEach(() => {
  installed?.dispose();
  installed = null;
});

describe("the ask tray's place in the page", () => {
  it("sits after the hold tray, inside the feed's scroll zone", () => {
    // Arrange / Act
    const { holdTray, askTray } = page();

    // Assert
    expect([askTray.previousElementSibling, askTray.parentElement?.id]).toEqual([holdTray, "feed-scroll"]);
  });

  it("ships hidden", () => {
    // Arrange / Act
    const { askTray } = page();

    // Assert
    expect(askTray.hidden).toBe(true);
  });
});

describe("installAskTray", () => {
  it.each(["question", "permission"] as const)("draws an open %s below the held prompts", async (kind) => {
    // Arrange
    const { feed, body, askTray } = page();
    installed = installAskTray(feed, askTray);
    const { card } = askRow(kind, "open");

    // Act
    body.append(card.parentElement as HTMLElement);
    await settle();

    // Assert
    expect([card.parentElement?.parentElement, askTray.hidden, card.classList.contains(DOCKED_ASK_CLASS)]).toEqual([
      askTray,
      false,
      true,
    ]);
  });

  it("draws each docked ask in a row wrapper of its own", () => {
    // Arrange
    const { feed, body, askTray } = page();
    const { row, card } = askRow("permission", "open");
    body.append(row);

    // Act
    installed = installAskTray(feed, askTray);

    // Assert
    expect(card.parentElement?.className).toBe(`feed-item ${ASK_SLOT_CLASS}`);
  });

  it("hides the open ask's home row in the feed", () => {
    // Arrange
    const { feed, body, askTray } = page();
    const { row } = askRow("question", "open");
    body.append(row);

    // Act
    installed = installAskTray(feed, askTray);

    // Assert
    expect(row.classList.contains(DOCKED_ASK_ROW_CLASS)).toBe(true);
  });

  it.each(["answered", "expired", "abandoned"])("leaves a %s ask in the feed", async (state) => {
    // Arrange
    const { feed, body, askTray } = page();
    installed = installAskTray(feed, askTray);
    const { row, card } = askRow("question", state);

    // Act
    body.append(row);
    await settle();

    // Assert
    expect([card.parentElement, askTray.hidden]).toEqual([row, true]);
  });

  it("puts the answered redraw back in the feed and empties the tray", async () => {
    // Arrange
    const { feed, body, askTray } = page();
    const { row, card } = askRow("permission", "open");
    body.append(row);
    installed = installAskTray(feed, askTray);
    const answered = askRow("permission", "allowOnce").card;

    // Act: the feed view removes the card it drew and draws the answer.
    card.remove();
    row.prepend(answered);
    await settle();

    // Assert
    expect([answered.parentElement, row.classList.contains(DOCKED_ASK_ROW_CLASS), askTray.hidden, askTray.childElementCount]).toEqual([
      row,
      false,
      true,
      0,
    ]);
  });

  it("records the docked ask's kind", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { feed, body, askTray } = page();
    body.append(askRow("permission", "open").row);

    // Act
    installed = installAskTray(feed, askTray);

    // Assert
    expect((await forwardedRecord(capture, "feed.ask-tray.docked")).context).toMatchObject({ kind: "permission" });
  });
});

describe("the ask tray's styles", () => {
  /** The declarations of the first rule whose selector list is exactly SELECTORS. */
  const ruleOf = (selectors: string): string =>
    new RegExp(`(^|\\n)${selectors.replace(/[.*+?^${}()|[\]\\#]/g, "\\$&")}\\s*\\{[^}]*\\}`).exec(css)?.[0] ?? "";

  it("lays the tray out in the held section's own padding and text scale", () => {
    // Arrange / Act
    const rule = ruleOf("#hold-tray,\n#ask-tray");

    // Assert
    expect([rule.includes("padding: 0.5rem 1rem 0;"), rule.includes("font-size: calc(1em * var(--feed-text-scale));")]).toEqual([
      true,
      true,
    ]);
  });

  it("spaces the docked asks by the feed's own row gap", () => {
    // Arrange / Act
    const rule = ruleOf(".feed-body,\n.feed-rows,\n#ask-tray");

    // Assert
    expect(rule.includes("gap: 0.5rem;")).toBe(true);
  });

  it("takes no slot while hidden", () => {
    // Arrange / Act / Assert
    expect(ruleOf("#ask-tray[hidden]")).toContain("display: none;");
  });

  it("gives the docked ask's home row no slot in the feed", () => {
    // Arrange / Act / Assert
    expect(ruleOf(".feed-item.ask-docked-row")).toContain("display: none;");
  });
});
