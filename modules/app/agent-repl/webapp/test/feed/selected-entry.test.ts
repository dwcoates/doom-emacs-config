// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  REVEAL_ATTRIBUTE,
  SELECTED_ENTRY_CLASS,
  SELECTED_ROW_ATTRIBUTE,
  cardOf,
  syncSelectedEntry,
} from "../../src/feed/selected-entry.js";

/** A row wrapper holding one card, as feed-view draws it. */
function rowWithCard(): { row: HTMLElement; card: HTMLElement } {
  const row = document.createElement("article");
  row.className = "feed-item";
  const card = document.createElement("div");
  card.className = "tool-card";
  row.append(card);
  return { row, card };
}

describe("cardOf", () => {
  it("answers the row's first element child", () => {
    // Arrange
    const { row, card } = rowWithCard();
    // Act, Assert
    expect(cardOf(row)).toBe(card);
  });

  it("answers null for a row that draws nothing", () => {
    // Arrange
    const row = document.createElement("article");
    // Act, Assert
    expect(cardOf(row)).toBeNull();
  });
});

describe("syncSelectedEntry", () => {
  it("marks the card of a row a jump landed on", () => {
    // Arrange
    const { row, card } = rowWithCard();
    row.setAttribute(REVEAL_ATTRIBUTE, "true");
    // Act
    syncSelectedEntry(row);
    // Assert
    expect(card.classList.contains(SELECTED_ENTRY_CLASS)).toBe(true);
  });

  it("marks the card of the selected row", () => {
    // Arrange
    const { row, card } = rowWithCard();
    row.setAttribute(SELECTED_ROW_ATTRIBUTE, "response");
    // Act
    syncSelectedEntry(row);
    // Assert
    expect(card.classList.contains(SELECTED_ENTRY_CLASS)).toBe(true);
  });

  it("never marks the row wrapper itself", () => {
    // Arrange
    const { row } = rowWithCard();
    row.setAttribute(REVEAL_ATTRIBUTE, "true");
    // Act
    syncSelectedEntry(row);
    // Assert
    expect(row.classList.contains(SELECTED_ENTRY_CLASS)).toBe(false);
  });

  it("unmarks the card once no selecting fact stands", () => {
    // Arrange
    const { row, card } = rowWithCard();
    row.setAttribute(REVEAL_ATTRIBUTE, "true");
    syncSelectedEntry(row);
    row.removeAttribute(REVEAL_ATTRIBUTE);
    // Act
    syncSelectedEntry(row);
    // Assert
    expect(card.classList.contains(SELECTED_ENTRY_CLASS)).toBe(false);
  });

  it("keeps the selection's mark when a jump's landing clears", () => {
    // Arrange — both acts selected the same row; the reveal times out first.
    const { row, card } = rowWithCard();
    row.setAttribute(SELECTED_ROW_ATTRIBUTE, "response");
    row.setAttribute(REVEAL_ATTRIBUTE, "true");
    syncSelectedEntry(row);
    row.removeAttribute(REVEAL_ATTRIBUTE);
    // Act
    syncSelectedEntry(row);
    // Assert
    expect(card.classList.contains(SELECTED_ENTRY_CLASS)).toBe(true);
  });

  it("tolerates a selected row that draws no card", () => {
    // Arrange
    const row = document.createElement("article");
    row.setAttribute(REVEAL_ATTRIBUTE, "true");
    // Act, Assert
    expect(() => syncSelectedEntry(row)).not.toThrow();
  });
});
