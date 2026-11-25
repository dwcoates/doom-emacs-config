// @vitest-environment jsdom
/**
 * THE REAL STYLESHEET IN A TEST'S DOCUMENT (`test/stylesheet.ts`): the mounts
 * that install it and take it down, and the cascade probes over it. The
 * readers of its text are `rules/stylesheet-text.ts`.
 */
import { describe, expect, it } from "vitest";
import { cascadedValue, cascadedValueIn, inDocument, inRail } from "./stylesheet.js";

describe("inDocument", () => {
  it("mounts the drawn element in the document with the real stylesheet", () => {
    // Arrange: a dot carrying a tone, which only the real stylesheet paints.
    const dot = document.createElement("span");
    dot.className = "st tone-green";

    // Act
    const seen = inDocument(dot, (drawn) => ({ same: drawn === dot, attached: document.body.contains(dot), paint: cascadedValue(dot, "color") }));

    // Assert
    expect(seen).toEqual({ same: true, attached: true, paint: "var(--ok)" });
  });

  it("takes the element and the stylesheet down once the check returns", () => {
    // Arrange
    const el = document.createElement("span");
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    inDocument(el, () => undefined);

    // Assert
    expect([el.isConnected, document.head.querySelectorAll("style").length]).toEqual([false, sheets]);
  });

  it("takes the element and the stylesheet down when the check throws", () => {
    // Arrange
    const el = document.createElement("span");
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    const act = (): void =>
      inDocument(el, () => {
        throw new Error("boom");
      });

    // Assert
    expect(act).toThrow("boom");
    expect([el.isConnected, document.head.querySelectorAll("style").length]).toEqual([false, sheets]);
  });
});

describe("cascadedValueIn", () => {
  it("answers the declaration the cascade hands the child inside the parent", () => {
    // Arrange / Act: a verdict's chip, which only the parent-scoped rule rounds.
    const radius = cascadedValueIn("q-verdict", "badge q-chip", "border-radius");

    // Assert
    expect(radius).toBe("var(--tab-radius)");
  });

  it("takes the pair and the stylesheet down once it answers", () => {
    // Arrange
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    cascadedValueIn("q-verdict", "badge q-chip", "color");

    // Assert
    expect([document.body.querySelector(".q-verdict"), document.head.querySelectorAll("style").length]).toEqual([null, sheets]);
  });
});

describe("inRail", () => {
  it("mounts the drawn element in the sidebar rail with the real stylesheet", () => {
    // Arrange: a dot carrying a tone, which only the real stylesheet paints.
    const dot = document.createElement("span");
    dot.className = "st tone-green";

    // Act
    const seen = inRail(dot, (rail) => ({
      id: rail.id,
      attached: document.body.contains(dot),
      paint: cascadedValue(dot, "color"),
    }));

    // Assert
    expect(seen).toEqual({ id: "ws-sidebar", attached: true, paint: "var(--ok)" });
  });

  it("takes the rail and the stylesheet down once the check returns", () => {
    // Arrange
    const dot = document.createElement("span");
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    inRail(dot, () => undefined);

    // Assert
    expect([document.getElementById("ws-sidebar"), document.head.querySelectorAll("style").length]).toEqual([null, sheets]);
  });

  it("takes the rail and the stylesheet down when the check throws", () => {
    // Arrange
    const sheets = document.head.querySelectorAll("style").length;

    // Act
    const act = (): void =>
      inRail(document.createElement("span"), () => {
        throw new Error("boom");
      });

    // Assert
    expect(act).toThrow("boom");
    expect([document.getElementById("ws-sidebar"), document.head.querySelectorAll("style").length]).toEqual([null, sheets]);
  });
});
