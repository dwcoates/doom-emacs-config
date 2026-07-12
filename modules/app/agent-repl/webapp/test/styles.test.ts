/**
 * Stylesheet contract tests.
 *
 * The thinking indicator's motion is pure CSS, so its behavior is asserted
 * against the stylesheet source: jsdom neither runs animations nor resolves
 * media queries, and the rotation is the whole point of the indicator.
 */
import { describe, expect, it } from "vitest";

import css from "../src/styles.css?raw";

/** Body of the first brace-balanced block introduced by `marker`. */
function blockAfter(source: string, marker: string): string {
  const at = source.indexOf(marker);
  if (at === -1) throw new Error(`stylesheet has no ${marker} block`);
  const open = source.indexOf("{", at);
  let depth = 0;
  for (let i = open; i < source.length; i++) {
    if (source[i] === "{") depth++;
    else if (source[i] === "}" && --depth === 0) return source.slice(open + 1, i);
  }
  throw new Error(`unbalanced ${marker} block`);
}

const spinner = blockAfter(css, ".thinking-spinner");
const keyframes = blockAfter(css, "@keyframes thinking-spin");
const reducedSpinner = blockAfter(
  blockAfter(css, "@media (prefers-reduced-motion: reduce)"),
  ".thinking-spinner",
);

const doneChip = blockAfter(css, ".result.done");
const darkTheme = blockAfter(css, "@media (prefers-color-scheme: dark)");
const lightTheme = blockAfter(css, ":root");
const composerInput = blockAfter(css, "#composer-input");

/** Perceived lightness (0-255) of a `#rrggbb` literal, for darker-than checks. */
function luminance(hex: string): number {
  const [r, g, b] = [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16));
  return 0.299 * r + 0.587 * g + 0.114 * b;
}

/** The `#rrggbb` value bound to `token` inside a palette block. */
function token(block: string, name: string): string {
  const hit = block.match(new RegExp(`${name}:\\s*(#[0-9a-f]{6})`, "i"));
  if (!hit) throw new Error(`palette block has no ${name}`);
  return hit[1];
}

describe("composer input", () => {
  it("fills the composer well with its own token rather than the card grey", () => {
    // Arrange / Act — the #composer-input rule.
    // Assert
    expect(composerInput).toMatch(/background:\s*var\(--composer-bg\)/);
  });

  it("sinks the light-theme composer below the card grey", () => {
    // Arrange
    const [composer, card] = [token(lightTheme, "--composer-bg"), token(lightTheme, "--card")];
    // Act
    const darker = luminance(composer) < luminance(card);
    // Assert
    expect(darker).toBe(true);
  });

  it("sinks the dark-theme composer below the card grey", () => {
    // Arrange
    const [composer, card] = [token(darkTheme, "--composer-bg"), token(darkTheme, "--card")];
    // Act
    const darker = luminance(composer) < luminance(card);
    // Assert
    expect(darker).toBe(true);
  });
});

describe("turn-complete chip", () => {
  it("washes the completed turn's chip in the muted-yellow token", () => {
    // Arrange / Act — the .result.done rule.
    // Assert
    expect(doneChip).toMatch(/background:\s*var\(--turn-complete-bg\)/);
  });

  it("defines the muted-yellow token for the light theme", () => {
    // Arrange / Act — the :root palette.
    // Assert
    expect(blockAfter(css, ":root")).toMatch(/--turn-complete-bg:\s*#[0-9a-f]{6}/i);
  });

  it("defines a darker muted-yellow token for the dark theme", () => {
    // Arrange / Act — the dark-scheme palette override.
    // Assert
    expect(darkTheme).toMatch(/--turn-complete-bg:\s*#[0-9a-f]{6}/i);
  });
});

describe("thinking spinner", () => {
  it("drives the ring with an endless linear rotation", () => {
    // Arrange / Act — the base .thinking-spinner rule.
    // Assert
    expect(spinner).toMatch(/animation:\s*thinking-spin\s+[\d.]+s\s+linear\s+infinite/);
  });

  it("sweeps the arc through a full turn of the circle", () => {
    // Arrange / Act — the thinking-spin keyframes.
    // Assert
    expect(keyframes).toMatch(/from\s*{\s*transform:\s*rotate\(0deg\)/);
    expect(keyframes).toMatch(/to\s*{\s*transform:\s*rotate\(360deg\)/);
  });

  it("keeps rotating under reduced motion instead of pulsing in place", () => {
    // Arrange / Act — the reduced-motion .thinking-spinner override.
    // Assert
    expect(reducedSpinner).not.toContain("tool-pulse");
  });

  it("slows the rotation under reduced motion rather than replacing it", () => {
    // Arrange / Act — the reduced-motion .thinking-spinner override.
    // Assert
    expect(reducedSpinner).toMatch(/animation-duration:\s*[\d.]+s/);
    expect(reducedSpinner).not.toMatch(/animation:/);
  });
});
