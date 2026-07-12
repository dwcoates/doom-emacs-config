/**
 * Stylesheet contract tests.
 *
 * The thinking indicator's motion is pure CSS, so its behavior is asserted
 * against the stylesheet source: jsdom neither runs animations nor resolves
 * media queries, and the rotation is the whole point of the indicator.
 */
import { describe, expect, it } from "vitest";

import { CAPPED_CLASSES, EXPANDED_CLASS } from "../src/expand.js";
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
const clearDivider = blockAfter(css, ".clear-divider");
const scrollZone = blockAfter(css, ".scroll-zone {");
const scrollZoneBox = blockAfter(css, ".scroll-zone-box");

/** Whether a `#rrggbb` literal reads as red: its red channel dominates both others. */
function isRed(hex: string): boolean {
  const [r, g, b] = [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16));
  return r > 2 * g && r > 2 * b;
}

/** Whether a `#rrggbb` literal reads as green: its green channel dominates both others. */
function isGreen(hex: string): boolean {
  const [r, g, b] = [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16));
  return g > 1.5 * r && g > 1.5 * b;
}

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

const alarmDot = blockAfter(css, ".spinner.alarm");
const alarmKeyframes = blockAfter(css, "@keyframes gone-alarm");

describe("session-gone alarm dot", () => {
  it("beats the dot between the error red and the thinking orange", () => {
    // Arrange / Act — the gone-alarm keyframes.
    // Assert
    expect(alarmKeyframes).toMatch(/0%,\s*100%\s*{\s*color:\s*var\(--err\)/);
    expect(alarmKeyframes).toMatch(/50%\s*{\s*color:\s*var\(--thinking\)/);
  });

  it("keeps the dot blinking endlessly rather than settling", () => {
    // Arrange / Act — the .spinner.alarm rule.
    // Assert
    expect(alarmDot).toMatch(/animation:\s*gone-alarm\s+[\d.]+s\s+[a-z-]+\s+infinite/);
  });

  it("shows the dot even when no turn was in flight when the session vanished", () => {
    // Arrange / Act — .spinner is opacity 0 until .on; .alarm must not need it.
    // Assert
    expect(alarmDot).toMatch(/opacity:\s*1/);
  });

  it("wins over the quiet .on pulse by being declared after it", () => {
    // Arrange / Act — equal specificity, so source order decides.
    // Assert
    expect(css.indexOf(".spinner.alarm")).toBeGreaterThan(css.indexOf(".spinner.on"));
  });
});

describe("remediation notice", () => {
  it("paints the notice in the remediation yellow", () => {
    // Arrange / Act — the #remediation rule.
    // Assert
    expect(blockAfter(css, "#remediation")).toMatch(/color:\s*var\(--remediation\)/);
  });

  it("defines the yellow token for the light theme", () => {
    // Arrange / Act — the :root palette.
    // Assert
    expect(blockAfter(css, ":root")).toMatch(/--remediation:\s*#[0-9a-f]{6}/i);
  });

  it("defines a brighter yellow token for the dark theme", () => {
    // Arrange / Act — the dark-scheme palette override.
    // Assert
    expect(darkTheme).toMatch(/--remediation:\s*#[0-9a-f]{6}/i);
  });
});

describe("clear divider", () => {
  it("paints the /clear boundary rule with its own token", () => {
    // Arrange / Act — the .clear-divider rule.
    // Assert
    expect(clearDivider).toMatch(/background:\s*var\(--clear-divider\)/);
  });

  it("draws the boundary rule thick enough to read as a break", () => {
    // Arrange
    const height = clearDivider.match(/height:\s*(\d+)px/);
    // Act / Assert
    expect(Number(height?.[1])).toBeGreaterThanOrEqual(3);
  });

  it("sizes the boundary rule to the feed column rather than the window", () => {
    // Arrange / Act — the .clear-divider rule claims no width of its own, so the
    // block fills exactly the bubble column that contains it.
    // Assert
    expect(clearDivider).not.toMatch(/\bwidth:/);
  });

  it("keeps the boundary rule inside the feed column's padding", () => {
    // Arrange / Act — no negative margin bleeds the rule out to the window edges.
    // Assert
    expect(clearDivider).not.toMatch(/margin[^:]*:[^;]*-\d/);
  });

  it("defines a red boundary token for the light theme", () => {
    // Arrange / Act
    const red = isRed(token(lightTheme, "--clear-divider"));
    // Assert
    expect(red).toBe(true);
  });

  it("defines a red boundary token for the dark theme", () => {
    // Arrange / Act
    const red = isRed(token(darkTheme, "--clear-divider"));
    // Assert
    expect(red).toBe(true);
  });
});

describe("final-response border", () => {
  const bubble = blockAfter(css, ".bubble ");
  const finalBubble = blockAfter(css, ".bubble.assistant.final-response");

  it("borders a turn's final response with the final-response token", () => {
    // Arrange / Act — the .bubble.assistant.final-response rule.
    // Assert
    expect(finalBubble).toMatch(/border-color:\s*var\(--final-response\)/);
  });

  it("reserves the border box on every bubble so the green border never reflows the feed", () => {
    // Arrange / Act — the shared .bubble rule lays the 2px out up front, and the
    // final-response rule only recolors it.
    // Assert
    expect(bubble).toMatch(/border:\s*2px\s+solid\s+transparent/);
    expect(finalBubble).not.toMatch(/border:\s/);
  });

  it("defines a green border token for the light theme", () => {
    // Arrange / Act
    const green = isGreen(token(lightTheme, "--final-response"));
    // Assert
    expect(green).toBe(true);
  });

  it("defines a green border token for the dark theme", () => {
    // Arrange / Act
    const green = isGreen(token(darkTheme, "--final-response"));
    // Assert
    expect(green).toBe(true);
  });

  it("brightens the dark-theme border token above the light-theme one", () => {
    // Arrange
    const [dark, light] = [
      token(darkTheme, "--final-response"),
      token(lightTheme, "--final-response"),
    ];
    // Act
    const brighter = luminance(dark) > luminance(light);
    // Assert
    expect(brighter).toBe(true);
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

describe("edge-scroll bars", () => {
  it("insets the left bar, so it abuts the section's left edge", () => {
    // Arrange / Act — the armed-section .scroll-zone rule.
    // Assert
    expect(scrollZone).toMatch(/box-shadow:[^;]*inset\s+2px\s+0\s+0\s+var\(--accent\)/);
  });

  it("insets the right bar, so it abuts the section's right edge", () => {
    // Arrange / Act — the armed-section .scroll-zone rule.
    // Assert
    expect(scrollZone).toMatch(/box-shadow:[^;]*inset\s+-2px\s+0\s+0\s+var\(--accent\)/);
  });

  it("leaves the bars unbounded vertically, so they run the section's full height", () => {
    // Arrange / Act — a vertical offset or spread would shorten the bars.
    // Assert
    expect(scrollZone).not.toMatch(/box-shadow:[^;]*inset\s+-?2px\s+0\s+0\s+[\d.]+px/);
    expect(scrollZone).not.toMatch(/height/);
  });

  it("hands the scroll cursor to the box the wheel turns, not the lit section", () => {
    // Arrange / Act — the armed-box .scroll-zone-box rule.
    // Assert
    expect(scrollZoneBox).toMatch(/cursor:\s*ns-resize/);
    expect(scrollZone).not.toMatch(/cursor/);
  });
});

const expanded = blockAfter(css, `.${EXPANDED_CLASS} {`);
/** Selector list of the rule that puts the click affordance on the capped sections. */
const zoomSelector = css.match(/([^};/]+)\{\s*cursor:\s*zoom-in/)?.[1] ?? "";

describe("click-to-expand", () => {
  it("lifts the height cap of an expanded section", () => {
    // Arrange / Act — the .expanded rule.
    // Assert
    expect(expanded).toMatch(/max-height:\s*none/);
  });

  it("drops the inner scrollbar of an expanded section, which has nothing left to scroll", () => {
    // Arrange / Act — the .expanded rule.
    // Assert
    expect(expanded).toMatch(/overflow-y:\s*visible/);
  });

  it("wins over every cap rule by being declared after them", () => {
    // Arrange — equal specificity (one class each), so source order decides.
    const lastCap = Math.max(...CAPPED_CLASSES.map((c) => css.lastIndexOf(`.${c} {`)));
    // Act / Assert
    expect(css.indexOf(`.${EXPANDED_CLASS} {`)).toBeGreaterThan(lastCap);
  });

  it("beats the cap without reaching for !important", () => {
    // Arrange / Act — the .expanded rule.
    // Assert
    expect(expanded).not.toContain("!important");
  });

  it("offers the click on every capped section the expander knows", () => {
    // Arrange / Act — the cursor rule's selector list.
    // Assert
    for (const cls of CAPPED_CLASSES) expect(zoomSelector).toContain(`.${cls}`);
  });

  it("names the click's next move once the section is open", () => {
    // Arrange / Act — the .expanded rule.
    // Assert
    expect(expanded).toMatch(/cursor:\s*zoom-out/);
  });

  it("keeps the armed box's scroll cursor over the expander's zoom cursor", () => {
    // Arrange / Act — .scroll-zone-box must out-order both zoom cursors at equal specificity.
    // Assert
    expect(css.indexOf(".scroll-zone-box")).toBeGreaterThan(css.indexOf("cursor: zoom-in"));
    expect(css.indexOf(".scroll-zone-box")).toBeGreaterThan(css.indexOf(`.${EXPANDED_CLASS} {`));
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

const toolSpinner = blockAfter(css, ".tool-spinner");
const appearKeyframes = blockAfter(css, "@keyframes tool-run-appear");
const reducedToolSpinner = blockAfter(
  blockAfter(css, "@media (prefers-reduced-motion: reduce)"),
  ".tool-spinner",
);

describe("running-tool spinner", () => {
  it("spins the running arc with the same endless rotation the thinking arc uses", () => {
    // Arrange / Act — the base .tool-spinner rule.
    // Assert
    expect(toolSpinner).toMatch(/thinking-spin\s+[\d.]+s\s+linear\s+infinite/);
  });

  it("holds the arc invisible until the call has run for a second", () => {
    // Arrange / Act — the fade-in's delay is what makes the arc a >1s signal.
    // Assert
    expect(toolSpinner).toMatch(/opacity:\s*0/);
    expect(toolSpinner).toMatch(/tool-run-appear\s+[\d.]+s\s+linear\s+1s\s+forwards/);
  });

  it("leaves the arc visible for the rest of the call rather than fading it back out", () => {
    // Arrange / Act — the tool-run-appear keyframes plus its forwards fill.
    // Assert
    expect(appearKeyframes).toMatch(/to\s*{\s*opacity:\s*1/);
  });

  it("reserves the arc's box up front so the badge does not jump when it appears", () => {
    // Arrange / Act — opacity hides it, display/visibility would reflow it.
    // Assert
    expect(toolSpinner).not.toMatch(/display:\s*none/);
    expect(toolSpinner).not.toMatch(/visibility:/);
  });

  it("slows the running arc under reduced motion while keeping its fade-in", () => {
    // Arrange / Act — per-animation durations, rotation first, fade-in second.
    // Assert
    expect(reducedToolSpinner).toMatch(/animation-duration:\s*[\d.]+s,\s*[\d.]+s/);
    expect(reducedToolSpinner).not.toMatch(/animation:/);
  });
});

const runBadge = blockAfter(css, ".badge.run");

describe("running badge", () => {
  it("tints the badge with the thinking orange rather than the accent", () => {
    // Arrange / Act — the .badge.run rule.
    // Assert
    expect(runBadge).toMatch(/color:\s*var\(--thinking\)/);
    expect(runBadge).not.toMatch(/color:\s*var\(--accent\)/);
  });

  it("keeps the badge's orange the same token the thinking spinner spins in", () => {
    // Arrange
    const badgeColor = runBadge.match(/color:\s*var\((--[a-z-]+)\)/)?.[1];
    // Act
    const spinnerColor = spinner.match(/border-top-color:\s*var\((--[a-z-]+)\)/)?.[1];
    // Assert
    expect(badgeColor).toBe(spinnerColor);
  });

  it("keeps the light-theme thinking token orange so the badge reads orange", () => {
    // Arrange
    const orange = token(lightTheme, "--thinking");
    // Act
    const [r, g, b] = [1, 3, 5].map((i) => parseInt(orange.slice(i, i + 2), 16));
    // Assert
    expect(r > g && g > b).toBe(true);
  });

  it("keeps the dark-theme thinking token orange so the badge reads orange", () => {
    // Arrange
    const orange = token(darkTheme, "--thinking");
    // Act
    const [r, g, b] = [1, 3, 5].map((i) => parseInt(orange.slice(i, i + 2), 16));
    // Assert
    expect(r > g && g > b).toBe(true);
  });
});
