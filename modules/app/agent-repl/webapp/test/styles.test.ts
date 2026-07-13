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

/** The red, green and blue channels of a `#rrggbb` literal, each 0-255. */
function channels(hex: string): [number, number, number] {
  const [r, g, b] = [1, 3, 5].map((i) => parseInt(hex.slice(i, i + 2), 16));
  return [r, g, b];
}

/** Whether a `#rrggbb` literal reads as red: its red channel dominates both others. */
function isRed(hex: string): boolean {
  const [r, g, b] = channels(hex);
  return r > 2 * g && r > 2 * b;
}

/** Whether a `#rrggbb` literal reads as green: its green channel dominates both others. */
function isGreen(hex: string): boolean {
  const [r, g, b] = channels(hex);
  return g > 1.5 * r && g > 1.5 * b;
}

/**
 * Hue (degrees, 0-360) of a `#rrggbb` literal. Hue survives the wash: a pale
 * tint and a deep one share it, where channel-dominance checks do not.
 */
function hue(hex: string): number {
  const [r, g, b] = channels(hex).map((c) => c / 255);
  const [max, min] = [Math.max(r, g, b), Math.min(r, g, b)];
  const delta = max - min;
  if (delta === 0) return 0;
  const raw =
    max === r
      ? ((g - b) / delta) % 6
      : max === g
        ? (b - r) / delta + 2
        : (r - g) / delta + 4;
  return (raw * 60 + 360) % 360;
}

/**
 * Whether a `#rrggbb` literal reads as turquoise: cyan-green, between teal and
 * aqua. BOTH special card washes live in this band on purpose — the skill
 * handoff and the spawned subagent — so hue alone never tells them apart. What
 * separates them is depth, which the "subagent card" suite pins.
 */
function isTurquoise(hex: string): boolean {
  const h = hue(hex);
  return h >= 160 && h <= 195;
}

/** Perceived lightness (0-255) of a `#rrggbb` literal, for darker-than checks. */
function luminance(hex: string): number {
  const [r, g, b] = channels(hex);
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

describe("working-frontier pulse", () => {
  const pulsing = blockAfter(css, ".bubble.assistant.pulsing {");
  const breathe = blockAfter(css, "@keyframes response-breathe");
  const reducedPulse = blockAfter(
    blockAfter(css, "@media (prefers-reduced-motion: reduce)"),
    ".bubble.assistant.pulsing",
  );
  /** Seconds the frontier bubble takes to complete one breath. */
  const period = Number(pulsing.match(/response-breathe\s+([\d.]+)s/)?.[1]);

  it("breathes the frontier bubble on an endless loop", () => {
    // Arrange / Act — the .bubble.assistant.pulsing rule.
    // Assert
    expect(pulsing).toMatch(/animation:\s*response-breathe\s+[\d.]+s\s+ease-in-out\s+infinite/);
  });

  it("holds the breath slower than every ticking indicator in the app", () => {
    // Arrange / Act — the spinners and the cursor all tick in around a second.
    // Assert
    expect(period).toBeGreaterThanOrEqual(3);
  });

  it("breathes toward the assistant-pulse token rather than to a color of its own", () => {
    // Arrange / Act — the @keyframes response-breathe midpoint.
    // Assert
    expect(breathe).toMatch(/background:\s*var\(--assistant-pulse\)/);
  });

  it("keeps the light-theme breath inside the assistant bubble's own hue", () => {
    // Arrange
    const [wash, pulse] = [token(lightTheme, "--assistant"), token(lightTheme, "--assistant-pulse")];
    // Act
    const drift = Math.abs(hue(pulse) - hue(wash));
    // Assert
    expect(drift).toBeLessThan(10);
  });

  it("keeps the dark-theme breath inside the assistant bubble's own hue", () => {
    // Arrange
    const [wash, pulse] = [token(darkTheme, "--assistant"), token(darkTheme, "--assistant-pulse")];
    // Act
    const drift = Math.abs(hue(pulse) - hue(wash));
    // Assert
    expect(drift).toBeLessThan(10);
  });

  it("deepens the light-theme wash at the top of the breath", () => {
    // Arrange
    const [wash, pulse] = [token(lightTheme, "--assistant"), token(lightTheme, "--assistant-pulse")];
    // Act
    const deeper = luminance(pulse) < luminance(wash);
    // Assert
    expect(deeper).toBe(true);
  });

  it("lifts the dark-theme wash at the top of the breath, where deepening would vanish", () => {
    // Arrange
    const [wash, pulse] = [token(darkTheme, "--assistant"), token(darkTheme, "--assistant-pulse")];
    // Act
    const lifted = luminance(pulse) > luminance(wash);
    // Assert
    expect(lifted).toBe(true);
  });

  it("keeps the light-theme breath shallow enough to read under prose", () => {
    // Arrange
    const [wash, pulse] = [token(lightTheme, "--assistant"), token(lightTheme, "--assistant-pulse")];
    // Act
    const depth = Math.abs(luminance(pulse) - luminance(wash));
    // Assert
    expect(depth).toBeLessThan(30);
  });

  it("keeps the dark-theme breath shallow enough to read under prose", () => {
    // Arrange
    const [wash, pulse] = [token(darkTheme, "--assistant"), token(darkTheme, "--assistant-pulse")];
    // Act
    const depth = Math.abs(luminance(pulse) - luminance(wash));
    // Assert
    expect(depth).toBeLessThan(30);
  });

  it("drops the breath entirely under reduced motion, since it is a hint and not the signal", () => {
    // Arrange / Act — the reduced-motion .bubble.assistant.pulsing override.
    // Assert
    expect(reducedPulse).toMatch(/animation:\s*none/);
  });
});

describe("skill-launch card", () => {
  const skillCard = blockAfter(css, ".tool-card.tool-skill");

  it("washes the skill-launch card in the skill token", () => {
    // Arrange / Act — the .tool-card.tool-skill rule.
    // Assert
    expect(skillCard).toMatch(/background:\s*var\(--skill-bg\)/);
  });

  it("recolors the card's existing border rather than adding a second one", () => {
    // Arrange / Act — .tool-card already lays out the 1px border.
    // Assert
    expect(skillCard).toMatch(/border-color:\s*var\(--skill-border\)/);
    expect(skillCard).not.toMatch(/border:\s/);
  });

  it("defines a turquoise wash token for the light theme", () => {
    // Arrange / Act
    const turquoise = isTurquoise(token(lightTheme, "--skill-bg"));
    // Assert
    expect(turquoise).toBe(true);
  });

  it("defines a turquoise wash token for the dark theme", () => {
    // Arrange / Act
    const turquoise = isTurquoise(token(darkTheme, "--skill-bg"));
    // Assert
    expect(turquoise).toBe(true);
  });

  it("darkens the dark-theme wash below the light-theme one, as every other wash does", () => {
    // Arrange
    const [dark, light] = [token(darkTheme, "--skill-bg"), token(lightTheme, "--skill-bg")];
    // Act
    const darker = luminance(dark) < luminance(light);
    // Assert
    expect(darker).toBe(true);
  });

  it("keeps the card's border turquoise in both themes", () => {
    // Arrange / Act
    const [light, dark] = [
      isTurquoise(token(lightTheme, "--skill-border")),
      isTurquoise(token(darkTheme, "--skill-border")),
    ];
    // Assert
    expect([light, dark]).toEqual([true, true]);
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

const agentCard = blockAfter(css, ".tool-card.tool-agent");
const agentJson = blockAfter(css, ".agent-json {");
const agentJsonOpen = blockAfter(css, ".agent-input.expanded .agent-json");

describe("subagent card", () => {
  it("washes the subagent card in the teal token rather than the tool-card grey", () => {
    // Arrange / Act — the .tool-card.tool-agent rule.
    // Assert
    expect(agentCard).toMatch(/background:\s*var\(--agent-card\)/);
  });

  it("carries the legacy Task name into the same teal wash", () => {
    // Arrange / Act — Agent and Task are one tool under two names.
    // Assert
    expect(css).toMatch(/\.tool-card\.tool-agent,\s*\.tool-card\.tool-task/);
  });

  it("outranks the grey .tool-card background by carrying an extra class", () => {
    // Arrange / Act — two classes beat one, so source order cannot strand the wash.
    // Assert
    expect(agentCard).not.toContain("!important");
    expect(css.indexOf(".tool-card.tool-agent")).toBeGreaterThan(css.indexOf(".tool-card {"));
  });

  it("defines a teal card token for the light theme", () => {
    // Arrange / Act
    const teal = isTurquoise(token(lightTheme, "--agent-card"));
    // Assert
    expect(teal).toBe(true);
  });

  it("defines a teal card token for the dark theme", () => {
    // Arrange / Act
    const teal = isTurquoise(token(darkTheme, "--agent-card"));
    // Assert
    expect(teal).toBe(true);
  });

  it("sinks the dark-theme card token below the light-theme one", () => {
    // Arrange
    const [dark, light] = [token(darkTheme, "--agent-card"), token(lightTheme, "--agent-card")];
    // Act
    const darker = luminance(dark) < luminance(light);
    // Assert
    expect(darker).toBe(true);
  });

  it("sinks the light-theme subagent wash below the skill wash it shares a hue with", () => {
    // Arrange — both washes are turquoise, so only depth can tell a spawned
    // subagent apart from a skill handoff sitting next to it in the feed.
    const [agent, skill] = [token(lightTheme, "--agent-card"), token(lightTheme, "--skill-bg")];
    // Act
    const separation = luminance(skill) - luminance(agent);
    // Assert — a token or two apart would still read as the same card.
    expect(separation).toBeGreaterThan(20);
  });

  it("lifts the dark-theme subagent wash clear of the skill wash it shares a hue with", () => {
    // Arrange — the dark theme inverts which of the two is the deeper wash.
    const [agent, skill] = [token(darkTheme, "--agent-card"), token(darkTheme, "--skill-bg")];
    // Act
    const separation = Math.abs(luminance(agent) - luminance(skill));
    // Assert
    expect(separation).toBeGreaterThan(10);
  });
});

describe("subagent input fold", () => {
  it("keeps the card's description class off the topbar roster's own .agent-desc", () => {
    // Arrange / Act — two different components: the roster row ellipsizes its
    // label, the card description wraps. One class for both would cross-style them.
    // Assert
    expect(blockAfter(css, ".agent-input-desc")).toMatch(/white-space:\s*pre-wrap/);
    expect(blockAfter(css, ".agent-desc {")).not.toMatch(/white-space/);
  });

  it("folds the subagent's input JSON away entirely until the card is opened", () => {
    // Arrange / Act — a height cap would still leak a peek of a pages-long prompt.
    // Assert
    expect(agentJson).toMatch(/display:\s*none/);
  });

  it("unfolds the input JSON once the section carries the expanded class", () => {
    // Arrange / Act — the .agent-input.expanded .agent-json rule.
    // Assert
    expect(agentJsonOpen).toMatch(/display:\s*block/);
  });

  it("outranks the fold from the open rule rather than reaching for !important", () => {
    // Arrange / Act — three classes beat one at any source position.
    // Assert
    expect(agentJsonOpen).not.toContain("!important");
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

  it("keeps rotating under reduced motion instead of swapping in another animation", () => {
    // Arrange / Act — the reduced-motion .thinking-spinner override.
    // Assert
    expect(reducedSpinner).not.toMatch(/animation-name:/);
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

  it("retires the in-body ••• pulse the arc now stands in for", () => {
    // Arrange / Act — the arc is the sole in-progress indicator, so neither the
    // pulsing body nor its keyframes may survive to compete with it.
    // Assert
    expect(css).not.toContain("tool-pulse");
    expect(css).not.toContain("tool-input-pending");
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
    const [r, g, b] = channels(orange);
    // Assert
    expect(r > g && g > b).toBe(true);
  });

  it("keeps the dark-theme thinking token orange so the badge reads orange", () => {
    // Arrange
    const orange = token(darkTheme, "--thinking");
    // Act
    const [r, g, b] = channels(orange);
    // Assert
    expect(r > g && g > b).toBe(true);
  });
});

/* The subagent roster drops out of the topbar, which is a fixed-height flex row:
   the overlay has to leave that row's layout entirely, and its anchor has to be
   the positioned ancestor it hangs from. Both are pure CSS, so both are asserted
   against the stylesheet source. */
const agentsMenu = blockAfter(css, ".agents-menu");
const agentsOverlay = blockAfter(css, ".agents-overlay");
const agentsToggle = blockAfter(css, ".info-agents {");
const runningDot = blockAfter(css, ".agent-dot.agent-starting,");
const reducedDot = blockAfter(
  blockAfter(css, "@media (prefers-reduced-motion: reduce)"),
  ".agent-dot.agent-starting,",
);

describe("subagent roster styles", () => {
  it("anchors the overlay on the menu that drops it", () => {
    // Arrange / Act — the overlay is absolute, so it needs a positioned ancestor.
    // Assert
    expect(agentsMenu).toMatch(/position:\s*relative/);
  });

  it("lifts the overlay out of the topbar's flex row", () => {
    // Arrange / Act — an in-flow roster would stretch the topbar to its height.
    // Assert
    expect(agentsOverlay).toMatch(/position:\s*absolute/);
  });

  it("hangs the overlay below the topbar rather than over it", () => {
    // Arrange / Act
    // Assert
    expect(agentsOverlay).toMatch(/top:\s*calc\(100% \+ [\d.]+rem\)/);
  });

  it("stacks the overlay above the feed it covers", () => {
    // Arrange / Act — an un-stacked overlay renders behind the feed's cards.
    // Assert
    expect(agentsOverlay).toMatch(/z-index:\s*\d+/);
  });

  it("scrolls the overlay instead of running it off the viewport", () => {
    // Arrange / Act — a long-running session spawns more agents than fit.
    // Assert
    expect(agentsOverlay).toMatch(/overflow-y:\s*auto/);
  });

  it("renders the chip as a pointer target so it reads as pressable", () => {
    // Arrange / Act
    // Assert
    expect(agentsToggle).toMatch(/cursor:\s*pointer/);
  });

  it("colors the chip with its own datapoint token", () => {
    // Arrange / Act — the chip joins the parent-workspace/model/tokens run.
    // Assert
    expect(agentsToggle).toMatch(/color:\s*var\(--info-agents\)/);
  });

  it("binds the chip's token in the light palette", () => {
    // Arrange / Act + Assert
    expect(token(lightTheme, "--info-agents")).toMatch(/^#[0-9a-f]{6}$/);
  });

  it("binds the chip's token in the dark palette", () => {
    // Arrange / Act + Assert
    expect(token(darkTheme, "--info-agents")).toMatch(/^#[0-9a-f]{6}$/);
  });

  it("pulses the dot of a subagent still working", () => {
    // Arrange / Act — a settled roster has no motion in it at all.
    // Assert
    expect(runningDot).toMatch(/animation:\s*pulse/);
  });

  it("stills the working dot under reduced motion", () => {
    // Arrange / Act
    // Assert
    expect(reducedDot).toMatch(/animation:\s*none/);
  });
});

const inlineCode = blockAfter(css, ".md code {");
const fencedBlock = blockAfter(css, ".md pre.md-code {");
const fencedInner = blockAfter(css, ".md pre.md-code code");

/** The value bound to `--code-bg` inside a palette block. */
function codeBg(block: string): string {
  const hit = block.match(/--code-bg:\s*([^;]+);/);
  if (!hit) throw new Error("palette block has no --code-bg");
  return hit[1];
}

/** Percentage of `color` a `color-mix(... , transparent)` declaration washes in. */
function washPercent(decl: string, color: string): number {
  const hit = decl.match(new RegExp(`color-mix\\(in srgb,\\s*${color}\\s+(\\d+)%,\\s*transparent\\)`));
  if (!hit) throw new Error(`declaration washes no ${color}: ${decl}`);
  return Number(hit[1]);
}

describe("inline-code chip", () => {
  it("washes an inline code span with its own token rather than an ad-hoc mix", () => {
    // Arrange / Act — the .md code rule.
    // Assert
    expect(inlineCode).toMatch(/background:\s*var\(--code-bg\)/);
  });

  it("darkens the light-theme chip by washing black into it", () => {
    // Arrange / Act
    const black = washPercent(codeBg(lightTheme), "#000000");
    // Assert
    expect(black).toBeGreaterThan(0);
  });

  it("darkens the dark-theme chip by washing black into it", () => {
    // Arrange / Act — a --fg wash would LIGHTEN the chip here, since --fg flips
    // light in the dark theme.
    const black = washPercent(codeBg(darkTheme), "#000000");
    // Assert
    expect(black).toBeGreaterThan(0);
  });

  it("deepens the dark-theme wash past the light-theme one", () => {
    // Arrange — the same wash over a dark bubble moves the eye far less than
    // it does over a white one.
    const [dark, light] = [washPercent(codeBg(darkTheme), "#000000"), washPercent(codeBg(lightTheme), "#000000")];
    // Act
    const deeper = dark > light;
    // Assert
    expect(deeper).toBe(true);
  });

  it("keeps the chip's wash off the fenced code block", () => {
    // Arrange / Act — a fenced block is its own surface, not a chip inline in prose.
    // Assert
    expect(fencedBlock).not.toMatch(/var\(--code-bg\)/);
  });

  it("strips the chip's wash from the code inside a fenced block", () => {
    // Arrange / Act — .md code also matches the <code> a fence wraps, so without
    // this override the chip wash would stack on top of the block's own.
    // Assert
    expect(fencedInner).toMatch(/background:\s*none/);
  });
});
