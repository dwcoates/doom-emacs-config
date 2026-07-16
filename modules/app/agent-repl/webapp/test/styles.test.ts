/**
 * Stylesheet contract tests.
 *
 * The thinking indicator's motion is pure CSS, so its behavior is asserted
 * against the stylesheet source: jsdom neither runs animations nor resolves
 * media queries, and the rotation is the whole point of the indicator.
 */
import { describe, expect, it } from "vitest";

import { CAPPED_CLASSES, EXPANDED_CLASS } from "../src/expand.js";
import { NAV_CURRENT_CLASS } from "../src/nav.js";
import { CURRENT_CLASS, MARK_CLASS, REVEAL_CLASS } from "../src/search.js";
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
 * aqua. The one async wash lives in this band — the skill handoff and the
 * spawned subagent share it deliberately (one token, one depth), which the
 * "subagent card" suite pins.
 */
function isTurquoise(hex: string): boolean {
  const h = hue(hex);
  return h >= 160 && h <= 195;
}

/**
 * Whether a `#rrggbb` literal reads as yellow: red and green both dominate a
 * low blue and sit close to each other, so the hue lands between green and
 * orange rather than drifting into either.
 */
function isYellow(hex: string): boolean {
  const [r, g, b] = channels(hex);
  return r > 2 * b && g > 2 * b && Math.abs(r - g) < 0.4 * r;
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

const topbarPicker = blockAfter(css, "#topbar select");

describe("topbar pickers", () => {
  it("sizes a picker to its selected option rather than to the widest one", () => {
    // Arrange / Act — the #topbar select rule.
    // Assert
    expect(topbarPicker).toMatch(/field-sizing:\s*content/);
  });

  it("leaves the picker unpinned by a width, which would defeat the content sizing", () => {
    // Arrange / Act — an explicit width would override the intrinsic size.
    // Assert
    expect(topbarPicker).not.toMatch(/(^|[^-])width:/);
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

describe("interrupted chip", () => {
  it("paints the interrupt-ended turn's chip in the interrupted yellow", () => {
    // Arrange / Act — the .result.interrupted rule.
    // Assert
    expect(blockAfter(css, ".result.interrupted")).toMatch(/color:\s*var\(--interrupted\)/);
  });

  it("defines a yellow interrupted token for the light theme", () => {
    // Arrange / Act
    const yellow = isYellow(token(lightTheme, "--interrupted"));
    // Assert
    expect(yellow).toBe(true);
  });

  it("defines a yellow interrupted token for the dark theme", () => {
    // Arrange / Act
    const yellow = isYellow(token(darkTheme, "--interrupted"));
    // Assert
    expect(yellow).toBe(true);
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
    // Arrange / Act — the shared .bubble rule lays the 1px out up front, and the
    // final-response rule only recolors it.
    // Assert
    expect(bubble).toMatch(/border:\s*1px\s+solid\s+transparent/);
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

describe("the bubble pulse", () => {
  const pulsing = blockAfter(css, ".bubble.pulsing {");
  const breathe = blockAfter(css, "@keyframes bubble-breathe");
  const reducedPulse = blockAfter(
    blockAfter(css, "@media (prefers-reduced-motion: reduce)"),
    ".bubble.pulsing",
  );
  /** Seconds a breathing bubble takes to complete one breath. */
  const period = Number(pulsing.match(/bubble-breathe\s+([\d.]+)s/)?.[1]);

  it("breathes a flagged bubble on an endless loop", () => {
    // Arrange / Act — the .bubble.pulsing rule.
    // Assert
    expect(pulsing).toMatch(/animation:\s*bubble-breathe\s+[\d.]+s\s+ease-in-out\s+infinite/);
  });

  it("holds the breath slower than every ticking indicator in the app", () => {
    // Arrange / Act — the spinners and the cursor all tick in around a second,
    // so a two-second breath still reads as the slowest motion in the app.
    // Assert
    expect(period).toBeGreaterThanOrEqual(2);
  });

  it("carries no hue of its own, breathing toward whatever the role names", () => {
    // Arrange / Act — the @keyframes bubble-breathe midpoint. One keyframe
    // serves both roles precisely because it names none of their colors.
    // Assert
    expect(breathe).toMatch(/background:\s*var\(--pulse-to\)/);
  });

  it("points the assistant bubble's breath at the assistant-pulse token", () => {
    // Arrange / Act — the .bubble.assistant rule.
    // Assert
    expect(blockAfter(css, ".bubble.assistant {")).toMatch(
      /--pulse-to:\s*var\(--assistant-pulse\)/,
    );
  });

  it("points the user bubble's breath at the user-pulse token", () => {
    // Arrange / Act — the .bubble.user rule.
    // Assert
    expect(blockAfter(css, ".bubble.user {")).toMatch(/--pulse-to:\s*var\(--user-pulse\)/);
  });

  it("drops the breath entirely under reduced motion, since it is a hint and not the signal", () => {
    // Arrange / Act — the reduced-motion .bubble.pulsing override.
    // Assert
    expect(reducedPulse).toMatch(/animation:\s*none/);
  });
});

/**
 * Every breathing section takes the SAME breath and differs only in the wash it
 * takes it from — the working frontier from the assistant purple and the
 * just-sent prompt from the user blue — so the palette contract is one table
 * run per role rather than suites drifting apart.
 */
describe.each([
  { role: "working-frontier", wash: "--assistant", far: "--assistant-pulse" },
  { role: "sent-prompt", wash: "--user", far: "--user-pulse" },
])("$role pulse palette", ({ wash, far }) => {
  it("keeps the light-theme breath inside the bubble's own hue", () => {
    // Arrange
    const [near, deep] = [token(lightTheme, wash), token(lightTheme, far)];
    // Act
    const drift = Math.abs(hue(deep) - hue(near));
    // Assert
    expect(drift).toBeLessThan(10);
  });

  it("keeps the dark-theme breath inside the bubble's own hue", () => {
    // Arrange
    const [near, deep] = [token(darkTheme, wash), token(darkTheme, far)];
    // Act
    const drift = Math.abs(hue(deep) - hue(near));
    // Assert
    expect(drift).toBeLessThan(10);
  });

  it("deepens the light-theme wash at the top of the breath", () => {
    // Arrange
    const [near, deep] = [token(lightTheme, wash), token(lightTheme, far)];
    // Act
    const deeper = luminance(deep) < luminance(near);
    // Assert
    expect(deeper).toBe(true);
  });

  it("lifts the dark-theme wash at the top of the breath, where deepening would vanish", () => {
    // Arrange
    const [near, deep] = [token(darkTheme, wash), token(darkTheme, far)];
    // Act
    const lifted = luminance(deep) > luminance(near);
    // Assert
    expect(lifted).toBe(true);
  });

  it("keeps the light-theme breath shallow enough to read under prose", () => {
    // Arrange
    const [near, deep] = [token(lightTheme, wash), token(lightTheme, far)];
    // Act
    const depth = Math.abs(luminance(deep) - luminance(near));
    // Assert
    expect(depth).toBeLessThan(30);
  });

  it("keeps the dark-theme breath shallow enough to read under prose", () => {
    // Arrange
    const [near, deep] = [token(darkTheme, wash), token(darkTheme, far)];
    // Act
    const depth = Math.abs(luminance(deep) - luminance(near));
    // Assert
    expect(depth).toBeLessThan(30);
  });
});

/**
 * The bubble breath is deliberately deeper than the tool-card breath: it is the
 * one motion under a freshly sent prompt or a caught-up frontier, so it must
 * register at a glance. It lives in a band whose ceiling ("shallow enough to
 * read under prose") the palette suite above pins and whose floor is here — a
 * breath shallower than this floor reads as no breath at all. Bubble-scoped on
 * purpose: the tool-card washes take a quieter breath and sit this floor out.
 */
describe.each([
  { role: "working-frontier", wash: "--assistant", far: "--assistant-pulse" },
  { role: "sent-prompt", wash: "--user", far: "--user-pulse" },
])("$role breath is perceptible", ({ wash, far }) => {
  /** Luminance units below which the breath stops registering as motion. */
  const FLOOR = 20;

  it("deepens the light-theme breath past the perceptibility floor", () => {
    // Arrange
    const [near, deep] = [token(lightTheme, wash), token(lightTheme, far)];
    // Act
    const depth = Math.abs(luminance(deep) - luminance(near));
    // Assert
    expect(depth).toBeGreaterThanOrEqual(FLOOR);
  });

  it("lifts the dark-theme breath past the perceptibility floor", () => {
    // Arrange
    const [near, deep] = [token(darkTheme, wash), token(darkTheme, far)];
    // Act
    const depth = Math.abs(luminance(deep) - luminance(near));
    // Assert
    expect(depth).toBeGreaterThanOrEqual(FLOOR);
  });
});

describe("turn stamp", () => {
  const bubble = blockAfter(css, ".bubble ");
  const body = blockAfter(css, ".bubble-body");
  const stamp = blockAfter(css, ".turn-ts");
  const bodyText = blockAfter(css, "pre {");

  it("lays every bubble out as a body column beside its stamp", () => {
    // Arrange / Act — the shared .bubble rule, so user and assistant match.
    // Assert
    expect(bubble).toMatch(/display:\s*flex/);
  });

  it("pins the stamp to the bubble's top rather than its first baseline", () => {
    // Arrange / Act
    // Assert
    expect(bubble).toMatch(/align-items:\s*flex-start/);
  });

  it("kerns the stamp off the body column it labels", () => {
    // Arrange
    const gap = bubble.match(/gap:\s*([\d.]+)rem/);
    // Act / Assert
    expect(Number(gap?.[1])).toBeGreaterThan(0);
  });

  it("gives the body the whole column the stamp does not take", () => {
    // Arrange / Act — flex:1 with min-width:0 so long content wraps instead of
    // pushing the stamp off the bubble's right edge.
    // Assert
    expect(body).toMatch(/flex:\s*1/);
    expect(body).toMatch(/min-width:\s*0/);
  });

  it("holds the stamp at its natural width while the body wraps", () => {
    // Arrange / Act
    // Assert
    expect(stamp).toMatch(/flex:\s*none/);
  });

  it("keeps the multi-token relative age on a single line", () => {
    // Arrange / Act — `5m 30s ago` carries inner spaces the fixed HH:MM never
    // had, so the stamp must not wrap at them into a two-line corner label.
    // Assert
    expect(stamp).toMatch(/white-space:\s*nowrap/);
  });

  it("sets the stamp smaller than the text it dates", () => {
    // Arrange
    const stampSize = Number(stamp.match(/font-size:\s*([\d.]+)rem/)?.[1]);
    const textSize = Number(bodyText.match(/font-size:\s*([\d.]+)rem/)?.[1]);
    // Act / Assert
    expect(stampSize).toBeLessThan(textSize);
  });

  it("mutes the stamp below the text it dates", () => {
    // Arrange / Act — the color is still derived from --muted, so the stamp
    // stays quieter than the --fg body text it labels.
    // Assert
    expect(stamp).toMatch(/color:[^;]*var\(--muted\)/);
  });

  it("darkens the stamp a shade toward the body text", () => {
    // Arrange / Act — mixing --muted with --fg pushes the grey a touch more
    // present than plain --muted, in both the light and dark themes.
    // Assert
    expect(stamp).toMatch(/color:\s*color-mix\([^;]*var\(--muted\)[^;]*var\(--fg\)/);
  });

  it("pulls the stamp in toward the bubble's top-right corner", () => {
    // Arrange
    const [top, right] = margins(stamp);
    // Act / Assert — negative top/right margins seat it nearer the corner
    // than the body padding alone would.
    expect(parseFloat(top)).toBeLessThan(0);
    expect(parseFloat(right)).toBeLessThan(0);
  });

  it("keeps a sliver of space so the stamp never abuts the corner", () => {
    // Arrange — the bubble's own padding on the sides the stamp eats into.
    const [padTop, padRight] = padding(bubble);
    const [top, right] = margins(stamp);
    // Act / Assert — each pull-in stays shy of the padding it consumes, so
    // some corner gap always survives.
    expect(Math.abs(parseFloat(top))).toBeLessThan(padTop);
    expect(Math.abs(parseFloat(right))).toBeLessThan(padRight);
  });

  it("reaches the markdown through the body column for its flush-edge margins", () => {
    // Arrange / Act — the first/last child of the markdown now sits one level in.
    // Assert
    expect(css).toMatch(/\.md \.bubble-body > :first-child \{ margin-top: 0; \}/);
    expect(css).toMatch(/\.md \.bubble-body > :last-child \{ margin-bottom: 0; \}/);
  });
});

describe("skill-launch card", () => {
  const skillCard = blockAfter(css, ".tool-card.tool-skill");

  it("washes the skill-launch card in the shared async token", () => {
    // Arrange / Act — the .tool-card.tool-skill rule.
    // Assert
    expect(skillCard).toMatch(/background:\s*var\(--async-card\)/);
  });

  it("recolors the card's existing border rather than adding a second one", () => {
    // Arrange / Act — .tool-card already lays out the 1px border.
    // Assert
    expect(skillCard).toMatch(/border-color:\s*var\(--async-border\)/);
    expect(skillCard).not.toMatch(/border:\s/);
  });

  it("keeps the card's border turquoise in both themes", () => {
    // Arrange / Act
    const [light, dark] = [
      isTurquoise(token(lightTheme, "--async-border")),
      isTurquoise(token(darkTheme, "--async-border")),
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

/** The chip as it sits inside the final response, rather than adrift in the feed. */
const nestedChip = blockAfter(css, ".bubble.assistant.final-response .bubble-body > .result");

/** The margin shorthand's four sides, in `[top, right, bottom, left]` order. */
function margins(rule: string): string[] {
  const shorthand = rule.match(/margin:\s*([^;]+);/);
  if (!shorthand) throw new Error("rule sets no margin shorthand");
  const sides = shorthand[1].trim().split(/\s+/);
  const [top, right = top, bottom = top, left = right] = sides;
  return [top, right, bottom, left];
}

/** The padding shorthand's four sides in rem, `[top, right, bottom, left]`. */
function padding(rule: string): number[] {
  const shorthand = rule.match(/padding:\s*([^;]+);/);
  if (!shorthand) throw new Error("rule sets no padding shorthand");
  const sides = shorthand[1].trim().split(/\s+/);
  const [top, right = top, bottom = top, left = right] = sides;
  return [top, right, bottom, left].map(parseFloat);
}

describe("nested turn-complete chip", () => {
  it("shrinks the nested chip back around its own text", () => {
    // Arrange / Act — the bubble's block flow would stretch it to full width.
    // Assert
    expect(nestedChip).toMatch(/width:\s*fit-content/);
  });

  it("centers the nested chip across the bubble", () => {
    // Arrange
    const [, right, , left] = margins(nestedChip);
    // Act / Assert — auto side margins are what center a fit-content box.
    expect([right, left]).toEqual(["auto", "auto"]);
  });

  it("holds the nested chip clear of the bubble's bottom edge", () => {
    // Arrange
    const [, , bottom] = margins(nestedChip);
    // Act / Assert — the kerning between the chip and the bubble's floor.
    expect(parseFloat(bottom)).toBeGreaterThan(0);
  });

  it("parts the nested chip from the answer's last line", () => {
    // Arrange
    const [top] = margins(nestedChip);
    // Act / Assert — the chip never crowds the prose it closes.
    expect(parseFloat(top)).toBeGreaterThan(0);
  });
});

const agentCard = blockAfter(css, ".tool-card.tool-agent");
const agentJson = blockAfter(css, ".agent-json {");
const agentJsonOpen = blockAfter(css, ".agent-input.expanded .agent-json");

describe("subagent card", () => {
  it("washes the subagent card in the async token rather than the tool-card grey", () => {
    // Arrange / Act — the .tool-card.tool-agent rule.
    // Assert
    expect(agentCard).toMatch(/background:\s*var\(--async-card\)/);
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
    const teal = isTurquoise(token(lightTheme, "--async-card"));
    // Assert
    expect(teal).toBe(true);
  });

  it("defines a teal card token for the dark theme", () => {
    // Arrange / Act
    const teal = isTurquoise(token(darkTheme, "--async-card"));
    // Assert
    expect(teal).toBe(true);
  });

  it("sinks the dark-theme card token below the light-theme one", () => {
    // Arrange
    const [dark, light] = [token(darkTheme, "--async-card"), token(lightTheme, "--async-card")];
    // Act
    const darker = luminance(dark) < luminance(light);
    // Assert
    expect(darker).toBe(true);
  });

  it("washes the skill handoff and the spawned subagent in the same rule", () => {
    // Arrange — one async teal for both: depth no longer tells a skill
    // handoff apart from a spawned subagent, the card's own content does.
    const skillCard = blockAfter(css, ".tool-card.tool-skill");
    // Act / Assert — both markers resolve to the same shared block.
    expect(skillCard).toBe(agentCard);
  });

  it("sinks the light-theme wash a tad below the old subagent teal", () => {
    // Arrange — #a7e8dd is the pre-unification subagent card, pinned here so
    // the requested darkening cannot silently regress.
    const wash = token(lightTheme, "--async-card");
    // Act / Assert
    expect(luminance(wash)).toBeLessThan(luminance("#a7e8dd"));
  });

  it("sinks the dark-theme wash a tad below the old subagent teal", () => {
    // Arrange — #0f6b60 is the pre-unification dark subagent card.
    const wash = token(darkTheme, "--async-card");
    // Act / Assert
    expect(luminance(wash)).toBeLessThan(luminance("#0f6b60"));
  });

  it("darkens the light-theme wash a second notch below the first-unification teal", () => {
    // Arrange — #8fdfd0 is the first-unification light card, pinned here so the
    // requested second darkening cannot silently regress back to it.
    const wash = token(lightTheme, "--async-card");
    // Act / Assert
    expect(luminance(wash)).toBeLessThan(luminance("#8fdfd0"));
  });

  it("darkens the dark-theme wash a second notch below the first-unification teal", () => {
    // Arrange — #0c5b52 is the first-unification dark card, pinned here so the
    // requested second darkening cannot silently regress back to it.
    const wash = token(darkTheme, "--async-card");
    // Act / Assert
    expect(luminance(wash)).toBeLessThan(luminance("#0c5b52"));
  });

  it("darkens the light-theme wash a third notch below the second-unification teal", () => {
    // Arrange — #84cdbf is the second-unification light card, pinned here so the
    // requested third darkening cannot silently regress back to it.
    const wash = token(lightTheme, "--async-card");
    // Act / Assert
    expect(luminance(wash)).toBeLessThan(luminance("#84cdbf"));
  });

  it("darkens the dark-theme wash a third notch below the second-unification teal", () => {
    // Arrange — #0b5048 is the second-unification dark card, pinned here so the
    // requested third darkening cannot silently regress back to it.
    const wash = token(darkTheme, "--async-card");
    // Act / Assert
    expect(luminance(wash)).toBeLessThan(luminance("#0b5048"));
  });

  it("keeps the darkened light-theme wash inside the turquoise band", () => {
    // Arrange / Act — darkening must not drift the hue off teal.
    const wash = token(lightTheme, "--async-card");
    // Assert
    expect(isTurquoise(wash)).toBe(true);
  });

  it("keeps the darkened dark-theme wash inside the turquoise band", () => {
    // Arrange / Act — darkening must not drift the hue off teal.
    const wash = token(darkTheme, "--async-card");
    // Assert
    expect(isTurquoise(wash)).toBe(true);
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

const interruptingPending = blockAfter(css, ".interrupting-pending");
const interruptingSpinner = blockAfter(css, ".interrupting-spinner {");
const reducedMotion = blockAfter(css, "@media (prefers-reduced-motion: reduce)");

describe("interrupting indicator", () => {
  it("reads in the alarm red rather than the thinking orange", () => {
    // Arrange / Act — the .interrupting-pending label rule.
    // Assert — --err (red), the opposite signal from --thinking (working orange).
    expect(interruptingPending).toMatch(/color:\s*var\(--err\)/);
  });

  it("spins the same thinking-spin animation the thinking indicator uses", () => {
    // Arrange / Act — the base .interrupting-spinner rule.
    // Assert — reusing thinking-spin means only the hue differs from thinking.
    expect(interruptingSpinner).toMatch(/animation:\s*thinking-spin\s+[\d.]+s\s+linear\s+infinite/);
  });

  it("draws the spinning arc in red", () => {
    // Arrange / Act — the base .interrupting-spinner rule.
    // Assert — the arc's leading edge is the alarm red.
    expect(interruptingSpinner).toMatch(/border-top-color:\s*var\(--err\)/);
  });

  it("slows the interrupting arc under reduced motion alongside the thinking arc", () => {
    // Arrange / Act — the reduced-motion block names the interrupting spinner too.
    // Assert
    expect(reducedMotion).toContain(".interrupting-spinner");
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

/* The compaction bar's motion is pure CSS, like the spinners above, so it is
   asserted against the stylesheet source rather than a jsdom render. */
const compactBar = blockAfter(css, ".compact-progress-bar");
const compactSweep = blockAfter(css, "@keyframes compact-progress-sweep");
const reducedCompactBar = blockAfter(
  blockAfter(css, "@media (prefers-reduced-motion: reduce)"),
  ".compact-progress-bar",
);

describe("compaction progress bar", () => {
  it("loops an indeterminate sweep rather than tracking a fraction", () => {
    // Arrange / Act — the .compact-progress-bar rule.
    // Assert
    expect(compactBar).toMatch(
      /animation:\s*compact-progress-sweep\s+[\d.]+s\s+[a-z-]+\s+infinite/,
    );
  });

  it("sweeps the fill from off the left edge to off the right", () => {
    // Arrange / Act — the compact-progress-sweep keyframes.
    // Assert
    expect(compactSweep).toMatch(/0%\s*{\s*left:\s*-\d+%/);
    expect(compactSweep).toMatch(/100%\s*{\s*left:\s*\d+%/);
  });

  it("rests the bar still under reduced motion, since the label carries the meaning", () => {
    // Arrange / Act — the reduced-motion .compact-progress-bar override.
    // Assert
    expect(reducedCompactBar).toMatch(/animation:\s*none/);
  });
});

describe("tool-card status chip", () => {
  it("backs every tool card's chip with the card grey so the async teal cannot tint it", () => {
    // Arrange / Act — the .tool-card .badge rule (brace-anchored: the
    // palette comment also names the selector).
    // Assert
    expect(blockAfter(css, ".tool-card .badge {")).toMatch(/background:\s*var\(--card\)/);
  });
});

describe("activity fold", () => {
  it("invites the click with a zoom cursor on the closed fold", () => {
    // Arrange / Act — the .agent-activity rule.
    // Assert
    expect(blockAfter(css, ".agent-activity {")).toMatch(/cursor:\s*zoom-in/);
  });

  it("offers the fold-back cursor on the open fold's ticker only", () => {
    // Arrange / Act — clicks inside the panel belong to the children.
    // Assert
    expect(blockAfter(css, ".agent-activity.open > .agent-ticker")).toMatch(/cursor:\s*zoom-out/);
  });

  it("mutes the ticker so the live line reads as status, not content", () => {
    // Arrange / Act — line-anchored: the open-fold cursor rule also
    // names the class mid-selector.
    // Assert
    expect(blockAfter(css, "\n.agent-ticker")).toMatch(/color:\s*var\(--muted\)/);
  });

  it("rounds the ticker into a pill, like a tab chip", () => {
    // Arrange / Act — the ticker wears the same pill radius as .tab-chip.
    // Assert
    expect(blockAfter(css, "\n.agent-ticker")).toMatch(/border-radius:\s*99px/);
  });

  it("backs the ticker pill with the card grey, matching a tab chip", () => {
    // Arrange / Act — same fill as .tab-chip, so the two read as one form.
    // Assert
    expect(blockAfter(css, "\n.agent-ticker")).toMatch(/background:\s*var\(--card\)/);
  });

  it("lays the child-feed panel on the main-GUI background so nested bubbles never clash with the card teal", () => {
    // Arrange / Act — the wrapper's whole purpose is to replace the async
    // teal under the nested bubbles with the neutral feed background.
    // Assert
    expect(blockAfter(css, ".agent-panel {")).toMatch(/background:\s*var\(--bg\)/);
  });

  it("wraps the child-feed panel as a rounded bubble", () => {
    // Arrange / Act — the wrapper is a bubble, not a bare background band.
    // Assert
    expect(blockAfter(css, ".agent-panel {")).toMatch(/border-radius:\s*8px/);
  });

  it("pads the child-feed panel so nested bubbles keep their border distance", () => {
    // Arrange / Act — the padding is the gap the nested bubbles had to the
    // card, now held against this wrapper (they shrink to make room).
    // Assert
    expect(blockAfter(css, ".agent-panel {")).toMatch(/padding:\s*0\.5rem\s+0\.75rem/);
  });

  it("tints the needs-permission badge in the accent", () => {
    // Arrange / Act
    // Assert
    expect(blockAfter(css, ".badge.perm")).toMatch(/color:\s*var\(--accent\)/);
  });
});

describe("tab groups", () => {
  it("lays the tab bar out as a wrapping flex row", () => {
    // Arrange / Act — an eight-agent fan-out must not overflow the card.
    const bar = blockAfter(css, ".tab-bar");
    // Assert
    expect(bar).toMatch(/display:\s*flex/);
    expect(bar).toMatch(/flex-wrap:\s*wrap/);
  });

  it("invites the click with a pointer cursor on each chip", () => {
    // Arrange / Act
    // Assert
    expect(blockAfter(css, ".tab-chip {")).toMatch(/cursor:\s*pointer/);
  });

  it("rings the selected chip in the accent", () => {
    // Arrange / Act
    // Assert
    expect(blockAfter(css, ".tab-chip.active")).toMatch(/border-color:\s*var\(--accent\)/);
  });
});

/* The subagent roster drops out of the topbar, which is a fixed-height flex row:
   the overlay has to leave that row's layout entirely, and its anchor has to be
   the positioned ancestor it hangs from. Both are pure CSS, so both are asserted
   against the stylesheet source. */
const agentsMenu = blockAfter(css, ".agents-menu");
const agentsOverlay = blockAfter(css, ".agents-overlay");
/* Every chip shares one button-reset rule; only the color rule is per-chip. */
const chipReset = blockAfter(css, ".info-agents, .info-tasks, .info-tokens {");
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

  it("reads every roster row as pressable, since a click jumps the feed to its bubble", () => {
    // Arrange / Act — the shared row block, covering both rosters' rows.
    const rosterRow = blockAfter(css, ".agent-row, .task-row {");
    // Assert
    expect(rosterRow).toMatch(/cursor:\s*pointer/);
  });

  it("renders the chip as a pointer target so it reads as pressable", () => {
    // Arrange / Act — the shared chip reset carries the affordance.
    // Assert
    expect(chipReset).toMatch(/cursor:\s*pointer/);
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

/* The agent-scoped topbar strip inside a subagent's bubble reuses the header
   strip's markup wholesale (see topbar.ts), so its styling is only the
   bubble-specific deltas: card-scale sizing, and overlays that must not hang
   off the card's left edge. */
const agentTopbar = blockAfter(css, ".agent-topbar {");
const agentTopbarOverlay = blockAfter(css, ".agent-topbar .agents-overlay,");

describe("agent bubble topbar styles", () => {
  it("sizes the bubble strip down to card scale", () => {
    // Arrange / Act — the header strip inherits the topbar's size; a bubble
    // strip at that size would out-shout the card head above it.
    // Assert
    expect(agentTopbar).toMatch(/font-size:/);
  });

  it("flips a bubble overlay to extend rightward from its chip", () => {
    // Arrange / Act — the shared overlay rule right-aligns for the header's
    // far-right chips; a bubble's chips sit at the card's LEFT.
    // Assert
    expect(agentTopbarOverlay).toMatch(/left:\s*0/);
    expect(agentTopbarOverlay).toMatch(/right:\s*auto/);
  });

  it("colors the bubble strip's datapoints through the shared topbar-info scope", () => {
    // Arrange / Act — .agent-topbar declares no info-* colors of its own; a
    // color it duplicated would drift from the header's the day one changes.
    // Assert
    expect(agentTopbar).not.toMatch(/--info-/);
  });
});

const tasksToggle = blockAfter(css, ".info-tasks {");
const inactiveDesc = blockAfter(css, ".agent-row.inactive .agent-desc,");
const taskRunningDot = blockAfter(css, ".task-dot.task-starting,");

describe("task roster styles", () => {
  it("colors the task chip with its own datapoint token", () => {
    // Arrange / Act — the task chip joins the same datapoint run as the agents chip.
    // Assert
    expect(tasksToggle).toMatch(/color:\s*var\(--info-tasks\)/);
  });

  it("binds the task chip's token in the light palette", () => {
    // Arrange / Act + Assert
    expect(token(lightTheme, "--info-tasks")).toMatch(/^#[0-9a-f]{6}$/);
  });

  it("binds the task chip's token in the dark palette", () => {
    // Arrange / Act + Assert
    expect(token(darkTheme, "--info-tasks")).toMatch(/^#[0-9a-f]{6}$/);
  });

  it("keeps the task chip hue clear of the agents chip violet", () => {
    // Arrange — two counters side by side must not read as one datapoint.
    const separation = Math.abs(hue(token(lightTheme, "--info-tasks")) - hue(token(lightTheme, "--info-agents")));
    // Act + Assert — a comfortable hue gap in the light palette.
    expect(Math.min(separation, 360 - separation)).toBeGreaterThan(40);
  });

  it("greys the headline of a settled (inactive) row", () => {
    // Arrange / Act — the inactive greying rule covers both rosters.
    // Assert
    expect(inactiveDesc).toMatch(/color:\s*var\(--muted\)/);
  });

  it("pulses the dot of a task still working", () => {
    // Arrange / Act — the task dot shares the roster's in-flight pulse.
    // Assert
    expect(taskRunningDot).toMatch(/animation:\s*pulse/);
  });
});

const inlineCode = blockAfter(css, ".md code, .q-text code, .q-opt code {");
const fencedBlock = blockAfter(css, ".md pre.md-code {");
const fencedInner = blockAfter(css, ".md pre.md-code code");
// Leading newline anchors the standalone reset rule: the same selector also
// appears mid-line inside the shared chip rule (".md code, .q-text code, …"),
// where it is preceded by a space rather than a line break.
const pickerCodeReset = blockAfter(css, "\n.q-text code, .q-opt code {");

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

/* The task timer is the one topbar datapoint that rewrites itself while the
   user is looking at it, so its styling has a job the other datapoints do not:
   hold the strip still as the digits move. Scoped to .topbar-info, the
   class both renderings of the strip carry (the header's #session-info and
   each subagent bubble's .agent-topbar). */
const taskTimer = blockAfter(css, ".topbar-info .info-time {");

describe("task timer styles", () => {
  it("colors the timer with its own datapoint token", () => {
    // Arrange / Act — it joins the parent-workspace/tokens/agents run.
    // Assert
    expect(taskTimer).toMatch(/color:\s*var\(--info-time\)/);
  });

  it("binds the timer's token in the light palette", () => {
    // Arrange / Act + Assert
    expect(token(lightTheme, "--info-time")).toMatch(/^#[0-9a-f]{6}$/);
  });

  it("binds the timer's token in the dark palette", () => {
    // Arrange / Act + Assert
    expect(token(darkTheme, "--info-time")).toMatch(/^#[0-9a-f]{6}$/);
  });

  it("holds the digits to a fixed width so a tick cannot twitch the strip", () => {
    // Arrange / Act — proportional digits would nudge tokens and agents on
    // every repaint.
    // Assert
    expect(taskTimer).toMatch(/font-variant-numeric:\s*tabular-nums/);
  });
});

describe("queued-card (§2.13 in-flight queue)", () => {
  const card = blockAfter(css, ".queued-card");
  const interruptBadge = blockAfter(css, ".queued-badge.interrupt");
  const userBubble = blockAfter(css, ".bubble.user");

  it("subdues the parked card with a dashed border, unlike the solid user bubble", () => {
    // Arrange / Act — the dashed border is the "parked, not active" cue.
    // Assert
    expect(card).toMatch(/border:\s*1px\s+dashed/);
  });

  it("draws the dashed border in the very --user blue the live bubble fills with", () => {
    // Arrange / Act — the border color ties the parked card to the user's own
    // prompt, at the code level the same token .bubble.user backgrounds with.
    // Assert
    expect(card).toMatch(/border:\s*1px\s+dashed\s+var\(--user\)/);
    expect(userBubble).toMatch(/background:\s*var\(--user\)/);
  });

  it("dims the parked card below full opacity", () => {
    // Arrange / Act — reduced opacity reads as held-back.
    // Assert
    const m = card.match(/opacity:\s*([0-9.]+)/);
    expect(m).not.toBeNull();
    expect(Number(m![1])).toBeLessThan(1);
  });

  it("washes the card in the muted card grey rather than the user fill", () => {
    // Arrange / Act — a queued message must not read as the saturated live bubble.
    // Assert
    expect(card).toMatch(/background:\s*var\(--card\)/);
    expect(userBubble).toMatch(/background:\s*var\(--user\)/);
  });

  it("lights the interrupt badge in the in-flight orange, since it preempts the turn", () => {
    // Arrange / Act — the escalating verdict borrows the app's "about to act" colour.
    // Assert
    expect(interruptBadge).toMatch(/color:\s*var\(--thinking\)/);
  });
});

const qOpt = blockAfter(css, ".q-opt {");
const qChip = blockAfter(css, ".q-chip");
const qText = blockAfter(css, ".q-text {");
const baseBadge = blockAfter(css, ".badge {");
const pendingQuestion = blockAfter(css, ".permission.pending.question");
const permissionBase = blockAfter(css, ".permission {");

/** The `color-mix(in srgb, var(--A) N%, var(--B))` declaration bound to `name`. */
function mixDecl(block: string, name: string): { mix: string; pct: number; base: string } {
  const hit = block.match(
    new RegExp(`${name}:\\s*color-mix\\(in srgb,\\s*var\\((--[a-z-]+)\\)\\s*(\\d+)%,\\s*var\\((--[a-z-]+)\\)\\)`, "i"),
  );
  if (!hit) throw new Error(`block has no color-mix bound to ${name}`);
  return { mix: hit[1], pct: Number(hit[2]), base: hit[3] };
}

/** Luminance of an srgb color-mix, linear in its channels, so linear in its ends. */
function mixLuminance(theme: string, decl: { mix: string; pct: number; base: string }): number {
  const p = decl.pct / 100;
  return p * luminance(token(theme, decl.mix)) + (1 - p) * luminance(token(theme, decl.base));
}

describe("AskUserQuestion option bubbles", () => {
  it("fills an option bubble with the shared picker token, not the near-invisible card grey", () => {
    // Arrange / Act — the .q-opt rule. --card sat one shade off the feed bg.
    // Assert
    expect(qOpt).toMatch(/background:\s*var\(--q-opt-bg\)/);
    expect(qOpt).not.toMatch(/background:\s*var\(--card\)/);
  });

  it("mixes the bubble token from the fg into the card, so the lift stays neutral", () => {
    // Arrange / Act — a --fg mix, never an --accent one, so it never competes
    // with the blue wash a SELECTED option takes.
    const decl = mixDecl(lightTheme, "--q-opt-bg");
    // Assert
    expect([decl.mix, decl.base]).toEqual(["--fg", "--card"]);
  });

  it("lifts the dark-theme bubble lighter than the card it started from", () => {
    // Arrange — the "make the bubbles lighter" ask, read in the dark theme.
    const decl = mixDecl(lightTheme, "--q-opt-bg");
    // Act
    const lifted = mixLuminance(darkTheme, decl) > luminance(token(darkTheme, "--card"));
    // Assert
    expect(lifted).toBe(true);
  });

  it("stands the dark-theme bubble further off the section bg than the old card did", () => {
    // Arrange
    const decl = mixDecl(lightTheme, "--q-opt-bg");
    const bg = luminance(token(darkTheme, "--bg"));
    // Act
    const bubbleSep = Math.abs(mixLuminance(darkTheme, decl) - bg);
    const cardSep = Math.abs(luminance(token(darkTheme, "--card")) - bg);
    // Assert
    expect(bubbleSep).toBeGreaterThan(cardSep);
  });

  it("stands the light-theme bubble further off the white section bg than the old card did", () => {
    // Arrange — on white the same mix lands darker, which is what raises contrast.
    const decl = mixDecl(lightTheme, "--q-opt-bg");
    const bg = luminance(token(lightTheme, "--bg"));
    // Act
    const bubbleSep = Math.abs(mixLuminance(lightTheme, decl) - bg);
    const cardSep = Math.abs(luminance(token(lightTheme, "--card")) - bg);
    // Assert
    expect(bubbleSep).toBeGreaterThan(cardSep);
  });
});

describe("AskUserQuestion question badge", () => {
  it("backs the badge with the very token the option bubbles use", () => {
    // Arrange / Act — the .q-chip rule, so the header reads as one of the chips.
    // Assert
    expect(qChip).toMatch(/background:\s*var\(--q-opt-bg\)/);
  });

  it("keeps the badge's blue foreground rather than recoloring the abbreviation", () => {
    // Arrange / Act — the blue text is the badge's identity, so only its fill moved.
    // Assert
    expect(qChip).toMatch(/color:\s*var\(--accent\)/);
  });

  it("sizes the badge 50% larger than a plain badge", () => {
    // Arrange
    const chipSize = Number(qChip.match(/font-size:\s*([\d.]+)rem/)?.[1]);
    const badgeSize = Number(baseBadge.match(/font-size:\s*([\d.]+)rem/)?.[1]);
    // Act
    const ratio = chipSize / badgeSize;
    // Assert
    expect(ratio).toBeCloseTo(1.5, 2);
  });
});

describe("AskUserQuestion question text", () => {
  it("italicizes the question so it reads as the model's own words", () => {
    // Arrange / Act — the .q-text rule.
    // Assert
    expect(qText).toMatch(/font-style:\s*italic/);
  });
});

describe("AskUserQuestion inline code", () => {
  it("joins the picker's code spans to the message-bubble chip rule", () => {
    // Arrange / Act — the shared inline-code selector now lists the picker.
    // Assert
    expect(css).toMatch(/\.md code,\s*\.q-text code,\s*\.q-opt code\s*\{/);
  });

  it("uprights code inside the italic question text", () => {
    // Arrange / Act — the picker-code reset rule; without it code inherits italic.
    // Assert
    expect(pickerCodeReset).toMatch(/font-style:\s*normal/);
  });

  it("unbolds code inside the bold option label", () => {
    // Arrange / Act — the picker-code reset rule; without it code inherits weight 600.
    // Assert
    expect(pickerCodeReset).toMatch(/font-weight:\s*normal/);
  });
});

describe("AskUserQuestion picker border", () => {
  it("rings the pending picker in the yellow token rather than the permission blue", () => {
    // Arrange / Act — the .permission.pending.question rule.
    // Assert
    expect(pendingQuestion).toMatch(/border-color:\s*var\(--q-border\)/);
  });

  it("leaves an ordinary permission prompt bordered in the accent blue", () => {
    // Arrange / Act — the base .permission rule is untouched, so allow/deny stays blue.
    // Assert
    expect(permissionBase).toMatch(/border:\s*1px\s+solid\s+var\(--accent\)/);
  });

  it("scopes the yellow to the pending picker, so a resolved one keeps its settled grey", () => {
    // Arrange / Act — the selector must carry .pending, or a resolved picker would
    // be re-yellowed over the muted grey it settles to.
    // Assert
    expect(css).toMatch(/\.permission\.pending\.question\s*{/);
  });

  it("defines a yellow border token for the light theme", () => {
    // Arrange / Act
    const yellow = isYellow(token(lightTheme, "--q-border"));
    // Assert
    expect(yellow).toBe(true);
  });

  it("defines a yellow border token for the dark theme", () => {
    // Arrange / Act
    const yellow = isYellow(token(darkTheme, "--q-border"));
    // Assert
    expect(yellow).toBe(true);
  });

  it("brightens the dark-theme border token above the light-theme one, as every wash does", () => {
    // Arrange
    const [dark, light] = [token(darkTheme, "--q-border"), token(lightTheme, "--q-border")];
    // Act
    const brighter = luminance(dark) > luminance(light);
    // Assert
    expect(brighter).toBe(true);
  });
});

describe("chess-game container", () => {
  const chessGame = blockAfter(css, ".chess-game {");
  const chessError = blockAfter(css, ".chess-game-error");

  it("maps the widget theme variables onto the webapp palette", () => {
    // Arrange / Act — the .chess-game rule.
    // Assert
    expect(chessGame).toMatch(/--chess-widget-bg:\s*var\(--card\)/);
    expect(chessGame).toMatch(/--chess-widget-fg:\s*var\(--fg\)/);
    expect(chessGame).toMatch(/--chess-widget-accent:\s*var\(--accent\)/);
  });

  it("styles the in-frame error readably", () => {
    // Arrange / Act / Assert
    expect(chessError).toMatch(/color:/);
    expect(chessError).toMatch(/border:/);
  });
});

/**
 * The cycle's current-bubble marker. Focus stays in the input box while
 * cycling, so this bar is the feed's only "you are here" signal — and it
 * must not shift layout, or every step would reflow the feed under the
 * user's eyes.
 */
describe("the nav marker", () => {
  it("marks the cycled bubble under the class nav.ts applies", () => {
    expect(css).toContain(`.feed-item.${NAV_CURRENT_CLASS}`);
  });

  it("draws the marker inside the wrapper's own box, so a step cannot reflow the feed", () => {
    // An inset shadow paints within the existing box; a real border or any
    // spacing would grow it, nudging every bubble below on each step.
    // (border-radius is exempt: it rounds the box without widening it.)
    const body = blockAfter(css, `.feed-item.${NAV_CURRENT_CLASS}`);
    expect(body).toMatch(/box-shadow:\s*inset/);
    expect(body).not.toMatch(/\bborder(-(top|right|bottom|left|width|style))?\s*:/);
    expect(body).not.toMatch(/\b(padding|margin)/);
  });
});

/* The tokens breakdown hangs off the same dropdown anchoring as the rosters
   (its stems join their shared rules), but its rows are stat pairs, not
   roster entries — the value alignment is its own contract. */
// The leading newline pins the marker to the chip's own color rule: the
// strip's scoped rule (".topbar-info .info-tokens") would otherwise match
// first, since it precedes the chips section.
const tokensToggle = blockAfter(css, "\n.info-tokens { color");
const tokensValue = blockAfter(css, ".tokens-value {");
const tokensSubRow = blockAfter(css, ".tokens-row.sub {");

describe("tokens dropdown styles", () => {
  it("anchors the overlay on the tokens menu that drops it", () => {
    // Arrange / Act — the shared dropdown rule must name the tokens stem.
    // Assert
    expect(css).toMatch(/\.agents-menu, \.tasks-menu, \.tokens-menu \{/);
  });

  it("lifts the tokens overlay out of the topbar's flex row", () => {
    // Arrange / Act — the shared overlay rule must name the tokens stem.
    // Assert
    expect(css).toMatch(/\.agents-overlay, \.tasks-overlay, \.tokens-overlay \{/);
  });

  it("renders the chip as a pointer target so it reads as pressable", () => {
    // Arrange / Act — the shared chip reset must name the tokens chip.
    // Assert
    expect(css).toMatch(/\.info-agents, \.info-tasks, \.info-tokens \{/);
    expect(chipReset).toMatch(/cursor:\s*pointer/);
  });

  it("keeps the chip on its established datapoint token", () => {
    // Arrange / Act — the figure's color survives the span-to-button move.
    // Assert
    expect(tokensToggle).toMatch(/color:\s*var\(--info-tokens\)/);
  });

  it("right-aligns the stat values off the labels", () => {
    // Arrange / Act — margin-left auto in the row's flex is the alignment.
    // Assert
    expect(tokensValue).toMatch(/margin-left:\s*auto/);
  });

  it("sets the stat figures in tabular digits so columns of them align", () => {
    // Arrange / Act
    // Assert
    expect(tokensValue).toMatch(/font-variant-numeric:\s*tabular-nums/);
  });

  it("indents a total's constituent rows beneath it", () => {
    // Arrange / Act — the sub rows read as children of the input total.
    // Assert
    expect(tokensSubRow).toMatch(/padding-left:/);
  });
});

describe("feed search marks", () => {
  const match = blockAfter(css, `.${MARK_CLASS} {`);
  const current = blockAfter(css, `.${CURRENT_CLASS} {`);
  const status = blockAfter(css, "#search-status {");

  it("styles every match under the class search.ts injects", () => {
    // Arrange / Act / Assert
    expect(match).toMatch(/background:\s*var\(--search-match\)/);
  });

  it("pins the marked text's color, so a highlighted token keeps its contrast", () => {
    // A mark lands inside .hljs-* spans whose color would otherwise carry the
    // text right across the wash.
    // Arrange / Act / Assert
    expect(match).toMatch(/color:\s*var\(--search-fg\)/);
  });

  it("washes the current match differently from its neighbours", () => {
    // Arrange / Act / Assert
    expect(current).toMatch(/background:\s*var\(--search-current\)/);
  });

  it("outranks the other matches by source order rather than specificity", () => {
    // Both classes ride the same element, so the later rule is the one that
    // wins — the discipline .expanded uses over the cap rules.
    // Arrange / Act
    const at = css.indexOf(`.${MARK_CLASS} {`);
    // Assert
    expect(css.indexOf(`.${CURRENT_CLASS} {`)).toBeGreaterThan(at);
  });

  it("beats the plain match wash without reaching for !important", () => {
    // Arrange / Act / Assert
    expect(current).not.toContain("!important");
  });

  it("collapses the status line until a search puts text in it", () => {
    // Arrange / Act / Assert
    expect(status).toMatch(/display:\s*none/);
    expect(css).toMatch(/#search-status\.on\s*\{\s*display:\s*block/);
  });

  it("lifts a revealed section's cap under the search's own class", () => {
    // Arrange / Act
    const revealed = blockAfter(css, `.${REVEAL_CLASS} {`);
    // Assert
    expect(revealed).toMatch(/max-height:\s*none/);
  });

  it("reveals the display folds the search opens, not just the capped ones", () => {
    // A subagent's whole prompt (.agent-json) and a tool's raw input JSON
    // (.folded-json) are display:none, not merely capped.
    // Arrange / Act / Assert
    expect(css).toMatch(/\.agent-input\.search-revealed \.agent-json/);
    expect(css).toMatch(/\.folded-input\.search-revealed \.folded-json/);
  });

  it("lifts the cap by source order rather than !important", () => {
    // Arrange / Act
    const lastCap = Math.max(...CAPPED_CLASSES.map((c) => css.lastIndexOf(`.${c} {`)));
    const revealed = blockAfter(css, `.${REVEAL_CLASS} {`);
    // Assert
    expect(css.indexOf(`.${REVEAL_CLASS} {`)).toBeGreaterThan(lastCap);
    expect(revealed).not.toContain("!important");
  });

  it("keeps the current match's wash brighter than the page it sits on", () => {
    // The mark is the thing being hunted for, so it is the one place the
    // stylesheet shouts against the background rather than deferring to it.
    // Arrange / Act
    const dark = blockAfter(blockAfter(css, "@media (prefers-color-scheme: dark)"), ":root");
    // Assert
    expect(luminance(token(dark, "--search-current"))).toBeGreaterThan(
      luminance(token(dark, "--bg")),
    );
  });

  it("washes the current match louder than the others in the dark theme too", () => {
    // Arrange / Act
    const dark = blockAfter(blockAfter(css, "@media (prefers-color-scheme: dark)"), ":root");
    // Assert — orange sits below yellow in perceived lightness, which is the
    // gap that makes the current match findable among a screen of hits.
    expect(luminance(token(dark, "--search-current"))).not.toBe(
      luminance(token(dark, "--search-match")),
    );
  });
});
