/**
 * THE STYLESHEET'S SELECTABILITY CONTRACT.
 *
 * Owner ruling, 2026-09-14: "ensure that everything in the webapp (all text)
 * can be copied to clipboard; I need to be able to do this to easily relay
 * errors." A reader relays an error by dragging across it and pressing Cmd-C,
 * and the one thing in this codebase that can take that away wholesale is a
 * `user-select: none` rule. Two of them used to: the workspaces rail's blanket
 * rule and the thinking fold's summary.
 *
 * So the rule is inverted here: `user-select: none` is FORBIDDEN unless its
 * selector is named below with a reason. The allowlist admits pure controls
 * whose glyph is a state, never a sentence — a chevron, a fold triangle, a
 * disclosure marker. Adding a selector to it is a decision someone makes
 * deliberately, in one line, rather than a rule that quietly lands in a 5000
 * line file.
 */
import { describe, expect, it } from "vitest";
import stylesheet from "../src/styles.css?raw";

/**
 * Selectors permitted to suppress selection, each with the one reason that
 * earns it: the element's text is a control's state, not information.
 */
const ALLOWED: ReadonlyMap<string, string> = new Map([
  ["#ws-sidebar [data-section-fold]", "the section fold triangle: '▸'/'▾' is the fold's state"],
  ["#ws-sidebar .repo-head .tri", "the repo header's fold triangle, the same glyph and the same state"],
  [".thinking summary::marker", "the disclosure marker only; the summary's TEXT stays user-select: text"],
]);

interface Rule {
  selectors: string[];
  declarations: string;
}

/** Every `selector { declarations }` pair in the file, comments stripped. */
function rulesOf(css: string): Rule[] {
  const withoutComments = css.replace(/\/\*[\s\S]*?\*\//g, "");
  const rules: Rule[] = [];
  const pattern = /([^{}]+)\{([^{}]*)\}/g;
  let match = pattern.exec(withoutComments);
  while (match !== null) {
    rules.push({
      selectors: (match[1] ?? "").split(",").map((one) => one.trim()).filter((one) => one !== ""),
      declarations: match[2] ?? "",
    });
    match = pattern.exec(withoutComments);
  }
  return rules;
}

/** The selectors on which any `user-select`/`-webkit-user-select` is `none`. */
function selectorsSuppressingSelection(css: string): string[] {
  const suppressing: string[] = [];
  for (const rule of rulesOf(css)) {
    if (!/(?:^|[\s;])-?(?:webkit-)?user-select\s*:\s*none/.test(rule.declarations)) continue;
    suppressing.push(...rule.selectors);
  }
  return suppressing;
}

describe("the stylesheet's selectability contract", () => {
  it("suppresses selection only on allowlisted pure controls", () => {
    // Arrange / Act
    const suppressing = selectorsSuppressingSelection(stylesheet);

    // Assert
    const unexplained = suppressing.filter((selector) => !ALLOWED.has(selector));
    expect(unexplained).toEqual([]);
  });

  it("leaves the thinking fold's summary text selectable", () => {
    // Arrange / Act
    const summaryRule = rulesOf(stylesheet.replace(/\/\*[\s\S]*?\*\//g, "")).find((rule) =>
      rule.selectors.includes(".thinking summary"),
    );

    // Assert
    expect(summaryRule?.declarations).toMatch(/user-select\s*:\s*text/);
  });

  it("lets the topbar's objective line take pointer events, so it can be dragged over", () => {
    // Arrange / Act
    const taskSummary = rulesOf(stylesheet).find((rule) => rule.selectors.includes("#task-summary"));

    // Assert
    expect(taskSummary?.declarations).not.toMatch(/pointer-events\s*:\s*none/);
  });

  it("keeps every allowlisted selector in use, so the allowlist cannot rot", () => {
    // Arrange / Act
    const suppressing = new Set(selectorsSuppressingSelection(stylesheet));

    // Assert
    expect([...ALLOWED.keys()].filter((selector) => !suppressing.has(selector))).toEqual([]);
  });
});

/**
 * THE BUBBLE GEOMETRY (owner ruling, 2026-09-14).
 *
 * "make the max width of response bubbles about 10% wider, and also make the
 * max response bubble length about 10% longer. Also ensure the scrollbar of the
 * response bubble (and all bubbles, in fact) abuts (or close to) the right hand
 * side of the bubble (currently there's quite a bit of gap), and starts UNDER
 * the token count / metadata in the top right corner of the bubble."
 *
 * Plus the owner's addition: the bar must be VISIBLE whenever the box overflows
 * and absent when it does not — the platform's hover-only overlay bar tells a
 * reader nothing about whether the answer continues below the fold.
 *
 * These read the file's TEXT rather than a computed style: every figure here is
 * written with `var()` and `calc()`, which jsdom hands back unresolved, so the
 * declaration is the only thing an assertion can honestly pin.
 */
/** The declarations of the FIRST rule whose selector list contains SELECTOR. */
function declarationsOf(selector: string): string | undefined {
  return rulesOf(stylesheet).find((rule) => rule.selectors.includes(selector))?.declarations;
}

/** Every scroll box the ruling names, by the selector the sheet caps it with. */
const SCROLL_BOXES: readonly string[] = [
  ".bubble > .bubble-scroll",
  ".tool-input",
  ".tool-output",
  ".tool-read-output",
  ".bash-input",
  ".bash-output",
  ".diff-output",
  ".skill-input",
  ".skill-content",
  ".fold-fixed > .agent-panel",
];

describe("the bubble geometry: the two caps", () => {
  it("widens the agent column cap by exactly ten percent, 75% to 82.5%", () => {
    // Arrange / Act
    const column = declarationsOf("#main-col");

    // Assert
    expect(column).toMatch(/--agent-bubble-cap:\s*82\.5%/);
  });

  it("lengthens the shared visible-line budget by exactly ten percent, 25 to 27.5", () => {
    // Arrange / Act
    const root = declarationsOf(":root");

    // Assert
    expect(root).toMatch(/--feed-cap-lines:\s*27\.5\s*;/);
  });

  it("caps the purple response bubble 15% below the shared cap, and only its max width", () => {
    // Arrange / Act
    const bubble = declarationsOf(".bubble.assistant");

    // Assert — 77% (owner ruling 2026-09-15); margin still biases off the unscaled cap.
    expect(bubble).toMatch(/max-width:\s*77%/);
    expect(bubble).toMatch(/margin-left:\s*calc\(\(100% - var\(--agent-bubble-cap\)\) \/ 2\)/);
  });

  it("caps the blue prompt bubble 15% below its prior 60%, and only its max width", () => {
    // Arrange / Act
    const bubble = declarationsOf(".bubble.user");

    // Assert — 77% (owner ruling 2026-09-15); margin still biases off the unscaled cap.
    expect(bubble).toMatch(/max-width:\s*77%/);
    expect(bubble).toMatch(/margin-right:\s*calc\(\(100% - var\(--agent-bubble-cap\)\) \/ 2\)/);
  });

  it("leaves no second copy of the old 75% cap behind", () => {
    // Arrange / Act / Assert
    expect(stylesheet.replace(/\/\*[\s\S]*?\*\//g, "")).not.toMatch(
      /--agent-bubble-cap:\s*75%/,
    );
  });

  it("leaves no second copy of the old 25-line budget behind", () => {
    // Arrange / Act / Assert
    expect(stylesheet.replace(/\/\*[\s\S]*?\*\//g, "")).not.toMatch(
      /--feed-cap-lines:\s*25\s*;/,
    );
  });
});

describe("the bubble geometry: the scrollbar on the inner edge", () => {
  it("drops the bubble's own right inset to the shared gap, so the box reaches the edge", () => {
    // Arrange / Act
    const bubble = declarationsOf(".bubble");

    // Assert
    expect(bubble).toMatch(/padding:\s*0\.6rem\s+var\(--bubble-scroll-gap\)\s+0\.6rem\s+0\.9rem/);
  });

  it("keeps the gap itself hairline, so 'abuts' is a promise the token can keep", () => {
    // Arrange / Act
    const column = declarationsOf("#main-col");

    // Assert
    expect(column).toMatch(/--bubble-scroll-gap:\s*2px/);
  });

  it("gives the bubble's scroll box no horizontal padding of its own", () => {
    // Arrange / Act
    const scroll = declarationsOf(".bubble > .bubble-scroll");

    // Assert
    expect(scroll).toMatch(/padding-left:\s*0\s*;[\s\S]*padding-right:\s*0\s*;/);
  });

  it("hands the inset the bubble gave up to the CONTENT wrapper inside the box", () => {
    // Arrange / Act
    const body = declarationsOf(".bubble-body");

    // Assert
    expect(body).toMatch(/padding-right:\s*calc\(0\.9rem - var\(--bubble-scroll-gap\)\)/);
  });

  it("steps a tool card's scroll boxes out of the card's own inset", () => {
    // Arrange / Act
    const boxes = declarationsOf(".tool-card > .tool-output");

    // Assert
    expect(boxes).toMatch(/margin-right:\s*calc\(var\(--bubble-scroll-gap\) - 0\.75rem\)/);
  });

  it("caps the SCROLL BOX rather than the content wrapper, so the strip cannot scroll away", () => {
    // Arrange / Act
    const capped = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.includes(".bubble > .bubble-scroll"),
    );

    // Assert
    expect(capped.some((rule) => /max-height:\s*calc\(var\(--cap-lines\)/.test(rule.declarations))).toBe(
      true,
    );
  });

  it("no longer caps the bubble body, which is now the content wrapper", () => {
    // Arrange / Act / Assert
    expect(rulesOf(stylesheet).some((rule) => rule.selectors.includes(".bubble > .bubble-body"))).toBe(
      false,
    );
  });

  it("retires the prompt bubble's one-cell grid, which put the stamp BESIDE the box", () => {
    // Arrange / Act
    const prompt = declarationsOf(".bubble.user");

    // Assert
    expect(prompt).not.toMatch(/display:\s*grid/);
  });

  it("stacks every bubble, so the metadata strip sits over the scroll box", () => {
    // Arrange / Act
    const bubble = declarationsOf(".bubble");

    // Assert
    expect(bubble).toMatch(/flex-direction:\s*column/);
  });
});

describe("the bubble geometry: a scrollbar that is there whenever it can scroll", () => {
  it.each(SCROLL_BOXES)("keeps %s on overflow-y: auto, never scroll", (selector) => {
    // Arrange / Act
    const capping = rulesOf(stylesheet).find(
      (rule) => rule.selectors.includes(selector) && /overflow-y/.test(rule.declarations),
    );

    // Assert
    expect(capping?.declarations).toMatch(/overflow-y:\s*auto\s*;/);
  });

  it.each(SCROLL_BOXES)("paints a persistent bar on %s by sizing its scrollbar", (selector) => {
    // Arrange / Act
    const sized = rulesOf(stylesheet).find((rule) =>
      rule.selectors.includes(`${selector}::-webkit-scrollbar`),
    );

    // Assert
    expect(sized?.declarations).toMatch(/width:\s*8px/);
  });

  it.each(SCROLL_BOXES)("gives %s a track in the existing border token", (selector) => {
    // Arrange / Act
    const track = rulesOf(stylesheet).find((rule) =>
      rule.selectors.includes(`${selector}::-webkit-scrollbar-track`),
    );

    // Assert
    expect(track?.declarations).toMatch(/background:\s*var\(--border\)/);
  });

  it.each(SCROLL_BOXES)("gives %s a thumb in the existing muted token", (selector) => {
    // Arrange / Act
    const thumb = rulesOf(stylesheet).find((rule) =>
      rule.selectors.includes(`${selector}::-webkit-scrollbar-thumb`),
    );

    // Assert
    expect(thumb?.declarations).toMatch(/background:\s*var\(--muted\)/);
  });

  it("never parks a dead channel on a box that fits, which overflow-y: scroll would", () => {
    // Arrange / Act / Assert
    expect(stylesheet.replace(/\/\*[\s\S]*?\*\//g, "")).not.toMatch(/overflow-y:\s*scroll/);
  });
});

/**
 * THE FOOTER STATUS WAVE'S STYLESHEET CONTRACT.
 *
 * Three facts live only in the file — the letters never re-measure, the wave
 * runs at the period the inline delays are computed against, and a reader who
 * asked for reduced motion gets none of it — so they are asserted here rather
 * than in the drawing suite, which can only see classes.
 */
describe("the footer status wave's stylesheet contract", () => {
  /** The declarations of the rule whose selector list contains SELECTOR. */
  function ruleFor(css: string, selector: string): string | undefined {
    return rulesOf(css).find((rule) => rule.selectors.includes(selector))?.declarations;
  }

  /**
   * The `prefers-reduced-motion: reduce` block, and the file without it.
   *
   * Split by brace matching rather than by a slice, because the same selector
   * appears on both sides: a plain `indexOf` walk would hand the base rule's
   * lookup the override's declarations, which is exactly backwards.
   */
  function reducedMotionSplit(): { block: string; rest: string } {
    const at = stylesheet.indexOf("@media (prefers-reduced-motion: reduce)");
    if (at === -1) throw new Error("no reduced-motion block in the stylesheet");
    const open = stylesheet.indexOf("{", at);
    let depth = 0;
    for (let cursor = open; cursor < stylesheet.length; cursor += 1) {
      if (stylesheet[cursor] === "{") depth += 1;
      if (stylesheet[cursor] === "}") depth -= 1;
      if (depth === 0) {
        return {
          block: stylesheet.slice(open + 1, cursor),
          rest: stylesheet.slice(0, at) + stylesheet.slice(cursor + 1),
        };
      }
    }
    throw new Error("the reduced-motion block is never closed");
  }

  /** The reduced-motion override's body. */
  function reducedMotionBlock(): string {
    return reducedMotionSplit().block;
  }

  /** The stylesheet with the reduced-motion overrides taken out. */
  function baseSheet(): string {
    return reducedMotionSplit().rest;
  }

  /**
   * The full body of the `@keyframes pfooter-status-color` block. The spectrum
   * sweep has many stops, so the body is captured up to the closing brace on its
   * own line rather than by the two-stop pattern the lighten keyframe used.
   */
  function colorKeyframeBody(): string {
    return /@keyframes pfooter-status-color \{([\s\S]*?)\n\}/.exec(stylesheet)?.[1] ?? "";
  }

  it("scales the letter rather than resizing it, so the word's width never moves", () => {
    // Arrange / Act
    const keyframes = /@keyframes pfooter-status-wave \{([^}]*\}[^}]*)\}/.exec(stylesheet)?.[1];

    // Assert — 1.18, nudged up from 1.15 (owner ruling, 2026-09-14).
    expect(keyframes).toMatch(/transform:\s*scale\(1\.18\)/);
  });

  it("never touches font-size, which would re-lay the whole strip out", () => {
    // Arrange / Act
    const declarations = ruleFor(baseSheet(), ".pfooter-wave-letter") ?? "";

    // Assert
    expect(declarations).not.toMatch(/font-size/);
  });

  it("makes the letter an inline-block, which is what lets a transform apply", () => {
    // Arrange / Act
    const declarations = ruleFor(baseSheet(), ".pfooter-wave-letter") ?? "";

    // Assert
    expect(declarations).toMatch(/display:\s*inline-block/);
  });

  it("alternates the pass, so the bulge travels forwards and then backwards", () => {
    // Arrange / Act
    const declarations = ruleFor(baseSheet(), ".pfooter-wave-letter") ?? "";

    // Assert
    expect(declarations).toMatch(/animation:\s*pfooter-status-wave 2\.6s ease-in-out infinite alternate/);
  });

  it("promotes no layer, so WebKit rasterises the glyph at the scale it paints", () => {
    // Arrange / Act
    const declarations = ruleFor(baseSheet(), ".pfooter-wave-letter") ?? "";

    // Assert
    expect(declarations).not.toMatch(/will-change|translateZ|backface-visibility/);
  });

  it("declares no animation-delay, which every letter overrides inline", () => {
    // Arrange / Act
    const declarations = ruleFor(baseSheet(), ".pfooter-wave-letter") ?? "";

    // Assert
    expect(declarations).not.toMatch(/animation-delay/);
  });

  it("stops the wave outright under prefers-reduced-motion", () => {
    // Arrange / Act
    const declarations = ruleFor(reducedMotionBlock(), ".pfooter-wave-letter") ?? "";

    // Assert
    expect(declarations).toMatch(/animation:\s*none/);
  });

  it("runs a per-letter colour sweep alongside the bulge, on the same letter", () => {
    // Arrange / Act
    const declarations = ruleFor(baseSheet(), ".pfooter-wave-letter") ?? "";

    // Assert — the colour animation rides the SAME shorthand as the bulge.
    expect(declarations).toMatch(/pfooter-status-color 2\.6s ease-in-out infinite alternate/);
  });

  it("runs the colour sweep at the scale wave's exact 2.6s period, so the two are locked", () => {
    // Arrange / Act — both names carry the same duration in the one shorthand.
    const declarations = ruleFor(baseSheet(), ".pfooter-wave-letter") ?? "";
    const durations = [...declarations.matchAll(/pfooter-status-\w+ (\d\.\d+)s/g)].map((m) => m[1]);

    // Assert — the bulge and the sweep share one duration.
    expect(durations).toEqual(["2.6", "2.6"]);
  });

  it("rests each letter at its arm tone (currentColor), so a letter is never transparent", () => {
    // Arrange / Act
    const keyframes = colorKeyframeBody();

    // Assert — the 0% stop is the inherited arm colour, legible at rest, so a
    // letter caught stopped shows the arm tone rather than a mid-spectrum hue.
    expect(keyframes).toMatch(/0%\s*\{\s*color:\s*currentColor/);
  });

  it("returns to the arm tone at the end of each pass, not to a random hue", () => {
    // Arrange / Act
    const keyframes = colorKeyframeBody();

    // Assert — the 100% stop is currentColor too, so `alternate` departs from
    // and returns to the arm tone rather than snapping between two hues.
    expect(keyframes).toMatch(/100%\s*\{\s*color:\s*currentColor/);
  });

  it("sweeps the full spectrum, not a single-hue lighten", () => {
    // Arrange / Act — the distinct HSL hues the running sweep steps through.
    const keyframes = colorKeyframeBody();
    const hues = new Set([...keyframes.matchAll(/hsl\((\d+)\s/g)].map((m) => Number(m[1])));

    // Assert — a rainbow of many hues (red..violet), never the old lone lighten.
    expect(keyframes).not.toMatch(/color-mix/);
    expect(hues.size).toBeGreaterThanOrEqual(5);
  });

  it("keeps every keyframe stop a fully opaque colour, never transparent", () => {
    // Arrange / Act — every stop is currentColor or an opaque hsl(), no alpha.
    const keyframes = colorKeyframeBody();
    const colors = [...keyframes.matchAll(/color:\s*([^;]+);/g)].map((m) => m[1].trim());

    // Assert
    expect(colors.length).toBeGreaterThan(0);
    for (const colour of colors) {
      expect(colour === "currentColor" || /^hsl\(\d+ \d+% \d+%\)$/.test(colour)).toBe(true);
    }
  });

  it("never clips a gradient to the word's text on the status-word path (the bug that shipped)", () => {
    // Arrange / Act — the whole letter/word path, base and reduced-motion alike.
    const word = ruleFor(stylesheet, ".pfooter-status-word") ?? "";
    const letter = ruleFor(stylesheet, ".pfooter-wave-letter") ?? "";
    const colorKeyframes = colorKeyframeBody();

    // Assert — none of background-clip:text, transparent text-fill, or color:transparent.
    for (const path of [word, letter, colorKeyframes]) {
      expect(path).not.toMatch(/background-clip:\s*text/);
      expect(path).not.toMatch(/-webkit-text-fill-color:\s*transparent/);
      expect(path).not.toMatch(/color:\s*transparent/);
    }
  });

  it("carries no whole-word gradient class at all, so the blanking bug cannot recur", () => {
    // Arrange / Act / Assert — the reverted approach's class must not exist.
    expect(stylesheet).not.toMatch(/pfooter-status-gradient/);
  });

  it("leaves the letter its legible arm colour under reduced motion, not a transparent fill", () => {
    // Arrange / Act
    const declarations = ruleFor(reducedMotionBlock(), ".pfooter-wave-letter") ?? "";

    // Assert — stopping the animation must not park a transparent colour.
    expect(declarations).not.toMatch(/color:\s*transparent/);
    expect(declarations).not.toMatch(/-webkit-text-fill-color:\s*transparent/);
  });

  it("sets no static colour on the letter, so a stopped letter falls back to the arm tone", () => {
    // Arrange / Act — the base rule carries only the animation, no fixed colour;
    // with the animation stopped (reduced motion, or seeked to rest) the letter
    // then inherits currentColor — the legible arm tone — never a keyframe hue.
    const declarations = ruleFor(baseSheet(), ".pfooter-wave-letter") ?? "";

    // Assert
    expect(declarations).not.toMatch(/(?:^|;|\{)\s*color:/);
  });
});

describe("the cost corner's hover hit area", () => {
  it("enlarges the hover region with padding and cancels it with an equal negative margin", () => {
    // Arrange / Act
    const corner = declarationsOf(".usage-corner");

    // Assert — the padding grows the hoverable box (roughly 2x wide, 2x tall);
    // the equal, opposite negative margin keeps the token figure in place and
    // shifts no neighbor.
    expect(corner).toMatch(/padding:\s*0\.4rem\s+1\.25rem/);
    expect(corner).toMatch(/margin:\s*-0\.4rem\s+-1\.25rem/);
  });

  it("reveals the duration off a hover anywhere in the bubble, not only the small corner", () => {
    // Arrange / Act — hovering the small corner used to put the cursor right
    // on top of the timestamp it had just revealed. Keying the reveal off the
    // whole bubble means most hover positions never sit near the duration.
    const bubbleWide = declarationsOf(".bubble.assistant:hover .usage-ago");

    // Assert
    expect(bubbleWide).toMatch(/max-width:\s*8rem/);
    expect(bubbleWide).toMatch(/opacity:\s*1/);
  });

  it("keeps the reveal on keyboard focus anywhere in the bubble, not only the corner", () => {
    // Arrange / Act
    const bubbleFocus = declarationsOf(".bubble.assistant:focus-within .usage-ago");

    // Assert
    expect(bubbleFocus).toMatch(/max-width:\s*8rem/);
  });
})

/**
 * THE PROMPT BUBBLE'S IN-FLIGHT BORDER (owner ruling, 2026-09-15).
 *
 * The border must appear exactly when the thinking glimmer starts and
 * disappear exactly when it ends, so it is keyed on the SAME
 * `data-wave="working"` attribute the glimmer itself reads (see
 * `startPromptWave` / `markPromptWave` in breathing.ts / feed-view.ts) —
 * never a separate class or a second JS toggle, since two independent
 * togglers is exactly what could drift apart.
 */
describe("the prompt bubble's in-flight border", () => {
  /** The raw text of the `@media (prefers-color-scheme: dark)` block, found
   * by balancing braces from its opening `{` — `rulesOf`'s flat scan cannot
   * tell a media block's own `:root` apart from the top-level one, so a dark
   * -theme override is asserted against this substring instead. */
  function darkThemeBlock(): string {
    const start = stylesheet.indexOf("@media (prefers-color-scheme: dark)");
    if (start === -1) throw new Error("no dark-theme media query found");
    const openBrace = stylesheet.indexOf("{", start);
    let depth = 0;
    let i = openBrace;
    for (; i < stylesheet.length; i++) {
      if (stylesheet[i] === "{") depth++;
      else if (stylesheet[i] === "}") {
        depth--;
        if (depth === 0) break;
      }
    }
    return stylesheet.slice(openBrace + 1, i);
  }

  it("reserves a 0.3px transparent border on every bubble, prompt included", () => {
    // Arrange / Act
    const bubble = declarationsOf(".bubble");

    // Assert
    expect(bubble).toMatch(/border:\s*0\.3px solid transparent/);
  });

  it("defines the light-purple token in the light theme", () => {
    // Arrange / Act
    const root = declarationsOf(":root");

    // Assert
    expect(root).toMatch(/--prompt-live-border:\s*#[0-9a-fA-F]{3,6}/);
  });

  it("redefines the token for the dark theme", () => {
    // Arrange / Act
    const dark = darkThemeBlock();

    // Assert
    expect(dark).toMatch(/--prompt-live-border:\s*#[0-9a-fA-F]{3,6}/);
  });

  it("colors the border with the token only while data-wave is working", () => {
    // Arrange / Act — the wave gradient and the border-color live in separate
    // rules on the same selector, so every rule on it is checked rather than
    // just the first `rulesOf` finds.
    const waving = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.includes('.bubble.user[data-wave="working"]'),
    );

    // Assert
    expect(
      waving.some((rule) => /border-color:\s*var\(--prompt-live-border\)/.test(rule.declarations)),
    ).toBe(true);
  });

  it("sets no border-color on the settled (non-waving) prompt bubble", () => {
    // Arrange / Act — the settled bubble only gets the base rule's
    // transparent reservation; nothing recolors it back to --prompt-live-border.
    const settled = declarationsOf(".bubble.user");

    // Assert
    expect(settled).not.toMatch(/border-color/);
  });
});

describe("the thinking bubble", () => {
  it("draws the thinking bubble non-bordered", () => {
    // Arrange / Act
    const rule = declarationsOf(".bubble.assistant.thinking-bubble");

    // Assert — the border is explicitly transparent, never a state color.
    expect(rule).toMatch(/border-color:\s*transparent/);
  });

  it("excludes the thinking bubble from the green final-answer rule", () => {
    // Arrange / Act — the green rule's own selector carries the exclusion.
    const rule = declarationsOf(".bubble.assistant.final-response:not(.thinking-bubble)");

    // Assert — the green declaration exists and applies only to non-thinking.
    expect(rule).toMatch(/border-color:\s*var\(--final-response\)/);
  });
});

describe("the selected-response border (reply-to-a-past-response)", () => {
  it("recolors the selected final-response bubble with the blue selection token", () => {
    // Arrange / Act
    const rule = declarationsOf(".bubble.assistant.final-response.response-selected");

    // Assert
    expect(rule).toMatch(/border-color:\s*var\(--selected-response\)/);
  });

  it("defines the --selected-response token so the blue rule resolves", () => {
    // Arrange / Act — comments are stripped, so a bare token declaration remains.
    const declared = /--selected-response:\s*#[0-9a-fA-F]{3,6}/.test(
      stylesheet.replace(/\/\*[\s\S]*?\*\//g, ""),
    );

    // Assert
    expect(declared).toBe(true);
  });

  it("declares the blue rule AFTER the green one, so the selection outranks the answer", () => {
    // Arrange — the green rule reserves the border and the blue rule replaces it;
    // equal specificity is broken by source order, so blue must come later.
    const css = stylesheet.replace(/\/\*[\s\S]*?\*\//g, "");
    // The green rule excludes thinking bubbles (`:not(.thinking-bubble)`), so
    // the concluded answer's border can never land on an intermediate reasoning
    // bubble that reuses `.bubble.assistant`.
    const green = css.indexOf(".bubble.assistant.final-response:not(.thinking-bubble) {");
    const blue = css.indexOf(".bubble.assistant.final-response.response-selected");

    // Assert
    expect(green).toBeGreaterThanOrEqual(0);
    expect(blue).toBeGreaterThan(green);
  });
});
