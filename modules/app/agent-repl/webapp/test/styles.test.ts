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
import { REVIVE_SHIMMER_PERIOD_MS } from "../src/sidebar/reviving.js";

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

/**
 * The raw text of the `@media (prefers-color-scheme: dark)` block, found by
 * balancing braces from its opening `{` — `rulesOf`'s flat scan cannot tell a
 * media block's own `:root` apart from the top-level one, so a dark-theme
 * override is asserted against this substring instead. Module scope because
 * more than one suite below pins a dark-theme token.
 */
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

/**
 * The boxes that CLIP while collapsed (owner ruling, 2026-09-15: "remove the
 * scroll from the collapsed bubble views"). Every click-to-expand capped section
 * and the response/prompt bubble's scroll box: no inner scroll while collapsed,
 * scroll revealed only once expanded. The nested subagent panel
 * (`.fold-fixed > .agent-panel`) is deliberately absent — it stays a fixed,
 * scrollable window, outside the collapse/expand model.
 */
const COLLAPSED_BOXES: readonly string[] = [
  ".bubble > .bubble-scroll",
  ".tool-input",
  ".tool-output",
  ".tool-read-output",
  ".bash-input",
  ".bash-output",
  ".diff-output",
  ".skill-input",
  ".skill-content",
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

  it("clips the bubble's horizontal axis so a bubble never side-scrolls", () => {
    // Arrange / Act
    const scroll = declarationsOf(".bubble > .bubble-scroll");

    // Assert — overflow-x is pinned to clip (not left unset, which the CSS
    // overflow spec would promote to auto once overflow-y is hidden/auto).
    expect(scroll).toMatch(/overflow-x:\s*clip\s*;/);
  });

  it("never lets the bubble scroll box compute a horizontal scrollbar", () => {
    // Arrange / Act — no overflow-x:auto/scroll anywhere on the bubble box.
    const scroll = declarationsOf(".bubble > .bubble-scroll");

    // Assert
    expect(scroll).not.toMatch(/overflow-x:\s*(auto|scroll)/);
    expect(scroll).not.toMatch(/overflow:\s*(auto|scroll)/);
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

    // Assert — FIX1 (2026-09-15): the cap rides the scroll box, now expressed as
    // min(the line budget, the 50vh ceiling) rather than the bare calc.
    expect(
      capped.some((rule) =>
        /max-height:\s*min\(calc\(var\(--cap-lines\)[\s\S]*\),\s*50vh\)/.test(rule.declarations),
      ),
    ).toBe(true);
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
  it.each(COLLAPSED_BOXES)("clips %s while collapsed, so it shows no inner scroll", (selector) => {
    // Arrange / Act — owner ruling, 2026-09-15: a collapsed section/bubble has
    // no inner scrollbar; the first overflow-y rule on the selector is the clip.
    const capping = rulesOf(stylesheet).find(
      (rule) => rule.selectors.includes(selector) && /overflow-y/.test(rule.declarations),
    );

    // Assert
    expect(capping?.declarations).toMatch(/overflow-y:\s*hidden\s*;/);
  });

  it("keeps the nested subagent panel scrollable, outside the collapse model", () => {
    // Arrange / Act — the fixed subagent activity panel is not click-to-expand,
    // so it stays a scrollable N-line window rather than a collapsed clip.
    const capping = rulesOf(stylesheet).find(
      (rule) =>
        rule.selectors.includes(".fold-fixed > .agent-panel") && /overflow-y/.test(rule.declarations),
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
 * SHRINK-TO-FIT, CAPPED AT THE MAX (owner ruling, 2026-09-15, reversing the
 * fill-cap pin of the same date).
 *
 * Every response/prompt bubble sizes to its widest RENDERED line via
 * `width: fit-content`, capped at the `max-width: 77%` ceiling — for ALL
 * bubbles, including those whose body drew a metaprompt tree. The old
 * `.bubble.bubble-fill-cap { width: 77% }` pin that stopped tree bubbles from
 * shrinking is gone, so no rule may pin any bubble's `width` to the cap.
 */
describe("the shrink-to-fit width: bubbles fit their content, capped at the max", () => {
  it("carries no `.bubble.bubble-fill-cap` rule: the full-cap width pin is gone", () => {
    // Arrange / Act
    const rule = declarationsOf(".bubble.bubble-fill-cap");

    // Assert — the pin was removed with `markFillCap`; nothing pins width to cap.
    expect(rule).toBeUndefined();
  });

  it("sizes both response and prompt bubbles to fit-content, capped at 77%", () => {
    // Arrange / Act — the assistant and prompt bubble rules.
    const assistant = declarationsOf(".bubble.assistant") ?? "";
    const user = declarationsOf(".bubble.user") ?? "";

    // Assert — fit-content up to the shared 77% cap, for both.
    expect(assistant).toMatch(/width:\s*fit-content/);
    expect(assistant).toMatch(/max-width:\s*77%/);
    expect(user).toMatch(/width:\s*fit-content/);
    expect(user).toMatch(/max-width:\s*77%/);
  });
});

/**
 * THE COLLAPSE / EXPAND HEIGHT MODEL (owner ruling, 2026-09-15).
 *
 * "remove the scroll from the collapsed bubble views. clicking bubbles to expand
 * them ... will only expand to a max size (half the height of the window), and
 * only when expanded ... is the scroll bar/scrollability revealed." So a
 * collapsed box clips (asserted in the scrollbar suite above), and `.expanded`
 * grows to at most 50vh and only then turns overflow back on.
 */
describe("the collapse/expand height model", () => {
  it("expands a capped section to at most half the viewport height", () => {
    // Arrange / Act
    const expanded = declarationsOf(".expanded");

    // Assert
    expect(expanded).toMatch(/max-height:\s*50vh/);
  });

  it("reveals scrolling only once expanded, via overflow-y auto", () => {
    // Arrange / Act
    const expanded = declarationsOf(".expanded");

    // Assert
    expect(expanded).toMatch(/overflow-y:\s*auto/);
  });

  it("retires the old expand-to-full-length model, which never revealed a bar", () => {
    // Arrange / Act
    const expanded = declarationsOf(".expanded");

    // Assert — no `max-height: none` and no `overflow-y: visible` on expand.
    expect(expanded).not.toMatch(/max-height:\s*none/);
    expect(expanded).not.toMatch(/overflow-y:\s*visible/);
  });

  it("caps an expanded response bubble at 50vh too, outranking its own cap", () => {
    // Arrange / Act — the bumped-specificity variant that beats the bubble scroll
    // box's own (0,2,0) cap and clip rules.
    const rule = declarationsOf(".bubble > .bubble-scroll.expanded");

    // Assert
    expect(rule).toMatch(/max-height:\s*50vh/);
    expect(rule).toMatch(/overflow-y:\s*auto/);
  });

  it("FIX1: caps the COLLAPSED bubble at min(the line budget, the 50vh ceiling)", () => {
    // Arrange / Act — the collapsed bubble's max-height rule (several rules carry
    // the `.bubble > .bubble-scroll` selector; the cap is the one that names it).
    const capRule = rulesOf(stylesheet).find(
      (rule) =>
        rule.selectors.includes(".bubble > .bubble-scroll") && /max-height/.test(rule.declarations),
    );

    // Assert — the same 50vh the expanded rule caps at is the ceiling here too,
    // so expanded (50vh) can never be smaller than collapsed (min(lines, 50vh)).
    expect(capRule?.declarations).toMatch(
      /max-height:\s*min\(calc\(var\(--cap-lines\) \* var\(--cap-line-h, 1\.4em\) \+ var\(--cap-extra, 0px\)\),\s*50vh\)/,
    );
  });

  it("FIX1: expresses the collapsed and expanded caps against the SAME 50vh, so expanded >= collapsed", () => {
    // Arrange / Act
    const collapsedCap = rulesOf(stylesheet).find(
      (rule) =>
        rule.selectors.includes(".bubble > .bubble-scroll") && /max-height/.test(rule.declarations),
    );
    const expanded = declarationsOf(".bubble > .bubble-scroll.expanded");

    // Assert — both caps name 50vh: collapsed = min(lines, 50vh) <= 50vh = expanded.
    expect(collapsedCap?.declarations).toMatch(/50vh/);
    expect(expanded).toMatch(/max-height:\s*50vh/);
  });
});

/**
 * THE "MORE BELOW" AFFORDANCE (owner ruling, 2026-09-15: "a signal that there's
 * more to reveal"). FIX2 draws a bottom fade + chevron on a collapsed
 * response/prompt bubble that overflows its cap, keyed entirely on `has-more`
 * (bubble-more.ts toggles the class). The signal is SCOPED to the two speaker
 * bubbles — never a tool-call section — and fades into each bubble's own bg.
 */
describe("the 'more below' affordance", () => {
  it("draws the fade from has-more, over the last 1.5em", () => {
    // Arrange / Act
    const fade = declarationsOf(".bubble.assistant > .bubble-scroll.has-more::after");

    // Assert
    expect(fade).toMatch(/height:\s*1\.5em/);
    expect(fade).toMatch(/bottom:\s*0/);
    expect(fade).toMatch(/pointer-events:\s*none/);
  });

  it("fades the assistant bubble into the assistant background token", () => {
    // Arrange / Act — the gradient rule (the selector also names a base ::after
    // rule for geometry, so pick the copy carrying the background).
    const gradient = rulesOf(stylesheet).find(
      (rule) =>
        rule.selectors.includes(".bubble.assistant > .bubble-scroll.has-more::after") &&
        /linear-gradient/.test(rule.declarations),
    );

    // Assert — dissolves into the bubble's OWN purple, not a hard edge.
    expect(gradient?.declarations).toMatch(
      /linear-gradient\(to bottom, transparent, var\(--assistant\)\)/,
    );
  });

  it("fades the user bubble into the user background token", () => {
    // Arrange / Act
    const gradient = rulesOf(stylesheet).find(
      (rule) =>
        rule.selectors.includes(".bubble.user > .bubble-scroll.has-more::after") &&
        /linear-gradient/.test(rule.declarations),
    );

    // Assert — dissolves into the bubble's OWN blue.
    expect(gradient?.declarations).toMatch(/linear-gradient\(to bottom, transparent, var\(--user\)\)/);
  });

  it("centers a chevron on the bottom edge from has-more", () => {
    // Arrange / Act
    const chevron = declarationsOf(".bubble.assistant > .bubble-scroll.has-more::before");

    // Assert — the ⌄ glyph (\2304), horizontally centered, click-through.
    expect(chevron).toMatch(/content:\s*"\\2304"/);
    expect(chevron).toMatch(/left:\s*50%/);
    expect(chevron).toMatch(/transform:\s*translateX\(-50%\)/);
    expect(chevron).toMatch(/pointer-events:\s*none/);
  });

  it("never puts the affordance on a tool-call section", () => {
    // Arrange / Act — every rule that keys on has-more.
    const withHasMore = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some((sel) => sel.includes(".has-more")),
    );

    // Assert — every such selector is scoped to a response/prompt bubble scroll
    // box, and none names a tool-call section's capped box.
    expect(withHasMore.length).toBeGreaterThan(0);
    const toolBoxes = [
      ".tool-input",
      ".tool-output",
      ".tool-read-output",
      ".bash-input",
      ".bash-output",
      ".diff-output",
      ".skill-input",
      ".skill-content",
    ];
    for (const rule of withHasMore) {
      for (const sel of rule.selectors) {
        expect(sel).toMatch(/\.bubble\.(assistant|user) > \.bubble-scroll\.has-more/);
        for (const box of toolBoxes) expect(sel.includes(box)).toBe(false);
      }
    }
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
  it("enlarges the hover region with padding and cancels it on three sides with an equal negative margin", () => {
    // Arrange / Act
    const corner = declarationsOf(".usage-corner");

    // Assert — the padding grows the hoverable box (roughly 2x wide, 2x tall).
    // Top/bottom/left cancel it exactly, keeping the token figure in place on
    // those sides and shifting no neighbor. The right margin is asserted
    // separately below — it departs from full cancellation on purpose, by
    // exactly one extra `--bubble-scroll-gap` (the corner-scoped edge gap).
    expect(corner).toMatch(/padding:\s*0\.4rem\s+1\.25rem/);
    expect(corner).toMatch(/margin-top:\s*-0\.4rem/);
    expect(corner).toMatch(/margin-bottom:\s*-0\.4rem/);
    expect(corner).toMatch(/margin-left:\s*-1\.25rem/);
  });

  it("reveals the duration off a hover anywhere in the bubble, not only the small corner", () => {
    // Arrange / Act — hovering the small corner used to put the cursor right
    // on top of the timestamp it had just revealed. Keying the reveal off the
    // whole bubble means most hover positions never sit near the duration.
    const bubbleWide = declarationsOf(".bubble.assistant:hover .usage-ago");

    // Assert — the reveal is opacity/offset only (see the constant-width test
    // below); it makes the already-reserved duration visible, it does not size it.
    expect(bubbleWide).toMatch(/opacity:\s*1/);
  });

  it("keeps the reveal on keyboard focus anywhere in the bubble, not only the corner", () => {
    // Arrange / Act
    const bubbleFocus = declarationsOf(".bubble.assistant:focus-within .usage-ago");

    // Assert
    expect(bubbleFocus).toMatch(/opacity:\s*1/);
  });

  it("floats the corner top-right so the prose's first line wraps beside it", () => {
    // Arrange / Act — the one-line-tall corner floats right inside the prose
    // body, so the FIRST prose line flows to its left and every line below it
    // (past the corner's single-row height) runs the bubble's full width.
    const corner = declarationsOf(".usage-corner");

    // Assert
    expect(corner).toMatch(/float:\s*right/);
  });

  it("reserves the duration's width even while it is collapsed, so it never sizes on reveal", () => {
    // Arrange / Act — the base rule keeps the duration's layout gap
    // (`margin-left`) whether or not it is exposed, and animates only opacity
    // and offset, so its footprint is constant and the first line cannot reflow.
    const base = declarationsOf(".usage-ago") ?? "";

    // Assert — the reserved gap is present in the base state, and no width or
    // margin is ever transitioned (the reveal touches neither).
    expect(base).toMatch(/margin-left:\s*0\.35rem/);
    expect(base).not.toMatch(/max-width/);
    expect(base).toMatch(/transition:[^;]*opacity[^;]*transform/s);
    expect(base).not.toMatch(/transition:[^;]*(?:max-width|margin)/s);
  });

  it("sizes the duration slot identically whether or not the corner is revealed", () => {
    // Arrange — the base declarations and the declarations the reveal adds.
    const base = declarationsOf(".usage-ago") ?? "";
    const revealed = declarationsOf(".usage-corner.usage-corner--revealed .usage-ago") ?? "";

    // Assert — neither state touches a width/margin property, so the corner's
    // reserved footprint is byte-for-byte the same collapsed and revealed and
    // exposing the duration cannot reflow the first prose line.
    for (const decls of [base, revealed]) {
      expect(decls).not.toMatch(/(?:^|[\s;])max-width\s*:/);
      expect(decls).not.toMatch(/(?:^|[\s;])width\s*:/);
    }
    expect(revealed).not.toMatch(/(?:^|[\s;])margin-left\s*:/);
  });

  it("renders the token figure at the same size as the revealed duration (owner ruling, 2026-09-15)", () => {
    // Arrange / Act — both read the one size declared on their shared
    // `.usage-corner` ancestor rather than each carrying its own number.
    const corner = declarationsOf(".usage-corner") ?? "";
    const stamp = declarationsOf(".usage-stamp") ?? "";
    const ago = declarationsOf(".usage-ago") ?? "";

    // Assert
    expect(corner).toMatch(/--usage-ago-font-size:\s*0\.85em/);
    expect(stamp).toMatch(/font-size:\s*var\(--usage-ago-font-size\)/);
    expect(ago).toMatch(/font-size:\s*var\(--usage-ago-font-size\)/);
  });

  it("sits the corner's right-edge gap at one --bubble-scroll-gap, twice as close as before, without touching --bubble-scroll-gap itself (owner ruling, 2026-09-15)", () => {
    // Arrange / Act
    const mainCol = declarationsOf("#main-col") ?? "";
    const corner = declarationsOf(".usage-corner") ?? "";

    // Assert — the global scrollbar-inset unit is untouched...
    expect(mainCol).toMatch(/--bubble-scroll-gap:\s*2px/);
    // ...the corner names its own edge gap as exactly one such unit (halved
    // from the previous 2x, so the token sits twice as close to the edge)...
    expect(corner).toMatch(/--usage-corner-edge-gap:\s*var\(--bubble-scroll-gap\)\s*;/);
    // ...and the corner's own right margin is the one place that departs
    // from the padding/margin cancellation (unlike top/bottom/left, asserted
    // above): it is less negative than the fully-cancelling `-1.25rem` by
    // exactly one `--bubble-scroll-gap`, which is what pulls the content the
    // extra, real, un-cancelled distance left of the flush position. Since
    // `.bubble`'s own right padding already contributes one
    // `--bubble-scroll-gap`, this second one brings the total gap from the
    // bubble's true edge to `--usage-corner-edge-gap` (2x).
    expect(corner).toMatch(
      /margin-right:\s*calc\(\s*var\(--usage-corner-edge-gap\)\s*-\s*var\(--bubble-scroll-gap\)\s*-\s*1\.25rem\s*\)/,
    );
  });

  it("keeps the corner's right-edge gap constant whether or not the duration is revealed", () => {
    // Arrange / Act — the edge gap must live ONLY on the base `.usage-corner`
    // rule and never be touched by any rule keyed on the revealed state, so
    // revealing the duration cannot change it (the no-reflow-on-hover
    // invariant extends to this gap, not only to the duration's own width).
    const revealedRules = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some((selector) => selector.includes("usage-corner--revealed")),
    );

    // Assert
    for (const rule of revealedRules) {
      expect(rule.declarations).not.toMatch(/margin-right/);
      expect(rule.declarations).not.toMatch(/--usage-corner-edge-gap/);
    }
  });

  it("collapses the token toward the right edge, not at its identity position (owner ruling, 2026-09-15: restored synchronized slide)", () => {
    // Arrange / Act — the token's base (collapsed) transform must be a
    // rightward translate by the shared slide distance, not `translateX(0)`
    // or no transform at all.
    const corner = declarationsOf(".usage-corner") ?? "";
    const stamp = declarationsOf(".usage-stamp") ?? "";

    // Assert — the corner declares the one shared distance, and the token's
    // base transform uses that same variable rather than a literal or a
    // no-op.
    expect(corner).toMatch(/--usage-slide-distance:\s*[^;]+;/);
    expect(stamp).toMatch(/transform:\s*translateX\(var\(--usage-slide-distance\)\)/);
  });

  it("collapses the duration off-right by the same shared distance the token collapses by", () => {
    // Arrange / Act
    const corner = declarationsOf(".usage-corner") ?? "";
    const ago = declarationsOf(".usage-ago") ?? "";
    const stamp = declarationsOf(".usage-stamp") ?? "";

    // Assert — both read `var(--usage-slide-distance)`, the single value
    // declared once on the shared `.usage-corner` ancestor, so the two
    // collapsed offsets are provably the same distance rather than two
    // numbers that merely look similar.
    expect(corner).toMatch(/--usage-slide-distance:\s*[^;]+;/);
    expect(ago).toMatch(/transform:\s*translateX\(var\(--usage-slide-distance\)\)/);
    expect(stamp).toMatch(/transform:\s*translateX\(var\(--usage-slide-distance\)\)/);
  });

  it("resolves both the token and the duration to translateX(0) under every reveal trigger", () => {
    // Arrange / Act — the same trigger selectors that reveal the duration
    // must also carry the token back to its resting (untranslated) position.
    const triggers = [
      ".bubble.assistant:hover",
      ".bubble.assistant:focus-within",
      ".usage-corner:hover",
      ".usage-corner:focus-within",
      ".usage-corner.usage-corner--revealed",
    ];

    // Assert
    for (const trigger of triggers) {
      const stampRule = declarationsOf(`${trigger} .usage-stamp`) ?? "";
      const agoRule = declarationsOf(`${trigger} .usage-ago`) ?? "";
      expect(stampRule).toMatch(/transform:\s*translateX\(0\)/);
      expect(agoRule).toMatch(/transform:\s*translateX\(0\)/);
    }
  });

  it("transitions the token's transform on the same duration and easing as the duration's", () => {
    // Arrange / Act
    const stamp = declarationsOf(".usage-stamp") ?? "";
    const ago = declarationsOf(".usage-ago") ?? "";

    // Assert — both carry `transform 0.5s ease`, so the slide is one
    // synchronized motion rather than two animations that happen to overlap.
    expect(stamp).toMatch(/transition:\s*transform 0\.5s ease/);
    expect(ago).toMatch(/transition:[^;]*transform 0\.5s ease/s);
  });

  it("never animates the token's width, max-width, or margin, so the reserved footprint stays constant", () => {
    // Arrange / Act — the token's base rule and every reveal-trigger rule
    // targeting it.
    const base = declarationsOf(".usage-stamp") ?? "";
    const revealed = declarationsOf(".usage-corner.usage-corner--revealed .usage-stamp") ?? "";

    // Assert — only `transform`/`transition` change; no layout-affecting
    // property is ever declared on the token, so the corner's floated
    // footprint (token layout width + duration layout width, unaffected by
    // `transform`) never changes and the first prose line never reflows.
    for (const decls of [base, revealed]) {
      expect(decls).not.toMatch(/(?:^|[\s;])width\s*:/);
      expect(decls).not.toMatch(/(?:^|[\s;])max-width\s*:/);
      expect(decls).not.toMatch(/(?:^|[\s;])margin/);
    }
  });

  it("disables the token's slide transition under reduced motion, alongside the duration's", () => {
    // Arrange / Act
    const reduced = rulesOf(stylesheet).find(
      (rule) =>
        rule.selectors.includes(".usage-stamp") && rule.selectors.includes(".usage-ago"),
    );

    // Assert — the reduced-motion override lists both selectors together and
    // drops the transition for both, so reduced motion leaves no slide on
    // either half of the pair.
    expect(reduced).toBeDefined();
    expect(reduced?.declarations).toMatch(/transition:\s*none/);
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

describe("the arriving response indicator", () => {
  it("carries no `.response-arriving` rule: the arriving ellipsis is gone", () => {
    // Arrange / Act — the response-specific arriving-indicator rule.
    const rule = declarationsOf(".response-arriving");

    // Assert — the rule was removed with the indicator node (owner ruling
    // 2026-09-15: a streaming response shows its prose only). The shared
    // `.animated-ellipsis` base stays: it still dresses the footer status
    // words (highlight.ts) and the plan card's planning indicator (plan.ts).
    expect(rule).toBeUndefined();
  });
});

describe("the thinking bubble", () => {
  /** The raw text of the `@media (prefers-color-scheme: dark)` block, found by
   * balancing braces from its opening `{` (see the same helper on the prompt
   * border suite). */
  function darkBlock(): string {
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

  it("gives the thinking bubble a light-orange border", () => {
    // Arrange / Act
    const rule = declarationsOf(".bubble.assistant.thinking-bubble");

    // Assert — the border is the light-orange thinking token (owner ruling
    // 2026-09-15), never green and never transparent.
    expect(rule).toMatch(/border-color:\s*var\(--thinking-border\)/);
  });

  it("borders the thinking bubble at the SAME thickness as the green final border", () => {
    // Arrange / Act — neither the thinking rule nor the green final-response
    // rule sets a border width or a `border` shorthand; both change only the
    // COLOR and inherit the base `.bubble { border: 0.3px solid transparent }`.
    const thinking = declarationsOf(".bubble.assistant.thinking-bubble") ?? "";
    const green = declarationsOf(".bubble.assistant.final-response:not(.thinking-bubble)") ?? "";

    // Assert — same reserved thickness, only the color differs.
    for (const decls of [thinking, green]) {
      expect(decls).not.toMatch(/border-width/);
      expect(decls).not.toMatch(/(?:^|[\s;])border\s*:/);
      expect(decls).toMatch(/border-color/);
    }
  });

  it("defines the light-orange token in the light theme", () => {
    // Arrange / Act — the top-level (light) :root palette.
    const root = declarationsOf(":root");

    // Assert
    expect(root).toMatch(/--thinking-border:\s*#[0-9a-fA-F]{3,6}/);
  });

  it("redefines the light-orange token for the dark theme", () => {
    // Arrange / Act
    const dark = darkBlock();

    // Assert — the dark palette lifts the token like every other state border.
    expect(dark).toMatch(/--thinking-border:\s*#[0-9a-fA-F]{3,6}/);
  });

  it("puts the orange only on thinking bubbles, never on a partial-final bubble", () => {
    // Arrange — every rule that paints the thinking-border color.
    const orangeRules = rulesOf(stylesheet).filter((r) =>
      /border-color:\s*var\(--thinking-border\)/.test(r.declarations),
    );

    // Assert — at least one such rule, and EVERY selector that wears it is a
    // `.thinking-bubble` selector, so a partial-final response (assistant, not
    // thinking, not final) can never match it and stays borderless.
    expect(orangeRules.length).toBeGreaterThan(0);
    for (const rule of orangeRules) {
      for (const selector of rule.selectors) {
        expect(selector).toContain(".thinking-bubble");
      }
    }
  });

  it("caps the thinking bubble's scroll box at two lines and sets nothing else", () => {
    // Arrange / Act
    const rule = declarationsOf(".bubble.assistant.thinking-bubble > .bubble-scroll");

    // Assert — only the line budget changes; the clip, fade, chevron and
    // expand rules stay the response bubble's own.
    expect(rule?.trim()).toMatch(/^--cap-lines:\s*2\s*;?$/);
  });

  it("excludes the thinking bubble from the green final-answer rule", () => {
    // Arrange / Act — the green rule's own selector carries the exclusion.
    const rule = declarationsOf(".bubble.assistant.final-response:not(.thinking-bubble)");

    // Assert — the green declaration exists and applies only to non-thinking.
    expect(rule).toMatch(/border-color:\s*var\(--final-response\)/);
  });
});

/**
 * THE AMBER ASYNC BORDER IS GONE (owner ruling, 2026-09-16: "remove the amber
 * async-quiescence border on response/prompt bubbles ENTIRELY — I never wanted
 * it"). A settled final response goes GREEN unconditionally; no bubble ever
 * wears the amber `--async` border while background/detached work is still
 * running. The `--async` token itself stays — the async-catalog badge, the
 * topbar/sidebar monitoring rows, and the parked-merge glyphs still use it.
 */
describe("the removed amber async border", () => {
  it("paints the amber async token on no bubble", () => {
    // Arrange — every rule that colors a border with the amber async token.
    const amberBorders = rulesOf(stylesheet).filter((r) =>
      /border-color:\s*var\(--async\)/.test(r.declarations),
    );

    // Assert — no rule paints an amber border on any .bubble selector.
    for (const rule of amberBorders) {
      for (const selector of rule.selectors) {
        expect(selector).not.toContain(".bubble");
      }
    }
  });

  it("settles a final-response bubble to the green border unconditionally", () => {
    // Arrange / Act — the only border color the settled answer bubble can take.
    const rule = declarationsOf(".bubble.assistant.final-response:not(.thinking-bubble)");

    // Assert — green, with nothing amber able to outrank it on settle.
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

/**
 * THE FEED TEXT ZOOM SCOPING (owner-requested, RPC-driven).
 *
 * The daemon-owned zoom rides ONE custom property, `--feed-text-scale`, which
 * multiplies into every feed-scroll font-size as `calc(<base> * var(...))`.
 * jsdom does not resolve calc()/var(), so these read the file's TEXT: the proof
 * that "only the feed, only text" scales is that the property is referenced
 * ONLY inside font-size declarations, and NEVER by the sidebar, topbar, footer,
 * composer or login chrome.
 */
const FEED_SCALE_VAR = "var(--feed-text-scale)";

/** The forbidden chrome: no selector containing any of these may reference the var. */
const FORBIDDEN_CHROME: readonly string[] = [
  "#composer",
  "#footer",
  ".pfooter",
  "#ws-sidebar",
  "#topbar",
  "#login",
];

describe("the feed text zoom scoping", () => {
  it("defines the scale property once, at :root, with a unity default", () => {
    // Arrange / Act
    const root = rulesOf(stylesheet).find((rule) => rule.selectors.includes(":root"));

    // Assert
    expect(root?.declarations).toMatch(/--feed-text-scale\s*:\s*1\b/);
  });

  it("references the scale only inside font-size declarations, never layout", () => {
    // Arrange / Act: every declaration in the sheet that mentions the var.
    const offenders: string[] = [];
    for (const rule of rulesOf(stylesheet)) {
      for (const decl of rule.declarations.split(";")) {
        if (!decl.includes(FEED_SCALE_VAR)) continue;
        if (!/^\s*font-size\s*:/.test(decl)) offenders.push(decl.trim());
      }
    }

    // Assert
    expect(offenders).toEqual([]);
  });

  it("never scales the sidebar, topbar, footer, composer or login chrome", () => {
    // Arrange / Act
    const leaks: string[] = [];
    for (const rule of rulesOf(stylesheet)) {
      if (!rule.declarations.includes(FEED_SCALE_VAR)) continue;
      for (const selector of rule.selectors) {
        if (FORBIDDEN_CHROME.some((chrome) => selector.includes(chrome))) leaks.push(selector);
      }
    }

    // Assert
    expect(leaks).toEqual([]);
  });

  it("does scale the core feed text: the bubble, markdown and tool cards", () => {
    // Arrange / Act
    const scaled = new Set<string>();
    for (const rule of rulesOf(stylesheet)) {
      if (!rule.declarations.includes(FEED_SCALE_VAR)) continue;
      for (const selector of rule.selectors) scaled.add(selector);
    }

    // Assert: the feed's spine carries the scale.
    expect(scaled.has("#feed")).toBe(true);
    expect(scaled.has(".md")).toBe(true);
    expect(scaled.has(".tool-card")).toBe(true);
  });
});

/**
 * THE CARD-LEVEL TOOL FOLD (owner ruling, 2026-09-15).
 *
 * A tool-call and a skill card are ONE click-to-expand unit: collapsed shows
 * the head (the title in full) and the input line (capped at two rows), with the
 * output section HIDDEN — no preview — until the whole `.tool-fold` card is
 * `.expanded`. These pin that model to the file, and — the load-bearing part —
 * that it never reaches the response/prompt bubble.
 */
describe("the card-level tool fold", () => {
  it("hides a collapsed card's output section entirely, showing no preview", () => {
    // Arrange / Act
    const rule = declarationsOf(".tool-fold:not(.expanded) > .tool-output");

    // Assert — the section is removed from layout while collapsed, not capped.
    expect(rule).toMatch(/display:\s*none/);
  });

  it("reveals the section, scrolling at 50vh, once the card is expanded", () => {
    // Arrange / Act
    const rule = declarationsOf(".tool-fold.expanded > .tool-output");

    // Assert
    expect(rule).toMatch(/max-height:\s*50vh/);
    expect(rule).toMatch(/overflow-y:\s*auto/);
  });

  it("caps a collapsed card's input line at two text rows", () => {
    // Arrange / Act
    const rule = declarationsOf(".tool-fold:not(.expanded) > .bash-input");

    // Assert
    expect(rule).toMatch(/-webkit-line-clamp:\s*2/);
  });

  it("applies that two-row cap to every input-line form, not just Bash", () => {
    // Arrange / Act — the one rule that clamps the collapsed header body.
    const clamp = rulesOf(stylesheet).find(
      (r) =>
        r.selectors.includes(".tool-fold:not(.expanded) > .bash-input") &&
        /-webkit-line-clamp:\s*2/.test(r.declarations),
    );

    // Assert — plain input, command, path and query all get the same cap.
    expect(clamp?.selectors).toEqual(
      expect.arrayContaining([
        ".tool-fold:not(.expanded) > .tool-input",
        ".tool-fold:not(.expanded) > .bash-input",
        ".tool-fold:not(.expanded) > .file-path",
        ".tool-fold:not(.expanded) > .tool-query",
      ]),
    );
  });

  it("excludes the TITLE from the two-row cap: the head is never clamped", () => {
    // Arrange / Act — every rule that clamps to two rows.
    const clamps = rulesOf(stylesheet).filter((r) => /-webkit-line-clamp:\s*2/.test(r.declarations));

    // Assert — none of them names the head or the title.
    expect(clamps.length).toBeGreaterThan(0);
    for (const rule of clamps) {
      for (const sel of rule.selectors) {
        expect(sel.includes(".tool-head")).toBe(false);
        expect(sel.includes(".tool-name")).toBe(false);
      }
    }
  });

  it("lifts the two-row header cap once the card is expanded", () => {
    // Arrange / Act
    const rule = declarationsOf(".tool-fold.expanded > .bash-input");

    // Assert
    expect(rule).toMatch(/-webkit-line-clamp:\s*none/);
  });

  it("never caps the card itself, so only the section scrolls at 50vh", () => {
    // Arrange / Act
    const rule = declarationsOf(".tool-fold.expanded");

    // Assert — the card grows to hold its 50vh section rather than clipping.
    expect(rule).toMatch(/max-height:\s*none/);
  });

  it("scopes the whole fold model to tool cards, never to a bubble", () => {
    // Arrange / Act — every rule that mentions the card fold.
    const foldRules = rulesOf(stylesheet).filter((r) =>
      r.selectors.some((sel) => sel.includes("tool-fold")),
    );

    // Assert — not one of them reaches a response/prompt/peer bubble.
    expect(foldRules.length).toBeGreaterThan(0);
    for (const rule of foldRules) {
      for (const sel of rule.selectors) expect(sel.includes("bubble")).toBe(false);
    }
  });
});

/**
 * REGRESSION GUARD: the response/prompt bubble collapse model is UNCHANGED by
 * the card-level tool fold. The bubble's scroll box must still cap-and-clip
 * exactly as it did, and the tool cards' hide-while-collapsed model must NOT
 * have leaked onto the response/prompt bubble (which shows a capped preview, not
 * a hidden body — only the PEER bubble hides its body while collapsed).
 */
describe("regression: the response/prompt bubble collapse model", () => {
  it("still caps the bubble scroll box at min(the line budget, 50vh)", () => {
    // Arrange / Act
    const capped = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.includes(".bubble > .bubble-scroll"),
    );

    // Assert — the same min(calc(...), 50vh) cap FIX1 established.
    expect(
      capped.some((rule) =>
        /max-height:\s*min\(calc\(var\(--cap-lines\)[\s\S]*\),\s*50vh\)/.test(rule.declarations),
      ),
    ).toBe(true);
  });

  it("still clips the collapsed bubble scroll box, revealing scroll only on expand", () => {
    // Arrange / Act — the first overflow-y rule on the box is the collapsed clip.
    const clip = rulesOf(stylesheet).find(
      (rule) =>
        rule.selectors.includes(".bubble > .bubble-scroll") && /overflow-y/.test(rule.declarations),
    );

    // Assert
    expect(clip?.declarations).toMatch(/overflow-y:\s*hidden\s*;/);
  });

  it("keeps hide-while-collapsed to the PEER bubble alone", () => {
    // Arrange / Act — the only bubble whose body is hidden (not capped) while
    // collapsed is the peer bubble, unchanged by this work.
    const peer = declarationsOf(".bubble.peer > .bubble-scroll:not(.expanded)");

    // Assert
    expect(peer).toMatch(/display:\s*none/);
  });

  it("never hides a response or prompt bubble's body while collapsed", () => {
    // Arrange / Act — every rule that removes an element from layout.
    const hiders = rulesOf(stylesheet).filter((rule) => /display:\s*none/.test(rule.declarations));

    // Assert — none of them collapses an assistant/user bubble's scroll box, so
    // the response/prompt bubble keeps its capped-preview model, not the tool
    // cards' hidden-section one.
    for (const rule of hiders) {
      for (const sel of rule.selectors) {
        expect(/\.bubble\.assistant\s*>\s*\.bubble-scroll/.test(sel)).toBe(false);
        expect(/\.bubble\.user\s*>\s*\.bubble-scroll/.test(sel)).toBe(false);
      }
    }
  });
});

/**
 * THE REVIVING SHIMMER'S STYLESHEET CONTRACT. The period the inline delays
 * seek against, the pan range that keeps the text filled, and the
 * reduced-motion stop all live only in the file.
 */
describe("the reviving shimmer's stylesheet contract", () => {
  const selector = "#ws-sidebar .row .name.reviving";

  /** Every rule for the shimmer's selector, in source order. */
  function shimmerRules(): string[] {
    return rulesOf(stylesheet)
      .filter((rule) => rule.selectors.includes(selector))
      .map((rule) => rule.declarations);
  }

  it("runs at the period the inline delays are computed against", () => {
    // Arrange / Act
    const [base] = shimmerRules();

    // Assert
    expect(base).toContain(`ws-revive-shimmer ${REVIVE_SHIMMER_PERIOD_MS / 1000}s`);
  });

  it("pans only between 0% and 100%, so the gradient always covers the text", () => {
    // Arrange / Act
    const keyframes = /@keyframes ws-revive-shimmer \{([\s\S]*?)\n\}/.exec(stylesheet)?.[1] ?? "";
    const positions = [...keyframes.matchAll(/background-position-x:\s*(-?\d+)%/g)].map((m) => Number(m[1]));

    // Assert
    expect(positions).toEqual([100, 0]);
  });

  it("stops the shimmer and its fill under prefers-reduced-motion", () => {
    // Arrange / Act: the override is the LAST rule for the selector.
    const rules = shimmerRules();
    const override = rules[rules.length - 1] ?? "";

    // Assert
    expect(rules.length).toBe(2);
    expect(override).toMatch(/animation:\s*none/);
    expect(override).toMatch(/background-image:\s*none/);
  });
});

/**
 * THE PROMPT GLIMMER'S INTENSITY (owner ruling, 2026-09-21: "make the
 * glimmering in the webapp response bubbles 50% more intense").
 *
 * The glimmer's strength is ONE thing: how much black the `--bubble-wave`
 * wash carries. Nothing else in the effect changes with intensity — the
 * gradient's geometry, its 3.2s period and its phase are all held elsewhere —
 * so the wash percentage is what an assertion can honestly pin, per theme.
 * Both themes carry the same +50%, because one ruling moved both.
 */
describe("the prompt glimmer's intensity", () => {
  /** The black percentage in a `--bubble-wave` declaration's color-mix. */
  function wavePercent(block: string): number {
    const declaration = /--bubble-wave:\s*color-mix\(in srgb,\s*#000000\s*([\d.]+)%/.exec(block);
    if (declaration === null) throw new Error("no --bubble-wave color-mix found");
    return Number(declaration[1]);
  }

  it("washes the light theme's band with 10.5% black, the ruling's +50% over 7%", () => {
    // Arrange / Act
    const root = declarationsOf(":root") ?? "";

    // Assert
    expect(wavePercent(root)).toBeCloseTo(10.5, 5);
  });

  it("washes the dark theme's band with 39% black, the same +50% over 26%", () => {
    // Arrange / Act
    const dark = darkThemeBlock();

    // Assert
    expect(wavePercent(dark)).toBeCloseTo(39, 5);
  });

  it("keeps the dark theme's band the deeper of the two, as a dark fill needs", () => {
    // Arrange / Act
    const light = wavePercent(declarationsOf(":root") ?? "");
    const dark = wavePercent(darkThemeBlock());

    // Assert
    expect(dark).toBeGreaterThan(light);
  });

  it("carries the intensity in the token alone, so the gradient rule is untouched by it", () => {
    // Arrange / Act — the band's own rule names the token and no literal wash.
    const waving = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.includes('.bubble.user[data-wave="working"]'),
    );
    const gradient = waving.find((rule) => /background-image/.test(rule.declarations))?.declarations ?? "";

    // Assert
    expect(gradient).toMatch(/var\(--bubble-wave\)/);
    expect(gradient).not.toMatch(/color-mix/);
  });

  it("holds the 3.2s period the effect had before the intensity ruling", () => {
    // Arrange / Act — intensity is the wash alone; the pass must not speed up.
    const waving = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.includes('.bubble.user[data-wave="working"]'),
    );

    // Assert
    expect(
      waving.some((rule) => /animation:\s*bubble-wave 3\.2s linear infinite/.test(rule.declarations)),
    ).toBe(true);
  });
});

// THE ASYNC BUBBLE'S EXPANDED CAP (owner ruling, 2026-09-23) is `75cqh`, which
// only means "three quarters of the feed" while the feed's scrolling viewport is
// the size container `cqh` resolves against. The cap itself is asserted on a
// mounted bubble in test/feed/bubble.test.ts; this pins the unit's anchor.
describe("the feed viewport as the async bubble cap's size container", () => {
  it("makes #feed-scroll a size container so cqh measures the feed's visible height", () => {
    // Arrange / Act
    const feedScroll = rulesOf(stylesheet).filter((rule) => rule.selectors.includes("#feed-scroll"));

    // Assert
    expect(
      feedScroll.some((rule) => /(?:^|[\s;])container-type\s*:\s*size\s*(?:;|$)/.test(rule.declarations)),
    ).toBe(true);
  });
});

/**
 * THE HELD PROMPT'S ONE-LINE FOLD (owner ruling, 2026-09-23): collapsed, only
 * the first line shows, clamped to one row; expanded, only the whole prompt.
 */
describe("the held prompt's one-line fold", () => {
  it("hides the whole prompt while the fold is collapsed", () => {
    // Arrange / Act
    const rule = declarationsOf(".held-fold:not(.expanded) > .queued-content");
    // Assert
    expect(rule).toMatch(/display:\s*none/);
  });

  it("hides the first-line face once the fold is expanded", () => {
    // Arrange / Act
    const rule = declarationsOf(".held-fold.expanded > .held-line");
    // Assert
    expect(rule).toMatch(/display:\s*none/);
  });

  it("clamps the first-line face to one row", () => {
    // Arrange / Act
    const rule = declarationsOf(".held-line");
    // Assert
    expect(rule).toMatch(/-webkit-line-clamp:\s*1/);
  });

  it("wears the feed's zoom-in cursor only on a fold with more to show", () => {
    // Arrange / Act
    const rule = declarationsOf(".held-fold.held-foldable");
    // Assert
    expect(rule).toMatch(/cursor:\s*zoom-in/);
  });
});
