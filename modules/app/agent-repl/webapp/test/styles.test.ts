// @vitest-environment jsdom
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
import { TITLE_FOLD_OPEN_SELECTOR } from "../src/feed/title-fold.js";
import { BUBBLE_CAP_LINES, BUBBLE_EXPAND_ONLY_CLASS } from "../src/bubble/draw.js";
import { HELD_STATUS_BADGES } from "../src/tray/held-prompt.js";

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

  it("caps every bubble at the one 77% token", () => {
    // Arrange / Act
    const column = declarationsOf("#main-col");
    const bubble = declarationsOf(".bubble");

    // Assert — 77% (owner ruling 2026-09-15), one token for every bubble (2026-09-23).
    expect(column).toMatch(/--bubble-max-width:\s*77%/);
    expect(bubble).toMatch(/max-width:\s*var\(--bubble-max-width\)/);
  });

  it("hangs a response on the left rail, its margin biased off the unscaled cap", () => {
    // Arrange / Act
    const role = declarationsOf('.bubble[data-role="response"]');

    // Assert
    expect(role).toMatch(/margin-left:\s*calc\(\(100% - var\(--agent-bubble-cap\)\) \/ 2\)/);
  });

  it("hangs a prompt on the right rail, its margin biased off the unscaled cap", () => {
    // Arrange / Act
    const role = declarationsOf('.bubble[data-role="prompt"]');

    // Assert
    expect(role).toMatch(/margin-right:\s*calc\(\(100% - var\(--agent-bubble-cap\)\) \/ 2\)/);
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
        /max-height:\s*min\(calc\(var\(--bubble-cap-lines\)[\s\S]*\),\s*50vh\)/.test(rule.declarations),
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
    const prompt = declarationsOf('.bubble[data-role="prompt"]');

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

    // Assert — sized by the one gutter token the tree budget also reads.
    expect(sized?.declarations).toMatch(/width:\s*var\(--scrollbar-gutter-width\)/);
  });

  it("declares the scrollbar gutter token once, in px", () => {
    // Arrange / Act — every declaration of the token, on the column the
    // bubbles hang in or anywhere else.
    const declared = rulesOf(stylesheet).flatMap(
      (rule) => rule.declarations.match(/--scrollbar-gutter-width:[^;]*/g) ?? [],
    );

    // Assert — px, because body.ts measures it and refuses anything else.
    expect(declared).toEqual(["--scrollbar-gutter-width: 8px"]);
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

  it("sizes every bubble to fit-content, capped at the one bubble width", () => {
    // Arrange / Act — the one rule every bubble kind takes.
    const bubble = declarationsOf(".bubble") ?? "";

    // Assert — fit-content up to the shared cap.
    expect(bubble).toMatch(/width:\s*fit-content/);
    expect(bubble).toMatch(/max-width:\s*var\(--bubble-max-width\)/);
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
      /max-height:\s*min\(calc\(var\(--bubble-cap-lines\) \* var\(--md-line-h\) \* 1em\),\s*50vh\)/,
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
 * more to reveal"). FIX2 draws a bottom fade on a collapsed bubble that
 * overflows its cap, keyed entirely on `has-more` — the fade ONLY, never a
 * chevron (owner ruling, 2026-09-23)
 * (bubble-more.ts toggles the class). The signal is SCOPED to the two speaker
 * bubbles — never a tool-call section — and fades into each bubble's own bg.
 */
describe("the 'more below' affordance", () => {
  it("draws the fade from has-more, over the last 1.5em", () => {
    // Arrange / Act
    const fade = declarationsOf(".bubble > .bubble-scroll.has-more::after");

    // Assert
    expect(fade).toMatch(/height:\s*1\.5em/);
    expect(fade).toMatch(/bottom:\s*0/);
    expect(fade).toMatch(/pointer-events:\s*none/);
  });

  it("fades every bubble into its own background token", () => {
    // Arrange / Act — the gradient rule (the selector also names a base ::after
    // rule for geometry, so pick the copy carrying the background).
    const gradient = rulesOf(stylesheet).find(
      (rule) =>
        rule.selectors.includes(".bubble > .bubble-scroll.has-more::after") &&
        /linear-gradient/.test(rule.declarations),
    );

    // Assert — dissolves into the bubble's OWN fill (its role's or held's), not a hard edge.
    expect(gradient?.declarations).toMatch(
      /linear-gradient\(to bottom, transparent, var\(--bubble-bg\)\)/,
    );
  });

  it("draws no chevron from has-more: the fade is the whole signal", () => {
    // Arrange / Act — every ::before a has-more rule draws, on any bubble or title.
    const chevrons = rulesOf(stylesheet)
      .flatMap((rule) => rule.selectors)
      .filter((sel) => sel.includes(".has-more") && sel.endsWith("::before"));

    // Assert
    expect(chevrons).toEqual([]);
  });

  it("draws no chevron glyph anywhere in the sheet", () => {
    // Arrange / Act — the ⌄ glyph (\2304) the old chevron was.
    const glyphs = rulesOf(stylesheet).filter((rule) => /\\2304/.test(rule.declarations));

    // Assert
    expect(glyphs.map((rule) => rule.selectors.join(", "))).toEqual([]);
  });

  it("draws the affordance only out of flow, so toggling has-more changes no layout", () => {
    // Arrange / Act — every rule keyed on has-more. THE USER OWNS THE SCROLL
    // (owner rule, 2026-09-23): the measurer toggles the class under a reader,
    // so it may add nothing but out-of-flow pseudo-elements and a cursor.
    const withHasMore = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some((sel) => sel.includes(".has-more")),
    );
    const inFlow = withHasMore.filter((rule) => {
      const pseudo = rule.selectors.every((sel) => /::(?:before|after)$/.test(sel));
      if (!pseudo) return !/^\s*cursor:[^;]*;?\s*$/.test(rule.declarations);
      const positioned = /position:\s*absolute/.test(rule.declarations);
      // A rule that only repaints or re-places a pseudo-element the base rule
      // already took out of flow (the fade's per-kind gradient).
      const decorative = rule.declarations
        .split(";")
        .map((decl) => decl.split(":")[0]?.trim() ?? "")
        .every((prop) => prop === "" || ["background", "left", "right", "transform"].includes(prop));
      return !positioned && !decorative;
    });

    // Assert
    expect(inFlow.map((rule) => rule.selectors.join(", "))).toEqual([]);
  });

  it("makes the box the affordance's containing block whether or not it wears has-more", () => {
    // Arrange
    const boxes = [".bubble > .bubble-scroll", ".title-fold"];

    // Act — whether any rule on the bare box (no has-more) makes it relative.
    const relative = boxes.map((box) =>
      rulesOf(stylesheet).some(
        (rule) => rule.selectors.includes(box) && /position:\s*relative/.test(rule.declarations),
      ),
    );

    // Assert
    expect(relative).toEqual([true, true]);
  });

  it("never puts the affordance on a tool-call section", () => {
    // Arrange / Act — every rule that keys on has-more.
    const withHasMore = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some((sel) => sel.includes(".has-more")),
    );

    // Assert — every such selector is scoped to a response/prompt bubble scroll
    // box or to a card's TITLE fold (owner ruling, 2026-09-23, which extended the
    // affordance to tool titles), and none names a tool-call section's capped box.
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
        expect(sel).toMatch(
          /^(?:\.bubble(?:\[data-cap-lines="0"\])? > \.bubble-scroll|\.title-fold(?:-standalone)?)\.has-more/,
        );
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

describe("the cost corner", () => {
  /** The five reveal triggers: the whole bubble, the corner, and the state class. */
  const TRIGGERS = [
    ".bubble.assistant:hover",
    ".bubble.assistant:focus-within",
    ".usage-corner:hover",
    ".usage-corner:focus-within",
    ".usage-corner.usage-corner--revealed",
  ] as const;

  describe("the one gap token", () => {
    it("defines the gap as the bubble's top padding plus the line's half-leading", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect(corner).toMatch(
        /--usage-corner-gap:\s*calc\(var\(--usage-bubble-pad-top\) \+ var\(--usage-half-leading\)\)/,
      );
    });

    it("derives the half-leading from the bubble's one leading", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect(corner).toMatch(
        /--usage-half-leading:\s*calc\(\(var\(--md-line-h\) - 1\) \/ 2 \* 1em\)/,
      );
    });

    it("mirrors .bubble's top padding exactly", () => {
      // Arrange / Act
      const bubblePadTop = /padding:\s*(\S+)/.exec(declarationsOf(".bubble") ?? "")?.[1];
      const mirrored = /--usage-bubble-pad-top:\s*([^;]+);/.exec(
        declarationsOf(".usage-corner") ?? "",
      )?.[1];

      // Assert
      expect(mirrored).toBe(bubblePadTop);
    });

    it("derives the top margin from the gap token", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect(corner).toMatch(
        /margin-top:\s*calc\(\s*var\(--usage-corner-gap\)\s*-\s*var\(--usage-half-leading\)\s*-\s*var\(--usage-bubble-pad-top\)\s*-\s*var\(--usage-hit-y\)\s*\)/,
      );
    });

    it("derives the right margin from the same gap token", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect(corner).toMatch(
        /margin-right:\s*calc\(\s*var\(--usage-corner-gap\)\s*-\s*var\(--bubble-scroll-gap\)\s*-\s*var\(--usage-hit-x\)\s*\)/,
      );
    });

    it("leaves the global --bubble-scroll-gap untouched", () => {
      // Arrange / Act
      const mainCol = declarationsOf("#main-col") ?? "";

      // Assert
      expect(mainCol).toMatch(/--bubble-scroll-gap:\s*2px/);
    });

    it("never touches a margin or the gap token from a reveal rule", () => {
      // Arrange / Act
      const revealRules = rulesOf(stylesheet).filter((rule) =>
        rule.selectors.some((selector) => TRIGGERS.some((trigger) => selector.startsWith(trigger))),
      );

      // Assert
      for (const rule of revealRules) {
        expect(rule.declarations).not.toMatch(/margin|--usage-corner-gap\s*:/);
      }
    });
  });

  describe("the hover hit area", () => {
    it("pads the corner by the hit tokens", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect(corner).toMatch(/padding:\s*var\(--usage-hit-y\) var\(--usage-hit-x\)/);
    });

    it("cancels the hit padding at the bottom and the left", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect([
        /margin-bottom:\s*calc\(0px - var\(--usage-hit-y\)\)/.test(corner),
        /margin-left:\s*calc\(0px - var\(--usage-hit-x\)\)/.test(corner),
      ]).toEqual([true, true]);
    });

    it("keeps the hit area's size", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect([
        /--usage-hit-x:\s*1\.25rem/.test(corner),
        /--usage-hit-y:\s*0\.4rem/.test(corner),
      ]).toEqual([true, true]);
    });
  });

  describe("the resting layout", () => {
    it("floats the corner top-right so the prose's first line wraps beside it", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect(corner).toMatch(/float:\s*right/);
    });

    it("parks the slider at 100% of its own width", () => {
      // Arrange / Act
      const slider = declarationsOf(".usage-slider") ?? "";

      // Assert
      expect(slider).toMatch(/transform:\s*translateX\(100%\)/);
    });

    it("parks the duration one gap token beyond its normal gap", () => {
      // Arrange / Act
      const ago = declarationsOf(".usage-ago") ?? "";

      // Assert
      expect(ago).toMatch(/transform:\s*translateX\(var\(--usage-corner-gap\)\)/);
    });

    it("fades the duration out at rest", () => {
      // Arrange / Act
      const ago = declarationsOf(".usage-ago") ?? "";

      // Assert
      expect(ago).toMatch(/opacity:\s*0/);
    });

    it("keeps the normal gap between the token and the duration", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";
      const ago = declarationsOf(".usage-ago") ?? "";

      // Assert
      expect([
        /--usage-pair-gap:\s*0\.35rem/.test(corner),
        /margin-left:\s*var\(--usage-pair-gap\)/.test(ago),
      ]).toEqual([true, true]);
    });

    it("makes the duration a block, so its transform applies", () => {
      // Arrange / Act
      const ago = declarationsOf(".usage-ago") ?? "";

      // Assert
      expect(ago).toMatch(/display:\s*block/);
    });

    it("declares no fixed-length slide distance anywhere in the sheet", () => {
      // Arrange / Act
      const transforms = rulesOf(stylesheet)
        .filter((rule) => rule.selectors.some((selector) => /usage-(?:slider|ago|stamp)/.test(selector)))
        .flatMap((rule) => rule.declarations.match(/translateX\([^)]*\)+/g) ?? []);

      // Assert: a percentage, zero, or the gap token, never a length.
      for (const transform of transforms) {
        expect(transform).toMatch(/^translateX\((?:100%|0|var\(--usage-corner-gap\))\)$/);
      }
    });
  });

  describe("the two-phase reveal", () => {
    it.each(TRIGGERS)("phase A slides the duration to its normal gap under %s", (trigger) => {
      // Arrange / Act
      const ago = declarationsOf(`${trigger} .usage-ago`) ?? "";

      // Assert
      expect([/transform:\s*translateX\(0\)/.test(ago), /opacity:\s*1/.test(ago)]).toEqual([
        true,
        true,
      ]);
    });

    it.each(TRIGGERS)("phase B slides the pair to rest under %s", (trigger) => {
      // Arrange / Act
      const slider = declarationsOf(`${trigger} .usage-slider`) ?? "";

      // Assert
      expect(slider).toMatch(/transform:\s*translateX\(0\)/);
    });

    it.each(TRIGGERS)("phase A starts at once under %s", (trigger) => {
      // Arrange / Act
      const ago = declarationsOf(`${trigger} .usage-ago`) ?? "";

      // Assert
      expect(ago).toMatch(/transition-delay:\s*0s/);
    });

    it.each(TRIGGERS)("phase B waits out phase A under %s", (trigger) => {
      // Arrange / Act
      const slider = declarationsOf(`${trigger} .usage-slider`) ?? "";

      // Assert
      expect(slider).toMatch(/transition-delay:\s*var\(--usage-phase\)/);
    });

    it("runs phase B back first on mouse-leave", () => {
      // Arrange / Act
      const slider = declarationsOf(".usage-slider") ?? "";

      // Assert
      expect(slider).toMatch(/transition:\s*transform var\(--usage-phase\) ease 0s/);
    });

    it("runs phase A back once phase B has returned", () => {
      // Arrange / Act
      const ago = declarationsOf(".usage-ago") ?? "";

      // Assert
      expect(ago).toMatch(
        /transition:\s*transform var\(--usage-phase\) ease var\(--usage-phase\),\s*opacity var\(--usage-phase\) ease var\(--usage-phase\)/,
      );
    });

    it("splits the half-second reveal into two quarter-second phases", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect(corner).toMatch(/--usage-phase:\s*0\.25s/);
    });

    it("gives the token no transform of its own, so it moves only with the slider", () => {
      // Arrange / Act
      const stamp = declarationsOf(".usage-stamp") ?? "";

      // Assert
      expect(stamp).not.toMatch(/(?:^|[\s;])transform\s*:|(?:^|[\s;])transition\s*:/);
    });

    it("never slides an arriving corner's empty slider out, whatever reveals it", () => {
      // Arrange / Act
      const arriving =
        declarationsOf(".bubble.assistant .usage-corner[data-arriving] .usage-slider") ?? "";

      // Assert
      expect(arriving).toMatch(/transform:\s*translateX\(100%\)/);
    });

    it("disables both phases under reduced motion", () => {
      // Arrange / Act
      const reduced = rulesOf(stylesheet).find(
        (rule) => rule.selectors.includes(".usage-slider") && rule.selectors.includes(".usage-ago"),
      );

      // Assert
      expect(reduced?.declarations).toMatch(/transition:\s*none/);
    });
  });

  describe("the pair's anchoring", () => {
    it("takes the slider out of flow at the corner's content edge", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";
      const slider = declarationsOf(".usage-slider") ?? "";

      // Assert
      expect([
        /position:\s*relative/.test(corner),
        /position:\s*absolute/.test(slider),
        /top:\s*var\(--usage-hit-y\)/.test(slider),
        /right:\s*var\(--usage-hit-x\)/.test(slider),
      ]).toEqual([true, true, true, true]);
    });

    it("hangs the token off the slider's left edge", () => {
      // Arrange / Act
      const stamp = declarationsOf(".usage-stamp") ?? "";

      // Assert
      expect([/position:\s*absolute/.test(stamp), /right:\s*100%/.test(stamp)]).toEqual([
        true,
        true,
      ]);
    });

    it("keeps the token's text on one line so its zero-width anchor cannot wrap it", () => {
      // Arrange / Act
      const stamp = declarationsOf(".usage-stamp") ?? "";

      // Assert
      expect(stamp).toMatch(/white-space:\s*nowrap/);
    });
  });

  describe("one size and one baseline", () => {
    it("sets the one font size on the corner", () => {
      // Arrange / Act
      const corner = declarationsOf(".usage-corner") ?? "";

      // Assert
      expect(corner).toMatch(/(?:^|[\s;])font-size:\s*0\.85em/);
    });

    it.each([".usage-stamp", ".usage-ago", ".usage-slider", ".usage-reserve", ".usage-corner::before"])(
      "lets %s inherit the corner's size and leading",
      (selector) => {
        // Arrange / Act
        const declarations = declarationsOf(selector) ?? "";

        // Assert
        expect(declarations).not.toMatch(/font-size|line-height|font:/);
      },
    );

    it("aligns the token to the top of the line the duration starts on", () => {
      // Arrange / Act
      const stamp = declarationsOf(".usage-stamp") ?? "";

      // Assert
      expect(stamp).toMatch(/(?:^|[\s;])top:\s*0/);
    });

    it("keeps each figure's own color", () => {
      // Arrange / Act
      const stamp = declarationsOf(".usage-stamp") ?? "";
      const ago = declarationsOf(".usage-ago") ?? "";

      // Assert
      expect([
        /color:\s*var\(--info-tokens\)/.test(stamp),
        /color:\s*var\(--muted\)/.test(ago),
      ]).toEqual([true, true]);
    });

    it("draws the duration in fixed-width figures", () => {
      // Arrange / Act
      const ago = declarationsOf(".usage-ago") ?? "";

      // Assert
      expect(ago).toMatch(/font-variant-numeric:\s*tabular-nums/);
    });
  });

  describe("the reserved footprint", () => {
    it("reserves the token's slot with a hidden in-flow copy of the token text", () => {
      // Arrange / Act
      const spacer = declarationsOf(".usage-corner::before") ?? "";

      // Assert
      expect([
        /content:\s*attr\(data-tokens\)/.test(spacer),
        /visibility:\s*hidden/.test(spacer),
        /(?:^|[\s;])width:\s*6ch/.test(spacer),
        /font-variant-numeric:\s*tabular-nums/.test(spacer),
      ]).toEqual([true, true, true, true]);
    });

    it("reserves the duration's gap in front of its widest label", () => {
      // Arrange / Act
      const reserve = declarationsOf(".usage-reserve") ?? "";

      // Assert
      expect([
        /visibility:\s*hidden/.test(reserve),
        /margin-left:\s*var\(--usage-pair-gap\)/.test(reserve),
        /font-variant-numeric:\s*tabular-nums/.test(reserve),
      ]).toEqual([true, true, true]);
    });

    it("stacks every reserved label in one grid cell, so the reserve is the widest", () => {
      // Arrange / Act
      const reserve = declarationsOf(".usage-reserve") ?? "";
      const label = declarationsOf(".usage-reserve > .usage-reserve-label") ?? "";

      // Assert
      expect([
        /display:\s*grid/.test(reserve),
        /grid-area:\s*1 \/ 1/.test(label),
        /white-space:\s*nowrap/.test(label),
      ]).toEqual([true, true, true]);
    });

    it("sizes nothing from a reveal rule, so hovering never reflows the first line", () => {
      // Arrange / Act
      const revealRules = rulesOf(stylesheet).filter((rule) =>
        rule.selectors.some((selector) => TRIGGERS.some((trigger) => selector.startsWith(trigger))),
      );

      // Assert: a reveal rule declares only transform, opacity and delay.
      for (const rule of revealRules) {
        const properties = rule.declarations
          .split(";")
          .map((declaration) => declaration.split(":")[0]?.trim())
          .filter((property) => property !== undefined && property !== "");
        expect(properties.every((p) => ["transform", "opacity", "transition-delay"].includes(p))).toBe(
          true,
        );
      }
    });
  });
});

/**
 * THE PROMPT BORDERS (owner ruling, 2026-09-23). A border lands on a prompt
 * once it is RECEIVED and stays: a user prompt wears the light purple
 * permanently, in flight and after its turn resolves, independent of the
 * working flag and its wave; an agent-to-agent prompt (the agent-addressed row
 * and the peer message) wears the one amber; a held prompt in the tray wears
 * none. Each case is asked of the stylesheet as the cascade would: which
 * border-setting rules a bubble with that role, variant, wave and hook classes
 * matches, beyond the base rule's transparent reservation.
 */
describe("the prompt borders", () => {
  /** The border declarations every non-base rule a bubble so marked matches sets, in source order. */
  function bordersOn(attrs: Readonly<Record<string, string>>, hooks: readonly string[]): string[] {
    const el = document.createElement("div");
    el.className = ["bubble", "md", ...hooks].join(" ");
    for (const [name, value] of Object.entries(attrs)) el.setAttribute(name, value);
    return rulesOf(stylesheet)
      .filter((rule) =>
        rule.selectors.some((sel) => sel !== ".bubble" && sel.includes(".bubble") && !sel.includes("::") && el.matches(sel)),
      )
      .flatMap((rule) => rule.declarations.match(/(?:^|;)\s*border(?:-color)?\s*:[^;]*/g) ?? [])
      .map((decl) => decl.replace(/^;?\s*/, "").trim());
  }

  /** A prompt bubble's attributes, waving or not. */
  function prompt(variant: string, working: boolean): Record<string, string> {
    const attrs: Record<string, string> = { "data-role": "prompt", "data-variant": variant };
    if (working) attrs["data-wave"] = "working";
    return attrs;
  }

  it("reserves a 0.3px transparent border on every bubble, prompt included", () => {
    // Arrange / Act
    const bubble = declarationsOf(".bubble");

    // Assert
    expect(bubble).toMatch(/border:\s*0\.3px solid transparent/);
  });

  it.each([
    ["in flight", true],
    ["after its turn resolves", false],
  ] as const)("borders a user prompt in the light purple %s", (_label, working) => {
    // Arrange / Act
    const borders = bordersOn(prompt("user", working), ["user"]);

    // Assert
    expect(borders).toEqual(["border-color: var(--prompt-live-border)"]);
  });

  it.each([
    ["an agent-addressed prompt in flight", "agent", true, ["user", "prompt-agent"]],
    ["an agent-addressed prompt at rest", "agent", false, ["user", "prompt-agent"]],
    ["a peer message", "peer", false, ["peer"]],
  ] as const)("borders %s in the one agent amber", (_label, variant, working, hooks) => {
    // Arrange / Act
    const borders = bordersOn(prompt(variant, working), hooks);

    // Assert
    expect(borders).toEqual(["border: 1px solid var(--agent-prompt-border)"]);
  });

  it.each([
    ["a prompt held behind the turn", ["held-right"]],
    ["a prompt held by a bounce", ["held-right", "lease-card"]],
  ] as const)("gives %s no border", (_label, hooks) => {
    // Arrange / Act
    const borders = bordersOn(prompt("held", false), hooks);

    // Assert
    expect(borders).toEqual([]);
  });

  it("keys no border on the working wave", () => {
    // Arrange / Act
    const waving = rulesOf(stylesheet).filter(
      (rule) => rule.selectors.some((sel) => sel.includes("data-wave")) && /(?:^|;)\s*border/.test(rule.declarations),
    );

    // Assert
    expect(waving.map((rule) => rule.selectors.join(", "))).toEqual([]);
  });

  it.each([
    ["--prompt-live-border", "light"],
    ["--agent-prompt-border", "light"],
    ["--prompt-live-border", "dark"],
    ["--agent-prompt-border", "dark"],
  ] as const)("defines %s in the %s theme", (token, theme) => {
    // Arrange / Act
    const block = theme === "light" ? (declarationsOf(":root") ?? "") : darkThemeBlock();

    // Assert
    expect(block).toMatch(new RegExp(`${token}:\\s*#[0-9a-fA-F]{3,6}`));
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
    const rule = declarationsOf('.bubble[data-variant="thinking"]');

    // Assert — the border is the light-orange thinking token (owner ruling
    // 2026-09-15), never green and never transparent.
    expect(rule).toMatch(/border-color:\s*var\(--thinking-border\)/);
  });

  it("borders the thinking bubble at the SAME thickness as the green final border", () => {
    // Arrange / Act — neither the thinking rule nor the green final-response
    // rule sets a border width or a `border` shorthand; both change only the
    // COLOR and inherit the base `.bubble { border: 0.3px solid transparent }`.
    const thinking = declarationsOf('.bubble[data-variant="thinking"]') ?? "";
    const green = declarationsOf('.bubble.final-response:not([data-variant="thinking"])') ?? "";

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

    // Assert — at least one such rule, and EVERY selector that wears it is the
    // thinking variant's, so a partial-final response (not thinking, not final)
    // can never match it and stays borderless.
    expect(orangeRules.length).toBeGreaterThan(0);
    for (const rule of orangeRules) {
      for (const selector of rule.selectors) {
        expect(selector).toContain('[data-variant="thinking"]');
      }
    }
  });

  it("caps a two-line bubble at two lines and sets nothing else", () => {
    // Arrange / Act — the thinking bubble's cap value (its spec, response.ts).
    const rule = declarationsOf('.bubble[data-cap-lines="2"]');

    // Assert — only the line budget changes; the clip, fade, chevron and
    // expand rules stay every bubble's own.
    expect(rule?.trim()).toMatch(/^--bubble-cap-lines:\s*2\s*;?$/);
  });

  it("excludes the thinking bubble from the green final-answer rule", () => {
    // Arrange / Act — the green rule's own selector carries the exclusion.
    const rule = declarationsOf('.bubble.final-response:not([data-variant="thinking"])');

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
    const rule = declarationsOf('.bubble.final-response:not([data-variant="thinking"])');

    // Assert — green, with nothing amber able to outrank it on settle.
    expect(rule).toMatch(/border-color:\s*var\(--final-response\)/);
  });
});

describe("the selected-entry mark (a jump's landing, a reply selection)", () => {
  it("rings the selected card with the selection token", () => {
    // Arrange / Act
    const rule = declarationsOf(".entry-selected:not(.final-response)");

    // Assert
    expect(rule).toMatch(/outline:\s*2px solid var\(--selected-response\)/);
  });

  it("pulls the ring inside the card's own box, so nothing reflows or clips", () => {
    // Arrange / Act — an outline takes no layout space; a negative offset of
    // its own width keeps it inside the row's paint containment.
    const rule = declarationsOf(".entry-selected:not(.final-response)");

    // Assert
    expect(rule).toMatch(/outline-offset:\s*-2px/);
  });

  it("never changes a border width or the card's own shadow to mark a card", () => {
    // Arrange / Act
    const rule = declarationsOf(".entry-selected:not(.final-response)") ?? "";

    // Assert
    expect(/\bborder(-width)?\s*:|box-shadow\s*:/.test(rule)).toBe(false);
  });

  it("no longer marks the full-width row wrapper", () => {
    // Arrange / Act
    const css = stylesheet.replace(/\/\*[\s\S]*?\*\//g, "");

    // Assert
    expect(css.includes(".row-revealed")).toBe(false);
  });

  it("keeps the ring off a selected final response, whose own border turns blue", () => {
    // Arrange
    const card = document.createElement("div");
    card.className = "bubble final-response entry-selected";

    // Act, Assert — the ring's selector does not match it.
    expect(card.matches(".entry-selected:not(.final-response)")).toBe(false);
  });

  it("recolors the selected final-response bubble with the blue selection token", () => {
    // Arrange / Act
    const rule = declarationsOf(".bubble.final-response.entry-selected");

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
    // The green rule excludes thinking bubbles (`:not([data-variant="thinking"])`),
    // so the concluded answer's border can never land on intermediate reasoning.
    const green = css.indexOf('.bubble.final-response:not([data-variant="thinking"]) {');
    const blue = css.indexOf(".bubble.final-response.entry-selected");

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
    expect(scaled.has(".bubble")).toBe(true);
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

  it("never caps the card itself, so only the section scrolls at 50vh", () => {
    // Arrange / Act
    const rule = declarationsOf(".tool-fold.expanded");

    // Assert — the card grows to hold its 50vh section rather than clipping.
    expect(rule).toMatch(/max-height:\s*none/);
  });

  it("scopes the whole fold model to tool cards, never to a bubble", () => {
    // Arrange / Act — every SELECTOR that mentions the card fold. Per selector
    // rather than per rule: the title fold's lift rule lists each fold that can
    // own a title, the `.tool-fold` card beside the `.bubble-fold` head (itself a
    // tool card), in one selector list.
    const foldSelectors = rulesOf(stylesheet).flatMap((r) =>
      r.selectors.filter((sel) => sel.includes("tool-fold")),
    );

    // Assert — not one of them reaches a response/prompt/peer bubble.
    expect(foldSelectors.length).toBeGreaterThan(0);
    for (const sel of foldSelectors) expect(sel.includes("bubble")).toBe(false);
  });
});

/**
 * THE TITLE FOLD (owner ruling, 2026-09-23). A tool card's TITLE — the shell
 * bubble's command, a tool call's input line, a skill's invocation, a hook's
 * headline, a subagent's description — is capped at two lines while the fold
 * that owns it is collapsed, and wears the response bubble's fade (never a
 * chevron, owner ruling 2026-09-23) when it overflows. It replaced the tool-call card's own input-line clamp
 * (`.tool-fold:not(.expanded) > .bash-input` and its three siblings), which had
 * no fade; the input line is now one of the title fold's sites.
 */
describe("the title fold", () => {
  /** The title-level classes a card draws its title line with. */
  const TITLE_CLASSES = [
    ".tool-input",
    ".bash-input",
    ".file-path",
    ".tool-query",
    ".shell-command",
    ".tool-name",
    ".subagent-description",
  ];

  it("caps a collapsed title at two text rows", () => {
    // Arrange / Act
    const rule = declarationsOf(".title-fold");

    // Assert
    expect(rule).toMatch(/-webkit-line-clamp:\s*2/);
    expect(rule).toMatch(/overflow:\s*hidden/);
  });

  it("drops each title line's own preview cap so two rows are the only limit", () => {
    // Arrange / Act
    const rule = declarationsOf(".title-fold");

    // Assert
    expect(rule).toMatch(/max-height:\s*none/);
  });

  it("is the only two-row clamp: no title class is clamped by its own name", () => {
    // Arrange / Act — every rule that clamps to two rows.
    const clamps = rulesOf(stylesheet).filter((r) => /-webkit-line-clamp:\s*2/.test(r.declarations));

    // Assert — one shared cap, keyed on the one class every title site wears.
    expect(clamps.map((r) => r.selectors)).toEqual([[".title-fold"]]);
  });

  it("has retired the tool-call card's own input-line clamp", () => {
    // Arrange / Act — any rule still naming a title class under the card fold.
    const handRolled = rulesOf(stylesheet).filter((r) =>
      r.selectors.some((sel) => sel.startsWith(".tool-fold") && TITLE_CLASSES.some((c) => sel.endsWith(c))),
    );

    // Assert
    expect(handRolled).toEqual([]);
  });

  it("lifts the cap on exactly the folds that can own a title", () => {
    // Arrange / Act
    const lift = rulesOf(stylesheet).find(
      (r) => r.selectors.includes(".title-fold.expanded") && /line-clamp:\s*none/.test(r.declarations),
    );

    // Assert — the stylesheet and the measurer read the same owners.
    expect(lift?.selectors).toEqual(TITLE_FOLD_OPEN_SELECTOR.split(",").map((one) => one.trim()));
  });

  it("shows the whole title once its fold is expanded", () => {
    // Arrange / Act
    const rule = declarationsOf(".tool-fold.expanded .title-fold");

    // Assert
    expect(rule).toMatch(/-webkit-line-clamp:\s*none/);
    expect(rule).toMatch(/max-height:\s*none/);
    expect(rule).toMatch(/overflow:\s*visible/);
  });

  it("draws the fade from the response bubble's shared fade rule", () => {
    // Arrange / Act — the geometry rule the response bubble's fade lives in.
    const fade = rulesOf(stylesheet).find((r) =>
      r.selectors.includes(".bubble > .bubble-scroll.has-more::after") &&
      /height:\s*1\.5em/.test(r.declarations),
    );

    // Assert
    expect(fade?.selectors).toContain(".title-fold.has-more::after");
  });

  it("draws no chevron on a title", () => {
    // Arrange / Act
    const chevron = rulesOf(stylesheet).find((r) => r.selectors.includes(".title-fold.has-more::before"));

    // Assert
    expect(chevron).toBeUndefined();
  });

  it("fades a title into its own card's background", () => {
    // Arrange / Act
    const gradient = rulesOf(stylesheet).find(
      (r) => r.selectors.includes(".title-fold.has-more::after") && /linear-gradient/.test(r.declarations),
    );

    // Assert
    expect(gradient?.declarations).toMatch(
      /linear-gradient\(to bottom, transparent, var\(--title-fold-bg\)\)/,
    );
  });

  it("lets a subagent's description wrap to its two rows, not a one-line ellipsis", () => {
    // Arrange / Act
    const rule = declarationsOf(".subagent-description");

    // Assert
    expect(rule).not.toMatch(/white-space:\s*nowrap/);
  });

  it("sets the fade's background to the tool card's own fill", () => {
    // Arrange / Act
    const card = rulesOf(stylesheet).find(
      (r) => r.selectors.includes(".tool-card") && /--title-fold-bg/.test(r.declarations),
    );

    // Assert
    expect(card?.declarations).toMatch(/--title-fold-bg:\s*var\(--card\)/);
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
        /max-height:\s*min\(calc\(var\(--bubble-cap-lines\)[\s\S]*\),\s*50vh\)/.test(rule.declarations),
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

  it("gives the peer message its header-only face through a zero-line cap, not a hider", () => {
    // Arrange / Act — the peer's own hide-while-collapsed rule is gone; its
    // collapsed face is the one cap mechanism's zero (peer-message.ts).
    const peer = declarationsOf(".bubble.peer > .bubble-scroll:not(.expanded)");

    // Assert
    expect([peer, declarationsOf('.bubble[data-cap-lines="0"]')]).toEqual([
      undefined,
      " --bubble-cap-lines: 0; ",
    ]);
  });

  it("never hides any bubble's body while collapsed", () => {
    // Arrange / Act — every rule that removes an element from layout.
    const hiders = rulesOf(stylesheet).filter((rule) => /display:\s*none/.test(rule.declarations));

    // Assert — none of them collapses an assistant/user bubble's scroll box, so
    // the response/prompt bubble keeps its capped-preview model, not the tool
    // cards' hidden-section one.
    for (const rule of hiders) {
      for (const sel of rule.selectors) {
        expect(/^\.bubble\S*\s*>\s*\.bubble-scroll(?!\S*::)/.test(sel)).toBe(false);
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
      rule.selectors.includes('.bubble[data-role="prompt"][data-wave="working"]'),
    );
    const gradient = waving.find((rule) => /background-image/.test(rule.declarations))?.declarations ?? "";

    // Assert
    expect(gradient).toMatch(/var\(--bubble-wave\)/);
    expect(gradient).not.toMatch(/color-mix/);
  });

  it("holds the 3.2s period the effect had before the intensity ruling", () => {
    // Arrange / Act — intensity is the wash alone; the pass must not speed up.
    const waving = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.includes('.bubble[data-role="prompt"][data-wave="working"]'),
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
 * THE HELD PROMPT IS A BUBBLE (owner rulings, 2026-09-23, superseding the
 * one-line fold of the same day): it collapses at two lines through the one cap
 * rule, and keeps no private fold of its own.
 */
describe("the held prompt's collapse", () => {
  it("keeps no private fold rule", () => {
    // Arrange / Act
    const folds = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some((sel) => /\.held-(?:fold|line|foldable)\b|\.queued-content\b/.test(sel)),
    );
    // Assert
    expect(folds.map((rule) => rule.selectors.join(", "))).toEqual([]);
  });

  it("frames a held prompt with no border of its variant's", () => {
    // Arrange / Act — a held prompt is not yet received (owner ruling, 2026-09-23).
    const frames = rulesOf(stylesheet).filter(
      (rule) => rule.selectors.some((sel) => sel.includes('[data-variant="held"]')) && /border/.test(rule.declarations),
    );
    // Assert
    expect(frames.map((rule) => rule.selectors.join(", "))).toEqual([]);
  });
});

/**
 * THE WARNING CHIP IS RED, AND IS THE ONE PLACE AN ERROR SHOWS (owner ruling,
 * 2026-09-23). The chip took the alarm `--err`, and the client-local failure
 * overlay it replaced — cards drawn over the page — left no rule behind.
 */
describe("the warning chip as the one error surface", () => {
  it("paints the chip in the alarm red: the LAST color the cascade reaches it with is --err", () => {
    // Arrange — every non-hover rule naming the chip that sets its color, in
    // file order; with equal specificity the last one is what paints it.
    const coloring = rulesOf(stylesheet).filter(
      (rule) =>
        rule.selectors.includes(".topbar-warning-chip") &&
        /(?:^|[\s;{])color\s*:/.test(rule.declarations),
    );

    // Act
    const last = coloring.at(-1)?.declarations ?? "";

    // Assert
    expect(last).toMatch(/(?:^|[\s;{])color\s*:\s*var\(--err\)\s*(?:;|$)/);
  });

  it("carries no rule for a failure overlay", () => {
    // Arrange / Act
    const overlay = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some((selector) => selector.includes("#failure-overlay")),
    );

    // Assert
    expect(overlay).toEqual([]);
  });

  it("carries no rule for a failure card", () => {
    // Arrange / Act
    const cards = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some((selector) => selector.includes(".failure-card")),
    );

    // Assert
    expect(cards).toEqual([]);
  });
});

/**
 * THE BORDER LADDER (owner ruling, 2026-09-23): thinking is yellow, a settled
 * mid-turn response is pear, and the turn's answer stays green.
 */
describe("the response border ladder", () => {
  it("paints thinking bubbles a red-leaning yellow", () => {
    // Arrange / Act
    const root = declarationsOf(":root") ?? "";
    // Assert
    expect(root).toMatch(/--thinking-border:\s*#e3a008/);
  });

  /** The hue, in degrees, of every `NAME: #rrggbb` declaration, in sheet order (light, then dark). */
  function huesOf(name: string): number[] {
    const re = new RegExp(`--${name}:\\s*#([0-9a-fA-F]{6})`, "g");
    return [...stylesheet.matchAll(re)].map((m) => {
      const [r, g, b] = [0, 2, 4].map((i) => parseInt(m[1].slice(i, i + 2), 16) / 255);
      const max = Math.max(r, g, b);
      const d = max - Math.min(r, g, b);
      const h = max === r ? ((g - b) / d) % 6 : max === g ? (b - r) / d + 2 : (r - g) / d + 4;
      return (h * 60 + 360) % 360;
    });
  }

  it("runs monotonically toward green in both themes: thinking, then interim, then the answer", () => {
    // Arrange / Act
    const thinking = huesOf("thinking-border");
    const interim = huesOf("interim-response-border");
    const answer = huesOf("final-response");
    // Assert — one light and one dark value each, and the hue climbs from
    // red-leaning yellow through yellow-leaning pear to green in each theme.
    expect([thinking.length, interim.length, answer.length]).toEqual([2, 2, 2]);
    for (const theme of [0, 1]) {
      expect(thinking[theme]).toBeLessThan(interim[theme]);
      expect(interim[theme]).toBeLessThan(answer[theme]);
    }
  });

  it("paints a settled mid-turn response pear", () => {
    // Arrange / Act
    const rule = declarationsOf('.bubble[data-variant="response"][data-state="success"]:not(.final-response)');
    // Assert
    expect(rule).toMatch(/border-color:\s*var\(--interim-response-border\)/);
  });

  it("defines the pear border in both themes", () => {
    // Arrange / Act
    const matches = stylesheet.match(/--interim-response-border:\s*#[0-9a-fA-F]{6}/g) ?? [];
    // Assert
    expect(matches.length).toBe(2);
  });

  it("keeps the turn's answer green", () => {
    // Arrange / Act
    const rule = declarationsOf('.bubble.final-response:not([data-variant="thinking"])');
    // Assert
    expect(rule).toMatch(/border-color:\s*var\(--final-response\)/);
  });
});

/**
 * NO BUBBLE SCROLLS HORIZONTALLY (owner ruling, 2026-09-23). A bubble's content
 * wraps when it reaches the bubble's max width and never side-scrolls; no text
 * is ever off the bubble. So no rule over bubble content — the bubble, its
 * markdown (`.md`), a metaprompt tree (`.mp-*`) or a code block — may turn a
 * horizontal scroller on, and each wide kind wraps instead.
 */
describe("no bubble content scrolls horizontally", () => {
  /** A selector that reaches content drawn inside a bubble. */
  const bubbleContent = (selector: string): boolean => /\.bubble|\.md\b|\.mp-|pre\.md-code/.test(selector);

  it("has no bubble-content rule that turns on a horizontal scroller", () => {
    // Arrange / Act — every bubble-content rule allowing horizontal overflow.
    const scrolling = rulesOf(stylesheet)
      .filter((rule) => rule.selectors.some(bubbleContent))
      .filter((rule) => /overflow(?:-x)?\s*:\s*(?:auto|scroll)/.test(rule.declarations))
      .flatMap((rule) => rule.selectors);
    // Assert
    expect(scrolling).toEqual([]);
  });

  it.each([".mp-tree", ".mp-prefix, .mp-content"])("wraps %s rather than holding every line whole", (selector) => {
    // Arrange / Act
    const rule = rulesOf(stylesheet).find((r) => r.selectors.join(", ") === selector)?.declarations ?? "";
    // Assert
    expect(rule).toMatch(/white-space:\s*pre-wrap/);
  });

  it("breaks an unsplittable word in bubble prose rather than letting it run off", () => {
    // Arrange / Act
    const rule = declarationsOf(".bubble-body") ?? "";
    // Assert
    expect(rule).toMatch(/overflow-wrap:\s*anywhere/);
  });

  it("breaks a table cell's unsplittable word rather than widening the table", () => {
    // Arrange / Act
    const rule = declarationsOf(".md td") ?? "";
    // Assert
    expect(rule).toMatch(/overflow-wrap:\s*anywhere/);
  });

  it.each([".mp-tree", ".md pre.md-code", ".md table"])(
    "starts %s below the cost corner's float, so its box is the body's full width",
    (selector) => {
      // Arrange / Act
      const rule = declarationsOf(selector) ?? "";
      // Assert
      expect(rule).toMatch(/clear:\s*right/);
    },
  );
});

describe("a double-width character in a tree", () => {
  it("is drawn in an inline box exactly two columns of the tree font wide", () => {
    // Arrange / Act
    const rule = declarationsOf(".mp-wide") ?? "";
    // Assert
    expect([/display:\s*inline-block/.test(rule), /width:\s*2ch/.test(rule)]).toEqual([true, true]);
  });

  it("keeps the tree's own font, so `ch` is one of the tree's columns", () => {
    // Arrange / Act
    const rule = declarationsOf(".mp-wide") ?? "";
    // Assert — no font of its own to change what `ch` measures.
    expect(rule).not.toMatch(/font/);
  });
});

describe("the response body custom element", () => {
  it("is laid out as a block, not a custom element's default inline", () => {
    // Arrange / Act
    const rule = declarationsOf("bubble-body.bubble-body") ?? "";
    // Assert
    expect(rule).toMatch(/display:\s*block/);
  });
});

/**
 * THE SHELL HEAD'S CLOCKS NEVER RE-WRAP IT (owner rule, 2026-09-23: the user
 * owns the scroll). The head is a wrapping flex row, and a clock repainted
 * every second must hold a fixed footprint so its growth cannot push a
 * neighbor onto a new line under the reader.
 */
describe("the shell head's clocks hold a fixed footprint", () => {
  const clocks = [".shell-clock", ".shell-quiet"];

  it.each(clocks)("%s is one unbreakable run", (selector) => {
    // Arrange / Act
    const decls = declarationsOf(selector) ?? "";
    // Assert
    expect(decls).toMatch(/white-space:\s*nowrap/);
  });

  it.each(clocks)("%s draws tabular figures", (selector) => {
    // Arrange / Act
    const decls = declarationsOf(selector) ?? "";
    // Assert
    expect(decls).toMatch(/font-variant-numeric:\s*tabular-nums/);
  });

  it.each(clocks)("%s reserves a minimum width", (selector) => {
    // Arrange / Act
    const decls = declarationsOf(selector) ?? "";
    // Assert
    expect(decls).toMatch(/min-width:\s*\d+ch/);
  });
});

/**
 * THE ONE BUBBLE RULE SET (owner rulings, 2026-09-23). Every blue and purple
 * bubble is drawn by src/bubble/draw.ts and styled by the rules on `.bubble`:
 * the role sets only the side and the fill, a variant or state sets only the
 * border, `data-cap-lines` sets the collapsed limit, and one leading and one
 * size serve every kind.
 */
describe("the one bubble rule set", () => {
  /** Whether SELECTOR styles a bubble ITSELF (not its strip, box or content). */
  const onBubble = (selector: string): boolean => /^\.bubble(?![-\w])[^\s>+~]*$/.test(selector);

  it("sets one leading for every bubble, the response's 1.5", () => {
    // Arrange / Act
    const bubble = declarationsOf(".bubble") ?? "";
    // Assert
    expect([bubble.match(/--md-line-h:\s*1\.5\s*;/) !== null, /line-height:\s*var\(--md-line-h\)/.test(bubble)]).toEqual([
      true,
      true,
    ]);
  });

  it("sets one font size for every bubble, the response's", () => {
    // Arrange / Act
    const bubble = declarationsOf(".bubble") ?? "";
    // Assert
    expect(bubble).toMatch(/font-size:\s*calc\(0\.9rem \* var\(--feed-text-scale\)\)/);
  });

  it("lets no other rule on a bubble set its size or leading", () => {
    // Arrange / Act
    const others = rulesOf(stylesheet).filter(
      (rule) =>
        rule.selectors.some((sel) => onBubble(sel) && sel !== ".bubble") &&
        /(?:^|;)\s*(?:font-size|line-height|--md-line-h)\s*:/.test(rule.declarations),
    );
    // Assert
    expect(others.map((rule) => rule.selectors.join(", "))).toEqual([]);
  });

  it("carries no second leading token for prompts", () => {
    // Arrange / Act / Assert
    expect(stylesheet.replace(/\/\*[\s\S]*?\*\//g, "")).not.toMatch(/--prompt-line-h/);
  });


  it("places and fills a bubble only through its role, and the held variant's named fill", () => {
    // Arrange / Act — every rule on a bubble that sets a side margin or its fill.
    const placing = rulesOf(stylesheet).filter(
      (rule) =>
        rule.selectors.some(onBubble) &&
        /(?:^|;)\s*(?:margin-left|margin-right|--bubble-bg|background(?:-color)?)\s*:/.test(rule.declarations),
    );
    // Assert — the base rule paints the role's token; the roles and held choose it.
    expect(placing.flatMap((rule) => rule.selectors).sort()).toEqual(
      [".bubble", '.bubble[data-role="prompt"]', '.bubble[data-role="response"]', '.bubble[data-variant="held"]'].sort(),
    );
  });

  it("lets a variant or state rule set the border and nothing else", () => {
    // Arrange / Act — every rule keyed on a variant, a state or a hook class.
    const keyed = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some(
        (sel) => onBubble(sel) && sel !== ".bubble" && !/^\.bubble\[data-(?:role|cap-lines)=/.test(sel) && !sel.includes("[hidden]"),
      ),
    );
    // The held variant's HALVED cap (owner spec, 2026-09-23) is the one width a
    // variant sets, and only as the one token halved: its own test below pins
    // the value, so here it is the one sanctioned property beyond the border.
    const held = '.bubble[data-variant="held"]';
    const beyondBorder = keyed.filter((rule) =>
      rule.declarations
        .split(";")
        .map((decl) => decl.split(":")[0]?.trim() ?? "")
        .some(
          (prop) =>
            prop !== "" &&
            !prop.startsWith("border") &&
            prop !== "--bubble-bg" &&
            !(prop === "max-width" && rule.selectors.length === 1 && rule.selectors[0] === held),
        ),
    );
    // Assert — the working wave's own animation is the one other thing a
    // prompt state draws (ruling f), and it lives on its own rule.
    expect(beyondBorder.flatMap((rule) => rule.selectors).filter((sel) => !sel.includes("[data-wave="))).toEqual([]);
  });

  it("draws a fenced code block through the one markdown rule, with no per-kind copy", () => {
    // Arrange / Act
    const copies = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some((sel) => /pre\.md-code$/.test(sel)),
    );
    // Assert
    expect(copies.flatMap((rule) => rule.selectors)).toEqual([".md pre.md-code"]);
  });

  it.each(BUBBLE_CAP_LINES.map((cap) => [String(cap)]))("maps the %s cap to a line count", (cap) => {
    // Arrange / Act
    const rule = declarationsOf(`.bubble[data-cap-lines="${cap}"]`) ?? "";
    // Assert
    expect(rule).toMatch(cap === "feed" ? /--bubble-cap-lines:\s*var\(--feed-cap-lines\)/ : new RegExp(`--bubble-cap-lines:\\s*${cap}\\s*;`));
  });

  it("caps the scroll box in the one leading, at most the 50vh the expanded box takes", () => {
    // Arrange / Act
    const cap = rulesOf(stylesheet).find(
      (rule) => rule.selectors.includes(".bubble > .bubble-scroll") && /max-height/.test(rule.declarations),
    );
    // Assert
    expect(cap?.declarations).toMatch(
      /max-height:\s*min\(calc\(var\(--bubble-cap-lines\) \* var\(--md-line-h\) \* 1em\),\s*50vh\)/,
    );
  });

});

/** The six hex digits of TOKEN's declaration in BLOCK, as [r, g, b]. */
function rgbOf(block: string, token: string): [number, number, number] {
  const hex = new RegExp(`${token}:\\s*#([0-9a-fA-F]{6})\\s*;`).exec(block)?.[1];
  if (hex === undefined) throw new Error(`no six-digit ${token} in the block`);
  return [0, 2, 4].map((i) => Number.parseInt(hex.slice(i, i + 2), 16)) as [number, number, number];
}

/** The spread between an RGB triple's strongest and weakest channel. */
const chroma = ([r, g, b]: [number, number, number]): number => Math.max(r, g, b) - Math.min(r, g, b);

describe("the held prompt's fill: much more grey than blue", () => {
  it.each([
    ["light", () => declarationsOf(":root") ?? ""],
    ["dark", () => darkThemeBlock()],
  ])("carries a subtle blue hue in the %s theme", (_theme, block) => {
    // Arrange / Act
    const [r, g, b] = rgbOf(block(), "--held-prompt-bg");
    // Assert — blue is the strongest channel, however slightly.
    expect(b > r && b >= g).toBe(true);
  });

  it.each([
    ["light", () => declarationsOf(":root") ?? ""],
    ["dark", () => darkThemeBlock()],
  ])("is far greyer than the prompt blue in the %s theme", (_theme, block) => {
    // Arrange / Act
    const held = chroma(rgbOf(block(), "--held-prompt-bg"));
    const prompt = chroma(rgbOf(block(), "--user"));
    // Assert — at most a third of the prompt blue's saturation.
    expect(held * 3).toBeLessThanOrEqual(prompt);
  });

  it("caps the held variant at half the one bubble width token", () => {
    // Arrange / Act
    const rule = declarationsOf('.bubble[data-variant="held"]') ?? "";
    // Assert
    expect(rule).toMatch(/max-width:\s*calc\(var\(--bubble-max-width\) \/ 2\)\s*;/);
  });

  it("is the one background the held variant sets", () => {
    // Arrange / Act
    const rule = declarationsOf('.bubble[data-variant="held"]') ?? "";
    // Assert
    expect(rule).toMatch(/--bubble-bg:\s*var\(--held-prompt-bg\)/);
  });
});

describe("the compaction summary's border is the compaction divider bar's", () => {
  it("borders the summary with the divider bar's own token", () => {
    // Arrange / Act
    const summary = declarationsOf('.bubble[data-variant="compaction"]') ?? "";
    const bar = declarationsOf(".sep-accent-compacted") ?? "";
    // Assert — one token, read by both; no copied value.
    expect([/border-color:\s*var\(--compact-rule\)/.test(summary), /background:\s*var\(--compact-rule\)/.test(bar)]).toEqual([
      true,
      true,
    ]);
  });

  it("gives the summary no fill of its own: it is a response bubble", () => {
    // Arrange / Act / Assert
    expect(stylesheet.replace(/\/\*[\s\S]*?\*\//g, "")).not.toMatch(/--compact-summary-bg/);
  });
});

/**
 * THE EXPAND-ONLY REGION (src/bubble/draw.ts `expandOnly`): chrome after a
 * bubble's scroll box that shows only while the one toggle has that box open.
 */
describe("the bubble's expand-only region", () => {
  /** Every rule whose selector names the expand-only class. */
  const regionRules = (): Rule[] =>
    rulesOf(stylesheet).filter((rule) => rule.selectors.some((sel) => sel.includes(`.${BUBBLE_EXPAND_ONLY_CLASS}`)));

  it("hides the region only behind a scroll box that is not expanded", () => {
    // Arrange / Act
    const selectors = regionRules().flatMap((rule) => rule.selectors);
    // Assert
    expect(selectors).toEqual([`.bubble-scroll:not(.expanded) ~ .${BUBBLE_EXPAND_ONLY_CLASS}`]);
  });

  it("takes the hidden region out of layout", () => {
    // Arrange / Act
    const declarations = regionRules().map((rule) => rule.declarations.trim());
    // Assert
    expect(declarations).toEqual(["display: none;"]);
  });
});

/**
 * THE HELD STATUS BADGES' COLORS (owner spec, 2026-09-23): each tone the one
 * table (src/tray/held-prompt.ts) assigns resolves to its semantic token, on the
 * tool cards' own `.badge`. WAITING red and INTERRUPTING green are the owner's.
 */
describe("the held status badges' colors", () => {
  const TOKENS: Readonly<Record<string, string>> = {
    ok: "--ok",
    err: "--err",
    run: "--thinking",
    muted: "--muted",
    amber: "--merge-border",
    teal: "--hibernated",
  };

  /**
   * The color a `.badge` of TONE is painted: the held card's own rule's when it
   * sets one, else the shared `.badge` rule's (a held rule may add only motion).
   */
  function toneColor(tone: string): string | undefined {
    const colorOf = (rule: string | undefined): string | undefined =>
      /(?:^|;)\s*color:\s*([^;]+);/.exec(rule ?? "")?.[1]?.trim();
    return colorOf(declarationsOf(`.badge.held-badge.${tone}`)) ?? colorOf(declarationsOf(`.badge.${tone}`));
  }

  it.each(Object.entries(TOKENS))("paints the %s tone in %s", (tone, token) => {
    // Arrange / Act
    const color = toneColor(tone);
    // Assert
    expect(color).toBe(`var(${token})`);
  });

  it("assigns only tones the stylesheet paints", () => {
    // Arrange / Act
    const unpainted = [...new Set(Object.values(HELD_STATUS_BADGES))].filter((tone) => toneColor(tone) === undefined);
    // Assert
    expect(unpainted).toEqual([]);
  });

  it("paints a prompt waiting for the turn's end red", () => {
    // Arrange / Act / Assert
    expect(toneColor(HELD_STATUS_BADGES.holdForTurnEnd)).toBe("var(--err)");
  });

  it("paints an interrupting prompt green", () => {
    // Arrange / Act / Assert
    expect(toneColor(HELD_STATUS_BADGES.interject)).toBe("var(--ok)");
  });

  it("leaves no rule for the retired queued badge classes", () => {
    // Arrange / Act
    const retired = rulesOf(stylesheet).filter((rule) =>
      rule.selectors.some((sel) => /\.queued-(?:badge|accepted)\b/.test(sel)),
    );
    // Assert
    expect(retired.map((rule) => rule.selectors.join(", "))).toEqual([]);
  });
});
