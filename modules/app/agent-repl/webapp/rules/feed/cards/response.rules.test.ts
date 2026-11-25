// @vitest-environment jsdom
/**
 * The response card's source and stylesheet rules: its one parse of a carried
 * reveal fact and its one note maker (src/feed/cards/response.ts), and the
 * stylesheet's bubble cap, selection ring, purple highlight, note rule and
 * one-line ellipsis clamp (src/styles.css).
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import stylesheet from "../../../src/styles.css?raw";
import responseSource from "../../../src/feed/cards/response.ts?raw";
import { drawFeedResponse } from "../../../src/feed/cards/response.js";
import { installClickExpand } from "../../../src/expand.js";
import { HAS_MORE_CLASS, refreshHasMore } from "../../../src/feed/bubble-more.js";
import { BUBBLE_SCROLL_CLASS } from "../../../src/feed/bubble-scroll.js";
import { plainProse } from "../../../test/prose.js";
import { response, rowContext } from "../../../test/feed/cards/response-context.js";
import { selectorOf } from "../../stylesheet-text.js";

/** The card's suite's clock and page, as its drawing tests have them (test/feed/cards/response.test.ts). */
beforeEach(() => {
  vi.useFakeTimers({
    toFake: ["setTimeout", "clearTimeout", "setInterval", "clearInterval", "Date", "requestAnimationFrame", "cancelAnimationFrame"],
  });
  vi.setSystemTime(0);
});
afterEach(() => {
  vi.useRealTimers();
  vi.unstubAllGlobals();
  document.body.replaceChildren();
});

describe("carriedNumber", () => {
  it("is the one parse every carried reveal fact goes through", () => {
    expect(responseSource.match(/Number\.parse(?:Int|Float)\(/g)).toHaveLength(1);
  });
});

describe("a final response revised from highlighted words", () => {
  it("highlights a final response's words in the one purple", () => {
    // Arrange
    const rule = /\.bubble\.final-response:not\(\.thinking-bubble\) \.response-prose ::selection \{([^}]*)\}/.exec(stylesheet);
    // Assert
    expect(rule?.[1]).toContain("background: var(--blocked)");
  });
});

describe("a footer note under a response's prose", () => {
  it("is drawn for both notes by the one stylesheet rule", () => {
    // Arrange
    const rule = /\.response-cut-short-marker,\s*\.response-revision-failed\s*\{([^}]*)\}/.exec(stylesheet);
    // Assert
    expect(rule?.[1]).toContain("color: var(--err)");
  });

  it("is the only way the response card makes a note", () => {
    // Arrange: the card's own source, every note going through footerNote.
    const made = responseSource.match(/document\.createElement\("span"\)/g) ?? [];
    // Assert
    expect(made).toHaveLength(1);
  });
});

describe("the wrapped tree a settled response carries", () => {
  it("resolves a tree bubble's width to fit-content, capped at 77%, never a full-cap pin", () => {
    // Assert — the 77% the cap token resolves to.
    expect(stylesheet).toMatch(/--bubble-max-width:\s*77%;/);
  });
});

describe("the data-driven final-answer green", () => {
  it("keeps the final answer's border and fill on a selected final answer, ringing it purple", () => {
    // Assert — the selection's rule sets the inset ring alone, so the answer's
    // border and fill stay.
    expect(stylesheet).toMatch(/\.bubble\.entry-selected\s*\{\s*outline:\s*0\.5px solid var\(--blocked\);\s*outline-offset:\s*-0\.5px;\s*\}/);
  });
});

/**
 * A ONE-LINE BUBBLE SAYS "MORE" WITH THE ELLIPSIS, NEVER THE FADE (owner
 * request, 2026-10-08): the held prompt's own mechanism (`data-more`
 * `ellipsis`), the collapsed body clamped to its line. jsdom lays nothing
 * out, so each case states the geometry the engine would give it.
 */
describe("the one-line bubble's more signal", () => {
  const LINE_PX = 20;

  /** The ellipsis clamp on a collapsed body, as the stylesheet writes it. */
  const CLAMPED_BODY = selectorOf(/([^{}]*\.bubble-body)\s*\{[^{}]*-webkit-line-clamp/, "ellipsis clamp");

  /** The box whose fade the ellipsis hides, as the stylesheet writes it (the pseudo dropped). */
  const FADE_HIDDEN_ON = selectorOf(/([^{}]*\.bubble-scroll)::after\s*\{\s*display:\s*none;\s*\}/, "hidden fade");

  /** A drawn response (THINKING or not) in ARM whose body renders LINES lines under a one-line cap. */
  function mounted(thinking: boolean, arm: "success" | "update", lines: number): { box: HTMLElement; body: HTMLElement } {
    const host = document.createElement("div");
    installClickExpand(host, () => "", (section) => refreshHasMore(section));
    const bubble = drawFeedResponse(
      response({ thinking, interim: !thinking, result: { case: arm, value: { prose: { prose: plainProse("weighing") } } } }),
      rowContext(),
    );
    host.append(bubble);
    document.body.append(host);
    const box = bubble.querySelector(`:scope > .${BUBBLE_SCROLL_CLASS}`) as HTMLElement;
    const body = box.querySelector(":scope > .bubble-body") as HTMLElement;
    const shown = Math.min(lines, 1) * LINE_PX;
    Object.defineProperty(box, "clientHeight", { configurable: true, value: shown });
    // A clamped body's own box ends at its cap; an unclamped one holds every line.
    const own = body.matches(CLAMPED_BODY) ? shown : lines * LINE_PX;
    Object.defineProperty(body, "offsetHeight", { configurable: true, value: own });
    Object.defineProperty(body, "scrollHeight", { configurable: true, value: lines * LINE_PX });
    refreshHasMore(box);
    return { box, body };
  }

  /** Whether the collapsed line ends in the ellipsis: the clamp holds it, and something follows it. */
  const ellipsized = ({ box, body }: { box: HTMLElement; body: HTMLElement }): boolean =>
    body.matches(CLAMPED_BODY) && box.classList.contains(HAS_MORE_CLASS);

  /** Whether the box draws the fade: it has more, and nothing hides the fade. */
  const faded = ({ box }: { box: HTMLElement }): boolean =>
    box.classList.contains(HAS_MORE_CLASS) && !box.matches(FADE_HIDDEN_ON);

  it.each([
    { name: "an arriving thinking bubble", thinking: true, arm: "update" },
    { name: "a landed thinking bubble", thinking: true, arm: "success" },
    { name: "an arriving interim response", thinking: false, arm: "update" },
    { name: "a settled interim response", thinking: false, arm: "success" },
  ] as const)("ends $name's one line in the ellipsis when more follows", ({ thinking, arm }) => {
    // Arrange / Act
    const drawn = mounted(thinking, arm, 4);
    // Assert
    expect([ellipsized(drawn), faded(drawn)]).toEqual([true, false]);
  });

  it("draws neither the ellipsis nor the fade on a one-line bubble with no more", () => {
    // Arrange / Act
    const drawn = mounted(true, "success", 1);
    // Assert
    expect([ellipsized(drawn), faded(drawn)]).toEqual([false, false]);
  });
});
