/**
 * The cross-language color contract, asserted from the TypeScript side.
 *
 * `proto/vocab/render-colors.json` is the ONE table naming which of the six
 * colors each RenderState and each ErrorClass takes. Go asserts against it,
 * elisp asserts against it, and this file is the third corner — which is what
 * makes a divergence between the three fail loudly instead of quietly.
 *
 * It checks the ASSIGNMENT, not the hex. Each renderer keeps its own shades
 * (a tab-bar background and a CSS dot legitimately want different ones); what
 * may never differ is which color a state gets.
 */
import { describe, expect, it } from "vitest";
import { ERROR_CLASSES, RenderState } from "../src/frontend-proto.js";
import { WORKSPACE_STATUSES } from "../src/sidebar.js";
// The fixture and the stylesheet are read as SOURCE, the same way the other
// contract tests read the CSS: the assertion is about what is checked in, not
// about what some runtime resolved.
import fixtureRaw from "../../proto/vocab/render-colors.json?raw";
import css from "../src/styles.css?raw";

interface ColorFixture {
  colors: string[];
  precedence: string[];
  render_states: Record<string, string>;
  /**
   * Surface name -> state name -> color. A surface may paint a state
   * differently from `render_states`, but only by declaring it here. This
   * renderer takes none: its rail carries a glyph and a status word beside
   * every dot, so a state that spends no color still reports itself.
   */
  surface_overrides?: Record<string, Record<string, string>>;
  error_classes: Record<string, string>;
}

const fixture: ColorFixture = JSON.parse(fixtureRaw) as ColorFixture;

/** The CSS variable each of the six colors is drawn with in this renderer. */
const COLOR_TOKENS: Record<string, string> = {
  blue: "--init",
  teal: "--hibernated",
  purple: "--blocked",
  red: "--err",
  yellow: "--async",
  green: "--ok",
};

describe("the shared color fixture", () => {
  it("covers every RenderState this frontend can decode", () => {
    // Arrange / Act — the decoder's enum is this end's authority on the
    // vocabulary, so a state it accepts with no color assignment would reach
    // a renderer that has to invent one.
    const missing = Object.keys(RenderState)
      .filter((k) => Number.isNaN(Number(k)))
      .filter((name) => fixture.render_states[`RENDER_STATE_${name}`] === undefined);
    // Assert
    expect(missing).toEqual([]);
  });

  it("covers every ErrorClass this frontend can decode", () => {
    // Arrange / Act
    const missing = ERROR_CLASSES.filter(
      (c) => fixture.error_classes[`ERROR_CLASS_${c}`] === undefined,
    );
    // Assert
    expect(missing).toEqual([]);
  });

  it("names a token this stylesheet actually defines for each of the six", () => {
    // Arrange / Act — a color the fixture ranks but this renderer cannot draw
    // is an assignment nothing can honor.
    const undefinedTokens = fixture.colors.filter(
      (c) => !css.includes(`${COLOR_TOKENS[c]}:`),
    );
    // Assert
    expect(undefinedTokens).toEqual([]);
  });

  it("ranks the six colors strongest-claim-first, matching the SSM", () => {
    // Arrange / Act — the SSM is the sole precedence authority; this restates
    // its order and may never reorder it. Teal sits between blue and purple:
    // the SSM ranks `hibernated` at 15, directly below the blue band and above
    // purple's 20, because hibernation makes the same actionability claim blue
    // does and only its reason is benign.
    // Assert
    expect(fixture.precedence).toEqual(["blue", "teal", "purple", "red", "yellow", "green"]);
  });

  it("keeps hibernated and severed on DIFFERENT colors", () => {
    // Arrange / Act — the two used to be one RENDER_STATE_DORMANT, which meant
    // both "asleep on purpose to reclaim ~500MB" and "the substrate is broken".
    // A color that fires on both means neither.
    // Assert
    expect(fixture.render_states.RENDER_STATE_HIBERNATED).not.toBe(
      fixture.render_states.RENDER_STATE_SEVERED,
    );
  });

  it("grants this renderer no surface override", () => {
    // Arrange / Act — the Emacs tab bar overrides the in-flight merge states
    // to red because it has neither a badge nor a glyph, so "none" renders
    // there as no state at all. This rail does have both, and a red merge dot
    // would collide with the thinking dot sitting beside it in the same list.
    // Assert
    expect(fixture.surface_overrides?.webapp).toBeUndefined();
  });

  it("ranks hibernated above every green state", () => {
    // Arrange — the tempting mistake is to rank a benign state below green.
    // That would let a stale `thinking` row from the turn a workspace was
    // hibernated after mask a workspace that is genuinely asleep.
    const rank = (color: string) => fixture.precedence.indexOf(color);
    // Act / Assert
    expect(rank(fixture.render_states.RENDER_STATE_HIBERNATED)).toBeLessThan(
      rank(fixture.render_states.RENDER_STATE_IDLE),
    );
  });
});

describe("the two-sided sidebar contract", () => {
  it("carries a status for every RenderState the fixture colors", () => {
    // Arrange / Act — sidebar.el asserts in a COMMENT that its wire table and
    // this union "are one contract and MUST stay in sync", and nothing checked
    // it. This is the check, from the fixture both sides now share.
    const missing = Object.entries(fixture.render_states)
      .filter(([name]) => name !== "RENDER_STATE_UNSPECIFIED")
      .map(([name]) => name.replace("RENDER_STATE_", "").toLowerCase().replace(/_/g, "-"))
      // Two states carry no roster status of their own, and BOTH frontends
      // collapse them the same way — which is the agreement being asserted:
      //   stop-failed → vendor-blocked (deprecated upstream, always was purple)
      //   idle        → ready (agent-repl--sidebar-status-wire maps :idle to
      //                 "ready" too; they are one green with one dot)
      .filter((status) => status !== "stop-failed" && status !== "idle")
      .filter((status) => !WORKSPACE_STATUSES.has(status));
    // Assert
    expect(missing).toEqual([]);
  });
});

describe("every colored status has a dot the stylesheet actually draws", () => {
  /** The dot rule's body for a status, or "" when no rule exists at all. */
  function dotRule(status: string): string {
    const at = css.indexOf(`#ws-sidebar .st-${status} {`);
    if (at === -1) return "";
    return css.slice(at, css.indexOf("}", at));
  }

  // Every state the fixture assigns one of the five colors, under the same two
  // collapses the sidebar contract above declares. A status with no rule
  // renders a colorless 9px hole, which is how `interrupted` shipped invisible.
  const colored = Object.entries(fixture.render_states)
    .filter(([, color]) => color !== "none")
    .map(([name, color]) => [name.replace("RENDER_STATE_", "").toLowerCase().replace(/_/g, "-"), color])
    .filter(([status]) => status !== "stop-failed" && status !== "idle");

  for (const [status, color] of colored) {
    it(`draws the ${status} dot in the ${color} the fixture assigns it`, () => {
      // Arrange / Act
      const rule = dotRule(status);
      // Assert
      expect(rule).toContain(`var(${COLOR_TOKENS[color]})`);
    });
  }
});

describe("the card's class color IS the state's color", () => {
  it("draws an INTERNAL failure with the blue the fixture assigns its states", () => {
    // Arrange / Act — the whole point of one table: a user cannot see a blue
    // workspace explained by a card of some other color.
    const want = COLOR_TOKENS[fixture.error_classes.ERROR_CLASS_INTERNAL];
    // Assert
    expect(css).toContain(`.failure-card.failure-internal {\n  border-color: var(${want});`);
  });

  it("draws an API failure with the purple the fixture assigns its state", () => {
    // Arrange / Act
    const want = COLOR_TOKENS[fixture.error_classes.ERROR_CLASS_API];
    // Assert
    expect(css).toContain(`.failure-card.failure-api {\n  border-color: var(${want});`);
  });

  it("assigns INTERNAL the same color the degraded state takes", () => {
    // Arrange / Act — INTERNAL resolves the workspace degraded/blue, so the
    // two entries must agree in the fixture itself, not merely in the CSS.
    // Assert
    expect(fixture.error_classes.ERROR_CLASS_INTERNAL).toBe(
      fixture.render_states.RENDER_STATE_DEGRADED,
    );
  });

  it("assigns API the same color the vendor-blocked state takes", () => {
    // Arrange / Act
    // Assert
    expect(fixture.error_classes.ERROR_CLASS_API).toBe(
      fixture.render_states.RENDER_STATE_VENDOR_BLOCKED,
    );
  });
});
