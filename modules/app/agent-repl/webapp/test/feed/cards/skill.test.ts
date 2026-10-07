// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  FeedRowSchema,
  FeedSkillSchema,
  type FeedSkill,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedSkill, SKILL_OUTCOME_ARMS } from "../../../src/feed/cards/skill.js";
import { EXPANDED_CLASS, installClickExpand } from "../../../src/expand.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { harness, rowContext } from "../harness.js";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";
import {
  HAS_MORE_CLASS,
  TITLE_FOLD_CLASS,
  TITLE_FOLD_STANDALONE_CLASS,
} from "../../../src/feed/bubble-more.js";
import { fireResize } from "../../resize-observer.js";
import { measureTitle } from "../title-measure.js";

/**
 * The oneof as an INIT shape rather than a built message: the fixtures below
 * hand plain object literals to `create`, which is what protobuf-es accepts,
 * while the built message type would demand a `$typeName` on every arm.
 */
type InitOfFeedSkill = MessageInitShape<typeof FeedSkillSchema>["outcome"];

/** A row context for a card drawn on its own, optionally over a previous draw. */
function rc(previous?: HTMLElement): RowContext {
  return rowContext(harness().ctx, create(FeedRowSchema, {}), { previous });
}

const INVOCATION = "/graphify";

/** A skill card with the composed invocation line and one OUTCOME. */
function skill(outcome: InitOfFeedSkill): FeedSkill {
  return create(FeedSkillSchema, { invocation: { text: INVOCATION }, outcome });
}

/** The loaded arm, with a document and optionally an allowances sentence. */
function loaded(markdown: string, allowances?: string): InitOfFeedSkill {
  return {
    case: "loaded",
    value: {
      document: { markdown },
      allowances: allowances === undefined ? undefined : { text: allowances },
    },
  };
}

beforeEach(() => {
  vi.useFakeTimers();
});

// The fake clock is this file's own; hand the real one back so a
// later file sharing this worker never inherits a frozen timer.
afterEach(() => {
  vi.useRealTimers();
});

describe("drawFeedSkill", () => {
  it("wears the skill card's own classes", () => {
    expect(drawFeedSkill(skill({ case: "running", value: {} }), rc()).className).toBe(
      "tool-card tool-skill",
    );
  });

  // The async teal wash is RETIRED (owner ruling, 2026-09-14): a skill card is
  // an ordinary grey tool card now. The `tool-skill` class stays (it still
  // marks the row's `data-unit` for nested work), but the sheet no longer
  // paints it `--async-card` -- it takes the shared tool-card fill (`--tool-card-bg`) instead.
  it("no longer paints the async teal wash, taking the grey card fill", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const card = drawFeedSkill(skill({ case: "running", value: {} }), rc());
      document.body.replaceChildren(card);
      // Act / Assert: the shared card fill, never the retired async teal.
      expect(cascadedValue(card, "background")).toBe("var(--tool-card-bg)");
    } finally {
      remove();
    }
  });

  it("draws the invocation line verbatim as the head", () => {
    const el = drawFeedSkill(skill({ case: "running", value: {} }), rc());
    expect(el.querySelector(".tool-name")?.textContent).toBe(INVOCATION);
  });

  const states = [
    { arm: "running", outcome: { case: "running", value: {} }, badge: "loading" },
    { arm: "loaded", outcome: loaded("# doc"), badge: "loaded" },
    { arm: "failed", outcome: { case: "failed", value: { text: "no such skill" } }, badge: "failed" },
    { arm: "denied", outcome: { case: "denied", value: {} }, badge: "denied" },
  ] as const;

  for (const c of states) {
    it(`carries ${c.arm} as the card's state`, () => {
      const el = drawFeedSkill(skill(c.outcome), rc());
      expect(el.getAttribute("data-state")).toBe(c.arm);
    });

    it(`badges the ${c.arm} arm`, () => {
      const el = drawFeedSkill(skill(c.outcome), rc());
      expect(el.querySelector(".tool-head .badge")?.textContent).toBe(c.badge);
    });
  }

  it("draws every outcome the schema carries", () => {
    expect([...SKILL_OUTCOME_ARMS].sort()).toEqual(
      ["running", "loaded", "failed", "denied"].sort(),
    );
  });

  it("draws no document box while the skill is still running", () => {
    const el = drawFeedSkill(skill({ case: "running", value: {} }), rc());
    expect(el.querySelector(".skill-content")).toBeNull();
  });

  it("draws no document box for a denied invocation", () => {
    const el = drawFeedSkill(skill({ case: "denied", value: {} }), rc());
    expect(el.querySelector(".skill-content")).toBeNull();
  });

  it("renders the loaded document as markdown", () => {
    const el = drawFeedSkill(skill(loaded("# heading")), rc());
    expect(el.querySelector(".skill-content")?.innerHTML).toContain("<h1>heading</h1>");
  });

  it("wears the shared capped box on the document, so it scrolls rather than clips", () => {
    const el = drawFeedSkill(skill(loaded("# heading")), rc());
    expect(el.querySelector(".skill-content")?.className).toBe(
      "tool-output skill-content skill-content-md",
    );
  });

  it("starts collapsed, marking the whole card one click-to-expand fold", () => {
    // Card-level fold (owner ruling, 2026-09-15): the card is a `.tool-fold`
    // and begins without `.expanded`, so it opens as a unit on a click.
    const el = drawFeedSkill(skill(loaded("# heading")), rc());
    expect(el.classList.contains("tool-fold")).toBe(true);
    expect(el.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("shows NO document preview while collapsed", () => {
    // The section is HIDDEN on a collapsed card, not a height-capped peek.
    const remove = installStylesheet();
    try {
      const el = drawFeedSkill(skill(loaded("# heading")), rc());
      document.body.replaceChildren(el);
      expect(cascadedValue(el.querySelector(".skill-content") as Element, "display")).toBe("none");
    } finally {
      remove();
    }
  });

  it("reveals the document, bounded by the card's ceiling alone, once the card is expanded", () => {
    const remove = installStylesheet();
    try {
      const el = drawFeedSkill(skill(loaded("# heading")), rc());
      el.classList.add(EXPANDED_CLASS);
      document.body.replaceChildren(el);
      const doc = el.querySelector(".skill-content") as Element;
      expect(cascadedValue(doc, "display")).toBe("block");
      expect(cascadedValue(doc, "max-height")).toBe("none");
    } finally {
      remove();
    }
  });

  it("opens the card on the reader's click, via the feed's card-level fold", () => {
    // The existing feed-wide click-to-expand toggles the whole card open; a
    // click on the head (the collapsed face) is what the reader aims at.
    const el = drawFeedSkill(skill(loaded("# heading")), rc());
    const feed = document.createElement("div");
    feed.append(el);
    document.body.replaceChildren(feed);
    installClickExpand(feed, () => "");
    (el.querySelector(".tool-head") as HTMLElement).dispatchEvent(
      new MouseEvent("click", { bubbles: true }),
    );
    expect(el.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("draws the allowances sentence verbatim when the skill declared any", () => {
    const el = drawFeedSkill(skill(loaded("# doc", "allows: Bash, Write")), rc());
    expect(el.querySelector(".skill-allowances")?.textContent).toBe("allows: Bash, Write");
  });

  it("draws no allowances line when the skill declared none", () => {
    const el = drawFeedSkill(skill(loaded("# doc")), rc());
    expect(el.querySelector(".skill-allowances")).toBeNull();
  });

  it("draws the failed reason verbatim", () => {
    const el = drawFeedSkill(skill({ case: "failed", value: { text: "no such skill" } }), rc());
    expect(el.querySelector(".skill-failed")?.textContent).toBe("no such skill");
  });

  it("draws no nest slot of its own — the feed core owns that seam", () => {
    const el = drawFeedSkill(skill(loaded("# doc")), rc());
    expect(el.querySelector("[data-nest]")).toBeNull();
  });
});

describe("drawFeedSkill malformed input", () => {
  it("refuses a card whose outcome oneof is unset", () => {
    expect(() => drawFeedSkill(create(FeedSkillSchema, { invocation: { text: "/x" } }), rc())).toThrow(
      MalformedView,
    );
  });

  it("refuses a card with no invocation line", () => {
    const u = create(FeedSkillSchema, { outcome: { case: "running", value: {} } });
    expect(() => drawFeedSkill(u, rc())).toThrow(MalformedView);
  });

  it("refuses a loaded card with no document", () => {
    const u = create(FeedSkillSchema, {
      invocation: { text: "/x" },
      outcome: { case: "loaded", value: {} },
    });
    expect(() => drawFeedSkill(u, rc())).toThrow(MalformedView);
  });

  it("refuses an outcome arm this build does not know", () => {
    const u = skill({ case: "running", value: {} });
    (u as unknown as { outcome: { case: string; value: unknown } }).outcome = {
      case: "quarantined",
      value: {},
    };
    expect(() => drawFeedSkill(u, rc())).toThrow(MalformedView);
  });
});

/**
 * THE INVOCATION IS THE CARD'S TITLE (owner ruling, 2026-09-23): the one
 * two-line title fold (title-fold.ts). A loaded card is a `.tool-fold` and owns
 * it; a card in any other arm has no fold, so the title is its own.
 */
describe("drawFeedSkill: the title fold", () => {
  /** A connected card of OUTCOME, and its invocation line. */
  function drawn(outcome: InitOfFeedSkill) {
    const el = drawFeedSkill(skill(outcome), rc());
    document.body.replaceChildren(el);
    return { el, title: el.querySelector(".tool-head > .tool-name") as HTMLElement };
  }

  const UNFOLDED: { arm: string; outcome: InitOfFeedSkill }[] = [
    { arm: "running", outcome: { case: "running", value: {} } },
    { arm: "failed", outcome: { case: "failed", value: { text: "no such skill" } } },
    { arm: "denied", outcome: { case: "denied", value: {} } },
  ];

  it("marks a loaded card's invocation with the one title-fold class", () => {
    // Arrange / Act
    const { title } = drawn(loaded("# doc"));

    // Assert
    expect(title.classList.contains(TITLE_FOLD_CLASS)).toBe(true);
  });

  it("defers a loaded card's title to the card's own fold", () => {
    // Arrange / Act
    const { title } = drawn(loaded("# doc"));

    // Assert
    expect(title.classList.contains(TITLE_FOLD_STANDALONE_CLASS)).toBe(false);
  });

  it.each(UNFOLDED)("makes a $arm card's title its own fold, since the card has none", ({ outcome }) => {
    // Arrange / Act
    const { title } = drawn(outcome);

    // Assert
    expect([...title.classList]).toEqual(["tool-name", TITLE_FOLD_CLASS, TITLE_FOLD_STANDALONE_CLASS]);
  });

  it("wears has-more when the invocation overflows its one line", () => {
    // Arrange
    const { title } = drawn(loaded("# doc"));
    measureTitle(title, true);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("keeps has-more off an invocation that fits its one line", () => {
    // Arrange
    const { title } = drawn(loaded("# doc"));
    measureTitle(title, false);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("drops has-more once a loaded card is expanded", () => {
    // Arrange
    const { el, title } = drawn(loaded("# doc"));
    measureTitle(title, true);
    fireResize(title);
    el.classList.add(EXPANDED_CLASS);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("opens a standalone title with the feed-wide click, the one toggle", () => {
    // Arrange
    const { el, title } = drawn({ case: "running", value: {} });
    const feed = document.createElement("div");
    feed.append(el);
    document.body.replaceChildren(feed);
    installClickExpand(feed, () => "");

    // Act
    title.dispatchEvent(new MouseEvent("click", { bubbles: true }));

    // Assert
    expect(title.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("drops has-more once a standalone title is expanded", () => {
    // Arrange
    const { title } = drawn({ case: "running", value: {} });
    measureTitle(title, true);
    fireResize(title);
    title.classList.add(EXPANDED_CLASS);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("clamps a collapsed card's invocation to one line", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { title } = drawn(loaded("# doc"));

      // Act / Assert
      expect(cascadedValue(title, "-webkit-line-clamp")).toBe("1");
    } finally {
      remove();
    }
  });

  it("shows the whole invocation once the loaded card is expanded", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { el, title } = drawn(loaded("# doc"));
      el.classList.add(EXPANDED_CLASS);

      // Act / Assert
      expect(cascadedValue(title, "-webkit-line-clamp")).toBe("none");
    } finally {
      remove();
    }
  });

  it("lays an expanded standalone title out whole, not in the 50vh section box", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { title } = drawn({ case: "running", value: {} });
      title.classList.add(EXPANDED_CLASS);

      // Act / Assert
      expect(cascadedValue(title, "max-height")).toBe("none");
    } finally {
      remove();
    }
  });
});
