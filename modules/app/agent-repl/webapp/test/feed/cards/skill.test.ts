// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedRowSchema,
  FeedSkillSchema,
  type FeedSkill,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedSkill, SKILL_OUTCOME_ARMS } from "../../../src/feed/cards/skill.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { harness, rowContext } from "../harness.js";

/** A row context for a card drawn on its own, optionally over a previous draw. */
function rc(previous?: HTMLElement): RowContext {
  return rowContext(harness().ctx, create(FeedRowSchema, {}), { previous }) as RowContext;
}

const INVOCATION = "/graphify";

/** A skill card with the composed invocation line and one OUTCOME. */
function skill(outcome: FeedSkill["outcome"]): FeedSkill {
  return create(FeedSkillSchema, { invocation: { text: INVOCATION }, outcome });
}

/** The loaded arm, with a document and optionally an allowances sentence. */
function loaded(markdown: string, allowances?: string): FeedSkill["outcome"] {
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

describe("drawFeedSkill", () => {
  it("wears the teal skill card's own classes", () => {
    expect(drawFeedSkill(skill({ case: "running", value: {} }), rc()).className).toBe(
      "tool-card tool-skill",
    );
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
      const el = drawFeedSkill(skill(c.outcome as FeedSkill["outcome"]), rc());
      expect(el.getAttribute("data-state")).toBe(c.arm);
    });

    it(`badges the ${c.arm} arm`, () => {
      const el = drawFeedSkill(skill(c.outcome as FeedSkill["outcome"]), rc());
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

  it("folds the document by default", () => {
    const el = drawFeedSkill(skill(loaded("# heading")), rc());
    expect(el.querySelector<HTMLElement>(".skill-content")?.hidden).toBe(true);
  });

  it("opens the document on the reader's click", () => {
    const el = drawFeedSkill(skill(loaded("# heading")), rc());
    el.querySelector<HTMLButtonElement>('[data-fold="skill-document"]')?.click();
    expect(el.querySelector<HTMLElement>(".skill-content")?.hidden).toBe(false);
  });

  it("keeps a document the reader opened open across a re-push", () => {
    const first = drawFeedSkill(skill(loaded("# heading")), rc());
    first.querySelector<HTMLButtonElement>('[data-fold="skill-document"]')?.click();
    const second = drawFeedSkill(skill(loaded("# heading\n\nmore")), rc(first));
    expect(second.querySelector<HTMLElement>(".skill-content")?.hidden).toBe(false);
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
