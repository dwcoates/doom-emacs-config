/**
 * skill-body — the ONE rendering of a skill's SKILL.md, and the check that
 * both deliveries of it actually share that one rendering.
 *
 * A skill's contents reach this frontend two ways: on the `skillBody`
 * conversation arm for an invocation that opened no bubble, and on
 * `AsyncSkillBubble.body` for one that did. The invariant the helper exists to
 * guarantee is that the reader cannot tell which arrived — so the last two
 * tests here hold both call sites to the helper's own markup, and a hand-rolled
 * section on either side fails rather than passing silently.
 */
import { describe, expect, it } from "vitest";
import { SkillBodySection } from "../src/skill-body.js";
import { AsyncBubbleCard, bubbleFoldId, type AsyncRenderContext } from "../src/async-render.js";
import { AsyncBubbleRegistry } from "../src/async-routing.js";
import type { AsyncBubble } from "../src/async-bubble.js";
import { renderItem, type PanelContext } from "../src/render.js";
import type { ToolItem } from "../src/store.js";

const BODY = "# Demo\n\n**do** the thing";
const NO_FOLD = { droppedBefore: 0, tailCap: 0 };
const LIVE = { case: "live", value: { lastActivityMs: 0 } } as const;

describe("SkillBodySection", () => {
  it("renders the skill's markdown rather than its escaped source", () => {
    expect(SkillBodySection(BODY)).toContain("<strong>do</strong>");
  });

  it("wears the capped-section class the click-to-expand mechanic keys on", () => {
    expect(SkillBodySection(BODY)).toContain("skill-content");
  });

  it("draws NOTHING for a body that has not resolved, rather than an empty box", () => {
    expect(SkillBodySection("")).toBe("");
  });

  it("escapes raw markup in the skill file rather than passing it through", () => {
    expect(SkillBodySection("<img onerror=1>")).not.toContain("<img");
  });
});

describe("both deliveries share the one section", () => {
  /** The card path: a Skill call whose body arrived on the conversation arm. */
  function cardHtml(body: string): string {
    const item: ToolItem = {
      kind: "tool",
      toolUseId: "tu1",
      toolName: "Skill",
      messageId: "m1",
      ts: "2026-05-24T10:00:00.000Z",
      input: { skill: "demo" },
      inputJson: `{"skill":"demo"}`,
      inputDone: true,
      skillBody: body,
    };
    return renderItem(item, undefined, undefined, {
      children: new Map(),
      isOpen: () => true,
    } satisfies PanelContext);
  }

  /** The bubble path: a skill window whose body arrived on the bubble. */
  function bubbleHtml(body: string): string {
    const bubble: AsyncBubble = {
      id: "b1",
      workspace: "/w",
      originToolUseId: "tu1",
      parentBubbleId: "",
      label: "/demo",
      startedAtMs: 0,
      liveness: LIVE,
      kind: {
        case: "skill",
        value: { skillName: "demo", args: "", body, emissions: [], fold: NO_FOLD },
      },
    };
    const registry = new AsyncBubbleRegistry();
    registry.applyDelta({ workspace: "/w", opened: [bubble], updates: [], throughSeq: 1, fence: "f" });
    const ctx: AsyncRenderContext = {
      registry,
      isOpen: () => true,
      renderEmissions: () => "",
    };
    return AsyncBubbleCard(registry.get("b1")!, ctx, new Set());
  }

  it("draws the helper's own section on the CARD path", () => {
    expect(cardHtml(BODY)).toContain(SkillBodySection(BODY));
  });

  it("draws the helper's own section on the BUBBLE path", () => {
    expect(bubbleHtml(BODY)).toContain(SkillBodySection(BODY));
  });

  it("keeps the fold id stable so an open body survives a re-render", () => {
    expect(bubbleHtml(BODY)).toContain(bubbleFoldId("b1"));
  });
});
