/**
 * skill-body — the ONE rendering of a skill's SKILL.md, and the check that its
 * call site actually uses that one rendering.
 *
 * The shared-shape test holds the card path to the helper's own markup, so a
 * hand-rolled section there fails rather than passing silently.
 */
import { describe, expect, it } from "vitest";
import { SkillBodySection } from "../src/skill-body.js";
import { renderItem, type PanelContext } from "../src/render.js";
import type { ToolItem } from "../src/store.js";

const BODY = "# Demo\n\n**do** the thing";

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

describe("the card path uses the one section", () => {
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

  it("draws the helper's own section on the CARD path", () => {
    expect(cardHtml(BODY)).toContain(SkillBodySection(BODY));
  });

  it("draws no section at all for a call that carried no body", () => {
    expect(cardHtml("")).not.toContain("skill-content");
  });
});
