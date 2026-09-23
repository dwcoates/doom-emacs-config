// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import stylesheet from "../../src/styles.css?raw";
import { FeedRowSchema, type FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  FEED_GROUP_CLASS,
  GROUP_TAB_MEMBER_ATTRIBUTE,
  activateGroupedMember,
  createToolGroupStore,
  groupGlyph,
  groupKey,
  groupKindOf,
  type GroupMember,
} from "../../src/feed/tool-group.js";
import {
  feedId,
  responseRow,
  subagentRow,
  toolCallRow,
  userPromptRow,
} from "./harness.js";

/** A FeedRow carrying one activity unit, by arm name, with an empty value. */
function activityRow(id: string, unit: string): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    row: { case: "activity", value: { unit: { case: unit, value: {} } } } as never,
  });
}

/** A FeedRow carrying a top-level row arm with an empty value. */
function rowArm(id: string, arm: string): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    row: { case: arm, value: {} } as never,
  });
}

/** A member whose element is a `<article data-feed-row>` wrapping a body. */
function member(id: string, body?: HTMLElement): GroupMember {
  const element = document.createElement("article");
  element.setAttribute("data-feed-row", id);
  if (body !== undefined) element.append(body);
  return { id, element };
}

describe("groupKindOf", () => {
  it("keys an inline tool call by its SPECIFIC tool name", () => {
    expect(groupKindOf(toolCallRow("a", "returned", { tool: "Bash" }))).toBe("tool:Bash");
  });

  it("keys Bash and Edit as DIFFERENT kinds, so they never share a group", () => {
    const bash = groupKindOf(toolCallRow("a", "returned", { tool: "Bash" }));
    const edit = groupKindOf(toolCallRow("b", "returned", { tool: "Edit" }));
    expect(bash).not.toBe(edit);
  });

  it("keys a skill card as skill", () => {
    expect(groupKindOf(activityRow("a", "skill"))).toBe("skill");
  });

  it("keys a hook card as hook", () => {
    expect(groupKindOf(activityRow("a", "hook"))).toBe("hook");
  });

  it("keys a synchronous subagent bubble as subagent", () => {
    expect(groupKindOf(subagentRow("a"))).toBe("subagent");
  });

  it("keys a detached subagent bubble as subagent, so it groups with the sync one", () => {
    expect(groupKindOf(subagentRow("a", { detached: true }))).toBe("subagent");
  });

  it("keys a detached shell head as shell", () => {
    expect(groupKindOf(rowArm("a", "shellHead"))).toBe("shell");
  });

  it("does not group a response bubble", () => {
    expect(groupKindOf(responseRow("a"))).toBeNull();
  });

  it("does not group a user prompt", () => {
    expect(groupKindOf(userPromptRow("a", "hi"))).toBeNull();
  });

  it("does not group a plan card", () => {
    expect(groupKindOf(activityRow("a", "plan"))).toBeNull();
  });

  it("does not group a findings card", () => {
    expect(groupKindOf(activityRow("a", "findings"))).toBeNull();
  });

  it("does not group an artifact card", () => {
    expect(groupKindOf(activityRow("a", "artifact"))).toBeNull();
  });

  it("does not group a merge bubble, which owns its own tabs", () => {
    expect(groupKindOf(activityRow("a", "merge"))).toBeNull();
  });

  it("does not group a question ask", () => {
    expect(groupKindOf(rowArm("a", "question"))).toBeNull();
  });

  it("does not group a permission ask", () => {
    expect(groupKindOf(rowArm("a", "permission"))).toBeNull();
  });
});

describe("groupGlyph", () => {
  it("draws the shell glyph for a Bash tool group", () => {
    expect(groupGlyph("tool:Bash")).toBe("$");
  });

  it("draws the shell glyph for a detached-shell group", () => {
    expect(groupGlyph("shell")).toBe("$");
  });

  it("falls back to the tool's initial for an unknown tool", () => {
    expect(groupGlyph("tool:Frobnicate")).toBe("F");
  });

  it("draws the generic glyph for an unknown non-tool kind", () => {
    expect(groupGlyph("mystery")).toBe("•");
  });
});

describe("createToolGroupStore", () => {
  it("draws ONE container with one tab per member, each icon + 1-based index", () => {
    // Arrange
    const store = createToolGroupStore();
    const members = [member("a"), member("b"), member("c")];
    // Act
    const wrapper = store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", members);
    // Assert: one group, three tabs, indices 1..3.
    const tabs = wrapper.querySelectorAll(".feed-group-tab");
    expect(tabs).toHaveLength(3);
    expect([...tabs].map((t) => t.querySelector(".feed-group-tab-index")?.textContent)).toEqual([
      "1",
      "2",
      "3",
    ]);
    expect([...tabs].map((t) => t.querySelector(".feed-group-tab-icon")?.textContent)).toEqual([
      "$",
      "$",
      "$",
    ]);
  });

  it("re-attaches no member already in place when the run grows", () => {
    // Arrange -- a re-attached member would lose the reader's scroll inside it.
    const store = createToolGroupStore();
    const members = [member("a"), member("b")];
    const wrapper = store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", members);
    const panel = wrapper.querySelector(".feed-group-panel");
    if (panel === null) throw new Error("the group drew no panel");
    const observer = new MutationObserver(() => undefined);
    observer.observe(panel, { childList: true });
    // Act
    store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", [...members, member("c")]);
    // Assert
    expect(observer.takeRecords().flatMap((record) => [...record.removedNodes])).toEqual([]);
  });

  it("moves every member's own element into the group's panel", () => {
    const store = createToolGroupStore();
    const members = [member("a"), member("b")];
    const wrapper = store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", members);
    expect(wrapper.querySelector('.feed-group-panel > [data-feed-row="a"]')).toBe(
      members[0].element,
    );
  });

  it("shows the NEWEST member of a brand-new group and hides the rest", () => {
    const store = createToolGroupStore();
    const members = [member("a"), member("b"), member("c")];
    store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", members);
    expect([members[0].element.hidden, members[1].element.hidden, members[2].element.hidden]).toEqual(
      [true, true, false],
    );
  });

  it("shows the member whose tab the reader selects", () => {
    // Arrange
    const store = createToolGroupStore();
    const members = [member("a"), member("b"), member("c")];
    const wrapper = store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", members);
    // Act: click the first tab.
    wrapper
      .querySelector<HTMLButtonElement>(`[${GROUP_TAB_MEMBER_ATTRIBUTE}="a"]`)
      ?.click();
    // Assert: member a is now the shown one.
    expect([members[0].element.hidden, members[2].element.hidden]).toEqual([false, true]);
  });

  it("keeps a member's own body untouched inside its tab, so its expand survives", () => {
    // Arrange: a member whose body is an expanded `.tool-fold` card.
    const store = createToolGroupStore();
    const fold = document.createElement("div");
    fold.className = "tool-fold expanded";
    const members = [member("a", fold), member("b")];
    store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", members);
    // Act: a live append re-lays the group.
    store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", [...members, member("c")]);
    // Assert: the reader's expand is still on the same element.
    expect(fold.classList.contains("expanded")).toBe(true);
  });

  it("PRESERVES the reader's selected tab across a live append", () => {
    // Arrange: the reader selects tab a in a two-member group.
    const store = createToolGroupStore();
    const a = member("a");
    const b = member("b");
    const key = groupKey("tool:Bash", "a");
    const wrapper = store.arrange(key, "tool:Bash", [a, b]);
    wrapper.querySelector<HTMLButtonElement>(`[${GROUP_TAB_MEMBER_ATTRIBUTE}="a"]`)?.click();
    // Act: a third same-kind card streams in and extends the run.
    const c = member("c");
    store.arrange(key, "tool:Bash", [a, b, c]);
    // Assert: the reader is still on a, not yanked onto the new newest.
    expect([a.element.hidden, c.element.hidden]).toEqual([false, true]);
  });

  it("reuses the SAME wrapper for a group placed again, so its state survives", () => {
    const store = createToolGroupStore();
    const key = groupKey("tool:Bash", "a");
    const first = store.arrange(key, "tool:Bash", [member("a"), member("b")]);
    const second = store.arrange(key, "tool:Bash", [member("a"), member("b"), member("c")]);
    expect(second).toBe(first);
  });

  it("disposes a group not placed since the previous prune", () => {
    // Arrange: a group placed and kept through one pass.
    const store = createToolGroupStore();
    const host = document.createElement("div");
    const wrapper = store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", [
      member("a"),
      member("b"),
    ]);
    host.append(wrapper);
    store.prune();
    expect(wrapper.parentElement).not.toBeNull();
    // Act: a later pass that does not place the group at all.
    store.prune();
    // Assert: its wrapper is detached.
    expect(wrapper.parentElement).toBeNull();
  });
});

describe("activateGroupedMember", () => {
  it("brings the tab holding an element to the front", () => {
    // Arrange: a group showing its newest member (b), with a hidden member a.
    const store = createToolGroupStore();
    const a = member("a");
    const b = member("b");
    store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", [a, b]);
    expect(a.element.hidden).toBe(true);
    // Act: reveal lands on member a (an inactive tab).
    activateGroupedMember(a.element);
    // Assert: a's tab is now the active one.
    expect(a.element.hidden).toBe(false);
  });

  it("leaves an element outside any group alone", () => {
    const loose = document.createElement("article");
    loose.setAttribute("data-feed-row", "x");
    // No throw, no group to touch.
    expect(() => activateGroupedMember(loose)).not.toThrow();
  });

  it("does nothing for an element that is in a group but names no row", () => {
    const store = createToolGroupStore();
    const wrapper = store.arrange(groupKey("tool:Bash", "a"), "tool:Bash", [
      member("a"),
      member("b"),
    ]);
    const group = wrapper.querySelector(`.${FEED_GROUP_CLASS}`);
    const stray = document.createElement("span");
    group?.append(stray);
    expect(() => activateGroupedMember(stray)).not.toThrow();
  });
});

describe("the tab strip's size (styles.css)", () => {
  // Owner-prescribed, in two rulings. The tabs were first halved (2026-09-20);
  // at that size they were hard to see, so every dimension then grew by a
  // QUARTER (2026-09-21) — the type, the icon gap and both paddings alike, so
  // the tab scales evenly rather than stretching in one direction. Pinned here,
  // alongside the module that draws them, so a future resize of
  // `.feed-group-strip`/`.feed-group-tab` is a deliberate edit to this test
  // rather than a silent drift.
  /** The declaration block for SELECTOR, exactly as it appears in the sheet. */
  function ruleBodyOf(selector: string): string {
    const pattern = new RegExp(
      `${selector.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")}\\s*\\{([^}]*)\\}`,
    );
    const match = pattern.exec(stylesheet);
    if (match === null) throw new Error(`no rule found for ${selector}`);
    return match[1];
  }

  it("grows the strip's gap and padding by a quarter", () => {
    const body = ruleBodyOf(".feed-group-strip");
    expect(body).toMatch(/(^|[;\s])gap\s*:\s*0\.156rem\s*;/);
    expect(body).toMatch(/(^|[;\s])padding\s*:\s*0\.062rem 0 0\.188rem\s*;/);
  });

  it("grows a tab's gap, padding and font-size by the same quarter", () => {
    const body = ruleBodyOf(".feed-group-tab");
    expect(body).toMatch(/(^|[;\s])gap\s*:\s*0\.156rem\s*;/);
    expect(body).toMatch(/(^|[;\s])padding\s*:\s*0\.094rem 0\.281rem\s*;/);
    expect(body).toMatch(/(^|[;\s])font-size\s*:\s*0\.625em\s*;/);
  });
});
