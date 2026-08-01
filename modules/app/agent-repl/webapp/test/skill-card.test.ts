// @vitest-environment jsdom
/**
 * The skill card: ONE card per invocation, carrying the invocation, its
 * result, and the skill's own SKILL.md, updated in place as the pieces land.
 *
 * The three seams the feature crosses are covered end to end here, because the
 * defect it fixes lives BETWEEN them: the daemon's `skillBody` arm arriving as
 * a user turn (the spurious prompt bubble) is an adapter fact, landing on the
 * right card is a store fact, and folding by default is a render fact.
 */
import { describe, expect, it } from "vitest";
import { decodeFrontendFrame } from "../src/frontend-proto.js";
import { StateAdapter, type AdapterEffect } from "../src/state-adapter.js";
import {
  ConversationStore,
  type ConversationItem,
  type ToolItem,
} from "../src/store.js";
import { FeedRenderer, renderItem } from "../src/render.js";
import type { PanelContext } from "../src/render.js";
import { EXPANDED_CLASS, sectionsIn, toggleExpanded } from "../src/expand.js";
import type { Actions } from "../src/render.js";

const BODY =
  "Base directory for this skill: /s/demo\n\n# Demo\n\n**do** the thing";

// --- fixtures ---------------------------------------------------------------

function applyOne(obj: unknown): AdapterEffect[] {
  return new StateAdapter().apply(decodeFrontendFrame(JSON.stringify(obj)));
}

/** The store items one conversation item decomposes into. */
function itemsFrom(item: Record<string, unknown>): ConversationItem[] {
  const effects = applyOne({
    conversationDelta: {
      sessionId: "s1",
      workspace: "ws",
      throughSeq: "9",
      // The daemon stamps a provenance on every item it builds; an envelope
      // without one is a malformed frame the adapter refuses (see the
      // provenance gate in state-adapter.ts), so the fixture speaks it.
      items: [{ source: "CONVERSATION_SOURCE_USER", ...item }],
    },
  });
  const conv = effects.find((e) => e.kind === "conversation-items");
  if (conv?.kind !== "conversation-items")
    throw new Error("no conversation-items effect");
  return conv.items;
}

/** The Skill CALL as the assistant record's tool_use block delivers it. */
function skillCall(overrides: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    toolUseId: "toolu_demo",
    toolName: "Skill",
    messageId: "m1",
    ts: "2026-05-24T10:00:00.000Z",
    input: { skill: "demo" },
    inputJson: `{"skill":"demo"}`,
    inputDone: true,
    ...overrides,
  };
}

/** A store fed the way the adapter feeds it: one conversation-items effect. */
function feed(
  store: ConversationStore,
  items: ConversationItem[],
  throughSeq: number,
): void {
  store.ingest([
    {
      kind: "conversation-items",
      workspace: "ws",
      sessionId: "s1",
      throughSeq,
      items,
    },
  ]);
}

const NOOP_ACTIONS: Actions = {
  decidePermission() {},
  answerQuestions() {},
  cancelQueued() {},
  runQueuedNow() {},
  acceptQueued() {},
};

// --- the wire arm -----------------------------------------------------------

describe("skillBody arm → store item", () => {
  it("decomposes into a tool item addressed to the Skill call", () => {
    // Arrange / Act
    const items = itemsFrom({
      uuid: "u-body",
      tsMs: "1000",
      skillBody: { toolUseId: "toolu_demo", bodyMarkdown: BODY },
    });

    // Assert
    expect(items).toHaveLength(1);
    expect(items[0]).toMatchObject({
      kind: "tool",
      toolUseId: "toolu_demo",
      skillBody: BODY,
    });
  });

  it("never decomposes into a user turn, so the body draws no prompt bubble", () => {
    // Arrange — THE DEFECT: the body used to arrive as a `user` record and
    // render as a page-long purple prompt nobody typed.
    // Act
    const items = itemsFrom({
      uuid: "u-body",
      tsMs: "1000",
      skillBody: { toolUseId: "toolu_demo", bodyMarkdown: BODY },
    });

    // Assert
    expect(items.some((i) => i.kind === "user-turn")).toBe(false);
  });

  it("carries no tool name, so it cannot wipe the call's own", () => {
    // Arrange / Act
    const items = itemsFrom({
      uuid: "u-body",
      tsMs: "1000",
      skillBody: { toolUseId: "toolu_demo", bodyMarkdown: BODY },
    });

    // Assert
    expect(items[0]).toMatchObject({ toolName: "" });
  });
});

// --- one card ---------------------------------------------------------------

describe("skill body reconciliation", () => {
  it("merges into the existing call rather than adding a second card", () => {
    // Arrange
    const store = new ConversationStore();
    feed(store, [skillCall()], 1);

    // Act
    feed(
      store,
      [
        {
          kind: "tool",
          toolUseId: "toolu_demo",
          toolName: "",
          messageId: "m2",
          ts: "2026-05-24T10:00:01.000Z",
          inputJson: "",
          inputDone: true,
          skillBody: BODY,
        },
      ],
      2,
    );

    // Assert
    const tools = store.state.items.filter(
      (i): i is ToolItem => i.kind === "tool",
    );
    expect(tools).toHaveLength(1);
    expect(tools[0]).toMatchObject({ toolName: "Skill", skillBody: BODY });
  });

  it("keeps the call's name and input when the body lands after it", () => {
    // Arrange
    const store = new ConversationStore();
    feed(store, [skillCall()], 1);

    // Act
    feed(
      store,
      [
        {
          kind: "tool",
          toolUseId: "toolu_demo",
          toolName: "",
          messageId: "m2",
          ts: "",
          inputJson: "",
          inputDone: true,
          skillBody: BODY,
        },
      ],
      2,
    );

    // Assert
    const tool = store.state.items[0] as ToolItem;
    expect(tool.input).toEqual({ skill: "demo" });
  });

  it("is idempotent, so a replayed body replaces rather than accumulates", () => {
    // Arrange
    const store = new ConversationStore();
    feed(store, [skillCall()], 1);
    const body: ToolItem = {
      kind: "tool",
      toolUseId: "toolu_demo",
      toolName: "",
      messageId: "m2",
      ts: "",
      inputJson: "",
      inputDone: true,
      skillBody: BODY,
    };
    feed(store, [body], 2);

    // Act — the resync/re-pull redelivery.
    feed(store, [body], 2);

    // Assert
    expect(store.state.items.filter((i) => i.kind === "tool")).toHaveLength(1);
  });
});

// --- rendering --------------------------------------------------------------

describe("skill card content", () => {
  it("renders the body inside the capped section, folded by default", () => {
    // Arrange — `skill-content` is the CAPPED_CLASSES entry the stylesheet
    // caps and the click handler expands; the absence of `expanded` is what
    // makes it FOLDED on first paint.
    const html = renderItem(skillCall({ skillBody: BODY }));

    // Assert
    expect(html).toContain("skill-content");
    expect(html).not.toContain(EXPANDED_CLASS);
  });

  it("renders the body as markdown rather than escaped text", () => {
    // Arrange / Act — a skill IS a markdown document.
    const html = renderItem(skillCall({ skillBody: BODY }));

    // Assert
    expect(html).toContain("<h1>Demo</h1>");
    expect(html).toContain("<strong>do</strong>");
  });

  it("draws no body section at all before the body has landed", () => {
    // Arrange / Act — the card is complete without it; the harness writes the
    // body as its own later record.
    const html = renderItem(skillCall());

    // Assert
    expect(html).not.toContain("skill-content-md");
  });

  it("keeps the teal card chrome the async family shares", () => {
    // Arrange / Act
    const html = renderItem(skillCall({ skillBody: BODY }));

    // Assert
    expect(html).toContain("tool-card tool-skill");
    expect(html).toContain(`<span class="tool-name">Skill</span>`);
  });
});

// --- fold persistence -------------------------------------------------------

describe("skill body fold state across a reconcile", () => {
  it("stays expanded when the same card updates under it", () => {
    // Arrange — a reader expands the body, then the card's result lands.
    const container = document.createElement("div");
    const renderer = new FeedRenderer(container, NOOP_ACTIONS);
    const store = new ConversationStore();
    feed(store, [skillCall({ skillBody: BODY })], 1);
    renderer.render(store.state);
    const section = sectionsIn(container).find((el) =>
      el.classList.contains("skill-content"),
    );
    if (!section) throw new Error("no skill-content section mounted");
    toggleExpanded(section);

    // Act — the same card is re-rendered with its result attached.
    feed(
      store,
      [
        {
          kind: "tool",
          toolUseId: "toolu_demo",
          toolName: "",
          messageId: "m3",
          ts: "",
          inputJson: "",
          inputDone: true,
          resultTs: "2026-05-24T10:00:02.000Z",
          result: { isError: true, content: "boom" },
        },
      ],
      2,
    );
    renderer.render(store.state);

    // Assert — the reader's expansion survived the update.
    const after = sectionsIn(container).find((el) =>
      el.classList.contains("skill-content"),
    );
    expect(after?.classList.contains(EXPANDED_CLASS)).toBe(true);
  });
});

// --- the rest of the family is unchanged ------------------------------------

describe("presenter registry", () => {
  it("gives a subagent card its agent topbar", () => {
    // Arrange — Task/Agent are the family's other registered kinds, and the
    // topbar is the only content they add over the base.
    const agent = skillCall({
      toolUseId: "t-agent",
      toolName: "Agent",
      input: { description: "dig" },
      inputJson: `{"description":"dig"}`,
    });

    // Act
    const panels = {
      isOpen: () => false,
      children: new Map(),
      agentTopbar: () => `<div class="agent-topbar">strip</div>`,
    } as unknown as PanelContext;
    const html = renderItem(agent, undefined, undefined, panels);

    // Assert
    expect(html).toContain(`<div class="agent-topbar">strip</div>`);
  });

  it("gives an unregistered kind no content beyond the base", () => {
    // Arrange — a Bash card has no presenter entry, so nothing may be added.
    const bash = skillCall({
      toolUseId: "t-bash",
      toolName: "Bash",
      input: { command: "ls" },
      inputJson: `{"command":"ls"}`,
    });

    // Act
    const html = renderItem(bash);

    // Assert
    expect(html).not.toContain("skill-content");
    expect(html).not.toContain("agent-topbar");
  });

  it("adds the body and nothing else to a skill card", () => {
    // Arrange — the registry's contract: a kind's presenter contributes only
    // its own section, so a skill card WITHOUT a body is the base content.
    const withBody = renderItem(skillCall({ skillBody: "just text" }));
    const without = renderItem(skillCall());

    // Act
    const added = withBody.replace(
      `<div class="tool-output skill-content skill-content-md">${"<p>just text</p>\n"}</div>`,
      "",
    );

    // Assert — removing the body section alone recovers the bodyless card.
    expect(added.replace(/\s+/g, " ").trim()).toBe(
      without.replace(/\s+/g, " ").trim(),
    );
  });
});
