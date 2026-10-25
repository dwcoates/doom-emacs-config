// @vitest-environment jsdom
/**
 * THE CHAT, over a scripted daemon: the view it draws from WatchChat, and the
 * gestures it hands the daemon (sibling switches, highlights, sightings).
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { ReportChatSightingsResponseSchema, type ReportChatSightingsRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_report_chat_sightings_pb";
import { SelectChatTextResponseSchema, type SelectChatTextRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_chat_text_pb";
import { SwitchChatSiblingResponseSchema, type SwitchChatSiblingRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_switch_chat_sibling_pb";
import { WatchChatResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_chat_pb";
import { ChatViewSchema, type ChatView } from "../../../proto/gen/ts/frontend/v1/chat_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { mountChat, pathSwapped, pendingQuestionOf, shownPathOf, SIGHTING_BATCH_MS, type ChatHandle } from "../../src/chat/chat.js";
import { HIGHLIGHT_CLASS, QUOTE_MARK_CLASS } from "../../src/chat/chat-view.js";
import { CONVERSATION_DRAWN_ATTRIBUTE } from "../../src/feed/conversation-drawn.js";
import { log } from "../../src/log.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { testAppContext } from "../rpc/app-context.js";
import { fakeTicker, SINK } from "../sidebar/harness.js";

const THREAD = create(WorkspaceRefSchema, { id: "thread-1", dir: "/zettel/threads/why" });

type ViewInit = MessageInitShape<typeof ChatViewSchema>;

/** A conversation of the given exchanges. */
function conversation(exchanges: unknown[], pending?: unknown): ViewInit {
  return { state: { case: "conversation", value: { exchanges: exchanges as never, pending: pending as never } } };
}

/** One exchange. */
function exchange(node: string, question: string, answer: string, extra: Record<string, unknown> = {}): unknown {
  return {
    node: { id: node },
    question: { text: { text: question }, quote: extra.quote === undefined ? undefined : { text: extra.quote } },
    siblings: extra.siblings === undefined ? undefined : { position: { label: extra.siblings } },
    answer: {
      markdown: { text: answer },
      quoteMarks: ((extra.marks as string[] | undefined) ?? []).map((text) => ({ text })),
      highlight: extra.highlight === undefined ? undefined : { text: extra.highlight },
    },
  };
}

/** A scripted daemon: views pushed by the test, gestures recorded. */
function daemon() {
  const views: ChatView[] = [];
  let wake: (() => void) | null = null;
  const switches: SwitchChatSiblingRequest[] = [];
  const selections: SelectChatTextRequest[] = [];
  const sightings: ReportChatSightingsRequest[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      async *watchChat(_request, context) {
        for (;;) {
          while (views.length > 0) {
            const view = views.shift() as ChatView;
            yield create(WatchChatResponseSchema, { view });
          }
          await new Promise<void>((resolve) => {
            wake = resolve;
            context.signal.addEventListener("abort", () => resolve(), { once: true });
          });
          if (context.signal.aborted) return;
        }
      },
      switchChatSibling(request) {
        switches.push(request);
        return create(SwitchChatSiblingResponseSchema, { result: { case: "success", value: {} } });
      },
      selectChatText(request) {
        selections.push(request);
        return create(SelectChatTextResponseSchema, { result: { case: "success", value: {} } });
      },
      reportChatSightings(request) {
        sightings.push(request);
        return create(ReportChatSightingsResponseSchema, { result: { case: "success", value: {} } });
      },
    });
  });
  return {
    transport,
    switches,
    selections,
    sightings,
    push(view: ViewInit) {
      views.push(create(ChatViewSchema, view as never));
      wake?.();
    },
  };
}

let host: HTMLElement;
let scroll: HTMLElement;
let handle: ChatHandle | null;
let seenCallback: ((el: Element) => void) | null;
let observed: Element[];

beforeEach(() => {
  scroll = document.createElement("div");
  host = document.createElement("div");
  scroll.append(host);
  document.body.append(scroll);
  handle = null;
  seenCallback = null;
  observed = [];
});

afterEach(() => {
  handle?.dispose();
  scroll.remove();
  vi.useRealTimers();
});

function mount(d: ReturnType<typeof daemon>, shown = true): void {
  const ctx = testAppContext({
    client: createAgentReplClient(d.transport),
    workspace: THREAD,
    ticker: fakeTicker(),
    failures: SINK,
    composerEnabled: false,
  });
  handle = mountChat(host, ctx, {
    scroll,
    shown: () => shown,
    observe: (onSeen) => {
      seenCallback = onSeen;
      return {
        observe: (el) => observed.push(el),
        unobserve: (el) => (observed = observed.filter((o) => o !== el)),
        disconnect: () => (observed = []),
      };
    },
  });
}

/** The element answering SELECTOR, once the chat has drawn it. */
async function drawn(selector: string): Promise<HTMLElement> {
  return vi.waitFor(() => {
    const el = host.querySelector<HTMLElement>(selector);
    if (el === null) throw new Error(`${selector} is not drawn yet`);
    return el;
  });
}

describe("the chat view", () => {
  it("draws the empty thread's hint", async () => {
    // Arrange
    const d = daemon();
    mount(d);

    // Act
    d.push({ state: { case: "empty", value: { hint: { text: "Ask your first question." } } } });

    // Assert
    expect((await drawn("[data-chat-empty]")).textContent).toBe("Ask your first question.");
  });

  it("marks the page's conversation drawn with its first view, so the thread's tab can open", async () => {
    // Arrange
    document.documentElement.removeAttribute(CONVERSATION_DRAWN_ATTRIBUTE);
    const d = daemon();
    mount(d);

    // Act
    d.push({ state: { case: "empty", value: { hint: { text: "Ask your first question." } } } });
    await drawn("[data-chat-empty]");

    // Assert
    await vi.waitFor(() => expect(document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE)).toBe("page"));
  });

  it("draws the path's questions and answers, a quote, and a sibling control", async () => {
    // Arrange
    const d = daemon();
    mount(d);

    // Act
    d.push(
      conversation([
        exchange("n1", "why is the sky blue?", "Because of **Rayleigh** scattering."),
        exchange("n2", "what is it?", "Light scattering.", { quote: "Rayleigh", siblings: "2 of 3" }),
      ]),
    );

    // Assert
    const second = await drawn('[data-chat-node="n2"]');
    expect(second.querySelector(".chat-q-quote")?.textContent).toBe("Rayleigh");
    expect(second.querySelector(".chat-q-stacked")).not.toBeNull();
    expect(second.querySelector("[data-chat-siblings] span")?.textContent).toBe("2 of 3");
    expect(host.querySelector('[data-chat-answer="n1"] strong')?.textContent).toBe("Rayleigh");
  });

  it("marks the words a child quoted and the held highlight", async () => {
    // Arrange
    const d = daemon();
    mount(d);

    // Act
    d.push(conversation([exchange("n1", "q", "the sky is blue today", { marks: ["sky"], highlight: "blue" })]));

    // Assert
    const answer = await drawn('[data-chat-answer="n1"]');
    expect(answer.querySelector(`mark.${QUOTE_MARK_CLASS}`)?.textContent).toBe("sky");
    expect(answer.querySelector(`mark.${HIGHLIGHT_CLASS}`)?.textContent).toBe("blue");
  });

  it.each([
    ["waiting", { case: "waiting", value: {} }, "[data-chat-waiting]", ""],
    ["streaming", { case: "streaming", value: { markdown: { text: "Half an *answer*" } } }, "[data-chat-streaming]", "Half an answer"],
  ])("draws the question in flight, %s", async (_name, progress, selector, text) => {
    // Arrange
    const d = daemon();
    mount(d);

    // Act
    d.push(conversation([], { question: { text: { text: "and then?" } }, answer: { progress } }));

    // Assert
    const pending = await drawn("[data-chat-pending]");
    expect(pending.querySelector("[data-chat-question]")?.textContent).toBe("and then?");
    expect(pending.querySelector(selector)?.textContent?.trim()).toBe(text);
  });
});

describe("the chat's gestures", () => {
  it.each([
    ["previous", 0, "previous"],
    ["next", 1, "next"],
  ])("a %s sibling click asks the daemon to switch", async (_name, button, way) => {
    // Arrange
    const d = daemon();
    mount(d);
    d.push(conversation([exchange("n2", "q", "a", { siblings: "1 of 2" })]));
    const control = await drawn("[data-chat-siblings]");

    // Act
    control.querySelectorAll<HTMLElement>("ar-button")[button].click();

    // Assert
    await vi.waitFor(() => expect(d.switches).toHaveLength(1));
    expect(d.switches[0].shown?.id).toBe("n2");
    expect(d.switches[0].direction?.way.case).toBe(way);
  });

  it("a horizontal swipe switches once per swipe", async () => {
    // Arrange
    const d = daemon();
    mount(d);
    d.push(conversation([exchange("n2", "q", "a", { siblings: "1 of 2" })]));
    const wrap = await drawn(".chat-q-wrap");

    // Act: one swipe travelling far past the distance.
    for (let i = 0; i < 6; i += 1) wrap.dispatchEvent(new WheelEvent("wheel", { deltaX: 40, bubbles: true }));

    // Assert
    await vi.waitFor(() => expect(d.switches).toHaveLength(1));
    expect(d.switches[0].direction?.way.case).toBe("next");
  });

  it("words selected in an answer are held as the highlight, with their context", async () => {
    // Arrange
    const d = daemon();
    mount(d);
    d.push(conversation([exchange("n1", "q", "the sky is blue")]));
    const answer = await drawn('[data-chat-answer="n1"]');
    const text = answer.querySelector("p")?.firstChild as Text;
    const range = document.createRange();
    range.setStart(text, 4);
    range.setEnd(text, 7);
    window.getSelection()?.removeAllRanges();
    window.getSelection()?.addRange(range);

    // Act
    answer.dispatchEvent(new MouseEvent("mouseup", { bubbles: true }));

    // Assert
    await vi.waitFor(() => expect(d.selections).toHaveLength(1));
    const state = d.selections[0].selection?.state;
    expect(state?.case).toBe("highlighted");
    if (state?.case === "highlighted") {
      expect(state.value.node?.id).toBe("n1");
      expect(state.value.quote?.text).toBe("sky");
      expect(state.value.quote?.questionContext?.text).toBe("the sky is blue");
    }
    window.getSelection()?.removeAllRanges();
  });

  it("a click that selects nothing lets a held highlight go", async () => {
    // Arrange
    const d = daemon();
    mount(d);
    d.push(conversation([exchange("n1", "q", "the sky is blue", { highlight: "sky" })]));
    const answer = await drawn('[data-chat-answer="n1"]');
    window.getSelection()?.removeAllRanges();

    // Act
    answer.dispatchEvent(new MouseEvent("mouseup", { bubbles: true }));

    // Assert
    await vi.waitFor(() => expect(d.selections).toHaveLength(1));
    expect(d.selections[0].selection?.state.case).toBe("cleared");
  });

  it("answers seen on a shown page are reported together", async () => {
    // Arrange
    const d = daemon();
    mount(d);
    d.push(conversation([exchange("n1", "q", "a"), exchange("n2", "q", "b")]));
    await drawn('[data-chat-answer="n2"]');
    vi.useFakeTimers();

    // Act
    for (const el of observed) seenCallback?.(el);
    vi.advanceTimersByTime(SIGHTING_BATCH_MS);
    vi.useRealTimers();

    // Assert
    await vi.waitFor(() => expect(d.sightings).toHaveLength(1));
    expect(d.sightings[0].nodes.map((n) => n.id)).toEqual(["n1", "n2"]);
  });

  it("answers on a hidden page are not seen", async () => {
    // Arrange
    const d = daemon();
    mount(d, false);
    d.push(conversation([exchange("n1", "q", "a")]));
    await drawn('[data-chat-answer="n1"]');
    vi.useFakeTimers();

    // Act
    for (const el of observed) seenCallback?.(el);
    vi.advanceTimersByTime(SIGHTING_BATCH_MS);

    // Assert
    expect(d.sightings).toHaveLength(0);
  });
});

/** A pending question, waiting for its answer. */
function pending(question: string, quote?: string): unknown {
  return {
    question: { text: { text: question }, quote: quote === undefined ? undefined : { text: quote } },
    answer: { progress: { case: "waiting", value: {} } },
  };
}

describe("pendingQuestionOf", () => {
  it("is null for the empty thread", () => {
    expect(pendingQuestionOf(create(ChatViewSchema, { state: { case: "empty", value: { hint: { text: "h" } } } }))).toBeNull();
  });

  it("is null for a conversation with no question in flight", () => {
    expect(pendingQuestionOf(create(ChatViewSchema, conversation([exchange("n1", "q", "a")]) as never))).toBeNull();
  });

  it("tells two questions of the same text apart by their quote", () => {
    const of = (quote?: string) => pendingQuestionOf(create(ChatViewSchema, conversation([], pending("why?", quote)) as never));
    expect(new Set([of(), of("these words"), of("other words")]).size).toBe(3);
  });

  it("is the same for the same question pushed again", () => {
    const of = () => pendingQuestionOf(create(ChatViewSchema, conversation([], pending("why?", "w")) as never));
    expect(of()).toBe(of());
  });
});

/**
 * A QUESTION JUST ASKED LANDS THE CHAT AT ITS END (owner request, 2026-10-25),
 * wherever the reader was. The scroll box is a reader scrolled up a long chat.
 */
describe("the chat's end after a question is asked", () => {
  let top: number;
  let moves: unknown[];

  beforeEach(() => {
    top = 0;
    Object.defineProperty(scroll, "scrollHeight", { configurable: true, get: () => 1000 });
    Object.defineProperty(scroll, "clientHeight", { configurable: true, get: () => 100 });
    Object.defineProperty(scroll, "scrollTop", { configurable: true, get: () => top, set: (v: number) => (top = v) });
    moves = [];
    vi.spyOn(log, "info").mockImplementation((message, fields) => {
      if (fields?.operation === "scroll.chat-moved") moves.push(fields.context);
    });
  });

  afterEach(() => vi.restoreAllMocks());

  /** Push VIEW, wait until QUESTION is the pending question drawn. */
  async function pushPending(d: ReturnType<typeof daemon>, view: ViewInit, question: string): Promise<void> {
    d.push(view);
    await vi.waitFor(() => expect(host.querySelector("[data-chat-pending] [data-chat-question]")?.textContent).toBe(question));
  }

  it("lands at the end when a question goes in flight", async () => {
    const d = daemon();
    mount(d);
    d.push(conversation([exchange("n1", "q", "a")]));
    await drawn('[data-chat-answer="n1"]');

    await pushPending(d, conversation([exchange("n1", "q", "a")], pending("why?")), "why?");

    // The end is the last answer's, short of the quarter-page leeway: 1000 - 100 * 1.25.
    expect([top, moves]).toEqual([875, [{ cause: "promptSent", from: 0, to: 875 }]]);
  });

  it("lands at the end when a different question replaces the one in flight", async () => {
    const d = daemon();
    mount(d);
    await pushPending(d, conversation([], pending("first?")), "first?");

    await pushPending(d, conversation([], pending("second?")), "second?");

    expect(moves).toEqual([{ cause: "promptSent", from: 0, to: 875 }]);
  });

  it("leaves a reader scrolled up where they are while the same question's answer arrives", async () => {
    const d = daemon();
    mount(d);
    d.push(conversation([]));
    await vi.waitFor(() => expect(host.querySelector(".chat-col")).not.toBeNull());
    await pushPending(d, conversation([], pending("why?")), "why?");
    top = 0;
    moves = [];

    d.push(conversation([], { ...(pending("why?") as object), answer: { progress: { case: "streaming", value: { markdown: { text: "because" } } } } }));
    await drawn("[data-chat-streaming]");

    expect([top, moves]).toEqual([0, []]);
  });

  it("does not count the thread's first view as a question asked", async () => {
    const d = daemon();
    mount(d);

    await pushPending(d, conversation([], pending("why?")), "why?");

    expect([top, moves]).toEqual([0, []]);
  });
});

describe("pathSwapped", () => {
  it.each([
    ["an exchange swapped for its sibling", ["n1", "n2"], ["n1", "n3"], true],
    ["a swap that shortens the path", ["n1", "n2", "n4"], ["n1", "n3"], true],
    ["an exchange added", ["n1"], ["n1", "n2"], false],
    ["the same path", ["n1", "n2"], ["n1", "n2"], false],
    ["the first view", [], ["n1"], false],
  ])("%s: %s", (_name, prev, next, want) => {
    expect(pathSwapped(prev, next)).toBe(want);
  });
});

describe("shownPathOf", () => {
  it("is the exchanges' nodes, oldest first", () => {
    const view = create(ChatViewSchema, conversation([exchange("n1", "q", "a"), exchange("n2", "q2", "a2")]));
    expect(shownPathOf(view)).toEqual(["n1", "n2"]);
  });

  it("is empty for the empty thread", () => {
    expect(shownPathOf(create(ChatViewSchema, { state: { case: "empty", value: { hint: { text: "ask" } } } }))).toEqual([]);
  });
});

/**
 * A SIBLING SWITCH MOVES NOTHING (owner request, 2026-10-25). The scroll box
 * is a reader at the very bottom of a long chat, the case that otherwise
 * followed the end.
 */
describe("the chat's scroll across a sibling switch", () => {
  let top: number;
  let moves: unknown[];

  beforeEach(() => {
    top = 900;
    Object.defineProperty(scroll, "scrollHeight", { configurable: true, get: () => 1000 });
    Object.defineProperty(scroll, "clientHeight", { configurable: true, get: () => 100 });
    Object.defineProperty(scroll, "scrollTop", { configurable: true, get: () => top, set: (v: number) => (top = v) });
    Object.defineProperty(host, "offsetHeight", { configurable: true, get: () => 800 });
    moves = [];
    vi.spyOn(log, "info").mockImplementation((_message, fields) => {
      if (fields?.operation === "scroll.chat-moved") moves.push(fields.context);
    });
  });

  afterEach(() => vi.restoreAllMocks());

  async function switchedTo(d: ReturnType<typeof daemon>): Promise<void> {
    d.push(conversation([exchange("n1", "q", "a"), exchange("n2", "q2", "a2"), exchange("n4", "q4", "a4")]));
    await drawn('[data-chat-answer="n4"]');
    top = 900; // the reader at the end, as the opening view left them
    moves = [];
    d.push(conversation([exchange("n1", "q", "a"), exchange("n3", "q3", "a3")]));
    await drawn('[data-chat-answer="n3"]');
  }

  it("leaves the scroll exactly where it was, following no end", async () => {
    const d = daemon();
    mount(d);

    await switchedTo(d);

    expect([top, moves]).toEqual([900, []]);
  });

  it("holds no height of its own, so the chat never stands past its end and leeway", async () => {
    const d = daemon();
    mount(d);

    await switchedTo(d);

    expect(host.style.minHeight).toBe("");
  });
});

describe("the chat's leeway past its end", () => {
  it("is the chat's own bottom padding, a quarter of the scroll box", async () => {
    const d = daemon();
    mount(d);
    d.push(conversation([exchange("n1", "q", "a")]));
    await drawn('[data-chat-answer="n1"]');

    expect(host.style.paddingBottom).toBe("25cqh");
  });
});

/**
 * A NODE THE USER ASKED TO SEE IS BROUGHT INTO VIEW (the sidebar's outline,
 * ShowChatNode): once per new reveal sequence, never for the one the page's
 * first view carried.
 */
describe("the chat's reveal of an asked-for node", () => {
  let moves: unknown[];

  beforeEach(() => {
    moves = [];
    vi.spyOn(log, "info").mockImplementation((_message, fields) => {
      if (fields?.operation === "scroll.chat-moved") moves.push((fields.context as { cause: string }).cause);
    });
  });

  afterEach(() => vi.restoreAllMocks());

  /** Three exchanges, revealing NODE at SEQUENCE when given. */
  function revealing(node?: string, sequence?: number): ViewInit {
    const view = conversation([exchange("n1", "q1", "a1"), exchange("n2", "q2", "a2"), exchange("n3", "q3", "a3")]);
    if (node !== undefined && view.state?.case === "conversation") {
      view.state.value.reveal = { node: { id: node }, sequence: BigInt(sequence ?? 1) };
    }
    return view;
  }

  it("brings a newly revealed node into view", async () => {
    const d = daemon();
    mount(d);
    d.push(revealing());
    await drawn('[data-chat-answer="n3"]');
    moves = [];

    d.push(revealing("n2", 1));
    await vi.waitFor(() => expect(moves).toEqual(["chatNodeRevealed"]));
  });

  it("records a reveal of a node it did not draw as the defect it is", async () => {
    const errors: unknown[] = [];
    vi.spyOn(log, "error").mockImplementation((_message, fields) => void errors.push(fields?.operation));
    const d = daemon();
    mount(d);
    d.push(revealing());
    await drawn('[data-chat-answer="n3"]');

    d.push(revealing("n9", 1));

    await vi.waitFor(() => expect(errors).toContain("chat.reveal-missing"));
    expect(moves).not.toContain("chatNodeRevealed");
  });

  it("does not act on the reveal the page's first view carries", async () => {
    const d = daemon();
    mount(d);

    d.push(revealing("n2", 4));
    await drawn('[data-chat-answer="n3"]');

    expect(moves).not.toContain("chatNodeRevealed");
  });

  it("acts on a reveal once, not again when the view is pushed unchanged", async () => {
    const d = daemon();
    mount(d);
    d.push(revealing());
    await drawn('[data-chat-answer="n3"]');
    d.push(revealing("n2", 1));
    await vi.waitFor(() => expect(moves).toContain("chatNodeRevealed"));
    moves = [];

    d.push(revealing("n2", 1));
    d.push(conversation([exchange("n1", "q1", "a1"), exchange("n2", "q2", "a2"), exchange("n3", "q3", "a3"), exchange("n4", "q4", "a4")]));
    await drawn('[data-chat-answer="n4"]');

    expect(moves).not.toContain("chatNodeRevealed");
  });
});
