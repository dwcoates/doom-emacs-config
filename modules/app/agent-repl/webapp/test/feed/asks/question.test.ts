// @vitest-environment jsdom
import { type Control } from "../../../src/control.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  UpdateQuestionDraftErrorSchema,
  UpdateQuestionDraftResponseSchema,
  type UpdateQuestionDraftResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_update_question_draft_pb";
import {
  FeedIdSchema,
  FeedQuestionItemSchema,
  FeedQuestionSchema,
  FeedRowSchema,
  type FeedQuestion,
  type FeedQuestionItem,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FailureKind } from "../../../../proto/gen/ts/frontend/v1/failure_pb";
import { createTicker } from "../../../src/clock.js";
import type { AgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  CONFIRM_TEXT,
  drawFeedQuestion,
  EXPIRED_TEXT,
  QUESTION_STATE_ARMS,
  ANSWERED_TEXT,
  QUESTION_TITLE,
  TAB_GLYPHS,
  TYPED_LABEL,
} from "../../../src/feed/asks/question.js";
import {
  drawFeedTab,
  drawFeedTabline,
  drawFeedTabStrip,
  FEED_TAB_CLASS,
  FEED_TAB_STRIP_CLASS,
  FEED_TABLINE_CLASS,
  FEED_TABLINE_CLOCK_CLASS,
} from "../../../src/feed/tabs.js";
import { armsOf } from "../arms.js";
import { askHarness, ROW_ID, settle as drain, WORKSPACE } from "./harness.js";
import { ASK_WAITING_WORD } from "../../../src/feed/asks/ask-words.js";
import { runningClock, stoppedClock, toolCardHeadParts } from "../cards/tool-card-head-shape.js";
import { orderFor } from "../../feed-order.js";
import { recordingFailures } from "../../failure-sink.js";

type InitState = MessageInitShape<typeof FeedQuestionSchema>["state"];
type InitDraft = { chosen?: string[]; otherText?: { text: string }; answered?: boolean };

/** One question of a batch, single- or multi-select. */
function item(
  opts: {
    header?: string;
    text?: string;
    multi?: boolean;
    options?: { label: string; description?: string }[];
  } = {},
): FeedQuestionItem {
  const options = (opts.options ?? [{ label: "OAuth 2.0" }, { label: "API key" }]).map((o) => ({
    label: { text: o.label },
    description: o.description === undefined ? undefined : { text: o.description },
  }));
  return create(FeedQuestionItemSchema, {
    header: { text: opts.header ?? "Auth method" },
    text: { text: opts.text ?? "Which auth method?" },
    options: opts.multi === true
      ? { case: "multiSelect", value: { options } }
      : { case: "singleSelect", value: { options } },
  });
}

/** A question card in one STATE. */
function question(state: InitState, questions: FeedQuestionItem[] = [item()]): FeedQuestion {
  return create(FeedQuestionSchema, { questions, state });
}

/** A standing card over QUESTIONS, showing ACTIVE, with one draft per question. */
function open(
  questions: FeedQuestionItem[] = [item()],
  opts: { active?: number; drafts?: InitDraft[] } = {},
): FeedQuestion {
  const drafts = questions.map((_q, i) => opts.drafts?.[i] ?? {});
  return question({ case: "open", value: { active: opts.active ?? 0, drafts } }, questions);
}

/** A refused edit carrying CAUSE. */
function refused(cause: MessageInitShape<typeof UpdateQuestionDraftErrorSchema>["cause"]) {
  return create(UpdateQuestionDraftResponseSchema, { result: { case: "error", value: { cause } } });
}

/** Click one option of the shown question, by its served label. */
function clickOpt(el: HTMLElement, label: string): void {
  el.querySelector<Control>(`[data-question-option="${label}"]`)?.click();
}

/** A batch of `n` single-select questions, each headed and texted by its index. */
function singles(n: number): FeedQuestionItem[] {
  return Array.from({ length: n }, (_v, i) => item({ header: `q${i}`, text: `q${i}?` }));
}

async function settle(): Promise<void> {
  await drain(vi.advanceTimersByTimeAsync);
}

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(0);
});
afterEach(() => {
  vi.useRealTimers();
});

describe("the card's chrome", () => {
  it.each([
    ["open", open()],
    ["answered", question({ case: "answered", value: { answers: [] } })],
    ["expired", question({ case: "expired", value: {} })],
  ] as const)("wears the tool card's chrome when %s", (_state, u) => {
    const el = drawFeedQuestion(u, askHarness().rc);
    expect(el.classList.contains("tool-card")).toBe(true);
    expect(el.querySelector(".tool-head .tool-name")?.textContent).toBe(QUESTION_TITLE);
  });

  it("heads with the title, the waiting badge and the running clock while it stands", () => {
    // Arrange
    vi.setSystemTime(30_000);
    const u = open();
    u.clock = runningClock(0);

    // Act
    const el = drawFeedQuestion(u, askHarness().rc);

    // Assert
    expect(toolCardHeadParts(el)).toEqual([`title:${QUESTION_TITLE}`, `badge:${ASK_WAITING_WORD}`, "clock:30s"]);
  });

  it("keeps the clock on the head and draws no tabline for a lone question", () => {
    // Arrange
    vi.setSystemTime(30_000);
    const u = open([item()]);
    u.clock = runningClock(0);

    // Act
    const el = drawFeedQuestion(u, askHarness().rc);

    // Assert
    expect([toolCardHeadParts(el).at(-1), el.querySelector(`.${FEED_TABLINE_CLASS}`)]).toEqual(["clock:30s", null]);
  });

  it("moves the clock off the head when the batch has tabs", () => {
    // Arrange
    vi.setSystemTime(30_000);
    const u = open(singles(2));
    u.clock = runningClock(0);

    // Act
    const el = drawFeedQuestion(u, askHarness().rc);

    // Assert
    expect(toolCardHeadParts(el)).toEqual([`title:${QUESTION_TITLE}`, `badge:${ASK_WAITING_WORD}`]);
  });

  it("draws the clock on the tabline, after the tabs, when the batch has tabs", () => {
    // Arrange
    vi.setSystemTime(30_000);
    const u = open(singles(2));
    u.clock = runningClock(0);

    // Act
    const el = drawFeedQuestion(u, askHarness().rc);

    // Assert
    const line = el.querySelector(`.${FEED_TABLINE_CLASS}`);
    expect(line?.firstElementChild?.hasAttribute("data-question-tabs")).toBe(true);
    expect(line?.querySelector(`.${FEED_TABLINE_CLOCK_CLASS} .tool-clock`)?.textContent).toBe("30s");
  });

  it("draws an empty tabline clock slot for a tabbed batch that states no clock", () => {
    // Act
    const el = drawFeedQuestion(open(singles(2)), askHarness().rc);

    // Assert
    expect(el.querySelector(`.${FEED_TABLINE_CLOCK_CLASS}`)?.childElementCount).toBe(0);
  });

  it("draws its tabline as the shared one", () => {
    // Arrange -- a hand-rolled tabline that drifted from the shared one fails here.
    const expected = drawFeedTabline(drawFeedTabStrip(), null);

    // Act
    const el = drawFeedQuestion(open(singles(2)), askHarness().rc);

    // Assert
    const line = el.querySelector(`.${FEED_TABLINE_CLASS}`);
    expect([...(line?.children ?? [])].map((c) => c.classList[0])).toEqual(
      [...expected.children].map((c) => c.classList[0]),
    );
  });

  it("heads with the answered badge and the stopped clock once answered", () => {
    // Arrange
    const u = question({ case: "answered", value: { answers: [] } });
    u.clock = stoppedClock(1_000, 135_000);

    // Act
    const el = drawFeedQuestion(u, askHarness().rc);

    // Assert
    expect(toolCardHeadParts(el)).toEqual([`title:${QUESTION_TITLE}`, `badge:${ANSWERED_TEXT}`, "clock:2m 14s"]);
  });

  it("draws no ticking stamp of its own once settled", () => {
    const el = drawFeedQuestion(question({ case: "answered", value: { answers: [] } }), askHarness().rc);
    expect(el.querySelector("[data-ticking]")).toBeNull();
  });

  it("draws every state the schema carries", () => {
    expect([...QUESTION_STATE_ARMS].sort()).toEqual(armsOf(FeedQuestionSchema.oneofs, "state").sort());
  });
});

describe("the standing batch", () => {
  it("draws one panel per question, in the batch's order", () => {
    const el = drawFeedQuestion(open([item({ text: "one?" }), item({ text: "two?" })]), askHarness().rc);
    expect([...el.querySelectorAll(".q-text")].map((t) => t.textContent)).toEqual(["one?", "two?"]);
  });

  it("shows only the question the daemon names active", () => {
    const el = drawFeedQuestion(open(singles(3), { active: 1 }), askHarness().rc);
    const hidden = [...el.querySelectorAll<HTMLElement>("[data-question-panel]")].map((p) => p.hidden);
    expect(hidden).toEqual([true, false, true]);
  });

  it("hooks each option by its SERVED label", () => {
    const el = drawFeedQuestion(open([item({ options: [{ label: "Read  Only" }] })]), askHarness().rc);
    expect(el.querySelector('[data-question-option="Read  Only"]')).not.toBeNull();
  });

  it("draws an option's description when the agent gave one", () => {
    const el = drawFeedQuestion(
      open([item({ options: [{ label: "OAuth 2.0", description: "the ordinary path" }] })]),
      askHarness().rc,
    );
    expect(el.querySelector(".q-opt-description")?.textContent).toBe("the ordinary path");
  });

  it("draws no description when the agent gave none", () => {
    const el = drawFeedQuestion(open(), askHarness().rc);
    expect(el.querySelector(".q-opt-description")).toBeNull();
  });

  it("marks the options the draft holds as selected, and only those", () => {
    const el = drawFeedQuestion(open([item()], { drafts: [{ chosen: ["API key"], answered: true }] }), askHarness().rc);
    const selected = [...el.querySelectorAll(".q-opt.selected")].map((o) => o.getAttribute("data-question-option"));
    expect(selected).toEqual(["API key"]);
    expect(el.querySelector('[data-question-option="API key"]')?.getAttribute("aria-pressed")).toBe("true");
  });

  it("draws an answer typed in the editor, verbatim", () => {
    const el = drawFeedQuestion(
      open([item()], { drafts: [{ otherText: { text: "mutual TLS" }, answered: true }] }),
      askHarness().rc,
    );
    const typed = el.querySelector("[data-question-typed]");
    expect(typed?.querySelector(".q-typed-label")?.textContent).toBe(TYPED_LABEL);
    expect(typed?.querySelector(".q-other-given")?.textContent).toBe("mutual TLS");
  });

  it("draws no typed line when nothing was typed", () => {
    const el = drawFeedQuestion(open(), askHarness().rc);
    expect(el.querySelector("[data-question-typed]")).toBeNull();
  });

  it("has no text field and no submit of its own", () => {
    const el = drawFeedQuestion(open([item(), item({ multi: true })]), askHarness().rc);
    expect(el.querySelector("input, textarea, [data-question-other], [data-question-submit]")).toBeNull();
  });

  it("draws no line saying where to type", () => {
    const el = drawFeedQuestion(open(), askHarness().rc);
    expect(el.querySelector("[data-ask-hint], .ask-hint")).toBeNull();
  });

  it("numbers each option from 1 in the served order", () => {
    const el = drawFeedQuestion(open([item({ options: [{ label: "a" }, { label: "b" }, { label: "c" }] })]), askHarness().rc);
    const numbers = [...el.querySelectorAll(".q-opt .q-opt-number")].map((n) => n.textContent);
    expect(numbers).toEqual(["1.", "2.", "3."]);
  });

  it("draws the number before the label, outside the hook's served value", () => {
    const el = drawFeedQuestion(open([item({ options: [{ label: "a" }] })]), askHarness().rc);
    const option = el.querySelector('[data-question-option="a"]');
    expect([...(option?.children ?? [])].map((c) => c.className)).toEqual(["q-opt-number", "q-opt-label"]);
  });

  it("draws the question text as inline markdown", () => {
    const el = drawFeedQuestion(open([item({ text: "Run `make test`?" })]), askHarness().rc);
    expect(el.querySelector(".q-text code")?.textContent).toBe("make test");
  });

  it("draws an option's label as inline markdown, keeping the served label on the hook", () => {
    const el = drawFeedQuestion(open([item({ options: [{ label: "use `foo()`" }] })]), askHarness().rc);
    const option = el.querySelector('[data-question-option="use `foo()`"]');
    expect(option?.querySelector(".q-opt-label code")?.textContent).toBe("foo()");
  });

  it("draws an option's description as inline markdown", () => {
    const el = drawFeedQuestion(open([item({ options: [{ label: "a", description: "**fast**" }] })]), askHarness().rc);
    expect(el.querySelector(".q-opt-description strong")?.textContent).toBe("fast");
  });

  it("escapes raw HTML in a served string rather than drawing it", () => {
    const el = drawFeedQuestion(open([item({ text: "<b>bold</b>?" })]), askHarness().rc);
    expect([el.querySelector(".q-text b"), el.querySelector(".q-text")?.textContent]).toEqual([null, "<b>bold</b>?"]);
  });

  it("draws the confirm only on a multi-select", () => {
    const el = drawFeedQuestion(open([item(), item({ multi: true })]), askHarness().rc);
    const panels = el.querySelectorAll("[data-question]");
    expect(panels[0]?.querySelector("[data-question-confirm]")).toBeNull();
    expect(panels[1]?.querySelector("[data-question-confirm]")?.textContent).toBe(CONFIRM_TEXT);
  });
});

describe("the tabs", () => {
  it("draws no tab strip for a one-question batch", () => {
    const el = drawFeedQuestion(open(), askHarness().rc);
    expect(el.querySelector("[data-question-tabs]")).toBeNull();
  });

  it("draws one shared feed tab per question of a batch", () => {
    const el = drawFeedQuestion(open(singles(3)), askHarness().rc);
    const strip = el.querySelector("[data-question-tabs]");
    expect(strip?.classList.contains(FEED_TAB_STRIP_CLASS)).toBe(true);
    expect(strip?.querySelectorAll(`.${FEED_TAB_CLASS}`)).toHaveLength(3);
  });

  it("draws each tab as the tool group's tab is drawn, labelled with its header", () => {
    // A hand-rolled tab that drifted from the shared one fails here.
    const el = drawFeedQuestion(open(singles(2)), askHarness().rc);
    const expected = drawFeedTab({ icon: "?", index: 1, label: "q0", active: true, onPick: () => {} });
    const first = el.querySelector<HTMLElement>('[data-question-tab="0"]');
    expect([...(first?.children ?? [])].map((c) => [c.className, c.textContent])).toEqual(
      [...expected.children].map((c) => [c.className, c.textContent]),
    );
  });

  it("draws a tab's header as inline markdown", () => {
    const el = drawFeedQuestion(open([item({ header: "`cfg`" }), item()]), askHarness().rc);
    expect(el.querySelector('[data-question-tab="0"] .feed-group-tab-label code')?.textContent).toBe("cfg");
  });

  it("marks the daemon's active question as the active tab", () => {
    const el = drawFeedQuestion(open(singles(3), { active: 2 }), askHarness().rc);
    const active = [...el.querySelectorAll("[data-question-tab]")].map((t) => t.classList.contains("is-active"));
    expect(active).toEqual([false, false, true]);
  });

  it("wears the answered glyph on an answered question's tab, and the pending one elsewhere", () => {
    const el = drawFeedQuestion(open(singles(2), { drafts: [{ chosen: ["API key"], answered: true }, {}] }), askHarness().rc);
    const glyphs = [...el.querySelectorAll("[data-question-tab] .feed-group-tab-icon")].map((g) => g.textContent);
    expect(glyphs).toEqual([TAB_GLYPHS.answered, TAB_GLYPHS.pending]);
    const flags = [...el.querySelectorAll("[data-question-tab]")].map((t) => t.getAttribute("data-answered"));
    expect(flags).toEqual(["true", "false"]);
  });
});

describe("the answers given so far", () => {
  const batch = (): FeedQuestionItem[] => [
    item({ header: "Auth", text: "Which auth?" }),
    item({ header: "Scope", text: "Which scope?", multi: true, options: [{ label: "read" }, { label: "write" }] }),
    item({ header: "Store", text: "Which store?" }),
  ];

  it("draws an answer typed for a question the card moved on from, its text italic", () => {
    const el = drawFeedQuestion(
      open(batch(), { active: 1, drafts: [{ otherText: { text: "mutual TLS" }, answered: true }] }),
      askHarness().rc,
    );
    const line = el.querySelector('[data-question-given="0"]');
    expect(line?.querySelector(".q-chip")?.textContent).toBe("Auth");
    const typed = line?.querySelector(".q-other-given");
    expect(typed?.textContent).toBe("mutual TLS");
    expect(typed?.className).toBe("q-other-given");
  });

  it("draws the labels chosen for an answered question", () => {
    const el = drawFeedQuestion(
      open(batch(), { active: 2, drafts: [{}, { chosen: ["read", "write"], answered: true }] }),
      askHarness().rc,
    );
    expect(el.querySelector('[data-question-given="1"] .q-chosen')?.textContent).toBe("read, write");
  });

  it("draws the answered questions in the batch's order", () => {
    const el = drawFeedQuestion(
      open(batch(), {
        active: 1,
        drafts: [{ otherText: { text: "a" }, answered: true }, {}, { otherText: { text: "c" }, answered: true }],
      }),
      askHarness().rc,
    );
    const lines = [...el.querySelectorAll("[data-question-given]")].map((l) => l.getAttribute("data-question-given"));
    expect(lines).toEqual(["0", "2"]);
  });

  it("draws no line for the shown question, whose panel draws its answer", () => {
    const el = drawFeedQuestion(
      open(batch(), { active: 0, drafts: [{ otherText: { text: "mutual TLS" }, answered: true }] }),
      askHarness().rc,
    );
    expect(el.querySelector("[data-question-given-so-far]")).toBeNull();
  });

  it("draws no line for a question only ticked, not yet answered", () => {
    const el = drawFeedQuestion(
      open(batch(), { active: 0, drafts: [{}, { chosen: ["read"], answered: false }] }),
      askHarness().rc,
    );
    expect(el.querySelector("[data-question-given-so-far]")).toBeNull();
  });

  it("draws no answers-so-far block on a one-question card", () => {
    const el = drawFeedQuestion(open([item()], { drafts: [{ otherText: { text: "x" }, answered: true }] }), askHarness().rc);
    expect(el.querySelector("[data-question-given-so-far]")).toBeNull();
  });

  it("draws a line exactly as the settled card draws the same answer", () => {
    const standing = drawFeedQuestion(
      open(batch(), { active: 1, drafts: [{ otherText: { text: "mutual TLS" }, answered: true }] }),
      askHarness().rc,
    ).querySelector('[data-question-given="0"]')!;
    const settled = drawFeedQuestion(
      question(
        { case: "answered", value: { answers: [{ header: { text: "Auth" }, chosen: [], otherText: { text: "mutual TLS" } }] } },
        batch(),
      ),
      askHarness().rc,
    ).querySelector(".q-verdict")!;
    standing.removeAttribute("data-question-given");
    expect(standing.outerHTML).toBe(settled.outerHTML);
  });

  it("draws the block between the tabs and the shown question", () => {
    const el = drawFeedQuestion(
      open(batch(), { active: 1, drafts: [{ otherText: { text: "a" }, answered: true }] }),
      askHarness().rc,
    );
    const order = [...el.querySelector(".q-tabbed")!.children].map((c) =>
      // The tabs ride the batch's tabline, the row they share with the clock.
      c.querySelector(":scope > [data-question-tabs]") !== null ? "tabs" : c.hasAttribute("data-question-given-so-far") ? "given" : c.className,
    );
    expect(order.slice(0, 3)).toEqual(["tabs", "given", "q-panels"]);
  });
});

describe("a click edits the daemon's draft", () => {
  it("sends a tab click as a show of its question, echoing the served text", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(open(singles(2)), h.rc);
    el.querySelector<Control>('[data-question-tab="1"]')?.click();
    await settle();
    expect(h.calls.draft).toHaveLength(1);
    expect(h.calls.draft[0]?.edit).toMatchObject({ case: "show", value: { questionText: "q1?" } });
  });

  it("sends an option click as a pick, echoing the served text and label", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(open(), h.rc);
    clickOpt(el, "API key");
    await settle();
    expect(h.calls.draft[0]?.edit).toMatchObject({
      case: "pick",
      value: { questionText: "Which auth method?", label: "API key" },
    });
  });

  it("sends a click on a held option as a pick too, which the daemon reads as a clear", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(open([item()], { drafts: [{ chosen: ["API key"], answered: true }] }), h.rc);
    clickOpt(el, "API key");
    await settle();
    expect(h.calls.draft[0]?.edit.case).toBe("pick");
  });

  it("sends a multi-select's confirm as a confirm of its question", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(open([item({ multi: true, text: "Which suites?" })]), h.rc);
    el.querySelector<Control>("[data-question-confirm]")?.click();
    await settle();
    expect(h.calls.draft[0]?.edit).toMatchObject({ case: "confirm", value: { questionText: "Which suites?" } });
  });

  it("echoes this card's own row and workspace", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(open(), h.rc);
    clickOpt(el, "OAuth 2.0");
    await settle();
    expect([h.calls.draft[0]?.question?.value, h.calls.draft[0]?.workspace?.id]).toEqual([ROW_ID, WORKSPACE.id]);
  });

  it("latches every control of the card while a click is in flight", () => {
    const h = askHarness();
    const el = drawFeedQuestion(open([item(), item({ multi: true })]), h.rc);
    clickOpt(el, "OAuth 2.0");
    const controls = [...el.querySelectorAll<Control>("ar-button")];
    expect(controls.length).toBeGreaterThan(0);
    expect(controls.every((c) => c.disabled)).toBe(true);
  });

  it("leaves the controls latched once the edit landed, for the row's re-push", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(open(), h.rc);
    clickOpt(el, "OAuth 2.0");
    await settle();
    expect(el.querySelector<Control>('[data-question-option="API key"]')?.disabled).toBe(true);
  });

  it("draws a transport failure at the card and gives the controls back", async () => {
    const h = askHarness({ fail: true });
    const el = drawFeedQuestion(open(), h.rc);
    clickOpt(el, "OAuth 2.0");
    await settle();
    expect(el.querySelector(".q-status .refusal")?.getAttribute("data-arm")).toBe("transport");
    expect(el.querySelector<Control>('[data-question-option="OAuth 2.0"]')?.disabled).toBe(false);
  });
});

describe("a refused edit", () => {
  const causes = [
    { arm: "unknownWorkspace", cause: { case: "unknownWorkspace", value: {} }, text: "the daemon does not know this workspace" },
    {
      arm: "workspaceRefMismatch",
      cause: { case: "workspaceRefMismatch", value: { registryDir: "/elsewhere" } },
      text: "this workspace's directory disagrees with the registry's: /elsewhere",
    },
    {
      arm: "transferringAway",
      cause: { case: "transferringAway", value: { address: "127.0.0.1:9931" } },
      text: "this workspace moved to another daemon at 127.0.0.1:9931",
    },
    {
      arm: "notYetAdopted",
      cause: { case: "notYetAdopted", value: {} },
      text: "the daemon has not finished adopting this workspace yet",
    },
    {
      arm: "askNotStanding",
      cause: { case: "askNotStanding", value: {} },
      text: "this question is no longer standing",
    },
    {
      arm: "unservedValue",
      cause: { case: "unservedValue", value: { text: "SAML" } },
      text: 'the question never served "SAML"',
    },
    {
      arm: "noSession",
      cause: { case: "noSession", value: {} },
      text: "the workspace has no session to answer",
    },
  ] as const;

  for (const c of causes) {
    it(`says what ${c.arm} means, at the card`, async () => {
      const h = askHarness({ draft: refused(c.cause) });
      const el = drawFeedQuestion(open(), h.rc);
      clickOpt(el, "OAuth 2.0");
      await settle();
      const drawn = el.querySelector(".q-status .refusal");
      expect([drawn?.getAttribute("data-arm"), drawn?.textContent]).toEqual([c.arm, c.text]);
    });
  }

  it("words every cause the schema declares", () => {
    expect(causes.map((c) => c.arm).sort()).toEqual(armsOf(UpdateQuestionDraftErrorSchema.oneofs, "cause").sort());
  });

  it("gives the controls back so the reader can act on the cause", async () => {
    const h = askHarness({ draft: refused({ case: "noSession", value: {} } as never) });
    const el = drawFeedQuestion(open(), h.rc);
    clickOpt(el, "OAuth 2.0");
    await settle();
    expect(el.querySelector<Control>('[data-question-option="OAuth 2.0"]')?.disabled).toBe(false);
  });

  it("clears the last refusal when the next click is sent", async () => {
    const h = askHarness({ draft: refused({ case: "noSession", value: {} } as never) });
    const el = drawFeedQuestion(open(), h.rc);
    clickOpt(el, "OAuth 2.0");
    await settle();
    clickOpt(el, "OAuth 2.0");
    await settle();
    expect(el.querySelectorAll(".q-status .refusal")).toHaveLength(1);
  });

  it("states an unset cause as unreadable rather than as a cause", async () => {
    const h = askHarness({
      draft: create(UpdateQuestionDraftResponseSchema, { result: { case: "error", value: {} } }),
    });
    const el = drawFeedQuestion(open(), h.rc);
    clickOpt(el, "OAuth 2.0");
    await settle();
    // An error with no cause set is a frame this build cannot read, not a
    // refusal with no words (src/rpc/refuse.ts).
    expect(el.querySelector(".q-status .refusal[data-arm='noSession']")).toBeNull();
  });
});

describe("the settled card", () => {
  it("draws one verdict line per question", () => {
    const el = drawFeedQuestion(
      question({
        case: "answered",
        value: {
          answers: [
            { header: { text: "Auth method" }, chosen: ["OAuth 2.0"] },
            { header: { text: "Scope" }, chosen: ["read"] },
          ],
        },
      }),
      askHarness().rc,
    );
    expect(el.querySelectorAll(".q-verdict").length).toBe(2);
  });

  it("draws the chosen labels verbatim", () => {
    const el = drawFeedQuestion(
      question({
        case: "answered",
        value: { answers: [{ header: { text: "Scope" }, chosen: ["read", "write"] }] },
      }),
      askHarness().rc,
    );
    expect(el.querySelector(".q-chosen")?.textContent).toBe("read, write");
  });

  it("draws each chosen label as its own inline markdown", () => {
    const el = drawFeedQuestion(
      question({
        case: "answered",
        value: { answers: [{ header: { text: "Scope" }, chosen: ["*a", "b*"] }] },
      }),
      askHarness().rc,
    );
    expect([el.querySelector(".q-chosen em"), el.querySelector(".q-chosen")?.textContent]).toEqual([null, "*a, b*"]);
  });

  it("draws the verdict's header chip as inline markdown", () => {
    const el = drawFeedQuestion(
      question({
        case: "answered",
        value: { answers: [{ header: { text: "`Scope`" }, chosen: ["read"] }] },
      }),
      askHarness().rc,
    );
    expect(el.querySelector(".q-chip code")?.textContent).toBe("Scope");
  });

  it("draws the free text that was given", () => {
    const el = drawFeedQuestion(
      question({
        case: "answered",
        value: { answers: [{ header: { text: "Scope" }, chosen: [], otherText: { text: "admin only" } }] },
      }),
      askHarness().rc,
    );
    expect(el.querySelector(".q-other-given")?.textContent).toBe("admin only");
  });

  it("draws no chosen labels for a free-text-only answer", () => {
    const el = drawFeedQuestion(
      question({
        case: "answered",
        value: { answers: [{ header: { text: "Scope" }, chosen: [], otherText: { text: "admin only" } }] },
      }),
      askHarness().rc,
    );
    expect(el.querySelector(".q-chosen")).toBeNull();
  });

  it("offers no controls once the batch is settled", () => {
    const el = drawFeedQuestion(question({ case: "answered", value: { answers: [] } }), askHarness().rc);
    expect(el.querySelector("ar-button")).toBeNull();
  });

  it("draws an expired ask as expired, never as pending forever", () => {
    const el = drawFeedQuestion(question({ case: "expired", value: {} }), askHarness().rc);
    expect(el.querySelector(".tool-head .badge")?.textContent).toBe(EXPIRED_TEXT);
  });

  it("offers no controls on an expired ask", () => {
    const el = drawFeedQuestion(question({ case: "expired", value: {} }), askHarness().rc);
    expect(el.querySelector("ar-button")).toBeNull();
  });
});

describe("drawFeedQuestion malformed input", () => {
  it("refuses a card whose state oneof is unset", () => {
    expect(() => drawFeedQuestion(create(FeedQuestionSchema, {}), askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a standing card whose drafts do not match its questions", () => {
    const u = question({ case: "open", value: { active: 0, drafts: [{}] } }, singles(2));
    expect(() => drawFeedQuestion(u, askHarness().rc)).toThrow(
      "malformed view at FeedQuestion.open.drafts: 1 drafts for 2 questions",
    );
  });

  it("refuses a standing card whose active question is out of range", () => {
    expect(() => drawFeedQuestion(open(singles(2), { active: 2 }), askHarness().rc)).toThrow(
      "malformed view at FeedQuestion.open.active: 2 names no question of 2",
    );
  });

  it("refuses a question whose options oneof is unset", () => {
    const bad = create(FeedQuestionItemSchema, { header: { text: "h" }, text: { text: "t" } });
    expect(() => drawFeedQuestion(open([bad]), askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a question with no text", () => {
    const bad = item();
    (bad as unknown as { text: undefined }).text = undefined;
    expect(() => drawFeedQuestion(open([bad]), askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a question with no header", () => {
    const bad = item();
    (bad as unknown as { header: undefined }).header = undefined;
    expect(() => drawFeedQuestion(open([bad]), askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses an option with no label", () => {
    const bad = item();
    (bad.options as unknown as { value: { options: { label: undefined }[] } }).value.options[0].label = undefined;
    expect(() => drawFeedQuestion(open([bad]), askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a state arm this build does not know", () => {
    const u = open();
    (u as unknown as { state: { case: string; value: unknown } }).state = { case: "withdrawn", value: {} };
    expect(() => drawFeedQuestion(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses an options arm that is neither single- nor multi-select, by name", () => {
    const bad = item();
    (bad as unknown as { options: { case: string; value: unknown } }).options = { case: "rankOrder", value: { options: [] } };
    expect(() => drawFeedQuestion(open([bad]), askHarness().rc)).toThrow(
      "malformed view at FeedQuestion.questions[0].options: arm 'rankOrder' is not one this build can draw",
    );
  });
});

/**
 * A row context whose UpdateQuestionDraft hands ANSWER back UNALTERED, and a
 * sink that keeps what was filed.
 *
 * `createRouterTransport` re-encodes the response through the frozen schema,
 * which DROPS an arm no descriptor knows. A fake client is how an arm a NEWER
 * daemon set actually reaches the renderer.
 */
function unalteredDraft(answer: UpdateQuestionDraftResponse): { rc: RowContext; filed: FailureKind[] } {
  const failures = recordingFailures();
  return {
    filed: failures.reports,
    rc: {
      ctx: testAppContext({
        client: { updateQuestionDraft: () => Promise.resolve(answer) } as unknown as AgentReplClient,
        workspace: WORKSPACE,
        ticker: createTicker(1000),
        failures,
        composerEnabled: false,
      }),
      feed: "root",
      arrival: "page",
      row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: ROW_ID }), order: orderFor(ROW_ID) }),
      revealRow: async () => false,
    },
  };
}

describe("an answer this build cannot read", () => {
  it("files the unknown result arm by name rather than drawing a refusal", async () => {
    // Arrange: a newer daemon answers with a result arm this build has no case for.
    const answer = create(UpdateQuestionDraftResponseSchema, { result: { case: "success", value: {} } });
    (answer as unknown as { result: { case: string; value: unknown } }).result = { case: "deferred", value: {} };
    const h = unalteredDraft(answer);
    const el = drawFeedQuestion(open(), h.rc);
    // Act
    clickOpt(el, "OAuth 2.0");
    await settle();
    // Assert
    const kind = h.filed[0]?.kind;
    expect(kind?.case === "frameUndecodable" ? [kind.value.frameHead, kind.value.cause] : null).toEqual([
      "UpdateQuestionDraftResponse.result",
      "arm 'deferred' is not one this build can draw",
    ]);
  });
});
