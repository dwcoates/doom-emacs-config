// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  AnswerQuestionErrorSchema,
  AnswerQuestionResponseSchema,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_question_pb";
import {
  FeedQuestionItemSchema,
  FeedQuestionSchema,
  type FeedQuestion,
  type FeedQuestionItem,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  drawFeedQuestion,
  EXPIRED_TEXT,
  QUESTION_STATE_ARMS,
  UNANSWERED_NOTE,
} from "../../../src/feed/asks/question.js";
import { armsOf } from "../arms.js";
import { askHarness, ROW_ID, settle as drain } from "./harness.js";

type InitState = MessageInitShape<typeof FeedQuestionSchema>["state"];

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

/** A refused answer carrying CAUSE. */
function refused(cause: MessageInitShape<typeof AnswerQuestionErrorSchema>["cause"]) {
  return create(AnswerQuestionResponseSchema, { result: { case: "error", value: { cause } } });
}

/** Tick one option's input, by its served label. */
function pick(el: HTMLElement, label: string): void {
  const input = el.querySelector<HTMLInputElement>(`[data-question-option="${label}"]`);
  if (input !== null) input.checked = true;
}

/** Type into one question's free-text field, by the question's index. */
function type(el: HTMLElement, index: number, text: string): void {
  const blocks = el.querySelectorAll<HTMLElement>("[data-question]");
  const field = blocks[index]?.querySelector<HTMLInputElement>("[data-question-other]");
  if (field != null) field.value = text;
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

describe("the standing batch", () => {
  it("draws one block per question, in the batch's order", () => {
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [item({ header: "one" }), item({ header: "two" })]),
      askHarness().rc,
    );
    expect([...el.querySelectorAll(".q-chip")].map((n) => n.textContent)).toEqual(["one", "two"]);
  });

  it("draws the question text verbatim", () => {
    const el = drawFeedQuestion(question({ case: "open", value: {} }), askHarness().rc);
    expect(el.querySelector(".q-text")?.textContent).toBe("Which auth method?");
  });

  it("draws radios for a single-select", () => {
    const el = drawFeedQuestion(question({ case: "open", value: {} }), askHarness().rc);
    expect(el.querySelector<HTMLInputElement>("[data-question-option]")?.type).toBe("radio");
  });

  it("draws checkboxes for a multi-select", () => {
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [item({ multi: true })]),
      askHarness().rc,
    );
    expect(el.querySelector<HTMLInputElement>("[data-question-option]")?.type).toBe("checkbox");
  });

  it("groups a single-select's radios per question, so two never share a pick", () => {
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [item(), item()]),
      askHarness().rc,
    );
    const names = [...el.querySelectorAll<HTMLInputElement>("[data-question-option]")].map(
      (i) => i.name,
    );
    expect(new Set(names).size).toBe(2);
  });

  it("hooks each option by its SERVED label", () => {
    const el = drawFeedQuestion(question({ case: "open", value: {} }), askHarness().rc);
    expect(
      [...el.querySelectorAll("[data-question-option]")].map((n) =>
        n.getAttribute("data-question-option"),
      ),
    ).toEqual(["OAuth 2.0", "API key"]);
  });

  it("draws an option's description when the agent gave one", () => {
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [
        item({ options: [{ label: "OAuth 2.0", description: "the usual" }] }),
      ]),
      askHarness().rc,
    );
    expect(el.querySelector(".q-opt-description")?.textContent).toBe("the usual");
  });

  it("draws no description when the agent gave none", () => {
    const el = drawFeedQuestion(question({ case: "open", value: {} }), askHarness().rc);
    expect(el.querySelector(".q-opt-description")).toBeNull();
  });

  it("always draws the free-text escape, per question", () => {
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [item(), item()]),
      askHarness().rc,
    );
    expect(el.querySelectorAll("[data-question-other]").length).toBe(2);
  });

  it("draws ONE submit for the whole batch", () => {
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [item(), item(), item()]),
      askHarness().rc,
    );
    expect(el.querySelectorAll("[data-question-submit]").length).toBe(1);
  });

  it("draws every state the schema carries", () => {
    expect([...QUESTION_STATE_ARMS].sort()).toEqual(
      armsOf(FeedQuestionSchema.oneofs, "state").sort(),
    );
  });
});

describe("submitting", () => {
  it("echoes this card's own row", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    pick(el, "OAuth 2.0");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question[0]?.question?.value).toBe(ROW_ID);
  });

  it("echoes each question's text verbatim", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    pick(el, "OAuth 2.0");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question[0]?.answers[0]?.questionText).toBe("Which auth method?");
  });

  it("echoes the chosen label verbatim", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    pick(el, "API key");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question[0]?.answers[0]?.chosen).toEqual(["API key"]);
  });

  it("sends every ticked label on a multi-select", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [item({ multi: true })]),
      h.rc,
    );
    pick(el, "OAuth 2.0");
    pick(el, "API key");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question[0]?.answers[0]?.chosen).toEqual(["OAuth 2.0", "API key"]);
  });

  it("never sends two labels for a single-select", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    // Both inputs forced on, which radios would not allow but a stray script
    // could: the collector must still hand back at most one.
    for (const input of el.querySelectorAll<HTMLInputElement>("[data-question-option]")) {
      input.checked = true;
    }
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question[0]?.answers[0]?.chosen.length).toBe(1);
  });

  it("carries the free text when the user typed some", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    pick(el, "OAuth 2.0");
    type(el, 0, "with PKCE");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question[0]?.answers[0]?.otherText?.text).toBe("with PKCE");
  });

  it("sends free text alone when nothing was ticked", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    type(el, 0, "neither, use mTLS");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question[0]?.answers[0]?.chosen).toEqual([]);
  });

  it("leaves the free text unset when nothing was typed", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    pick(el, "OAuth 2.0");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question[0]?.answers[0]?.otherText).toBeUndefined();
  });

  it("sends one answer per question of the batch", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [item({ text: "one?" }), item({ text: "two?" })]),
      h.rc,
    );
    type(el, 0, "a");
    type(el, 1, "b");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question[0]?.answers.map((a) => a.questionText)).toEqual(["one?", "two?"]);
  });

  it("latches the submit inert while the batch is in flight", () => {
    const el = drawFeedQuestion(question({ case: "open", value: {} }), askHarness().rc);
    pick(el, "OAuth 2.0");
    const submit = el.querySelector<HTMLButtonElement>("[data-question-submit]");
    submit?.click();
    expect(submit?.disabled).toBe(true);
  });

  it("draws a transport failure at the submit", async () => {
    const h = askHarness({ fail: true });
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    pick(el, "OAuth 2.0");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(el.querySelector(".perm-actions .refusal")?.getAttribute("data-arm")).toBe("transport");
  });
});

describe("an incomplete batch", () => {
  it("blocks the submit rather than sending a partial batch", async () => {
    const h = askHarness();
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [item(), item()]),
      h.rc,
    );
    type(el, 0, "a");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(h.calls.question.length).toBe(0);
  });

  it("says so beside the question that is missing", async () => {
    const el = drawFeedQuestion(
      question({ case: "open", value: {} }, [item(), item()]),
      askHarness().rc,
    );
    type(el, 0, "a");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    const notes = [...el.querySelectorAll<HTMLElement>(".q-note")].map((n) => n.hidden);
    expect(notes).toEqual([true, false]);
  });

  it("words the note the same way everywhere", async () => {
    const el = drawFeedQuestion(question({ case: "open", value: {} }), askHarness().rc);
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    expect(el.querySelector(".q-note")?.textContent).toBe(UNANSWERED_NOTE);
  });

  it("clears the note once the question is answered", async () => {
    const el = drawFeedQuestion(question({ case: "open", value: {} }), askHarness().rc);
    const submit = el.querySelector<HTMLButtonElement>("[data-question-submit]");
    submit?.click();
    await settle();
    pick(el, "OAuth 2.0");
    submit?.click();
    await settle();
    expect(el.querySelector<HTMLElement>(".q-note")?.hidden).toBe(true);
  });
});

describe("a refused batch", () => {
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
      text: "this ask is no longer standing",
    },
    {
      arm: "unservedValue",
      cause: { case: "unservedValue", value: { text: "SAML" } },
      text: 'the batch never served "SAML"',
    },
    {
      arm: "multiPickOnSingleSelect",
      cause: { case: "multiPickOnSingleSelect", value: {} },
      text: "several options were picked on a single-select question",
    },
    {
      arm: "noSession",
      cause: { case: "noSession", value: {} },
      text: "the workspace has no session to answer",
    },
  ] as const;

  for (const c of causes) {
    it(`says what ${c.arm} means, at the submit`, async () => {
      const h = askHarness({ question: refused(c.cause as never) });
      const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
      pick(el, "OAuth 2.0");
      el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
      await settle();
      const drawn = el.querySelector(".perm-actions .refusal");
      expect([drawn?.getAttribute("data-arm"), drawn?.textContent]).toEqual([c.arm, c.text]);
    });
  }

  it("words every cause the schema declares", () => {
    expect(causes.map((c) => c.arm).sort()).toEqual(
      armsOf(AnswerQuestionErrorSchema.oneofs, "cause").sort(),
    );
  });

  it("gives the submit back so the reader can act on the cause", async () => {
    const h = askHarness({ question: refused({ case: "noSession", value: {} } as never) });
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    pick(el, "OAuth 2.0");
    const submit = el.querySelector<HTMLButtonElement>("[data-question-submit]");
    submit?.click();
    await settle();
    expect(submit?.disabled).toBe(false);
  });

  it("states an unset cause as unreadable rather than as a cause", async () => {
    const h = askHarness({
      question: create(AnswerQuestionResponseSchema, { result: { case: "error", value: {} } }),
    });
    const el = drawFeedQuestion(question({ case: "open", value: {} }), h.rc);
    pick(el, "OAuth 2.0");
    el.querySelector<HTMLButtonElement>("[data-question-submit]")?.click();
    await settle();
    // An error with no cause set is a frame this build cannot read, not a
    // refusal with no words: it is reported through the failure sink, and no
    // sentence is invented at the control (src/rpc/refuse.ts).
    expect(el.querySelector(".perm-actions .refusal")).toBeNull();
  });
});

describe("the settled card", () => {
  it("draws one verdict line per question", () => {
    const el = drawFeedQuestion(
      question({
        case: "answered",
        value: {
          atMs: 0n,
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
        value: {
          atMs: 0n,
          answers: [{ header: { text: "Auth method" }, chosen: ["OAuth 2.0", "API key"] }],
        },
      }),
      askHarness().rc,
    );
    expect(el.querySelector(".q-chosen")?.textContent).toBe("OAuth 2.0, API key");
  });

  it("draws the free text that was given", () => {
    const el = drawFeedQuestion(
      question({
        case: "answered",
        value: {
          atMs: 0n,
          answers: [
            { header: { text: "Auth method" }, chosen: [], otherText: { text: "use mTLS" } },
          ],
        },
      }),
      askHarness().rc,
    );
    expect(el.querySelector(".q-other-given")?.textContent).toBe("use mTLS");
  });

  it("draws no chosen labels for a free-text-only answer", () => {
    const el = drawFeedQuestion(
      question({
        case: "answered",
        value: {
          atMs: 0n,
          answers: [
            { header: { text: "Auth method" }, chosen: [], otherText: { text: "use mTLS" } },
          ],
        },
      }),
      askHarness().rc,
    );
    expect(el.querySelector(".q-chosen")).toBeNull();
  });

  it("offers no submit once the batch is settled", () => {
    const el = drawFeedQuestion(
      question({ case: "answered", value: { atMs: 0n, answers: [] } }),
      askHarness().rc,
    );
    expect(el.querySelector("[data-question-submit]")).toBeNull();
  });

  it("stamps when the answers landed", () => {
    vi.setSystemTime(60_000);
    const el = drawFeedQuestion(
      question({ case: "answered", value: { atMs: 0n, answers: [] } }),
      askHarness().rc,
    );
    expect(el.querySelector(".q-when")?.textContent).toBe("1m ago");
  });

  it("reads the stamp's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the answer instant does not share the shared ticker's phase.
    vi.setSystemTime(4920);
    const el = drawFeedQuestion(
      question({ case: "answered", value: { atMs: 0n, answers: [] } }),
      askHarness().rc,
    );
    // Assert: five real seconds ago reads 5s, not the lagging 4s.
    expect(el.querySelector(".q-when")?.textContent).toBe("5s ago");
  });

  it("draws an expired ask as expired, never as pending forever", () => {
    const el = drawFeedQuestion(
      question({ case: "expired", value: { atMs: 0n } }),
      askHarness().rc,
    );
    expect(el.querySelector(".q-verdict .badge")?.textContent).toBe(EXPIRED_TEXT);
  });

  it("offers no submit on an expired ask", () => {
    const el = drawFeedQuestion(
      question({ case: "expired", value: { atMs: 0n } }),
      askHarness().rc,
    );
    expect(el.querySelector("[data-question-submit]")).toBeNull();
  });
});

describe("drawFeedQuestion malformed input", () => {
  it("refuses a card whose state oneof is unset", () => {
    expect(() => drawFeedQuestion(create(FeedQuestionSchema, {}), askHarness().rc)).toThrow(
      MalformedView,
    );
  });

  it("refuses a question whose options oneof is unset", () => {
    const bad = create(FeedQuestionItemSchema, {
      header: { text: "h" },
      text: { text: "t" },
    });
    expect(() =>
      drawFeedQuestion(question({ case: "open", value: {} }, [bad]), askHarness().rc),
    ).toThrow(MalformedView);
  });

  it("refuses a question with no text", () => {
    const bad = item();
    (bad as unknown as { text: undefined }).text = undefined;
    expect(() =>
      drawFeedQuestion(question({ case: "open", value: {} }, [bad]), askHarness().rc),
    ).toThrow(MalformedView);
  });

  it("refuses a question with no header", () => {
    const bad = item();
    (bad as unknown as { header: undefined }).header = undefined;
    expect(() =>
      drawFeedQuestion(question({ case: "open", value: {} }, [bad]), askHarness().rc),
    ).toThrow(MalformedView);
  });

  it("refuses an option with no label", () => {
    const bad = item();
    (
      bad.options as unknown as { value: { options: { label: undefined }[] } }
    ).value.options[0].label = undefined;
    expect(() =>
      drawFeedQuestion(question({ case: "open", value: {} }, [bad]), askHarness().rc),
    ).toThrow(MalformedView);
  });

  it("refuses a state arm this build does not know", () => {
    const u = question({ case: "open", value: {} });
    (u as unknown as { state: { case: string; value: unknown } }).state = {
      case: "withdrawn",
      value: {},
    };
    expect(() => drawFeedQuestion(u, askHarness().rc)).toThrow(MalformedView);
  });
});
