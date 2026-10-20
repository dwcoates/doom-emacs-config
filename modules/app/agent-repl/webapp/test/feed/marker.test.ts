// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  FeedOutcomeMarkerSchema,
  type FeedOutcomeMarker,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { PromptOrigin } from "../../../proto/gen/ts/conversation/v1/prompt_origin_pb";
import { SubmitPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import renderColors from "../../../proto/vocab/render-colors.json";
import {
  MARKER_GLYPH_CHARS,
  OUTCOME_CARD_CLASS,
  OUTCOME_MARKER_CLASS,
  drawOutcomeCard,
  drawOutcomeMarker,
  formatMarkerTime,
} from "../../src/feed/marker.js";
import {
  LOGIN_REQUESTED_EVENT,
  SIGN_IN_ACTION,
  SIGN_IN_ACTION_ATTRIBUTE,
  drawSignInAction,
  type LoginRequestedDetail,
} from "../../src/login/request.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { CAPPED_CLASSES } from "../../src/expand.js";
import { countingTicker, harness, settle, type FeedScript } from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

type MarkerInit = MessageInitShape<typeof FeedOutcomeMarkerSchema>;

function marker(init: MarkerInit): FeedOutcomeMarker {
  return create(FeedOutcomeMarkerSchema, init);
}

const NEUTRAL: MarkerInit = { label: { text: "interrupted" }, family: { case: "neutral", value: {} } };

function vendor(expansion: NonNullable<Extract<MarkerInit["family"], { case: "vendorFault" }>["value"]>["expansion"]): MarkerInit {
  return {
    label: { text: "vendor error" },
    detail: { text: "rate limited" },
    family: { case: "vendorFault", value: { expansion } },
  };
}

const VENDOR: MarkerInit = vendor({ time: { atMs: 1_000_000n }, errorType: { text: "rate_limited" } });

const SAID = { content: { blocks: [{ block: { case: "text" as const, value: { text: "hello again" } } }] } };

function agentRepl(extra: { restarted?: { atMs: bigint }; resend?: { said: typeof SAID } } = {}): MarkerInit {
  return {
    label: { text: "agent-repl" },
    detail: { text: "query died" },
    family: {
      case: "agentReplFault",
      value: {
        expansion: {
          time: { atMs: 1_000_000n },
          whatDied: {
            what: {
              case: "query",
              value: { line: { text: "the query died — the SDK's iterator threw" }, thrown: { text: "socket hang up" } },
            },
          },
          ...extra,
        },
      },
    },
  };
}

const AGENT_REPL: MarkerInit = agentRepl();


function draw(init: MarkerInit, script: FeedScript = {}) {
  const h = harness(script);
  const el = drawOutcomeMarker(marker(init), { ctx: h.ctx }, "FeedOutcomeMarker");
  document.body.append(el);
  return { el, h };
}

function drawCard(init: MarkerInit, face: string[] = []) {
  const h = harness({});
  const el = drawOutcomeCard(marker(init), { ctx: h.ctx }, "FeedOutcomeMarker", face);
  document.body.append(el);
  return el;
}

function pill(el: HTMLElement): HTMLElement {
  return el.querySelector(".outcome-marker-pill") as HTMLElement;
}

function head(el: HTMLElement): HTMLElement {
  return el.querySelector(".outcome-card-head") as HTMLElement;
}

function said(el: HTMLElement): (string | null)[] {
  return [...el.querySelectorAll(".outcome-card-said")].map((line) => line.textContent);
}

function lineValue(el: HTMLElement, name: string): string | null | undefined {
  return el.querySelector(`.outcome-marker-line[data-line="${name}"] .outcome-marker-value`)?.textContent;
}

describe("drawOutcomeMarker: the anatomy", () => {
  it("draws a fault's glyph, label and detail, in that order, as the card's head", () => {
    const { el } = draw(VENDOR);
    expect(head(el).textContent).toBe("◆vendor error · rate limited");
  });

  it("draws a neutral marker's label alone when no detail is sent", () => {
    const { el } = draw(NEUTRAL);
    expect(pill(el).textContent).toBe("◼interrupted");
  });

  it("carries the outcome marker class and its family", () => {
    const { el } = draw(VENDOR);
    expect([el.classList.contains(OUTCOME_MARKER_CLASS), el.getAttribute("data-family")]).toEqual([true, "vendorFault"]);
  });

  it("is no bubble", () => {
    const { el } = draw(VENDOR);
    expect(el.querySelector(".bubble")).toBeNull();
  });

  it.each([
    ["neutral", NEUTRAL, "◼"],
    ["vendorFault", VENDOR, "◆"],
    ["agentReplFault", AGENT_REPL, "✕"],
  ])("draws the %s family's glyph from the shared vocabulary", (_name, init, char) => {
    const { el } = draw(init);
    expect(el.querySelector(".outcome-marker-glyph")?.textContent).toBe(char);
  });

  it.each([
    ["neutral", NEUTRAL, "tone-none"],
    ["vendorFault", VENDOR, "tone-turquoise"],
    ["agentReplFault", AGENT_REPL, "tone-blue"],
  ])("paints the %s family with the shared vocabulary's tone", (_name, init, tone) => {
    const { el } = draw(init);
    expect(el.classList.contains(tone)).toBe(true);
  });

  it("refuses a marker with no family", () => {
    expect(() => draw({ label: { text: "x" } })).toThrow(MalformedView);
  });

  it("refuses a marker with no label", () => {
    expect(() => draw({ family: { case: "neutral", value: {} } })).toThrow(MalformedView);
  });

  it("refuses a fault with no expansion", () => {
    expect(() => draw({ label: { text: "x" }, family: { case: "vendorFault", value: {} } })).toThrow(MalformedView);
  });

  it("builds no <button>: its actions are the one control", () => {
    const { el } = draw(vendor({ errorType: { text: "x" }, signIn: {}, resend: { said: SAID } }));
    expect(el.querySelector("button")).toBeNull();
  });
});

describe("drawOutcomeMarker: a neutral marker is a pill, a fault is a card", () => {
  it("draws a neutral marker as a plain-text pill, no card", () => {
    const { el } = draw(NEUTRAL);
    expect([pill(el).localName, el.classList.contains(OUTCOME_CARD_CLASS)]).toEqual(["span", false]);
  });

  it("draws nothing to open on a neutral marker", () => {
    const { el } = draw(NEUTRAL);
    expect(el.querySelector(".outcome-marker-expansion")).toBeNull();
  });

  it.each([
    ["vendorFault", VENDOR],
    ["agentReplFault", AGENT_REPL],
  ])("draws a %s marker as a tool card, with no pill", (_name, init) => {
    const { el } = draw(init);
    expect([el.classList.contains(OUTCOME_CARD_CLASS), el.classList.contains("tool-card"), pill(el)]).toEqual([true, true, null]);
  });

  it.each([
    ["vendorFault", VENDOR],
    ["agentReplFault", AGENT_REPL],
  ])("makes a %s card the tool card's fold, its expansion the fold's hidden section", (_name, init) => {
    const { el } = draw(init);
    const expansion = el.querySelector(".outcome-marker-expansion") as HTMLElement;
    expect([el.classList.contains("tool-fold"), expansion.parentElement === el, expansion.classList.contains("tool-output")])
      .toEqual([true, true, true]);
  });

  it("is a fold the feed's click handler opens, like a tool card", () => {
    const { el } = draw(VENDOR);
    expect(CAPPED_CLASSES.some((c) => el.classList.contains(c))).toBe(true);
  });
});

describe("the pill and the card share one head", () => {
  it("draws the same glyph, label and detail markup in a pill and in a card", () => {
    const init: MarkerInit = { label: { text: "vendor notice" }, detail: { text: "allowance exhausted" }, family: { case: "neutral", value: {} } };
    const { el: asPill } = draw(init);
    const asCard = drawCard(init);
    expect(head(asCard).innerHTML).toBe(pill(asPill).innerHTML);
  });

  it("gives both drawings the same root tone and family attribute", () => {
    const { el: asPill } = draw(NEUTRAL);
    const asCard = drawCard(NEUTRAL);
    expect([asCard.getAttribute("data-family"), asCard.classList.contains("tone-none")])
      .toEqual([asPill.getAttribute("data-family"), asPill.classList.contains("tone-none")]);
  });
});

describe("drawOutcomeCard: the closed face", () => {
  it("shows the vendor's message under the head, outside the expansion", () => {
    const el = drawCard(vendor({ errorType: { text: "overloaded" }, message: { text: "The API is overloaded." } }));
    expect([said(el), el.querySelector(".outcome-marker-expansion [data-line='message']")]).toEqual([["The API is overloaded."], null]);
  });

  it("shows no message line when the vendor recorded none", () => {
    const el = drawCard(VENDOR);
    expect(said(el)).toEqual([]);
  });

  it("shows the folded notices after the message, oldest first", () => {
    const el = drawCard({
      ...vendor({ errorType: { text: "internal" }, message: { text: "Internal server error" } }),
      notices: [{ text: "API Error: 500" }, { text: "API Error: 500 again" }],
    } as MarkerInit);
    expect(said(el)).toEqual(["Internal server error", "API Error: 500", "API Error: 500 again"]);
  });

  it("shows an agent-repl fault's notices, which has no vendor message", () => {
    const el = drawCard({ ...AGENT_REPL, notices: [{ text: "Claude's response was interrupted" }] } as MarkerInit);
    expect(said(el)).toEqual(["Claude's response was interrupted"]);
  });

  it("draws a standalone notice as a neutral card whose face is the notice's words", () => {
    const el = drawCard({ label: { text: "vendor notice" }, family: { case: "neutral", value: {} } }, ["You're approaching your limit"]);
    expect([el.classList.contains(OUTCOME_CARD_CLASS), el.classList.contains("tool-fold"), head(el).textContent, said(el)])
      .toEqual([true, false, "◼vendor notice", ["You're approaching your limit"]]);
  });

  it("draws the vendor's words as plain text, never as markup", () => {
    const el = drawCard(NEUTRAL, ["<b>API Error</b>"]);
    expect([said(el), el.querySelector(".outcome-card-said b")]).toEqual([["<b>API Error</b>"], null]);
  });
});

describe("drawOutcomeMarker: a vendor fault's expansion", () => {
  it("draws the time as the reader's wall clock", () => {
    const { el } = draw(VENDOR);
    expect(lineValue(el, "time")).toBe(formatMarkerTime(1_000_000));
  });

  it("draws no time line when none is sent", () => {
    const { el } = draw(vendor({ errorType: { text: "compaction_failed" } }));
    expect(el.querySelector('[data-line="time"]')).toBeNull();
  });

  it("draws the vendor's error type verbatim", () => {
    const { el } = draw(VENDOR);
    expect(lineValue(el, "error-type")).toBe("rate_limited");
  });

  it("draws the vendor's message verbatim in the face when sent", () => {
    const { el } = draw(vendor({ errorType: { text: "x" }, message: { text: "Rate limit reached" } }));
    expect(said(el)).toEqual(["Rate limit reached"]);
  });

  it.each(["message", "retries", "retry", "model", "account"])("draws no %s line when none is sent", (name) => {
    const { el } = draw(VENDOR);
    expect(el.querySelector(`[data-line="${name}"]`)).toBeNull();
  });

  it("draws the retries made", () => {
    const { el } = draw(vendor({ errorType: { text: "x" }, retries: { count: 3 } }));
    expect(lineValue(el, "retries")).toBe("3");
  });

  it("draws the model and the account verbatim", () => {
    const { el } = draw(vendor({ errorType: { text: "x" }, model: { name: "claude-opus-5" }, account: { email: "me@example.com" } }));
    expect([lineValue(el, "model"), lineValue(el, "account")]).toEqual(["claude-opus-5", "me@example.com"]);
  });

  it("counts down to the vendor's wait", () => {
    const { el } = draw(vendor({ errorType: { text: "x" }, retryAt: { atMs: 1_042_000n } }));
    expect(lineValue(el, "retry")).toBe("retry in 42s");
  });

  it("says the wait is over once it has passed, and stops ticking", () => {
    const ticker = countingTicker();
    const h = harness({ ticker });
    const el = drawOutcomeMarker(
      marker(vendor({ errorType: { text: "x" }, retryAt: { atMs: 1_001_000n } })),
      { ctx: h.ctx },
      "FeedOutcomeMarker",
    );
    document.body.append(el);
    vi.advanceTimersByTime(2_000);
    expect([lineValue(el, "retry"), ticker.live()]).toEqual(["ready to retry", 0]);
  });

  it("draws no actions when none is sent", () => {
    const { el } = draw(VENDOR);
    expect(el.querySelector(".outcome-marker-actions")).toBeNull();
  });
});

describe("drawOutcomeMarker: an agent-repl fault's expansion", () => {
  it("draws the time", () => {
    const { el } = draw(AGENT_REPL);
    expect(lineValue(el, "time")).toBe(formatMarkerTime(1_000_000));
  });

  it("draws what died, and how", () => {
    const { el } = draw(AGENT_REPL);
    expect(el.querySelector('[data-line="what-died"]')?.textContent).toBe("querythe query died — the SDK's iterator threw");
  });

  it("draws what the query threw", () => {
    const { el } = draw(AGENT_REPL);
    expect(lineValue(el, "thrown")).toBe("socket hang up");
  });

  it("draws a process death", () => {
    const { el } = draw({
      label: { text: "agent-repl" },
      family: {
        case: "agentReplFault",
        value: {
          expansion: {
            time: { atMs: 1_000_000n },
            whatDied: { what: { case: "process", value: { line: { text: "the agent process died" } } } },
          },
        },
      },
    });
    expect(el.querySelector('[data-line="what-died"] .outcome-marker-key')?.textContent).toBe("process");
  });

  it("says the restart worked when it did", () => {
    const { el } = draw(agentRepl({ restarted: { atMs: 1_005_000n } }));
    expect(lineValue(el, "restarted")).toBe(`the session started again at ${formatMarkerTime(1_005_000)}`);
  });

  it("says nothing of a restart not yet seen", () => {
    const { el } = draw(AGENT_REPL);
    expect(el.querySelector('[data-line="restarted"]')).toBeNull();
  });

  it("refuses an agent-repl fault that says nothing of what died", () => {
    expect(() =>
      draw({
        label: { text: "agent-repl" },
        family: { case: "agentReplFault", value: { expansion: { time: { atMs: 1n } } } },
      }),
    ).toThrow(MalformedView);
  });
});

describe("drawOutcomeMarker: the actions", () => {
  it("asks the page's login overlay to open on sign in", () => {
    const { el } = draw(vendor({ errorType: { text: "authentication_failed" }, signIn: {} }));
    const asked: HTMLElement[] = [];
    const onRequest = (event: Event): void => {
      event.preventDefault();
      asked.push((event as CustomEvent<LoginRequestedDetail>).detail.control);
    };
    document.addEventListener(LOGIN_REQUESTED_EVENT, onRequest);
    const button = el.querySelector('[data-action="sign-in"]') as HTMLElement;
    button.click();
    document.removeEventListener(LOGIN_REQUESTED_EVENT, onRequest);
    expect(asked).toEqual([button]);
  });

  it("draws the shared sign-in action, in the marker's action class", () => {
    const { el } = draw(vendor({ errorType: { text: "authentication_failed" }, signIn: {} }));
    const button = el.querySelector(`[${SIGN_IN_ACTION_ATTRIBUTE}="${SIGN_IN_ACTION}"]`) as HTMLElement;
    const shared = drawSignInAction("outcome-marker-action", "x");
    expect([button.localName, button.className, button.textContent]).toEqual([shared.localName, shared.className, shared.textContent]);
  });

  it("resends the prompt as said through SubmitPrompt, as a card action", async () => {
    const { el, h } = draw(vendor({ errorType: { text: "x" }, resend: { said: SAID } }));
    (el.querySelector('[data-action="resend"]') as HTMLElement).click();
    await settle((ms) => vi.advanceTimersByTimeAsync(ms));
    const req = h.calls.submitPrompt[0];
    expect([
      h.calls.submitPrompt.length,
      req?.origin,
      req?.said?.content?.blocks[0]?.block.value,
      req?.idempotencyKey !== "",
    ]).toEqual([1, PromptOrigin.WEBAPP_CARD_ACTION, { $typeName: "conversation.v1.TextBlock", text: "hello again" }, true]);
  });

  it("marks the resend accepted", async () => {
    const { el } = draw(vendor({ errorType: { text: "x" }, resend: { said: SAID } }));
    const button = el.querySelector('[data-action="resend"]') as HTMLElement;
    button.click();
    await settle((ms) => vi.advanceTimersByTimeAsync(ms));
    expect(button.getAttribute("data-resent")).toBe("true");
  });

  it("draws a refused resend at the action", async () => {
    const { el } = draw(vendor({ errorType: { text: "x" }, resend: { said: SAID } }), {
      submitPrompt: () =>
        create(SubmitPromptResponseSchema, {
          result: { case: "error", value: { reason: { case: "noSession", value: {} } } },
        }),
    });
    (el.querySelector('[data-action="resend"]') as HTMLElement).click();
    await settle((ms) => vi.advanceTimersByTimeAsync(ms));
    expect(el.querySelector('.outcome-marker-actions .refusal')?.getAttribute("data-arm")).toBe("noSession");
  });

  it("offers a resend on an agent-repl fault too", () => {
    const { el } = draw(agentRepl({ resend: { said: SAID } }));
    expect(el.querySelector('[data-action="resend"]')).not.toBeNull();
  });
});

describe("the outcome marker's glyphs", () => {
  it("draws a character for every glyph name the shared vocabulary names", () => {
    const names = Object.values(renderColors.feed_outcome_marker_glyphs);
    expect(names.every((name) => MARKER_GLYPH_CHARS[name] !== undefined)).toBe(true);
  });
});
