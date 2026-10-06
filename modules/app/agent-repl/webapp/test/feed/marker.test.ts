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
  OUTCOME_MARKER_CLASS,
  drawOutcomeMarker,
  formatMarkerTime,
} from "../../src/feed/marker.js";
import { LOGIN_REQUESTED_EVENT, type LoginRequestedDetail } from "../../src/login/request.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { CONTROL_TAG } from "../../src/control.js";
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


function draw(init: MarkerInit, script: FeedScript = {}, previous?: Element) {
  const h = harness(script);
  const el = drawOutcomeMarker(marker(init), { ctx: h.ctx, previous }, "FeedOutcomeMarker");
  document.body.append(el);
  return { el, h };
}

function pill(el: HTMLElement): HTMLElement {
  return el.querySelector(".outcome-marker-pill") as HTMLElement;
}

function lineValue(el: HTMLElement, name: string): string | null | undefined {
  return el.querySelector(`.outcome-marker-line[data-line="${name}"] .outcome-marker-value`)?.textContent;
}

describe("drawOutcomeMarker: the anatomy", () => {
  it("draws the glyph, the label and the detail, in that order", () => {
    const { el } = draw(VENDOR);
    expect(pill(el).textContent).toBe("◆vendor error · rate limited›");
  });

  it("draws the label alone when no detail is sent", () => {
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

  it("builds no <button>: its controls are the one control", () => {
    const { el } = draw(vendor({ errorType: { text: "x" }, signIn: {}, resend: { said: SAID } }));
    expect([el.querySelector("button"), pill(el).localName]).toEqual([null, CONTROL_TAG]);
  });
});

describe("drawOutcomeMarker: only a fault expands", () => {
  it("draws no chevron on a neutral marker", () => {
    const { el } = draw(NEUTRAL);
    expect(el.querySelector(".outcome-marker-chevron")).toBeNull();
  });

  it("draws nothing to open on a neutral marker", () => {
    const { el } = draw(NEUTRAL);
    expect(el.querySelector(".outcome-marker-expansion")).toBeNull();
  });

  it("makes a neutral marker's pill plain text, no control", () => {
    const { el } = draw(NEUTRAL);
    expect(pill(el).localName).toBe("span");
  });

  it.each([
    ["vendorFault", VENDOR],
    ["agentReplFault", AGENT_REPL],
  ])("draws a %s marker collapsed, with its closed chevron", (_name, init) => {
    const { el } = draw(init);
    const expansion = el.querySelector(".outcome-marker-expansion") as HTMLElement;
    expect([expansion.hidden, el.querySelector(".outcome-marker-chevron")?.textContent]).toEqual([true, "›"]);
  });

  it("opens the expansion in place on a click", () => {
    const { el } = draw(VENDOR);
    pill(el).click();
    const expansion = el.querySelector(".outcome-marker-expansion") as HTMLElement;
    expect([expansion.hidden, pill(el).getAttribute("aria-expanded"), el.querySelector(".outcome-marker-chevron")?.textContent]).toEqual([false, "true", "⌄"]);
  });

  it("closes it again on a second click", () => {
    const { el } = draw(VENDOR);
    pill(el).click();
    pill(el).click();
    expect((el.querySelector(".outcome-marker-expansion") as HTMLElement).hidden).toBe(true);
  });

  it("keeps a reader's open expansion across a re-push", () => {
    const { el: first } = draw(VENDOR);
    pill(first).click();
    const { el: again } = draw(VENDOR, {}, first);
    expect((again.querySelector(".outcome-marker-expansion") as HTMLElement).hidden).toBe(false);
  });

  it("draws the expansion inside the marker, never outside it", () => {
    const { el } = draw(VENDOR);
    expect(el.contains(el.querySelector(".outcome-marker-expansion"))).toBe(true);
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

  it("draws the vendor's message verbatim when sent", () => {
    const { el } = draw(vendor({ errorType: { text: "x" }, message: { text: "Rate limit reached" } }));
    expect(lineValue(el, "message")).toBe("Rate limit reached");
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
