// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  AnswerColdGateErrorSchema,
  AnswerColdGateResponseSchema,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_cold_gate_pb";
import { SessionCompactScope } from "../../../../proto/gen/ts/conversation/v1/session_pb";
import {
  FeedColdGateSchema,
  type FeedColdGate,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  COLD_GATE_COPY,
  drawFeedColdGate,
  scopeLabel,
} from "../../../src/feed/asks/cold-gate.js";
import { armsOf } from "../arms.js";
import { askHarness, ROW_ID, settle as drain } from "./harness.js";

type InitState = MessageInitShape<typeof FeedColdGateSchema>["state"];

const MODEL = "claude-opus-5";
const SUMMARIZER = "claude-haiku-4";

/** The standing gate's facts. */
function standing(
  opts: {
    tokens?: bigint;
    lastRequestMs?: bigint;
    models?: string[];
    scopes?: SessionCompactScope[];
  } = {},
): InitState {
  return {
    case: "standing",
    value: {
      contextTokens: { tokens: opts.tokens ?? 182_000n },
      lastRequest: { atMs: opts.lastRequestMs ?? 0n },
      model: { model: { name: MODEL } },
      compact: {
        models: (opts.models ?? [SUMMARIZER, MODEL]).map((name) => ({ model: { name } })),
        scopes: opts.scopes ?? [
          SessionCompactScope.ALL,
          SessionCompactScope.PROMPTS,
          SessionCompactScope.RESPONSES,
        ],
      },
    },
  };
}

/** A gate row in one STATE. */
function gate(state: InitState): FeedColdGate {
  return create(FeedColdGateSchema, { state });
}

/** A refused answer carrying CAUSE. */
function refused(cause: MessageInitShape<typeof AnswerColdGateErrorSchema>["cause"]) {
  return create(AnswerColdGateResponseSchema, { result: { case: "error", value: { cause } } });
}

/** Open the compact submenu, which the compact row's opener reveals. */
function openSubmenu(el: HTMLElement): void {
  el.querySelector<HTMLButtonElement>("[data-compact-open]")?.click();
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

describe("scopeLabel", () => {
  const cases = [
    { scope: SessionCompactScope.ALL, want: "everything" },
    { scope: SessionCompactScope.PROMPTS, want: "my prompts only" },
    { scope: SessionCompactScope.RESPONSES, want: "the agent's responses only" },
  ] as const;

  for (const c of cases) {
    it(`words ${SessionCompactScope[c.scope]} as "${c.want}"`, () => {
      expect(scopeLabel(c.scope, "path")).toBe(c.want);
    });
  }

  it("refuses UNSPECIFIED, which is never offered and never sent", () => {
    expect(() => scopeLabel(SessionCompactScope.UNSPECIFIED, "path")).toThrow(MalformedView);
  });

  it("words every scope the schema declares except UNSPECIFIED", () => {
    expect(Object.keys(COLD_GATE_COPY.scopes).sort()).toEqual(
      ["ALL", "PROMPTS", "RESPONSES"].sort(),
    );
  });
});

describe("the standing gate", () => {
  it("draws the title from the one copy object", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(el.querySelector(".hibernation-heading")?.textContent).toBe(COLD_GATE_COPY.title);
  });

  it("formats the raw token count into the lead", () => {
    const el = drawFeedColdGate(gate(standing({ tokens: 182_000n })), askHarness().rc);
    expect(el.querySelector(".hibernation-context")?.textContent).toContain("182k");
  });

  it("names the session's model verbatim in the lead", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(el.querySelector(".hibernation-context")?.textContent).toContain(MODEL);
  });

  it("ticks the lapse from the last vendor request", () => {
    vi.setSystemTime(7_200_000);
    const el = drawFeedColdGate(gate(standing({ lastRequestMs: 0n })), askHarness().rc);
    expect(el.querySelector(".hibernation-since")?.textContent).toBe("last vendor request 2h ago");
  });

  it("keeps ticking the lapse as the gate stands", async () => {
    const el = drawFeedColdGate(gate(standing({ lastRequestMs: 0n })), askHarness().rc);
    document.body.append(el);
    await vi.advanceTimersByTimeAsync(65_000);
    expect(el.querySelector(".hibernation-since")?.textContent).toBe("last vendor request 1m 5s ago");
  });

  it("draws the pay and clear buttons", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(
      [...el.querySelectorAll("[data-cold-gate]")].map((n) => n.getAttribute("data-cold-gate")),
    ).toEqual(["pay", "clear", "compact"]);
  });

  it("says what each choice keeps, not only what it costs", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect([...el.querySelectorAll(".hibernation-option-text")].map((n) => n.textContent)).toEqual([
      COLD_GATE_COPY.pay.hint,
      COLD_GATE_COPY.clear.hint,
      COLD_GATE_COPY.compact.hint,
    ]);
  });

  it("keeps the compact submenu closed until the reader opens it", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(el.querySelector<HTMLElement>(".cold-gate-submenu")?.hidden).toBe(true);
  });

  it("opens the compact submenu on the opener's click", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    openSubmenu(el);
    expect(el.querySelector<HTMLElement>(".cold-gate-submenu")?.hidden).toBe(false);
  });

  it("offers one summarizer radio per served model", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(
      [...el.querySelectorAll("[data-compact-model]")].map((n) =>
        n.getAttribute("data-compact-model"),
      ),
    ).toEqual([SUMMARIZER, MODEL]);
  });

  it("offers one scope radio per served scope", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(
      [...el.querySelectorAll("[data-compact-scope]")].map((n) =>
        n.getAttribute("data-compact-scope"),
      ),
    ).toEqual(["ALL", "PROMPTS", "RESPONSES"]);
  });

  it("words each offered scope with the submenu's own labels", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect([...el.querySelectorAll(".cold-gate-choice")].slice(2).map((n) => n.textContent)).toEqual(
      [
        COLD_GATE_COPY.scopes.ALL,
        COLD_GATE_COPY.scopes.PROMPTS,
        COLD_GATE_COPY.scopes.RESPONSES,
      ],
    );
  });

  it("draws only the scopes the menu served", () => {
    const el = drawFeedColdGate(
      gate(standing({ scopes: [SessionCompactScope.ALL] })),
      askHarness().rc,
    );
    expect(el.querySelectorAll("[data-compact-scope]").length).toBe(1);
  });

  it("refuses a menu offering UNSPECIFIED", () => {
    expect(() =>
      drawFeedColdGate(
        gate(standing({ scopes: [SessionCompactScope.UNSPECIFIED] })),
        askHarness().rc,
      ),
    ).toThrow(MalformedView);
  });

  it("pre-selects the first served summarizer, so the send is always legal", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(el.querySelector<HTMLInputElement>("[data-compact-model]")?.checked).toBe(true);
  });

  it("pre-selects the first served scope", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(el.querySelector<HTMLInputElement>("[data-compact-scope]")?.checked).toBe(true);
  });
});

describe("answering the gate", () => {
  const simple = [
    { hook: "pay", arm: "pay" },
    { hook: "clear", arm: "clear" },
  ] as const;

  for (const c of simple) {
    it(`sends the ${c.arm} arm`, async () => {
      const h = askHarness();
      const el = drawFeedColdGate(gate(standing()), h.rc);
      el.querySelector<HTMLButtonElement>(`[data-cold-gate="${c.hook}"]`)?.click();
      await settle();
      expect(h.calls.coldGate[0]?.choice.case).toBe(c.arm);
    });
  }

  it("echoes the gate's own row", async () => {
    const h = askHarness();
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<HTMLButtonElement>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect(h.calls.coldGate[0]?.gate?.value).toBe(ROW_ID);
  });

  it("echoes the summarizer the reader picked", async () => {
    const h = askHarness();
    const el = drawFeedColdGate(gate(standing()), h.rc);
    openSubmenu(el);
    const second = el.querySelectorAll<HTMLInputElement>("[data-compact-model]")[1];
    if (second !== undefined) second.checked = true;
    el.querySelector<HTMLButtonElement>('[data-cold-gate="compact"]')?.click();
    await settle();
    const choice = h.calls.coldGate[0]?.choice;
    expect(choice?.case === "compact" ? choice.value.model?.name : null).toBe(MODEL);
  });

  it("echoes the scope the reader picked", async () => {
    const h = askHarness();
    const el = drawFeedColdGate(gate(standing()), h.rc);
    openSubmenu(el);
    const third = el.querySelectorAll<HTMLInputElement>("[data-compact-scope]")[2];
    if (third !== undefined) third.checked = true;
    el.querySelector<HTMLButtonElement>('[data-cold-gate="compact"]')?.click();
    await settle();
    const choice = h.calls.coldGate[0]?.choice;
    expect(choice?.case === "compact" ? choice.value.scope : null).toBe(
      SessionCompactScope.RESPONSES,
    );
  });

  it("latches every button inert while the answer is in flight", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    el.querySelector<HTMLButtonElement>('[data-cold-gate="pay"]')?.click();
    expect([...el.querySelectorAll("button")].every((b) => b.disabled)).toBe(true);
  });

  it("draws nothing on success — the resolved trace is the row's re-push", async () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    el.querySelector<HTMLButtonElement>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect(el.querySelector(".refusal")).toBeNull();
  });

  it("draws a transport failure at the buttons", async () => {
    const h = askHarness({ fail: true });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<HTMLButtonElement>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect(el.querySelector(".hibernation-actions .refusal")?.getAttribute("data-arm")).toBe(
      "transport",
    );
  });
});

describe("a refused gate answer", () => {
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
      arm: "noColdGate",
      cause: { case: "noColdGate", value: {} },
      text: "no cold gate is standing for this workspace",
    },
    {
      arm: "unservedRemediation",
      cause: { case: "unservedRemediation", value: {} },
      text: "the gate never offered that remediation",
    },
    {
      arm: "noSession",
      cause: { case: "noSession", value: {} },
      text: "the workspace has no session to answer",
    },
  ] as const;

  for (const c of causes) {
    it(`says what ${c.arm} means, at the buttons`, async () => {
      const h = askHarness({ coldGate: refused(c.cause as never) });
      const el = drawFeedColdGate(gate(standing()), h.rc);
      el.querySelector<HTMLButtonElement>('[data-cold-gate="pay"]')?.click();
      await settle();
      const drawn = el.querySelector(".hibernation-actions .refusal");
      expect([drawn?.getAttribute("data-arm"), drawn?.textContent]).toEqual([c.arm, c.text]);
    });
  }

  it("words every cause the schema declares", () => {
    expect(causes.map((c) => c.arm).sort()).toEqual(
      armsOf(AnswerColdGateErrorSchema.oneofs, "cause").sort(),
    );
  });

  it("gives the buttons back so the reader can act on the cause", async () => {
    const h = askHarness({ coldGate: refused({ case: "noSession", value: {} } as never) });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    const pay = el.querySelector<HTMLButtonElement>('[data-cold-gate="pay"]');
    pay?.click();
    await settle();
    expect(pay?.disabled).toBe(false);
  });

  it("states an unset cause as unreadable rather than as a cause", async () => {
    const h = askHarness({
      coldGate: create(AnswerColdGateResponseSchema, { result: { case: "error", value: {} } }),
    });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<HTMLButtonElement>('[data-cold-gate="pay"]')?.click();
    await settle();
    // An error with no cause set is a frame this build cannot read, not a
    // refusal with no words: it is reported through the failure sink, and no
    // sentence is invented at the control (src/rpc/refuse.ts).
    expect(el.querySelector(".hibernation-actions .refusal")).toBeNull();
  });
});

describe("the resolved trace", () => {
  const traces = [
    { arm: "pay", choice: { case: "pay", value: {} }, text: COLD_GATE_COPY.resolved.pay },
    { arm: "clear", choice: { case: "clear", value: {} }, text: COLD_GATE_COPY.resolved.clear },
  ] as const;

  for (const c of traces) {
    it(`traces the ${c.arm} remediation`, () => {
      const el = drawFeedColdGate(
        gate({ case: "resolved", value: { atMs: 0n, choice: c.choice as never } }),
        askHarness().rc,
      );
      expect(el.querySelector(".cold-gate-trace")?.textContent).toBe(c.text);
    });
  }

  const scopes = [
    { scope: SessionCompactScope.ALL, words: "everything" },
    { scope: SessionCompactScope.PROMPTS, words: "my prompts only" },
    { scope: SessionCompactScope.RESPONSES, words: "the agent's responses only" },
  ] as const;

  for (const c of scopes) {
    it(`traces a compaction of ${SessionCompactScope[c.scope]} with its scope words`, () => {
      const el = drawFeedColdGate(
        gate({
          case: "resolved",
          value: {
            atMs: 0n,
            choice: {
              case: "compact",
              value: { model: { model: { name: SUMMARIZER } }, scope: c.scope },
            },
          },
        }),
        askHarness().rc,
      );
      expect(el.querySelector(".cold-gate-trace")?.textContent).toBe(
        `compacted ${c.words} with ${SUMMARIZER}`,
      );
    });
  }

  it("refuses a compact trace whose scope is UNSPECIFIED", () => {
    expect(() =>
      drawFeedColdGate(
        gate({
          case: "resolved",
          value: {
            atMs: 0n,
            choice: {
              case: "compact",
              value: {
                model: { model: { name: SUMMARIZER } },
                scope: SessionCompactScope.UNSPECIFIED,
              },
            },
          },
        }),
        askHarness().rc,
      ),
    ).toThrow(MalformedView);
  });

  it("carries the chosen arm as the trace's hook", () => {
    const el = drawFeedColdGate(
      gate({ case: "resolved", value: { atMs: 0n, choice: { case: "clear", value: {} } } }),
      askHarness().rc,
    );
    expect(el.getAttribute("data-arm")).toBe("clear");
  });

  it("stamps when the choice landed", () => {
    vi.setSystemTime(60_000);
    const el = drawFeedColdGate(
      gate({ case: "resolved", value: { atMs: 0n, choice: { case: "pay", value: {} } } }),
      askHarness().rc,
    );
    expect(el.querySelector(".cold-gate-when")?.textContent).toBe("1m ago");
  });

  it("offers no buttons once the gate is resolved", () => {
    const el = drawFeedColdGate(
      gate({ case: "resolved", value: { atMs: 0n, choice: { case: "pay", value: {} } } }),
      askHarness().rc,
    );
    expect(el.querySelector("[data-cold-gate]")).toBeNull();
  });
});

describe("drawFeedColdGate malformed input", () => {
  it("refuses a row whose state oneof is unset", () => {
    expect(() => drawFeedColdGate(create(FeedColdGateSchema, {}), askHarness().rc)).toThrow(
      MalformedView,
    );
  });

  it("refuses a standing gate with no token count", () => {
    const u = gate(standing());
    (u.state as unknown as { value: { contextTokens: undefined } }).value.contextTokens = undefined;
    expect(() => drawFeedColdGate(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a standing gate with no last request", () => {
    const u = gate(standing());
    (u.state as unknown as { value: { lastRequest: undefined } }).value.lastRequest = undefined;
    expect(() => drawFeedColdGate(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a standing gate with no compact menu", () => {
    const u = gate(standing());
    (u.state as unknown as { value: { compact: undefined } }).value.compact = undefined;
    expect(() => drawFeedColdGate(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a negative token count", () => {
    const u = gate(standing());
    (u.state as unknown as { value: { contextTokens: { tokens: bigint } } }).value.contextTokens.tokens =
      -1n;
    expect(() => drawFeedColdGate(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a token count past a safe integer", () => {
    const u = gate(standing({ tokens: BigInt(Number.MAX_SAFE_INTEGER) + 10n }));
    expect(() => drawFeedColdGate(u, askHarness().rc)).toThrow(MalformedView);
  });

  it("refuses a resolved trace whose choice oneof is unset", () => {
    expect(() =>
      drawFeedColdGate(gate({ case: "resolved", value: { atMs: 0n } }), askHarness().rc),
    ).toThrow(MalformedView);
  });

  it("refuses a state arm this build does not know", () => {
    const u = gate(standing());
    (u as unknown as { state: { case: string; value: unknown } }).state = {
      case: "warming",
      value: {},
    };
    expect(() => drawFeedColdGate(u, askHarness().rc)).toThrow(MalformedView);
  });
});
