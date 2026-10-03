// @vitest-environment jsdom
import { type Control } from "../../../src/control.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { Code } from "@connectrpc/connect";
import {
  AnswerColdGateErrorSchema,
  AnswerColdGateResponseSchema,
  type AnswerColdGateResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_cold_gate_pb";
import { SessionCompactScope } from "../../../../proto/gen/ts/conversation/v1/session_pb";
import {
  FeedColdGateSchema,
  FeedIdSchema,
  FeedRowSchema,
  type FeedColdGate,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FailureKind } from "../../../../proto/gen/ts/frontend/v1/failure_pb";
import { createTicker } from "../../../src/clock.js";
import type { AgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  COLD_GATE_COPY,
  drawFeedColdGate,
  scopeLabel,
  scopeName,
} from "../../../src/feed/asks/cold-gate.js";
import {
  publishCompactionProgress,
  resetCompactionProgress,
} from "../../../src/footer/progress.js";
import { armsOf } from "../arms.js";
import { askHarness, ROW_ID, settle as drain, WORKSPACE } from "./harness.js";
import { orderFor } from "../../feed-order.js";
import { stopClocks } from "../../../src/feed/ticking.js";

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
    /** True for a gate whose account is not offered compaction. */
    noCompact?: boolean;
  } = {},
): InitState {
  return {
    case: "standing",
    value: {
      contextTokens: { tokens: opts.tokens ?? 182_000n },
      lastRequest: { atMs: opts.lastRequestMs ?? 0n },
      model: { model: { name: MODEL } },
      ...(opts.noCompact
        ? {}
        : {
            compact: {
              models: (opts.models ?? [SUMMARIZER, MODEL]).map((name) => ({ model: { name } })),
              scopes: opts.scopes ?? [
                SessionCompactScope.ALL,
                SessionCompactScope.PROMPTS,
                SessionCompactScope.RESPONSES,
              ],
            },
          }),
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
  el.querySelector<Control>("[data-compact-open]")?.click();
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
  resetCompactionProgress();
});

/** The gate's progress slot, as the card built it. */
function progressSlot(el: HTMLElement): HTMLElement | null {
  return el.querySelector<HTMLElement>("[data-cold-gate-progress]");
}

describe("scopeName", () => {
  const cases = [
    SessionCompactScope.ALL,
    SessionCompactScope.PROMPTS,
    SessionCompactScope.RESPONSES,
  ] as const;

  for (const scope of cases) {
    it(`names ${SessionCompactScope[scope]} by its enum name`, () => {
      expect(scopeName(scope, "path")).toBe(SessionCompactScope[scope]);
    });
  }

  it("refuses UNSPECIFIED, which has no name this build can draw", () => {
    expect(() => scopeName(SessionCompactScope.UNSPECIFIED, "path")).toThrow(MalformedView);
  });
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

  it("keeps ticking the lapse after its turn's clocks are stopped", async () => {
    const el = drawFeedColdGate(gate(standing({ lastRequestMs: 0n })), askHarness().rc);
    document.body.append(el);
    stopClocks(el);
    await vi.advanceTimersByTimeAsync(65_000);
    expect(el.querySelector(".hibernation-since")?.textContent).toBe("last vendor request 1m 5s ago");
  });

  it("reads the lapse's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the last request does not share the shared ticker's phase.
    vi.setSystemTime(4920);
    const el = drawFeedColdGate(gate(standing({ lastRequestMs: 0n })), askHarness().rc);
    // Assert: five real seconds of lapse reads 5s, not the lagging 4s.
    expect(el.querySelector(".hibernation-since")?.textContent).toBe("last vendor request 5s ago");
  });

  it("draws the pay and clear buttons", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(
      [...el.querySelectorAll("[data-cold-gate]")].map((n) => n.getAttribute("data-cold-gate")),
    ).toEqual(["pay", "clear", "compact"]);
  });

  it("draws only pay and clear for a gate that offers no compaction", () => {
    const el = drawFeedColdGate(gate(standing({ noCompact: true })), askHarness().rc);
    expect(
      [...el.querySelectorAll("[data-cold-gate]")].map((n) => n.getAttribute("data-cold-gate")),
    ).toEqual(["pay", "clear"]);
  });

  it("draws no compact opener or submenu for a gate that offers no compaction", () => {
    const el = drawFeedColdGate(gate(standing({ noCompact: true })), askHarness().rc);
    expect(el.querySelector("[data-compact-open]")).toBeNull();
    expect(el.querySelector(".cold-gate-submenu")).toBeNull();
  });

  it("keeps the progress slot on a gate that offers no compaction", () => {
    const el = drawFeedColdGate(gate(standing({ noCompact: true })), askHarness().rc);
    expect(el.querySelector<HTMLElement>("[data-cold-gate-progress]")?.hidden).toBe(true);
  });

  it("says what each choice keeps as its button's tooltip", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect([...el.querySelectorAll<HTMLElement>(".cold-gate-buttons > *")].map((n) => n.title)).toEqual([
      COLD_GATE_COPY.compact.hint,
      COLD_GATE_COPY.pay.hint,
      COLD_GATE_COPY.clear.hint,
    ]);
  });

  it("draws no explanatory sentence beside the buttons", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect(el.querySelector(".hibernation-option-text")).toBeNull();
  });

  it("puts every button on the one row, compact first", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    expect([...el.querySelectorAll(".cold-gate-buttons > *")].map((n) => n.textContent)).toEqual([
      COLD_GATE_COPY.compact.label,
      COLD_GATE_COPY.pay.label,
      COLD_GATE_COPY.clear.label,
    ]);
  });

  it("gives compact the pay button's fill and a green border", async () => {
    const css = (await import("../../../src/styles.css?raw")).default;
    const rule = /\.cold-gate-buttons > ar-button\.hibernation-compact\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    expect([rule.includes("background: var(--bg)"), rule.includes("border-color: var(--ok)")]).toEqual([true, true]);
  });

  it("sizes the buttons equal, to the widest label, centred", async () => {
    const css = (await import("../../../src/styles.css?raw")).default;
    const rule = /\.cold-gate-buttons\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    const label = /\.cold-gate-buttons > ar-button\s*\{[^}]*\}/.exec(css)?.[0] ?? "";
    expect([
      rule.includes("grid-auto-columns: 1fr"),
      rule.includes("width: max-content"),
      label.includes("text-align: center"),
    ]).toEqual([true, true, true]);
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
      el.querySelector<Control>(`[data-cold-gate="${c.hook}"]`)?.click();
      await settle();
      expect(h.calls.coldGate[0]?.choice.case).toBe(c.arm);
    });
  }

  for (const c of simple) {
    it(`sends the ${c.arm} arm from a gate that offers no compaction`, async () => {
      const h = askHarness();
      const el = drawFeedColdGate(gate(standing({ noCompact: true })), h.rc);
      el.querySelector<Control>(`[data-cold-gate="${c.hook}"]`)?.click();
      await settle();
      expect(h.calls.coldGate[0]?.choice.case).toBe(c.arm);
    });
  }

  it("echoes the gate's own row", async () => {
    const h = askHarness();
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect(h.calls.coldGate[0]?.gate?.value).toBe(ROW_ID);
  });

  it("echoes the summarizer the reader picked", async () => {
    const h = askHarness();
    const el = drawFeedColdGate(gate(standing()), h.rc);
    openSubmenu(el);
    const second = el.querySelectorAll<HTMLInputElement>("[data-compact-model]")[1];
    if (second !== undefined) second.checked = true;
    el.querySelector<Control>('[data-cold-gate="compact"]')?.click();
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
    el.querySelector<Control>('[data-cold-gate="compact"]')?.click();
    await settle();
    const choice = h.calls.coldGate[0]?.choice;
    expect(choice?.case === "compact" ? choice.value.scope : null).toBe(
      SessionCompactScope.RESPONSES,
    );
  });

  it("latches every button inert while the answer is in flight", () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    expect([...el.querySelectorAll<Control>("ar-button")].every((b) => b.disabled)).toBe(true);
  });

  // GROUNDED 2026-09-14 (owner's report): "compact and resume" ran a ~60s
  // compaction with the buttons inert and NOTHING anywhere saying so. The
  // daemon composes the phase line on the footer's own stream; the card draws
  // it while it waits, and draws nothing of its own.
  it("draws the daemon's compaction line while the answer is in flight", () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    publishCompactionProgress("compacting · summarizing 412 messages");
    // Act
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    // Assert
    expect(progressSlot(el)?.textContent).toBe("compacting · summarizing 412 messages");
  });

  it("redraws the slot as the daemon pushes the next phase", () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    // Act
    publishCompactionProgress("compacting · writing the summary");
    // Assert
    expect(progressSlot(el)?.textContent).toBe("compacting · writing the summary");
  });

  it("draws exactly the sentence the footer carried, composing none of its own", () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    // Act
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    publishCompactionProgress("anything at all");
    // Assert: the whole slot IS the daemon's string — no stem, no suffix.
    expect(progressSlot(el)?.textContent).toBe("anything at all");
  });

  it("shows nothing while the footer carries no compaction line", () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    // Act
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    // Assert
    expect(progressSlot(el)?.textContent).toBe("");
  });

  it("keeps the slot out of the card while there is nothing to say", () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    // Act
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    // Assert
    expect(progressSlot(el)?.hidden).toBe(true);
  });

  it("reveals the slot the moment the daemon has a phase to report", () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    // Act
    publishCompactionProgress("compacting · reading the transcript");
    // Assert
    expect(progressSlot(el)?.hidden).toBe(false);
  });

  it("clears the slot once the answer resolves", async () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    publishCompactionProgress("compacting · summarizing 412 messages");
    // Act
    await settle();
    // Assert
    expect(progressSlot(el)?.textContent).toBe("");
  });

  it("stops following the footer once the answer resolved", async () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    // Act
    publishCompactionProgress("compacting · a LATER compaction entirely");
    // Assert
    expect(progressSlot(el)?.textContent).toBe("");
  });

  it("clears the slot when the answer was refused", async () => {
    // Arrange
    const h = askHarness({ fail: true });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    publishCompactionProgress("compacting · summarizing 412 messages");
    // Act
    await settle();
    // Assert
    expect(progressSlot(el)?.textContent).toBe("");
  });

  it("draws nothing on success — the resolved trace is the row's re-push", async () => {
    const el = drawFeedColdGate(gate(standing()), askHarness().rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect(el.querySelector(".refusal")).toBeNull();
  });

  /** What the daemon answered on 2026-09-13, relayed from the shim verbatim. */
  const REFUSED_START = "shim.v1.StartSession: the producer has already written rows";

  it("draws a transport failure at the buttons when the daemon was NOT reached", async () => {
    const h = askHarness({ fail: true, failCode: Code.Unavailable });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect(el.querySelector(".hibernation-actions .refusal")?.getAttribute("data-arm")).toBe(
      "transport",
    );
  });

  // GROUNDED 2026-09-13: the owner answered this gate with `clear`, the daemon
  // answered `internal` with the shim's own account of the refused start, and
  // the card drew nothing the owner could act on.
  it("draws the daemon's OWN account when the daemon answered a failure", async () => {
    const h = askHarness({ fail: true, failCode: Code.Internal, failMessage: REFUSED_START });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect(el.querySelector(".hibernation-actions .refusal")?.textContent).toContain(
      REFUSED_START,
    );
  });

  it("does not report an unreachable daemon that answered", async () => {
    const h = askHarness({ fail: true, failCode: Code.Internal, failMessage: REFUSED_START });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect(el.querySelector(".hibernation-actions .refusal")?.getAttribute("data-arm")).toBe(
      "failed",
    );
  });

  it("leaves the gate ANSWERABLE after a failed answer", async () => {
    // The whole point of drawing at the click: the user's next move is to try
    // again, and a card latched inert offers no next move at all.
    const h = askHarness({ fail: true });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect([...el.querySelectorAll<Control>("ar-button")].every((b) => !b.disabled)).toBe(true);
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
    {
      arm: "reopenFailed",
      cause: { case: "reopenFailed", value: { detail: "the producer has already written rows" } },
      text: "the session did not come back from the re-open: the producer has already written rows",
    },
  ] as const;

  for (const c of causes) {
    it(`says what ${c.arm} means, at the buttons`, async () => {
      const h = askHarness({ coldGate: refused(c.cause) });
      const el = drawFeedColdGate(gate(standing()), h.rc);
      el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
      await settle();
      const drawn = el.querySelector(".hibernation-actions .refusal");
      expect([drawn?.getAttribute("data-arm"), drawn?.textContent]).toEqual([c.arm, c.text]);
    });
  }

  it("words a re-open failure the daemon could not explain", async () => {
    // A FAILED RE-OPEN WITH NO ACCOUNT still says what happened. The empty
    // detail is legal on the wire, and the sentence must not trail a colon
    // into nothing.
    const h = askHarness({ coldGate: refused({ case: "reopenFailed", value: { detail: "" } }) });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect(el.querySelector(".hibernation-actions .refusal")?.textContent).toBe(
      "the session did not come back from the re-open",
    );
  });

  it("leaves the gate answerable after a failed re-open", async () => {
    // THE GATE IS NOT SPENT: the daemon did not retire it, so the buttons come
    // back and the user can try another remediation.
    const h = askHarness({
      coldGate: refused({ case: "reopenFailed", value: { detail: "the shim refused" } }),
    });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    expect([...el.querySelectorAll<Control>("ar-button")].every((b) => !b.disabled)).toBe(true);
  });

  it("words every cause the schema declares", () => {
    expect(causes.map((c) => c.arm).sort()).toEqual(
      armsOf(AnswerColdGateErrorSchema.oneofs, "cause").sort(),
    );
  });

  it("gives the buttons back so the reader can act on the cause", async () => {
    const h = askHarness({ coldGate: refused({ case: "noSession", value: {} } as never) });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    const pay = el.querySelector<Control>('[data-cold-gate="pay"]');
    pay?.click();
    await settle();
    expect(pay?.disabled).toBe(false);
  });

  it("states an unset cause as unreadable rather than as a cause", async () => {
    const h = askHarness({
      coldGate: create(AnswerColdGateResponseSchema, { result: { case: "error", value: {} } }),
    });
    const el = drawFeedColdGate(gate(standing()), h.rc);
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
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

  it("carries the scope's enum name on the trace, not its number", () => {
    // Arrange / Act
    const el = drawFeedColdGate(
      gate({
        case: "resolved",
        value: {
          atMs: 0n,
          choice: {
            case: "compact",
            value: {
              model: { model: { name: SUMMARIZER } },
              scope: SessionCompactScope.RESPONSES,
            },
          },
        },
      }),
      askHarness().rc,
    );
    // Assert
    expect(
      el.querySelector("[data-compact-scope]")?.getAttribute("data-compact-scope"),
    ).toBe("RESPONSES");
  });

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

  it("keeps ticking the resolution's age after its turn's clocks are stopped", async () => {
    vi.setSystemTime(0);
    const el = drawFeedColdGate(
      gate({ case: "resolved", value: { atMs: 0n, choice: { case: "pay", value: {} } } }),
      askHarness().rc,
    );
    document.body.append(el);
    stopClocks(el);
    await vi.advanceTimersByTimeAsync(5_000);
    expect(el.querySelector(".cold-gate-when")?.textContent).toBe("5s ago");
  });

  it("reads the resolution's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the resolution instant does not share the ticker's phase.
    vi.setSystemTime(4920);
    const el = drawFeedColdGate(
      gate({ case: "resolved", value: { atMs: 0n, choice: { case: "pay", value: {} } } }),
      askHarness().rc,
    );
    // Assert: five real seconds ago reads 5s, not the lagging 4s.
    expect(el.querySelector(".cold-gate-when")?.textContent).toBe("5s ago");
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

/**
 * A row context whose AnswerColdGate hands ANSWER back UNALTERED, and a sink
 * that keeps what was filed.
 *
 * `createRouterTransport` re-encodes the response through the frozen schema,
 * which DROPS an arm no descriptor knows and turns "an arm this build cannot
 * draw" into "a oneof sets no arm" — a different refusal from the one under
 * test. A fake client is how an arm a NEWER daemon set actually reaches the
 * renderer.
 */
function unalteredColdGate(answer: AnswerColdGateResponse): {
  rc: RowContext;
  filed: FailureKind[];
} {
  const filed: FailureKind[] = [];
  return {
    filed,
    rc: {
      ctx: testAppContext({
        client: {
          answerColdGate: () => Promise.resolve(answer),
        } as unknown as AgentReplClient,
        workspace: WORKSPACE,
        ticker: createTicker(1000),
        failures: { report: (kind) => filed.push(kind), retract: () => {} },
        composerEnabled: false,
      }),
      feed: "root",
      row: create(FeedRowSchema, { id: create(FeedIdSchema, { value: ROW_ID }), order: orderFor(ROW_ID) }),
      revealRow: async () => false,
    },
  };
}

/** Catch what a click handler throws, which jsdom reports as a window error. */
function thrownByClick(click: () => void): unknown {
  let caught: unknown;
  const onError = (event: ErrorEvent): void => {
    caught = event.error;
    event.preventDefault();
  };
  window.addEventListener("error", onError);
  try {
    click();
  } finally {
    window.removeEventListener("error", onError);
  }
  return caught;
}

describe("a gate answer this build cannot read", () => {
  it("files the unknown result arm by name rather than drawing a refusal", async () => {
    // Arrange: a newer daemon answers with a result arm this build has no case for.
    const answer = create(AnswerColdGateResponseSchema, {
      result: { case: "success", value: {} },
    });
    (answer as unknown as { result: { case: string; value: unknown } }).result = {
      case: "deferred",
      value: {},
    };
    const h = unalteredColdGate(answer);
    const el = drawFeedColdGate(gate(standing()), h.rc);
    // Act
    el.querySelector<Control>('[data-cold-gate="pay"]')?.click();
    await settle();
    // Assert
    const kind = h.filed[0]?.kind;
    expect(kind?.case === "frameUndecodable" ? [kind.value.frameHead, kind.value.cause] : null,
    ).toEqual([
      "AnswerColdGateResponse.result",
      "arm 'deferred' is not one this build can draw",
    ]);
  });
});

describe("a compact menu that cannot be answered", () => {
  it("refuses to send when the menu offered no summarizer", () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing({ models: [] })), askHarness().rc);
    openSubmenu(el);
    // Act
    const err = thrownByClick(() =>
      el.querySelector<Control>('[data-cold-gate="compact"]')?.click(),
    );
    // Assert
    expect([(err as MalformedView).name, (err as MalformedView).detail]).toEqual([
      "MalformedView",
      "the compact menu offered no model or no scope",
    ]);
  });

  it("refuses to send when the menu offered no scope", () => {
    // Arrange
    const el = drawFeedColdGate(gate(standing({ scopes: [] })), askHarness().rc);
    openSubmenu(el);
    // Act
    const err = thrownByClick(() =>
      el.querySelector<Control>('[data-cold-gate="compact"]')?.click(),
    );
    // Assert
    expect([(err as MalformedView).name, (err as MalformedView).detail]).toEqual([
      "MalformedView",
      "the compact menu offered no model or no scope",
    ]);
  });
});

describe("a resolved trace this build cannot read", () => {
  it("refuses a choice arm this build does not know, by name", () => {
    // Arrange
    const u = gate({ case: "resolved", value: { atMs: 0n, choice: { case: "pay", value: {} } } });
    (
      u.state as unknown as { value: { choice: { case: string; value: unknown } } }
    ).value.choice = { case: "rehydrate", value: {} };
    // Act / Assert
    expect(() => drawFeedColdGate(u, askHarness().rc)).toThrow(
      "malformed view at FeedColdGate.resolved.choice: arm 'rehydrate' is not one this build can draw",
    );
  });
});
