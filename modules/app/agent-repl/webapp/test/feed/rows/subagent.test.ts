// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedSubagentSchema,
  FeedSubagentSettledSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { InterruptResponseSchema } from "../../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { INTERRUPT_ERROR_ARMS } from "../../../src/interrupt-error.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  SUBAGENT_SETTLED_ARMS,
  drawFeedSubagent,
} from "../../../src/feed/rows/subagent.js";
import { harness, rowContext, subagentRow, type Harness } from "../harness.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

/** Draw the head of a row built by the fixture. */
function drawRow(
  row: ReturnType<typeof subagentRow>,
  h: Harness = harness(),
): { el: HTMLElement; h: Harness } {
  const subagent =
    row.row.case === "detachedSubagent"
      ? row.row.value.subagent
      : row.row.case === "activity" && row.row.value.unit.case === "subagent"
        ? row.row.value.unit.value
        : undefined;
  if (subagent === undefined) throw new Error("fixture is not a subagent row");
  return { el: drawFeedSubagent(subagent, rowContext(h.ctx, row)), h };
}

/** Let the scripted answer land. */
async function settle(): Promise<void> {
  for (let i = 0; i < 30; i += 1) await vi.advanceTimersByTimeAsync(0);
}

describe("drawFeedSubagent: the head's parts", () => {
  it("draws the label the daemon resolved", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector(".subagent-label")?.textContent).toBe("Explore");
  });

  it("draws the description when the spawn carried one", () => {
    const { el } = drawRow(subagentRow("b1", { description: "sweep the repo" }));
    expect(el.querySelector(".subagent-description")?.textContent).toBe("sweep the repo");
  });

  it("draws NO description when the spawn carried none, never a synthesized one", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector(".subagent-description")).toBeNull();
  });

  it("draws the daemon-formatted token sum verbatim", () => {
    const { el } = drawRow(subagentRow("b1", { tokens: "12.4k tok" }));
    expect(el.querySelector(".subagent-tokens")?.textContent).toBe("12.4k tok");
  });

  it("draws no token figure before any usage was observed", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector(".subagent-tokens")).toBeNull();
  });

  it("refuses a head with no label", () => {
    const { ctx } = harness();
    const row = subagentRow("b1");
    const bad = create(FeedSubagentSchema, {
      runtime: { startedAtMs: 0n },
      state: { case: "live", value: {} },
    });
    expect(() => drawFeedSubagent(bad, rowContext(ctx, row))).toThrow(MalformedView);
  });

  it("refuses a head with no runtime, there being no clock to draw", () => {
    const { ctx } = harness();
    const bad = create(FeedSubagentSchema, {
      label: { text: "Explore" },
      state: { case: "live", value: {} },
    });
    expect(() => drawFeedSubagent(bad, rowContext(ctx, subagentRow("b1")))).toThrow(MalformedView);
  });

  it("refuses a head whose state arm is unset", () => {
    const { ctx } = harness();
    const bad = create(FeedSubagentSchema, {
      label: { text: "Explore" },
      runtime: { startedAtMs: 0n },
    });
    expect(() => drawFeedSubagent(bad, rowContext(ctx, subagentRow("b1")))).toThrow(MalformedView);
  });
});

describe("drawFeedSubagent: the clocks", () => {
  it("counts up from the ORIGINAL start while live", () => {
    const { el } = drawRow(subagentRow("b1", { startedAtMs: 940_000n }));
    expect(el.querySelector(".subagent-clock")?.textContent).toBe("1m");
  });

  it("ticks the live clock on the shared clock", () => {
    const { el } = drawRow(subagentRow("b1", { startedAtMs: 1_000_000n }));
    document.body.append(el);
    vi.advanceTimersByTime(5_000);
    expect(el.querySelector(".subagent-clock")?.textContent).toBe("5s");
  });

  it("reads the live clock's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the start does not share the shared ticker's phase.
    const { el } = drawRow(subagentRow("b1", { startedAtMs: 1_000_000n - 4920n }));
    // Assert: five real seconds of running reads 5s, not the lagging 4s.
    expect(el.querySelector(".subagent-clock")?.textContent).toBe("5s");
  });

  it("stops the clock at the settled instant", () => {
    const { el } = drawRow(
      subagentRow("b1", {
        startedAtMs: 1_000_000n,
        settled: { endedAtMs: 1_030_000n, outcome: "succeeded" },
      }),
    );
    document.body.append(el);
    vi.advanceTimersByTime(60_000);
    expect(el.querySelector(".subagent-clock")?.textContent).toBe("30s");
  });

  it("ticks 'quiet for' from the last beat the daemon observed", () => {
    const { el } = drawRow(subagentRow("b1", { lastProgressMs: 990_000n }));
    expect(el.querySelector(".subagent-quiet")?.textContent).toBe("quiet for 10s");
  });

  it("draws no quietness figure before the first beat", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector(".subagent-quiet")).toBeNull();
  });
});

describe("drawFeedSubagent: the settled arms", () => {
  it("words every settled arm the schema declares", () => {
    const schemaArms = FeedSubagentSettledSchema.oneofs
      .filter((oneof) => oneof.name === "outcome")
      .flatMap((oneof) => oneof.fields.map((field) => field.localName));
    expect([...SUBAGENT_SETTLED_ARMS].sort()).toEqual([...schemaArms].sort());
  });

  it.each(["succeeded", "failed", "cancelled", "lost"] as const)(
    "says %s as its own state",
    (outcome) => {
      const { el } = drawRow(
        subagentRow("b1", { settled: { endedAtMs: 1_000_000n, outcome } }),
      );
      expect(el.getAttribute("data-state")).toBe(outcome);
    },
  );

  it("never draws `lost` as a failure", () => {
    const lost = drawRow(subagentRow("b1", { settled: { endedAtMs: 1n, outcome: "lost" } })).el;
    const failed = drawRow(subagentRow("b2", { settled: { endedAtMs: 1n, outcome: "failed" } })).el;
    expect(lost.querySelector(".subagent-outcome")?.textContent).not.toBe(
      failed.querySelector(".subagent-outcome")?.textContent,
    );
  });

  it("gives `lost` its own dot, not the error hue", () => {
    const { el } = drawRow(subagentRow("b1", { settled: { endedAtMs: 1n, outcome: "lost" } }));
    expect(el.querySelector(".agent-dot")?.classList.contains("agent-lost")).toBe(true);
  });

  it("refuses a settled head whose outcome arm is unset", () => {
    const { ctx } = harness();
    const bad = create(FeedSubagentSchema, {
      label: { text: "Explore" },
      runtime: { startedAtMs: 0n },
      state: { case: "settled", value: { endedAtMs: 1n } },
    });
    expect(() => drawFeedSubagent(bad, rowContext(ctx, subagentRow("b1")))).toThrow(MalformedView);
  });
});

describe("drawFeedSubagent: the stop control", () => {
  it("offers a stop on a LIVE DETACHED bubble, which outlives its turn", () => {
    const { el } = drawRow(subagentRow("b1", { detached: true }));
    expect(el.querySelector("[data-interrupt]")).not.toBeNull();
  });

  it("offers none on a synchronous spawn, whose stop is the turn's", () => {
    const { el } = drawRow(subagentRow("b1"));
    expect(el.querySelector("[data-interrupt]")).toBeNull();
  });

  it("offers none on a settled detached bubble", () => {
    const { el } = drawRow(
      subagentRow("b1", { detached: true, settled: { endedAtMs: 1n, outcome: "succeeded" } }),
    );
    expect(el.querySelector("[data-interrupt]")).toBeNull();
  });

  it("interrupts by the bubble row's OWN id, echoed verbatim", async () => {
    const { el, h } = drawRow(subagentRow("b1", { detached: true }));
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(h.calls.interrupt[0]?.target).toEqual({
      case: "detached",
      value: { $typeName: "frontend.v1.FeedId", value: "b1" },
    });
  });

  it("draws the interrupted_detached answer at the control", async () => {
    const { el } = drawRow(subagentRow("b1", { detached: true }));
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".subagent-stop-outcome")?.getAttribute("data-outcome")).toBe(
      "interruptedDetached",
    );
  });

  it("draws nothing_running as an ANSWER, not as a failure", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "success",
            value: { outcome: { case: "nothingRunning", value: {} } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")).toBeNull();
  });

  it("clears the answer once it has been on screen long enough to read", async () => {
    const { el } = drawRow(subagentRow("b1", { detached: true }));
    document.body.append(el);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    await vi.advanceTimersByTimeAsync(5_000);
    expect(el.querySelector(".subagent-stop-outcome")).toBeNull();
  });

  it("draws the confirm_required refusal at the control that made the call", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "confirmRequired", value: { liveAgentCount: 3n } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe("confirmRequired");
  });

  it("names the count the challenge carried", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "confirmRequired", value: { liveAgentCount: 3n } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.textContent).toContain("3");
  });

  it.each(INTERRUPT_ERROR_ARMS)("draws the %s refusal by its own arm", async (arm) => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: { case: "error", value: { kind: { case: arm as never, value: {} as never } } },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe(arm);
  });

  it("names the registry's directory when the workspace ref disagrees with it", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "workspaceRefMismatch", value: { registryDir: "/w/other" } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.textContent).toContain("/w/other");
  });

  it("offers no confirm step, the challenge not applying to a detached target", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "confirmRequired", value: { liveAgentCount: 1n } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector("[data-interrupt-confirm]")).toBeNull();
  });

  it("does not resend the stop after the challenge", async () => {
    const h = harness({
      interrupt: () =>
        create(InterruptResponseSchema, {
          result: {
            case: "error",
            value: { kind: { case: "confirmRequired", value: { liveAgentCount: 1n } } },
          },
        }),
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(h.calls.interrupt).toHaveLength(1);
  });

  it("tells the reader when the click never reached the daemon", async () => {
    const h = harness({
      interrupt: () => {
        throw new Error("gone");
      },
    });
    const { el } = drawRow(subagentRow("b1", { detached: true }), h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });
});
