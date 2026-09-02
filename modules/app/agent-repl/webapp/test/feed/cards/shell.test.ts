// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  InterruptErrorSchema,
  InterruptResponseSchema,
  type InterruptResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import {
  FeedRowSchema,
  FeedShellSchema,
  type FeedShell,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  drawFeedShell,
  PROMPT_CHROME,
  SHELL_SETTLED_ARMS,
  STOP_OUTCOME_MS,
} from "../../../src/feed/cards/shell.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { armsOf } from "../arms.js";
import { feedId, harness, rowContext, WORKSPACE } from "../harness.js";

const ROW = "shell-1";
const COMMAND = "npm run build -- --watch";

/** A row context on a detached-shell row, with the Interrupt verb scripted. */
function ctxFor(answer?: InterruptResponse) {
  const h = harness({ interrupt: () => answer ?? interruptedDetached(1n) });
  const row = create(FeedRowSchema, {
    id: feedId(ROW),
    row: { case: "detachedShell", value: { shell: {} } },
  });
  return { h, rc: rowContext(h.ctx, row) as RowContext };
}

/** The ordinary success: N detached targets stopped. */
function interruptedDetached(count: bigint): InterruptResponse {
  return create(InterruptResponseSchema, {
    result: {
      case: "success",
      value: { outcome: { case: "interruptedDetached", value: { count } } },
    },
  });
}

/** A shell bubble, live or settled, with every optional part switchable. */
function shell(
  opts: {
    startedAtMs?: bigint;
    spool?: { text: string; omitted?: string };
    lastProgressMs?: bigint;
    settled?: {
      endedAtMs: bigint;
      outcome: "completed" | "cancelled" | "lost";
      exit?: number;
    };
  } = {},
): FeedShell {
  return create(FeedShellSchema, {
    command: { text: COMMAND },
    runtime: { startedAtMs: opts.startedAtMs ?? 0n },
    spool:
      opts.spool === undefined
        ? undefined
        : {
            text: opts.spool.text,
            omitted: opts.spool.omitted === undefined ? undefined : { text: opts.spool.omitted },
          },
    state:
      opts.settled === undefined
        ? {
            case: "live",
            value: {
              lastProgress:
                opts.lastProgressMs === undefined ? undefined : { atMs: opts.lastProgressMs },
            },
          }
        : {
            case: "settled",
            value: {
              endedAtMs: opts.settled.endedAtMs,
              exit: opts.settled.exit === undefined ? undefined : { code: opts.settled.exit },
              outcome: { case: opts.settled.outcome, value: {} },
            },
          },
  });
}

/** Let a scripted answer settle; the router hands it back on a timer. */
async function settle(): Promise<void> {
  for (let i = 0; i < 30; i += 1) await vi.advanceTimersByTimeAsync(0);
}

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(0);
});

describe("drawFeedShell head", () => {
  it("draws the client's own $ chrome", () => {
    const el = drawFeedShell(shell(), ctxFor().rc);
    expect(el.querySelector(".shell-prompt")?.textContent).toBe(PROMPT_CHROME);
  });

  it("draws the command verbatim", () => {
    const el = drawFeedShell(shell(), ctxFor().rc);
    expect(el.querySelector(".shell-command-text")?.textContent).toBe(COMMAND);
  });

  it("carries live as the bubble's state while the command runs", () => {
    expect(drawFeedShell(shell(), ctxFor().rc).getAttribute("data-state")).toBe("live");
  });

  it("breathes the dot while the command runs", () => {
    const el = drawFeedShell(shell(), ctxFor().rc);
    expect(el.querySelector(".agent-dot")?.classList.contains("agent-running")).toBe(true);
  });
});

describe("drawFeedShell clocks", () => {
  it("counts up from the original start while live", () => {
    vi.setSystemTime(45_000);
    const el = drawFeedShell(shell({ startedAtMs: 0n }), ctxFor().rc);
    expect(el.querySelector(".shell-clock")?.textContent).toBe("45s");
  });

  it("ticks the live clock forward", async () => {
    const el = drawFeedShell(shell({ startedAtMs: 0n }), ctxFor().rc);
    document.body.append(el);
    await vi.advanceTimersByTimeAsync(3000);
    expect(el.querySelector(".shell-clock")?.textContent).toBe("3s");
  });

  it("stops the clock at the settled instant", () => {
    vi.setSystemTime(999_999);
    const el = drawFeedShell(
      shell({ startedAtMs: 0n, settled: { endedAtMs: 12_000n, outcome: "completed" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-clock")?.textContent).toBe("12s");
  });

  it("draws the quiet-for reading from the last observed append", () => {
    vi.setSystemTime(20_000);
    const el = drawFeedShell(shell({ lastProgressMs: 8000n }), ctxFor().rc);
    expect(el.querySelector(".shell-quiet")?.textContent).toBe("quiet for 12s");
  });

  it("draws no quiet-for reading before the first byte", () => {
    const el = drawFeedShell(shell(), ctxFor().rc);
    expect(el.querySelector(".shell-quiet")).toBeNull();
  });
});

describe("drawFeedShell spool", () => {
  it("draws the tail verbatim", () => {
    const el = drawFeedShell(shell({ spool: { text: "line one\nline two" } }), ctxFor().rc);
    expect(el.querySelector(".shell-tail")?.textContent).toBe("line one\nline two");
  });

  it("wears the shared capped output box, so the tail scrolls rather than clips", () => {
    const el = drawFeedShell(shell({ spool: { text: "x" } }), ctxFor().rc);
    expect(el.querySelector(".shell-tail")?.className).toBe(
      "tool-output bash-output shell-tail",
    );
  });

  it("follows the tail, so a redraw shows the newest output", () => {
    const el = drawFeedShell(shell({ spool: { text: "x" } }), ctxFor().rc);
    const box = el.querySelector<HTMLElement>(".shell-tail");
    expect(box?.scrollTop).toBe(box?.scrollHeight);
  });

  it("draws the omitted line above the box when the daemon capped", () => {
    const el = drawFeedShell(
      shell({ spool: { text: "x", omitted: "1,204 earlier lines not shown" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-omitted")?.textContent).toBe(
      "1,204 earlier lines not shown",
    );
  });

  it("draws the omitted line OUTSIDE the box, so the count stays put", () => {
    const el = drawFeedShell(
      shell({ spool: { text: "x", omitted: "1 earlier line not shown" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-tail .shell-omitted")).toBeNull();
  });

  it("draws no omitted line when nothing was capped", () => {
    const el = drawFeedShell(shell({ spool: { text: "x" } }), ctxFor().rc);
    expect(el.querySelector(".shell-omitted")).toBeNull();
  });

  it("draws no spool box while the command has produced nothing", () => {
    expect(drawFeedShell(shell(), ctxFor().rc).querySelector(".shell-spool")).toBeNull();
  });
});

describe("drawFeedShell settled", () => {
  const outcomes = [
    { arm: "completed", word: "completed", dot: "agent-done" },
    { arm: "cancelled", word: "stopped", dot: "agent-done" },
    { arm: "lost", word: "lost sight of", dot: "agent-lost" },
  ] as const;

  for (const c of outcomes) {
    it(`says "${c.word}" for the ${c.arm} arm`, () => {
      const el = drawFeedShell(
        shell({ settled: { endedAtMs: 1n, outcome: c.arm } }),
        ctxFor().rc,
      );
      expect(el.querySelector(".shell-outcome")?.textContent).toBe(c.word);
    });

    it(`carries ${c.arm} as the bubble's state`, () => {
      const el = drawFeedShell(
        shell({ settled: { endedAtMs: 1n, outcome: c.arm } }),
        ctxFor().rc,
      );
      expect(el.getAttribute("data-state")).toBe(c.arm);
    });

    it(`dots the ${c.arm} arm as ${c.dot}`, () => {
      const el = drawFeedShell(
        shell({ settled: { endedAtMs: 1n, outcome: c.arm } }),
        ctxFor().rc,
      );
      expect(el.querySelector(".agent-dot")?.classList.contains(c.dot)).toBe(true);
    });
  }

  it("never draws a lost shell in the error register", () => {
    const el = drawFeedShell(shell({ settled: { endedAtMs: 1n, outcome: "lost" } }), ctxFor().rc);
    expect(el.querySelector(".shell-outcome")?.classList.contains("shell-outcome-lost")).toBe(true);
  });

  it("draws every settled outcome the schema carries", () => {
    expect([...SHELL_SETTLED_ARMS].sort()).toEqual(["completed", "cancelled", "lost"].sort());
  });

  it("draws the exit chip when the terminator carried a code", () => {
    const el = drawFeedShell(
      shell({ settled: { endedAtMs: 1n, outcome: "completed", exit: 1 } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-exit")?.textContent).toBe("exit 1");
  });

  it("draws a zero exit in the success tone", () => {
    const el = drawFeedShell(
      shell({ settled: { endedAtMs: 1n, outcome: "completed", exit: 0 } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-exit")?.className).toBe("badge ok shell-exit");
  });

  it("draws a non-zero exit in the error tone", () => {
    const el = drawFeedShell(
      shell({ settled: { endedAtMs: 1n, outcome: "completed", exit: 2 } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-exit")?.className).toBe("badge err shell-exit");
  });

  it("draws no exit chip when no code was carried, never a zero", () => {
    const el = drawFeedShell(
      shell({ settled: { endedAtMs: 1n, outcome: "lost" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-exit")).toBeNull();
  });

  it("offers no stop control on a settled shell", () => {
    const el = drawFeedShell(
      shell({ settled: { endedAtMs: 1n, outcome: "completed" } }),
      ctxFor().rc,
    );
    expect(el.querySelector("[data-interrupt]")).toBeNull();
  });
});

describe("the stop control", () => {
  it("names this row as its interrupt target", () => {
    const el = drawFeedShell(shell(), ctxFor().rc);
    expect(el.querySelector("[data-interrupt]")?.getAttribute("data-interrupt")).toBe(ROW);
  });

  it("interrupts the detached target by this row's own id", async () => {
    const { h, rc } = ctxFor();
    const el = drawFeedShell(shell(), rc);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    expect(h.calls.interrupt.map((r) => r.target.value)).toEqual([feedId(ROW)]);
  });

  it("echoes the workspace on the interrupt", async () => {
    const { h, rc } = ctxFor();
    const el = drawFeedShell(shell(), rc);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    expect(h.calls.interrupt[0]?.workspace).toEqual(WORKSPACE);
  });

  it("draws the success outcome at the control", async () => {
    const { rc } = ctxFor(interruptedDetached(1n));
    const el = drawFeedShell(shell(), rc);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".shell-stop-outcome")?.textContent).toBe("stopped 1");
  });

  it("draws nothing-running as the answer it is, not a failure", async () => {
    const { rc } = ctxFor(
      create(InterruptResponseSchema, {
        result: { case: "success", value: { outcome: { case: "nothingRunning", value: {} } } },
      }),
    );
    const el = drawFeedShell(shell(), rc);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")).toBeNull();
  });

  it("clears the outcome once it has been readable long enough", async () => {
    const { rc } = ctxFor(interruptedDetached(1n));
    const el = drawFeedShell(shell(), rc);
    document.body.append(el);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    await vi.advanceTimersByTimeAsync(STOP_OUTCOME_MS + 1000);
    expect(el.querySelector(".shell-stop-outcome")).toBeNull();
  });

  it("latches the button inert while the stop is in flight", () => {
    const { rc } = ctxFor();
    const el = drawFeedShell(shell(), rc);
    const button = el.querySelector<HTMLButtonElement>("[data-interrupt]");
    button?.click();
    expect(button?.disabled).toBe(true);
  });

  it("draws no confirm step — that challenge is the turn target's alone", async () => {
    const { rc } = ctxFor(
      create(InterruptResponseSchema, {
        result: {
          case: "error",
          value: { kind: { case: "confirmRequired", value: { liveAgentCount: 3n } } },
        },
      }),
    );
    const el = drawFeedShell(shell(), rc);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector("[data-interrupt-confirm]")).toBeNull();
  });

  it("gives the button back after a refusal, so the user can try again", async () => {
    const { rc } = ctxFor(
      create(InterruptResponseSchema, {
        result: {
          case: "error",
          value: { kind: { case: "confirmRequired", value: { liveAgentCount: 3n } } },
        },
      }),
    );
    const el = drawFeedShell(shell(), rc);
    const button = el.querySelector<HTMLButtonElement>("[data-interrupt]");
    button?.click();
    await settle();
    expect(button?.disabled).toBe(false);
  });

  it("states an unset cause as unreadable rather than as a cause", async () => {
    const { rc } = ctxFor(
      create(InterruptResponseSchema, { result: { case: "error", value: {} } }),
    );
    const el = drawFeedShell(shell(), rc);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    // An error with no cause set is a frame this build cannot read, not a
    // refusal with no words: it is reported through the failure sink, and no
    // sentence is invented at the control (src/rpc/refuse.ts).
    expect(el.querySelector(".shell-stop .refusal")).toBeNull();
  });
});

describe("the stop's typed refusals", () => {
  const causes = [
    {
      arm: "confirmRequired",
      kind: { case: "confirmRequired", value: { liveAgentCount: 3n } },
      text: "stopping would also end 3 live agent(s)",
    },
    { arm: "unknownWorkspace", kind: { case: "unknownWorkspace", value: {} }, text: "the daemon does not know this workspace" },
    {
      arm: "workspaceRefMismatch",
      kind: { case: "workspaceRefMismatch", value: { registryDir: "/elsewhere" } },
      text: "this workspace's directory disagrees with the registry's: /elsewhere",
    },
    {
      arm: "transferringAway",
      kind: { case: "transferringAway", value: { address: "127.0.0.1:9931" } },
      text: "this workspace moved to another daemon at 127.0.0.1:9931",
    },
    {
      arm: "notYetAdopted",
      kind: { case: "notYetAdopted", value: {} },
      text: "the daemon has not finished adopting this workspace yet",
    },
    {
      arm: "notDetachedWork",
      kind: { case: "notDetachedWork", value: {} },
      text: "this row names no detached work",
    },
    {
      arm: "noSession",
      kind: { case: "noSession", value: {} },
      text: "the workspace has no session to interrupt",
    },
    {
      arm: "shimRefused",
      kind: { case: "shimRefused", value: { detail: "the shim says nothing is running" } },
      text: "the shim says nothing is running",
    },
  ] as const;

  for (const c of causes) {
    it(`says what ${c.arm} means, beside the control`, async () => {
      const { rc } = ctxFor(
        create(InterruptResponseSchema, {
          result: { case: "error", value: { kind: c.kind as never } },
        }),
      );
      const el = drawFeedShell(shell(), rc);
      el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
      await settle();
      const drawn = el.querySelector(".shell-stop .refusal");
      expect([drawn?.getAttribute("data-arm"), drawn?.textContent]).toEqual([c.arm, c.text]);
    });
  }

  it("words every cause the schema declares", () => {
    expect(causes.map((c) => c.arm).sort()).toEqual(
      armsOf(InterruptErrorSchema.oneofs, "kind").sort(),
    );
  });
});

describe("drawFeedShell malformed input", () => {
  it("refuses a bubble whose state oneof is unset", () => {
    const u = create(FeedShellSchema, { command: { text: COMMAND }, runtime: { startedAtMs: 0n } });
    expect(() => drawFeedShell(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a bubble with no command", () => {
    const u = shell();
    (u as unknown as { command: undefined }).command = undefined;
    expect(() => drawFeedShell(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a bubble with no runtime", () => {
    const u = shell();
    (u as unknown as { runtime: undefined }).runtime = undefined;
    expect(() => drawFeedShell(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a settled bubble whose outcome oneof is unset", () => {
    const u = shell({ settled: { endedAtMs: 1n, outcome: "completed" } });
    (u.state as unknown as { value: { outcome: { case: undefined } } }).value.outcome = {
      case: undefined,
    };
    expect(() => drawFeedShell(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a state arm this build does not know", () => {
    const u = shell();
    (u as unknown as { state: { case: string; value: unknown } }).state = {
      case: "queued",
      value: {},
    };
    expect(() => drawFeedShell(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a settled outcome arm this build does not know", () => {
    const u = shell({ settled: { endedAtMs: 1n, outcome: "completed" } });
    (u.state as unknown as { value: { outcome: { case: string; value: unknown } } }).value.outcome =
      { case: "evicted", value: {} };
    expect(() => drawFeedShell(u, ctxFor().rc)).toThrow(MalformedView);
  });
});
