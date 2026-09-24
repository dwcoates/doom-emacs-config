// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  InterruptErrorSchema,
  InterruptResponseSchema,
  type InterruptResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import {
  FeedRowSchema,
  FeedShellLostSchema,
  FeedShellSchema,
  type FeedShell,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { createTicker } from "../../../src/clock.js";
import { testAppContext } from "../../rpc/app-context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  drawFeedShellBody,
  drawFeedShellHead,
  PROMPT_CHROME,
  SHELL_LOST_CAUSE_ARMS,
  SHELL_SETTLED_ARMS,
  STOP_OUTCOME_MS,
} from "../../../src/feed/cards/shell.js";
import { TICKING_ATTRIBUTE } from "../../../src/feed/ticking.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { armsOf } from "../arms.js";
import {
  HAS_MORE_CLASS,
  TITLE_FOLD_CLASS,
  TITLE_FOLD_STANDALONE_CLASS,
} from "../../../src/feed/bubble-more.js";
import { fireResize } from "../../resize-observer.js";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";
import { inBubbleFold, measureTitle } from "../title-measure.js";
import { countingTicker, feedId, harness, rowContext, WORKSPACE } from "../harness.js";

const ROW = "shell-1";
const COMMAND = "npm run build -- --watch";

/** A row context on a detached-shell row, with the Interrupt verb scripted. */
function ctxFor(answer?: InterruptResponse) {
  const h = harness({ interrupt: () => answer ?? interruptedDetached(1n) });
  const row = create(FeedRowSchema, {
    id: feedId(ROW),
    row: { case: "detachedShell", value: { shell: {} } },
  });
  return { h, rc: rowContext(h.ctx, row) };
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
      lostHow?: "fileVanished" | "wentSilent" | "sweptUp";
      exit?: number;
    };
    workId?: string;
  } = {},
): FeedShell {
  return create(FeedShellSchema, {
    command: { text: COMMAND },
    workId: opts.workId === undefined ? undefined : { text: opts.workId },
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
              outcome:
                opts.settled.outcome === "lost" && opts.settled.lostHow !== undefined
                  ? { case: "lost", value: { how: { case: opts.settled.lostHow, value: {} } } }
                  : { case: opts.settled.outcome, value: {} },
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

// The fake clock is this file's own; hand the real one back so a
// later file sharing this worker never inherits a frozen timer.
afterEach(() => {
  vi.useRealTimers();
});

describe("drawFeedShellHead head", () => {
  it("draws the client's own $ chrome", () => {
    const el = drawFeedShellHead(shell(), ctxFor().rc);
    expect(el.querySelector(".shell-prompt")?.textContent).toBe(PROMPT_CHROME);
  });

  it("draws the command verbatim", () => {
    const el = drawFeedShellHead(shell(), ctxFor().rc);
    expect(el.querySelector(".shell-command-text")?.textContent).toBe(COMMAND);
  });

  it("carries live as the bubble's state while the command runs", () => {
    expect(drawFeedShellHead(shell(), ctxFor().rc).getAttribute("data-state")).toBe("live");
  });

  it("breathes the dot while the command runs", () => {
    const el = drawFeedShellHead(shell(), ctxFor().rc);
    expect(el.querySelector(".agent-dot")?.classList.contains("agent-running")).toBe(true);
  });

  it("draws no spool on the head — the spool is the body", () => {
    const el = drawFeedShellHead(shell({ spool: { text: "line one\n" } }), ctxFor().rc);
    expect(el.querySelector(".shell-spool")).toBeNull();
  });

  // A SHELL REPORTS NO TOKENS (owner ruling, 2026-09-14): the token count on a
  // bubble head is unique to subagent (agent) heads, and a shell head is never
  // to invent one -- command, clock and stop are all it carries. `FeedShell`
  // has no tokens field, so the head cannot draw the subagent's token span.
  it("carries no token count, a shell reporting none", () => {
    const el = drawFeedShellHead(shell(), ctxFor().rc);
    expect(el.querySelector(".subagent-tokens")).toBeNull();
  });
});

describe("drawFeedShellBody is spool-only", () => {
  it("draws neither the command nor the clock — they live on the head", () => {
    const el = drawFeedShellBody(shell({ spool: { text: "line one\n" } }), ctxFor().rc);
    expect([el.querySelector(".shell-command"), el.querySelector(".shell-clock")]).toEqual([
      null,
      null,
    ]);
  });

  it("offers no stop control — the stop lives on the head", () => {
    const el = drawFeedShellBody(shell({ spool: { text: "line one\n" } }), ctxFor().rc);
    expect(el.querySelector("[data-interrupt]")).toBeNull();
  });
});

describe("drawFeedShellHead clocks", () => {
  it("counts up from the original start while live", () => {
    vi.setSystemTime(45_000);
    const el = drawFeedShellHead(shell({ startedAtMs: 0n }), ctxFor().rc);
    expect(el.querySelector(".shell-clock")?.textContent).toBe("45s");
  });

  it("ticks the live clock forward", async () => {
    const el = drawFeedShellHead(shell({ startedAtMs: 0n }), ctxFor().rc);
    document.body.append(el);
    await vi.advanceTimersByTimeAsync(3000);
    expect(el.querySelector(".shell-clock")?.textContent).toBe("3s");
  });

  it("reads the live clock's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the start does not share the shared ticker's phase.
    vi.setSystemTime(4920);
    const el = drawFeedShellHead(shell({ startedAtMs: 0n }), ctxFor().rc);
    // Assert: five real seconds of running reads 5s, not the lagging 4s.
    expect(el.querySelector(".shell-clock")?.textContent).toBe("5s");
  });

  it("stops the clock at the settled instant", () => {
    vi.setSystemTime(999_999);
    const el = drawFeedShellHead(
      shell({ startedAtMs: 0n, settled: { endedAtMs: 12_000n, outcome: "completed" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-clock")?.textContent).toBe("12s");
  });

  it("holds no live subscription once the command has settled", () => {
    // Arrange: a ticker whose live subscriptions the test can count.
    const ticker = countingTicker();
    const h = harness({ ticker });
    const row = create(FeedRowSchema, {
      id: feedId(ROW),
      row: { case: "detachedShell", value: { shell: {} } },
    });
    // Act: the terminal frame.
    const el = drawFeedShellHead(
      shell({ startedAtMs: 0n, settled: { endedAtMs: 12_000n, outcome: "completed" } }),
      rowContext(h.ctx, row),
    );
    document.body.append(el);
    // Assert: a settled card subscribes to nothing.
    expect(ticker.live()).toBe(0);
    el.remove();
  });

  it("freezes the settled figure at the message's own duration, not the wall clock", () => {
    vi.setSystemTime(999_999);
    const el = drawFeedShellHead(
      shell({ startedAtMs: 1000n, settled: { endedAtMs: 8000n, outcome: "completed" } }),
      ctxFor().rc,
    );
    document.body.append(el);
    vi.advanceTimersByTime(60_000);
    expect(el.querySelector(".shell-clock")?.textContent).toBe("7s");
    el.remove();
  });

  it("clears the marker a live draw left, so the settled element reads as stopped", () => {
    const el = drawFeedShellHead(
      shell({ startedAtMs: 0n, settled: { endedAtMs: 12_000n, outcome: "completed" } }),
      ctxFor().rc,
    );
    expect(el.querySelectorAll(`[${TICKING_ATTRIBUTE}]`)).toHaveLength(0);
  });

  it("draws the quiet-for reading from the last observed append", () => {
    vi.setSystemTime(20_000);
    const el = drawFeedShellHead(shell({ lastProgressMs: 8000n }), ctxFor().rc);
    expect(el.querySelector(".shell-quiet")?.textContent).toBe("quiet for 12s");
  });

  it("reads the quiet-for's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the last append does not share the shared ticker's phase.
    vi.setSystemTime(12_920);
    const el = drawFeedShellHead(shell({ lastProgressMs: 8000n }), ctxFor().rc);
    // Assert: five real seconds of silence reads 5s, not the lagging 4s.
    expect(el.querySelector(".shell-quiet")?.textContent).toBe("quiet for 5s");
  });

  it("draws no quiet-for reading before the first byte", () => {
    const el = drawFeedShellHead(shell(), ctxFor().rc);
    expect(el.querySelector(".shell-quiet")).toBeNull();
  });
});

describe("drawFeedShellBody spool", () => {
  it("draws the tail verbatim", () => {
    const el = drawFeedShellBody(shell({ spool: { text: "line one\nline two" } }), ctxFor().rc);
    expect(el.querySelector(".shell-tail")?.textContent).toBe("line one\nline two");
  });

  it("wears the shared capped output box, so the tail scrolls rather than clips", () => {
    const el = drawFeedShellBody(shell({ spool: { text: "x" } }), ctxFor().rc);
    expect(el.querySelector(".shell-tail")?.className).toBe(
      "tool-output bash-output shell-tail",
    );
  });

  it("never scrolls its own tail box (removed trigger: the user owns the scroll)", () => {
    // Arrange -- a box with 500px of content, which the old self-follow would
    // have scrolled to its end on every draw.
    const height = vi.spyOn(HTMLElement.prototype, "scrollHeight", "get").mockReturnValue(500);
    try {
      // Act
      const el = drawFeedShellBody(shell({ spool: { text: "x" } }), ctxFor().rc);
      // Assert
      expect(el.querySelector<HTMLElement>(".shell-tail")?.scrollTop).toBe(0);
    } finally {
      height.mockRestore();
    }
  });

  it("draws the omitted line above the box when the daemon capped", () => {
    const el = drawFeedShellBody(
      shell({ spool: { text: "x", omitted: "1,204 earlier lines not shown" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-omitted")?.textContent).toBe(
      "1,204 earlier lines not shown",
    );
  });

  it("draws the omitted line OUTSIDE the box, so the count stays put", () => {
    const el = drawFeedShellBody(
      shell({ spool: { text: "x", omitted: "1 earlier line not shown" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-tail .shell-omitted")).toBeNull();
  });

  it("draws no omitted line when nothing was capped", () => {
    const el = drawFeedShellBody(shell({ spool: { text: "x" } }), ctxFor().rc);
    expect(el.querySelector(".shell-omitted")).toBeNull();
  });

  it("draws no spool box while the command has produced nothing", () => {
    expect(drawFeedShellBody(shell(), ctxFor().rc).querySelector(".shell-spool")).toBeNull();
  });
});

describe("drawFeedShellHead settled", () => {
  const outcomes = [
    { arm: "completed", word: "completed", dot: "agent-done" },
    { arm: "cancelled", word: "stopped", dot: "agent-done" },
    { arm: "lost", word: "lost sight of", dot: "agent-lost" },
  ] as const;

  for (const c of outcomes) {
    it(`says "${c.word}" for the ${c.arm} arm`, () => {
      const el = drawFeedShellHead(
        shell({ settled: { endedAtMs: 1n, outcome: c.arm } }),
        ctxFor().rc,
      );
      expect(el.querySelector(".shell-outcome")?.textContent).toBe(c.word);
    });

    it(`carries ${c.arm} as the bubble's state`, () => {
      const el = drawFeedShellHead(
        shell({ settled: { endedAtMs: 1n, outcome: c.arm } }),
        ctxFor().rc,
      );
      expect(el.getAttribute("data-state")).toBe(c.arm);
    });

    it(`dots the ${c.arm} arm as ${c.dot}`, () => {
      const el = drawFeedShellHead(
        shell({ settled: { endedAtMs: 1n, outcome: c.arm } }),
        ctxFor().rc,
      );
      expect(el.querySelector(".agent-dot")?.classList.contains(c.dot)).toBe(true);
    });
  }

  it('says "file vanished" as the lost cause when the file went away', () => {
    const el = drawFeedShellHead(
      shell({ settled: { endedAtMs: 1n, outcome: "lost", lostHow: "fileVanished" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-outcome")?.textContent).toBe("lost sight of: file vanished");
  });

  it('says "went silent" as the lost cause when the run produced nothing', () => {
    const el = drawFeedShellHead(
      shell({ settled: { endedAtMs: 1n, outcome: "lost", lostHow: "wentSilent" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-outcome")?.textContent).toBe("lost sight of: went silent");
  });

  it('says "swept up at boot" as the lost cause when a boot sweep closed it', () => {
    const el = drawFeedShellHead(
      shell({ settled: { endedAtMs: 1n, outcome: "lost", lostHow: "sweptUp" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-outcome")?.textContent).toBe("lost sight of: swept up at boot");
  });

  it("says the plain word when an older daemon ruled no cause", () => {
    const el = drawFeedShellHead(shell({ settled: { endedAtMs: 1n, outcome: "lost" } }), ctxFor().rc);
    expect(el.querySelector(".shell-outcome")?.textContent).toBe("lost sight of");
  });

  it("words every lost cause the schema declares", () => {
    const schemaArms = FeedShellLostSchema.oneofs
      .filter((oneof) => oneof.name === "how")
      .flatMap((oneof) => oneof.fields.map((field) => field.localName));
    expect([...SHELL_LOST_CAUSE_ARMS].sort()).toEqual([...schemaArms].sort());
  });

  it("never draws a lost shell in the error register", () => {
    const el = drawFeedShellHead(shell({ settled: { endedAtMs: 1n, outcome: "lost" } }), ctxFor().rc);
    expect(el.querySelector(".shell-outcome")?.classList.contains("shell-outcome-lost")).toBe(true);
  });

  it("draws every settled outcome the schema carries", () => {
    expect([...SHELL_SETTLED_ARMS].sort()).toEqual(["completed", "cancelled", "lost"].sort());
  });

  it("draws the exit chip when the terminator carried a code", () => {
    const el = drawFeedShellHead(
      shell({ settled: { endedAtMs: 1n, outcome: "completed", exit: 1 } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-exit")?.textContent).toBe("exit 1");
  });

  it("draws a zero exit in the success tone", () => {
    const el = drawFeedShellHead(
      shell({ settled: { endedAtMs: 1n, outcome: "completed", exit: 0 } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-exit")?.className).toBe("badge ok shell-exit");
  });

  it("draws a non-zero exit in the error tone", () => {
    const el = drawFeedShellHead(
      shell({ settled: { endedAtMs: 1n, outcome: "completed", exit: 2 } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-exit")?.className).toBe("badge err shell-exit");
  });

  it("draws no exit chip when no code was carried, never a zero", () => {
    const el = drawFeedShellHead(
      shell({ settled: { endedAtMs: 1n, outcome: "lost" } }),
      ctxFor().rc,
    );
    expect(el.querySelector(".shell-exit")).toBeNull();
  });

  it("offers no stop control on a settled shell", () => {
    const el = drawFeedShellHead(
      shell({ settled: { endedAtMs: 1n, outcome: "completed" } }),
      ctxFor().rc,
    );
    expect(el.querySelector("[data-interrupt]")).toBeNull();
  });
});

describe("the stop control", () => {
  it("names this row as its interrupt target", () => {
    const el = drawFeedShellHead(shell(), ctxFor().rc);
    expect(el.querySelector("[data-interrupt]")?.getAttribute("data-interrupt")).toBe(ROW);
  });

  it("interrupts the detached target by this row's own id", async () => {
    const { h, rc } = ctxFor();
    const el = drawFeedShellHead(shell(), rc);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    expect(h.calls.interrupt.map((r) => r.target.value)).toEqual([feedId(ROW)]);
  });

  it("echoes the workspace on the interrupt", async () => {
    const { h, rc } = ctxFor();
    const el = drawFeedShellHead(shell(), rc);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    expect(h.calls.interrupt[0]?.workspace).toEqual(WORKSPACE);
  });

  it("draws the success outcome at the control", async () => {
    const { rc } = ctxFor(interruptedDetached(1n));
    const el = drawFeedShellHead(shell(), rc);
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
    const el = drawFeedShellHead(shell(), rc);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")).toBeNull();
  });

  it("clears the outcome once it has been readable long enough", async () => {
    const { rc } = ctxFor(interruptedDetached(1n));
    const el = drawFeedShellHead(shell(), rc);
    document.body.append(el);
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    await vi.advanceTimersByTimeAsync(STOP_OUTCOME_MS + 1000);
    expect(el.querySelector(".shell-stop-outcome")).toBeNull();
  });

  it("latches the button inert while the stop is in flight", () => {
    const { rc } = ctxFor();
    const el = drawFeedShellHead(shell(), rc);
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
    const el = drawFeedShellHead(shell(), rc);
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
    const el = drawFeedShellHead(shell(), rc);
    const button = el.querySelector<HTMLButtonElement>("[data-interrupt]");
    button?.click();
    await settle();
    expect(button?.disabled).toBe(false);
  });

  it("states an unset cause as unreadable rather than as a cause", async () => {
    const { rc } = ctxFor(
      create(InterruptResponseSchema, { result: { case: "error", value: {} } }),
    );
    const el = drawFeedShellHead(shell(), rc);
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
      const el = drawFeedShellHead(shell(), rc);
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

describe("drawFeedShellHead malformed input", () => {
  it("refuses a bubble whose state oneof is unset", () => {
    const u = create(FeedShellSchema, { command: { text: COMMAND }, runtime: { startedAtMs: 0n } });
    expect(() => drawFeedShellHead(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a bubble with no command", () => {
    const u = shell();
    (u as unknown as { command: undefined }).command = undefined;
    expect(() => drawFeedShellHead(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a bubble with no runtime", () => {
    const u = shell();
    (u as unknown as { runtime: undefined }).runtime = undefined;
    expect(() => drawFeedShellHead(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a settled bubble whose outcome oneof is unset", () => {
    const u = shell({ settled: { endedAtMs: 1n, outcome: "completed" } });
    (u.state as unknown as { value: { outcome: { case: undefined } } }).value.outcome = {
      case: undefined,
    };
    expect(() => drawFeedShellHead(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a state arm this build does not know", () => {
    const u = shell();
    (u as unknown as { state: { case: string; value: unknown } }).state = {
      case: "queued",
      value: {},
    };
    expect(() => drawFeedShellHead(u, ctxFor().rc)).toThrow(MalformedView);
  });

  it("refuses a settled outcome arm this build does not know", () => {
    const u = shell({ settled: { endedAtMs: 1n, outcome: "completed" } });
    (u.state as unknown as { value: { outcome: { case: string; value: unknown } } }).value.outcome =
      { case: "evicted", value: {} };
    expect(() => drawFeedShellHead(u, ctxFor().rc)).toThrow(MalformedView);
  });
});

/**
 * The stop's answer, handed over UNALTERED.
 *
 * A fabricated arm cannot travel through `createRouterTransport`: the router
 * re-encodes the response through the frozen schema, which drops a case no
 * descriptor knows and turns the arm this test is about into "a oneof sets no
 * arm". A fake client hands `drawAnswer` the message as written, which is the
 * only way to reach the unknown-arm refusals a NEWER daemon would produce.
 */
function ctxAnswering(answer: InterruptResponse): {
  rc: RowContext;
  reported: { cause: string; frameHead: string }[];
} {
  const reported: { cause: string; frameHead: string }[] = [];
  const ctx = testAppContext({
    client: {
      interrupt: () => Promise.resolve(answer),
    } as unknown as Parameters<typeof testAppContext>[0]["client"],
    workspace: WORKSPACE,
    ticker: createTicker(1000),
    failures: {
      report: (kind) => {
        if (kind.kind.case === "frameUndecodable") {
          reported.push({
            cause: kind.kind.value.cause,
            frameHead: kind.kind.value.frameHead,
          });
        }
      },
      retract: () => {},
    },
    composerEnabled: false,
  });
  const row = create(FeedRowSchema, {
    id: feedId(ROW),
    row: { case: "detachedShell", value: { shell: {} } },
  });
  return { rc: rowContext(ctx, row), reported };
}

describe("the stop's unreadable answers", () => {
  it("refuses a success outcome arm this build has no word for", async () => {
    // Arrange: the shape a NEWER daemon's outcome arrives in.
    const answer = create(InterruptResponseSchema, {
      result: { case: "success", value: { outcome: { case: "nothingRunning", value: {} } } },
    });
    (answer.result.value as { outcome: unknown }).outcome = { case: "quiesced", value: {} };
    const { rc, reported } = ctxAnswering(answer);
    const el = drawFeedShellHead(shell(), rc);
    // Act
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    // Assert: reported once by path and arm, with no outcome word invented.
    expect([reported, el.querySelector(".shell-stop-outcome")]).toEqual([
      [
        {
          cause: "arm 'quiesced' is not one this build can draw",
          frameHead: "InterruptSuccess.outcome",
        },
      ],
      null,
    ]);
  });

  it("refuses a result arm this build has no case for", async () => {
    // Arrange
    const answer = create(InterruptResponseSchema, {
      result: { case: "success", value: { outcome: { case: "nothingRunning", value: {} } } },
    });
    (answer as { result: unknown }).result = { case: "deferred", value: {} };
    const { rc, reported } = ctxAnswering(answer);
    const el = drawFeedShellHead(shell(), rc);
    // Act
    el.querySelector<HTMLButtonElement>("[data-interrupt]")?.click();
    await settle();
    // Assert
    expect(reported).toEqual([
      {
        cause: "arm 'deferred' is not one this build can draw",
        frameHead: "InterruptResponse.result",
      },
    ]);
  });
});

/**
 * THE COMMAND IS THE BUBBLE'S TITLE (owner ruling, 2026-09-23): the one
 * two-line title fold (title-fold.ts), owned by the bubble's fold (bubble.ts).
 */
describe("drawFeedShellHead title fold", () => {
  /** A head of SHELL seated in a collapsed bubble, and its command. */
  function seated(u: FeedShell = shell()) {
    const head = drawFeedShellHead(u, ctxFor().rc);
    const bubble = inBubbleFold(head);
    return { bubble, title: head.querySelector(".shell-command") as HTMLElement };
  }

  it("marks the command with the one title-fold class", () => {
    // Arrange / Act
    const { title } = seated();

    // Assert
    expect(title.classList.contains(TITLE_FOLD_CLASS)).toBe(true);
  });

  it("defers the command's fold to the bubble rather than making it its own", () => {
    // Arrange / Act
    const { title } = seated();

    // Assert
    expect(title.classList.contains(TITLE_FOLD_STANDALONE_CLASS)).toBe(false);
  });

  it("wears has-more when the command overflows its two lines", () => {
    // Arrange
    const { title } = seated();
    measureTitle(title, true);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("keeps has-more off a command that fits its two lines", () => {
    // Arrange
    const { title } = seated();
    measureTitle(title, false);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("drops has-more once the bubble is expanded", () => {
    // Arrange
    const { bubble, title } = seated();
    measureTitle(title, true);
    fireResize(title);
    bubble.setAttribute("data-expanded", "true");

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(false);
  });

  it("keeps measuring a settled head's command after the head's terminal stop", () => {
    // Arrange
    const { title } = seated(shell({ settled: { endedAtMs: 5000n, outcome: "completed", exit: 0 } }));
    measureTitle(title, true);

    // Act
    fireResize(title);

    // Assert
    expect(title.classList.contains(HAS_MORE_CLASS)).toBe(true);
  });

  it("clamps the command to two lines while the bubble is collapsed", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { title } = seated();

      // Act / Assert
      expect(cascadedValue(title, "-webkit-line-clamp")).toBe("2");
    } finally {
      remove();
    }
  });

  it("shows the whole command once the bubble is expanded", () => {
    // Arrange
    const remove = installStylesheet();
    try {
      const { bubble, title } = seated();
      bubble.setAttribute("data-expanded", "true");

      // Act / Assert
      expect(cascadedValue(title, "-webkit-line-clamp")).toBe("none");
    } finally {
      remove();
    }
  });
});

describe("drawFeedShellHead: the detached-work id", () => {
  it.each([
    { name: "a live head names its work", opts: { workId: "work-3" }, want: "work-3" },
    {
      name: "a settled head still names its work",
      opts: { workId: "work-3", settled: { endedAtMs: 5_000n, outcome: "completed" as const } },
      want: "work-3",
    },
  ])("$name", ({ opts, want }) => {
    // Arrange
    const { rc } = ctxFor();

    // Act
    const el = drawFeedShellHead(shell(opts), rc);

    // Assert
    expect(el.querySelector(".shell-head .async-work-id")?.textContent).toBe(want);
  });
});
