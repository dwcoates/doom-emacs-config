/**
 * The footer suites' shared fixtures: a scripted `AgentRepl` with a hand-fed
 * `WatchFooter` tail, a context wired to it, and builders for the view.
 *
 * NOT A SUITE — vitest collects only `*.test.ts`. It exists so every footer
 * suite scripts the same two verbs the same way, and so a `FooterView` fixture
 * is built once from the generated schemas rather than five times by hand.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  InterruptResponseSchema,
  type InterruptRequest,
  type InterruptResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import {
  WatchFooterResponseSchema,
  type WatchFooterRequest,
  type WatchFooterResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_footer_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { FeedIdSchema, type FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  FooterAgentRowSchema,
  FooterCronRowSchema,
  FooterExpandedSchema,
  FooterExpandedTokensSchema,
  FooterLiveWorkChipsSchema,
  FooterMonitorRowSchema,
  FooterShellRowSchema,
  FooterStatusIdleSchema,
  FooterSubStatusIdleReadySchema,
  FooterStripSchema,
  FooterTaskRowSchema,
  FooterTokensCellSchema,
  FooterViewSchema,
  type FooterExpanded,
  type FooterStatus,
  type FooterStrip,
  type FooterView,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { createTicker } from "../../src/clock.js";
import type { TransientExpirySchedule } from "../../src/footer/expiry.js";
import type { ClientFailureArm, FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";

export const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });

/** Records every failure the footer reports, so a test can assert the arms. */
export class RecordingSink implements FailureSink {
  readonly reported: string[] = [];
  readonly retracted: ClientFailureArm[] = [];
  report(kind: FailureKind): void {
    this.reported.push(kind.kind.case ?? "unset");
  }
  retract(arm: ClientFailureArm): void {
    this.retracted.push(arm);
  }
}

/**
 * A stream the test feeds by hand.
 *
 * A generator that RETURNS would end the stream, which the standing-stream
 * machinery correctly treats as a transport failure and reopens; a channel
 * keeps it open for as long as the test needs it.
 */
export class Channel<T> {
  private readonly waiting: ((result: IteratorResult<T>) => void)[] = [];
  private readonly buffered: T[] = [];
  private closed = false;

  push(value: T): void {
    const next = this.waiting.shift();
    if (next !== undefined) {
      next({ value, done: false });
      return;
    }
    this.buffered.push(value);
  }

  close(): void {
    this.closed = true;
    for (const waiter of this.waiting.splice(0)) {
      waiter({ value: undefined as never, done: true });
    }
  }

  async *iterate(): AsyncGenerator<T> {
    for (;;) {
      const buffered = this.buffered.shift();
      if (buffered !== undefined) {
        yield buffered;
        continue;
      }
      if (this.closed) return;
      const result = await new Promise<IteratorResult<T>>((resolve) => {
        this.waiting.push(resolve);
      });
      if (result.done === true) return;
      yield result.value;
    }
  }
}

/** What the scripted daemon answers with. */
export interface FooterScript {
  interrupt?: (req: InterruptRequest) => InterruptResponse;
}

/** Every request the scripted daemon received, in order. */
export interface FooterCalls {
  watchFooter: WatchFooterRequest[];
  interrupt: InterruptRequest[];
}

export interface Harness {
  ctx: AppContext;
  calls: FooterCalls;
  sink: RecordingSink;
  tail: Channel<WatchFooterResponse>;
}

/** A context whose client speaks to the scripted daemon. */
export function harness(script: FooterScript = {}): Harness {
  const calls: FooterCalls = { watchFooter: [], interrupt: [] };
  const sink = new RecordingSink();
  const tail = new Channel<WatchFooterResponse>();
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      watchFooter: async function* (req) {
        calls.watchFooter.push(req);
        yield* tail.iterate();
      },
      interrupt: (req) => {
        calls.interrupt.push(req);
        return script.interrupt?.(req) ?? interruptSuccess("interruptedTurn");
      },
    });
  });
  return {
    calls,
    sink,
    tail,
    ctx: testAppContext({
      client: createAgentReplClient(transport),
      workspace: WORKSPACE,
      ticker: createTicker(1000),
      failures: sink,
      composerEnabled: false,
    }),
  };
}

/** An Interrupt success carrying OUTCOME (`interrupted_detached` takes COUNT). */
export function interruptSuccess(
  outcome: "interruptedTurn" | "interruptedDetached" | "nothingRunning",
  count = 0n,
): InterruptResponse {
  return create(InterruptResponseSchema, {
    result: {
      case: "success",
      value: {
        outcome:
          outcome === "interruptedDetached"
            ? { case: "interruptedDetached", value: { count } }
            : { case: outcome, value: {} },
      },
    },
  });
}

/** The are-you-sure challenge, the one refusal arm that is not a dead end. */
export function confirmRequired(liveAgentCount: bigint): InterruptResponse {
  return interruptRefused("confirmRequired", { liveAgentCount });
}

/** Any refusal arm, by its generated case name and its own init. */
export function interruptRefused(
  arm: string,
  value: Record<string, unknown> = {},
): InterruptResponse {
  return create(InterruptResponseSchema, {
    result: {
      case: "error",
      value: { kind: { case: arm as never, value: value as never } },
    },
  });
}

/** A feed id. */
export function feedId(value: string): FeedId {
  return create(FeedIdSchema, { value });
}

/**
 * The quietest legal activity cell for STATUSCASE: the empty enduring line,
 * or — for waiting, which has no unpinned branch — a gated-call salient line.
 */
export function quietActivity(statusCase: string): Record<string, unknown> {
  if (statusCase === "waiting") {
    return { salient: { at: { atMs: 0n }, kind: { case: "gatedCall", value: { text: "Bash: ls" } } } };
  }
  return { tier: { case: "unpinned", value: { enduring: {} } } };
}

/** An expiry schedule for draws whose re-render no test watches. */
export const IGNORED_EXPIRY: TransientExpirySchedule = { schedule: () => {} };

/** The idle status, the quietest legal one. */
export function idleStatus(): FooterStatus["status"] {
  return {
    case: "idle",
    value: create(FooterStatusIdleSchema, {
      substatus: { case: "ready", value: create(FooterSubStatusIdleReadySchema, {}) },
      activity: quietActivity("idle"),
    }),
  };
}

/** What a test varies about the strip. */
export interface StripInit {
  status?: FooterStatus["status"];
  /** Set to make the clock live; unset draws the idle dash and no stop. */
  turnStartedAtMs?: bigint;
  tokens?: MessageInitShape<typeof FooterTokensCellSchema>;
  liveWork?: MessageInitShape<typeof FooterLiveWorkChipsSchema>;
}

/** A strip carrying STATUS and whatever else the test cares about. */
export function strip(init: StripInit = {}): FooterStrip {
  return create(FooterStripSchema, {
    status: { status: init.status ?? idleStatus() },
    clock: init.turnStartedAtMs === undefined ? {} : { turnStartedAtMs: init.turnStartedAtMs },
    tokens: init.tokens ?? { input: { text: "0 in" } },
    liveWork: init.liveWork ?? {},
  });
}

/** What a test varies about the expanded section. */
export interface ExpandedInit {
  tokens?: MessageInitShape<typeof FooterExpandedTokensSchema>;
  agents?: readonly MessageInitShape<typeof FooterAgentRowSchema>[];
  tasks?: readonly MessageInitShape<typeof FooterTaskRowSchema>[];
  shells?: readonly MessageInitShape<typeof FooterShellRowSchema>[];
  monitors?: readonly MessageInitShape<typeof FooterMonitorRowSchema>[];
  crons?: readonly MessageInitShape<typeof FooterCronRowSchema>[];
}

/**
 * Every panel, populated as the daemon always populates them: the token
 * lines are ALWAYS SET (with no value until the turn reports one) and every
 * row list exists even when it is empty.
 */
export function expanded(init: ExpandedInit = {}): FooterExpanded {
  return create(FooterExpandedSchema, {
    tokens: init.tokens ?? {
      contextGrowth: {},
      input: {},
      cacheRead: {},
      cacheWrite: {},
      output: {},
      thinking: {},
      firstToken: {},
    },
    agents: { rows: [...(init.agents ?? [])] },
    tasks: { rows: [...(init.tasks ?? [])] },
    shells: { rows: [...(init.shells ?? [])] },
    monitors: { rows: [...(init.monitors ?? [])] },
    crons: { rows: [...(init.crons ?? [])] },
  });
}

/** A whole view. */
export function footerView(init: { strip?: FooterStrip; expanded?: FooterExpanded } = {}): FooterView {
  return create(FooterViewSchema, {
    strip: init.strip ?? strip(),
    expanded: init.expanded ?? expanded(),
  });
}

/** One push carrying VIEW. */
export function pushView(view: FooterView): WatchFooterResponse {
  return create(WatchFooterResponseSchema, { footer: view });
}
