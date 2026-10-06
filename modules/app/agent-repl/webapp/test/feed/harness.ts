/**
 * The feed suites' shared fixtures: a scripted `AgentRepl`, row builders, and a
 * stub renderer registry.
 *
 * NOT A SUITE — vitest collects only `*.test.ts`, so this file is imported by
 * the suites and never run on its own. It exists so every feed suite scripts
 * the same four verbs the same way, and so a row fixture is built once from the
 * generated schemas rather than a dozen times by hand.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  OpenFeedResponseSchema,
  type OpenFeedRequest,
  type OpenFeedResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import {
  WatchFeedResponseSchema,
  type WatchFeedRequest,
  type WatchFeedResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import {
  GetFeedPageResponseSchema,
  type GetFeedPageRequest,
  type GetFeedPageResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_feed_page_pb";
import {
  InterruptResponseSchema,
  type InterruptRequest,
  type InterruptResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import {
  SelectFeedRowResponseSchema,
  type SelectFeedRowRequest,
  type SelectFeedRowResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_feed_row_pb";
import {
  SubmitPromptResponseSchema,
  type SubmitPromptRequest,
  type SubmitPromptResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import {
  type LoadFeedThroughRequest,
  type LoadFeedThroughResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_load_feed_through_pb";
import { FeedWatchTokenSchema } from "../../../proto/gen/ts/agentrepl/v1/feed_token_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { TurnIdSchema } from "../../../proto/gen/ts/conversation/v1/turn_pb";
import {
  FeedAgentPromptSchema,
  FeedBreadcrumbsSchema,
  FeedIdSchema,
  FeedMergeSchema,
  FeedPageSchema,
  FeedRowSchema,
  FeedSelectionSchema,
  FeedSessionSeparationSchema,
  FeedSubagentSchema,
  FeedTurnEndedSchema,
  FeedUserPromptSchema,
  type FeedBreadcrumb,
  type FeedId,
  type FeedPage,
  type FeedRow,
  type FeedSelection,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { createTicker, type Ticker } from "../../src/clock.js";
import type { ClientFailureArm, FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { orderFor } from "../feed-order.js";
import type { RowContext, RowRenderers } from "../../src/feed/renderers.js";

/** The walk every fixture page with more names (FeedPageHasMore.walk). */
export const FIXTURE_WALK = "w-fixture";

export const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });

/** A ticker that reports how many subscriptions are LIVE right now. */
export interface CountingTicker extends Ticker {
  /** Subscriptions taken and not yet dropped. A leak is this number, not zero. */
  live(): number;
}

/**
 * A ticker whose live subscriptions a test can COUNT.
 *
 * A stopped clock is not the same as a frozen reading: an element can go on
 * holding a subscription that repaints the same text forever, which is exactly
 * the defect this wrapper exists to make assertable. The count is the
 * assertion; the rendered text is the symptom.
 */
export function countingTicker(inner: Ticker = createTicker(1000)): CountingTicker {
  let live = 0;
  return {
    now: () => inner.now(),
    subscribe(fn: (nowMs: number) => void): () => void {
      live += 1;
      const unsubscribe = inner.subscribe(fn);
      let dropped = false;
      return () => {
        if (!dropped) {
          dropped = true;
          live -= 1;
        }
        unsubscribe();
      };
    },
    live: () => live,
  };
}

/** Records every failure the feed reports, so a test can assert the arms. */
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
 * A scripted generator that RETURNS would end the stream, which the standing-
 * stream machinery correctly treats as a transport failure and reopens; a
 * channel keeps the stream open for as long as the test needs it, which is what
 * a real WatchFeed does.
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

/** What a scripted daemon answers with, and what it was asked. */
export interface FeedScript {
  /** Answers OpenFeed, by the requested feed id ("" = the root feed). */
  openFeed?: (req: OpenFeedRequest) => OpenFeedResponse;
  /** The tail each opened token yields. Keyed by token value. */
  channels?: Map<string, Channel<WatchFeedResponse>>;
  getFeedPage?: (req: GetFeedPageRequest) => GetFeedPageResponse;
  interrupt?: (req: InterruptRequest) => InterruptResponse;
  /**
   * The frames LoadFeedThrough streams, as an async generator the test owns.
   * Unscripted, the stream ends at once (a contract violation a test can read).
   */
  loadFeedThrough?: (req: LoadFeedThroughRequest) => AsyncIterable<LoadFeedThroughResponse>;
  /** Answers SelectFeedRow; a success selecting nothing when unscripted. */
  selectFeedRow?: (req: SelectFeedRowRequest) => SelectFeedRowResponse;
  /** Answers SubmitPrompt (an outcome marker's resend); a minted turn when unscripted. */
  submitPrompt?: (req: SubmitPromptRequest) => SubmitPromptResponse;
  /** The page's clock. Pass a `countingTicker` to assert on live subscriptions. */
  ticker?: Ticker;
}

/** Every request the scripted daemon received, in order. */
export interface FeedCalls {
  openFeed: OpenFeedRequest[];
  watchFeed: WatchFeedRequest[];
  getFeedPage: GetFeedPageRequest[];
  interrupt: InterruptRequest[];
  loadFeedThrough: LoadFeedThroughRequest[];
  selectFeedRow: SelectFeedRowRequest[];
  submitPrompt: SubmitPromptRequest[];
}

export interface Harness {
  ctx: AppContext;
  calls: FeedCalls;
  sink: RecordingSink;
  channels: Map<string, Channel<WatchFeedResponse>>;
}

/** A context whose client speaks to the scripted daemon. */
export function harness(script: FeedScript = {}): Harness {
  const calls: FeedCalls = { openFeed: [], watchFeed: [], getFeedPage: [], interrupt: [], loadFeedThrough: [], selectFeedRow: [], submitPrompt: [] };
  const channels = script.channels ?? new Map<string, Channel<WatchFeedResponse>>();
  const sink = new RecordingSink();
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      openFeed: (req) => {
        calls.openFeed.push(req);
        return script.openFeed?.(req) ?? openSuccess(page([]), tokenFor(req));
      },
      watchFeed: async function* (req) {
        calls.watchFeed.push(req);
        const channel = channels.get(req.watch?.value ?? "");
        if (channel === undefined) {
          // No scripted tail: stand open forever rather than ending, which a
          // standing stream would rightly read as a transport failure.
          await new Promise(() => {});
          return;
        }
        yield* channel.iterate();
      },
      getFeedPage: (req) => {
        calls.getFeedPage.push(req);
        return (
          script.getFeedPage?.(req) ??
          create(GetFeedPageResponseSchema, { result: { case: "success", value: page([]) } })
        );
      },
      loadFeedThrough: async function* (req) {
        calls.loadFeedThrough.push(req);
        if (script.loadFeedThrough !== undefined) yield* script.loadFeedThrough(req);
      },
      interrupt: (req) => {
        calls.interrupt.push(req);
        return (
          script.interrupt?.(req) ??
          create(InterruptResponseSchema, {
            result: {
              case: "success",
              value: { outcome: { case: "interruptedDetached", value: { count: 1n } } },
            },
          })
        );
      },
      submitPrompt: (req) => {
        calls.submitPrompt.push(req);
        return (
          script.submitPrompt?.(req) ??
          create(SubmitPromptResponseSchema, {
            result: { case: "success", value: { outcome: { case: "turn", value: { turn: { value: "resent-turn" } } } } },
          })
        );
      },
      selectFeedRow: (req) => {
        calls.selectFeedRow.push(req);
        return (
          script.selectFeedRow?.(req) ??
          create(SelectFeedRowResponseSchema, {
            result: { case: "success", value: { outcome: { case: "none", value: {} } } },
          })
        );
      },
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: script.ticker ?? createTicker(1000),
    failures: sink,
    composerEnabled: false,
  });
  return { ctx, calls, sink, channels };
}

/** The token a scripted open mints for the feed it was asked about. */
export function tokenFor(req: OpenFeedRequest): string {
  return `tok:${req.feed?.value ?? "root"}`;
}

/** An OpenFeed success answering PAGE and minting TOKEN. */
export function openSuccess(answer: FeedPage, token: string): OpenFeedResponse {
  return create(OpenFeedResponseSchema, {
    result: {
      case: "success",
      value: { page: answer, watch: create(FeedWatchTokenSchema, { value: token }) },
    },
  });
}

/** A page of ROWS. */
export function page(
  rows: readonly FeedRow[],
  opts: { hasMore?: boolean; crumbs?: readonly FeedBreadcrumb[] } = {},
): FeedPage {
  return create(FeedPageSchema, {
    result: {
      case: "success",
      value: {
        rows: [...rows],
        edge:
          opts.hasMore === true
            ? { case: "hasMore", value: { walk: { value: FIXTURE_WALK } } }
            : { case: "atStart", value: {} },
        breadcrumbs: create(FeedBreadcrumbsSchema, { crumbs: [...(opts.crumbs ?? [])] }),
      },
    },
  });
}

/** A feed id. */
export function feedId(value: string): FeedId {
  return create(FeedIdSchema, { value });
}

/** A user-prompt row; WORKING is the daemon's flag for its turn. */
export function userPromptRow(
  id: string,
  text: string,
  turn?: string,
  working = false,
): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    turn: turn === undefined ? undefined : create(TurnIdSchema, { value: turn }),
    row: {
      case: "userPrompt",
      value: create(FeedUserPromptSchema, {
        author: { label: "You" },
        result: {
          case: "success",
          value: { body: { blocks: [{ block: { case: "text", value: { text } } }] } },
        },
        working,
      }),
    },
  });
}

/** An agent-prompt row; WORKING is the daemon's flag for its turn. */
export function agentPromptRow(
  id: string,
  address: string,
  text: string,
  turn?: string,
  working = false,
): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    turn: turn === undefined ? undefined : create(TurnIdSchema, { value: turn }),
    row: {
      case: "agentPrompt",
      value: create(FeedAgentPromptSchema, {
        address: { text: address },
        body: { blocks: [{ block: { case: "text", value: { text } } }] },
        working,
      }),
    },
  });
}

/**
 * A turn_ended row: the terminal fact for TURN, drawn as the named outcome.
 *
 * The arm matters to what the row DRAWS; to everything that only asks whether
 * the turn is over, its mere presence is the whole answer.
 */
export function turnEndedRow(
  id: string,
  turn: string | undefined,
  outcome: "concluded" | "errored" | "interrupted" = "concluded",
  answer?: string,
): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    turn: turn === undefined ? undefined : create(TurnIdSchema, { value: turn }),
    row: {
      case: "turnEnded",
      value: create(FeedTurnEndedSchema, {
        endedAtMs: 0n,
        outcome:
          outcome === "errored"
            ? {
                case: "errored",
                value: {
                  headline: { text: "the agent's stream ended without a close" },
                  error: { case: "queryDied", value: {} },
                },
              }
            : outcome === "interrupted"
              ? { case: "interrupted", value: {} }
              : {
                  case: "concluded",
                  value: answer === undefined ? {} : { answer: feedId(answer) },
                },
      }),
    },
  });
}

/**
 * A separation row: the divider a context cut left.
 *
 * ARM is what the cut was, and it is the whole of what decides whether the row
 * BOUNDS the feed — a compaction and a clear do, and nothing else does.
 */
export function separationRow(
  id: string,
  arm: "cleared" | "compacted" | "compactionFailed" = "compacted",
  summary = "what survived",
): FeedRow {
  type SeparationInit = MessageInitShape<typeof FeedSessionSeparationSchema>;
  const kind: SeparationInit["kind"] =
    arm === "cleared"
      ? { case: "cleared", value: {} }
      : arm === "compactionFailed"
        ? {
            case: "compactionFailed",
            value: {
              error: "the summarizer refused",
              marker: {
                label: { text: "compaction failed" },
                family: {
                  case: "vendorFault",
                  value: { expansion: { errorType: { text: "compaction_failed" }, message: { text: "the summarizer refused" } } },
                },
              },
            },
          }
        : { case: "compacted", value: { summary: { markdown: summary } } };
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row: {
      case: "separation",
      value: { label: { text: "context compacted" }, kind },
    },
  });
}

/** A response row, the commonest activity. */
export function responseRow(
  id: string,
  markdown = "hi",
  parent?: string,
  turn?: string,
): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    parent: parent === undefined ? undefined : { row: feedId(parent) },
    turn: turn === undefined ? undefined : create(TurnIdSchema, { value: turn }),
    row: {
      case: "activity",
      value: {
        unit: {
          case: "response",
          value: { result: { case: "success", value: { prose: { markdown } } } },
        },
      },
    },
  });
}

/**
 * A tool-call row: running with an observed beat (which ticks), or returned.
 *
 * The beat is what makes a running card hold a clock, so a test about clocks
 * has to ask for one — an unset `last_progress` draws no clock at all.
 */
export function toolCallRow(
  id: string,
  state: "running" | "returned",
  opts: { turn?: string; beatAtMs?: bigint; tool?: string } = {},
): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    turn: opts.turn === undefined ? undefined : create(TurnIdSchema, { value: opts.turn }),
    row: {
      case: "activity",
      value: {
        unit: {
          case: "simpleToolCall",
          value: {
            name: { text: opts.tool ?? "Bash" },
            input: { text: "$ ls" },
            outcome:
              state === "running"
                ? { case: "running", value: { lastProgress: { atMs: opts.beatAtMs ?? 0n } } }
                : {
                    case: "returned",
                    value: {
                      verdict: { case: "succeeded", value: {} },
                      form: { case: "text", value: { text: "ok" } },
                    },
                  },
          },
        },
      },
    },
  });
}

/** A live subagent bubble row, sync (`activity`) or detached. */
export function subagentRow(
  id: string,
  opts: {
    detached?: boolean;
    startedAtMs?: bigint;
    lastProgressMs?: bigint;
    settled?: {
      endedAtMs: bigint;
      outcome: "succeeded" | "failed" | "cancelled" | "lost";
      lostHow?: "fileVanished" | "wentSilent" | "sweptUp";
    };
    description?: string;
    tokens?: string;
    workId?: string;
  } = {},
): FeedRow {
  const subagent = create(FeedSubagentSchema, {
    label: { text: "Explore" },
    workId: opts.workId === undefined ? undefined : { text: opts.workId },
    description: opts.description === undefined ? undefined : { text: opts.description },
    tokens: opts.tokens === undefined ? undefined : { text: opts.tokens },
    runtime: { startedAtMs: opts.startedAtMs ?? 0n },
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
              outcome:
                opts.settled.outcome === "lost" && opts.settled.lostHow !== undefined
                  ? { case: "lost", value: { how: { case: opts.settled.lostHow, value: {} } } }
                  : { case: opts.settled.outcome, value: {} },
            },
          },
  });
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row:
      opts.detached === true
        ? { case: "detachedSubagent", value: { subagent } }
        : { case: "activity", value: { unit: { case: "subagent", value: subagent } } },
  });
}

/** A merge bubble row. */
export function mergeRow(id: string, folded = true): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row: {
      case: "activity",
      value: {
        unit: {
          case: "merge",
          value: create(FeedMergeSchema, {
            head: {
              glyph: { icon: "merge" },
              label: { text: "branch → master" },
              runtime: { startedAtMs: 0n },
              fold: { folded },
            },
            result: { case: "update", value: {} },
          }),
        },
      },
    },
  });
}

/** A merge-tab row: only ever a top-level row of a merge bubble's own feed. */
export function mergeTabRow(id: string, label = "queue"): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row: {
      case: "mergeTab",
      value: {
        label: { text: label, round: 1 },
        kind: { case: "queue", value: { state: { case: "live", value: {} } } },
      },
    },
  });
}

/** A tail push carrying ROW. */
export function push(row: FeedRow): WatchFeedResponse {
  return create(WatchFeedResponseSchema, { row });
}

/**
 * A tail push carrying the daemon-global FEED TEXT SCALE (and no row): pushed
 * on EVERY open feed's watch, sub-feeds included, and replayed the instant a
 * tail is accepted.
 */
export function pushScale(scale: number): WatchFeedResponse {
  return create(WatchFeedResponseSchema, { feedTextScale: { scale } });
}

/**
 * The feed selection the daemon holds, in the test's shorthand: nothing
 * selected (and what the viewport does), or the id of a selected final
 * response or prompt.
 */
export type SelectionShape =
  | { none: "returnToTail" | "stay" }
  | { response: string }
  | { prompt: string };

/** The daemon's `FeedSelection` for SHAPE. */
export function selectionOf(shape: SelectionShape): FeedSelection {
  if ("none" in shape) {
    return create(FeedSelectionSchema, {
      selection: { case: "none", value: { viewport: { case: shape.none, value: {} } } },
    });
  }
  if ("response" in shape) {
    return create(FeedSelectionSchema, {
      selection: { case: "response", value: { row: feedId(shape.response) } },
    });
  }
  return create(FeedSelectionSchema, {
    selection: { case: "prompt", value: { row: feedId(shape.prompt) } },
  });
}

/**
 * A tail push carrying the feed SELECTION (and no row): the daemon's
 * per-workspace selection, pushed on the root feed's watch.
 */
export function pushSelection(shape: SelectionShape): WatchFeedResponse {
  return create(WatchFeedResponseSchema, { selection: selectionOf(shape) });
}

/** Stub renderers: each draws a marked element naming the arm it was given. */
export function stubRenderers(overrides: Partial<RowRenderers> = {}): RowRenderers {
  const stub =
    (name: string) =>
    (): HTMLElement => {
      const el = document.createElement("div");
      el.className = `stub-${name}`;
      el.textContent = name;
      return el;
    };
  return {
    response: stub("response"),
    simpleToolCall: stub("simpleToolCall"),
    hook: stub("hook"),
    skill: stub("skill"),
    artifact: stub("artifact"),
    plan: stub("plan"),
    findings: stub("findings"),
    subagentResult: stub("subagentResult"),
    shell: stub("shell"),
    shellHead: stub("shellHead"),
    permission: stub("permission"),
    question: stub("question"),
    coldGate: stub("coldGate"),
    mergeHead: stub("mergeHead"),
    commandPanel: stub("commandPanel"),
    commandRefused: stub("commandRefused"),
    mergeBody: (mount) => {
      const el = document.createElement("div");
      el.className = "stub-mergeBody";
      mount.append(el);
      return { dispose: () => el.remove() };
    },
    ...overrides,
  };
}

/** A row context for a renderer under test on its own. */
export function rowContext(ctx: AppContext, row: FeedRow, extra: Partial<RowContext> = {}) {
  return {
    ctx,
    feed: "root" as const,
    row,
    revealRow: async () => false,
    ...extra,
  };
}

/** Let every scripted answer settle; the router hands them back on timers. */
export async function settle(advance: (ms: number) => Promise<unknown>): Promise<void> {
  for (let i = 0; i < 30; i += 1) await advance(0);
}
