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
  FoldMergeBubbleResponseSchema,
  type FoldMergeBubbleRequest,
  type FoldMergeBubbleResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_fold_merge_bubble_pb";
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
import {
  FeedHeldItemSchema,
  FeedHeldSchema,
  type FeedHeld,
  type FeedHeldItem,
} from "../../../proto/gen/ts/frontend/v1/feed_held_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { TurnIdSchema } from "../../../proto/gen/ts/conversation/v1/turn_pb";
import {
  FeedAgentPromptSchema,
  FeedBreadcrumbsSchema,
  FeedIdSchema,
  FeedMergeErrorSchema,
  FeedMergeSchema,
  FeedMergeSuccessSchema,
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
import { sentTo } from "./addresses.js";


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
  /**
   * Answers OpenFeed, by the requested feed id ("" = the root feed). A promise
   * holds the open in flight until the test resolves it.
   */
  openFeed?: (req: OpenFeedRequest) => OpenFeedResponse | Promise<OpenFeedResponse>;
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
  /** Answers FoldMergeBubble; a success when unscripted. */
  foldMergeBubble?: (req: FoldMergeBubbleRequest) => FoldMergeBubbleResponse;
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
  foldMergeBubble: FoldMergeBubbleRequest[];
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
  const calls: FeedCalls = { openFeed: [], watchFeed: [], getFeedPage: [], interrupt: [], loadFeedThrough: [], selectFeedRow: [], foldMergeBubble: [], submitPrompt: [] };
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
      foldMergeBubble: (req) => {
        calls.foldMergeBubble.push(req);
        return (
          script.foldMergeBubble?.(req) ??
          create(FoldMergeBubbleResponseSchema, { result: { case: "success", value: {} } })
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

/** The token the root feed's scripted open answers, and so the key its live tail is watched by. */
export const ROOT_WATCH_TOKEN = "tok:root";

/**
 * A harness whose ROOT feed has a live tail: CHANNEL, registered under the
 * token the root's open answers, so every frame a test pushes on it reaches
 * the root feed's watch. SCRIPT scripts everything else.
 */
export function liveRootHarness(script: Omit<FeedScript, "channels"> = {}): { h: Harness; channel: Channel<WatchFeedResponse> } {
  const channel = new Channel<WatchFeedResponse>();
  const h = harness({ ...script, channels: new Map([[ROOT_WATCH_TOKEN, channel]]) });
  return { h, channel };
}

/**
 * The feed's scroll BOX holding the HOST the feed mounts into, made the
 * document's whole body.
 */
export function scrollHost(): { scroll: HTMLDivElement; host: HTMLDivElement } {
  const scroll = document.createElement("div");
  const host = document.createElement("div");
  scroll.append(host);
  document.body.replaceChildren(scroll);
  return { scroll, host };
}

/** The token a scripted open mints for the feed it was asked about. */
export function tokenFor(req: OpenFeedRequest): string {
  return req.feed === undefined ? ROOT_WATCH_TOKEN : `tok:${req.feed.value}`;
}

/**
 * An OpenFeed success answering PAGE and minting TOKEN. The ROOT feed's open
 * states its held section, HELD (the empty one unless said); a sub-feed's
 * states none.
 */
export function openSuccess(answer: FeedPage, token: string, held?: FeedHeld): OpenFeedResponse {
  const root = token === "tok:root";
  return create(OpenFeedResponseSchema, {
    result: {
      case: "success",
      value: {
        page: answer,
        watch: create(FeedWatchTokenSchema, { value: token }),
        ...(root ? { held: held ?? create(FeedHeldSchema, {}) } : {}),
      },
    },
  });
}

/** The root feed's held section, holding ITEMS in order. */
export function heldSection(items: readonly FeedHeldItem[]): FeedHeld {
  return create(FeedHeldSchema, { items: [...items] });
}

/** A held prompt item for TURN, keyed by ID. */
export function heldPromptItem(id: string, turn: string, text = "held words"): FeedHeldItem {
  return create(FeedHeldItemSchema, {
    id: feedId(id),
    item: {
      case: "prompt",
      value: {
        turn: { value: turn },
        said: { content: { blocks: [{ block: { case: "text", value: { text } } }] } },
        queuedAt: { atMs: 0n },
        classification: { case: "classifying", value: {} },
        badge: {
          label: "classifying",
          detail: "queued — classifying",
          standsFor: { case: "classifying", value: {} },
        },
      },
    },
  });
}

/**
 * A tail push carrying a change of the held section: HELD whole, the ROWS
 * that moved with it, and the FOLDS it made, each a folded item id and the
 * item id it was folded into.
 */
export function pushHeld(
  held: FeedHeld,
  rows: readonly FeedRow[] = [],
  folds: ReadonlyArray<readonly [folded: string, into: string]> = [],
): WatchFeedResponse {
  return create(WatchFeedResponseSchema, {
    held: { held, rows: [...rows], folds: folds.map(([folded, into]) => ({ folded: feedId(folded), into: feedId(into) })) },
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
            ? { case: "hasMore", value: {} }
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
  awaitingUser = false,
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
        awaitingUser,
      }),
    },
  });
}

/**
 * An agent-prompt row; WORKING and AWAITINGUSER are the daemon's flags for its
 * turn.
 */
export function agentPromptRow(
  id: string,
  address: string,
  text: string,
  turn?: string,
  working = false,
  awaitingUser = false,
): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    turn: turn === undefined ? undefined : create(TurnIdSchema, { value: turn }),
    row: {
      case: "agentPrompt",
      value: create(FeedAgentPromptSchema, {
        address: sentTo(address),
        body: { blocks: [{ block: { case: "text", value: { text } } }] },
        working,
        awaitingUser,
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

/** The flags a settled response row of `settledResponseRow` wears. */
export type SettledResponseFlags = { thinking: true } | { interim: true };

/** A settled response row reading MARKDOWN and wearing FLAGS. */
export function settledResponseRow(id: string, markdown: string, flags: SettledResponseFlags): FeedRow {
  return create(FeedRowSchema, {
    id: feedId(id),
    order: orderFor(id),
    row: {
      case: "activity",
      value: {
        unit: {
          case: "response",
          value: { ...flags, result: { case: "success", value: { prose: { markdown } } } },
        },
      },
    },
  });
}

/** A settled thinking bubble's row: a response the daemon flags `thinking`. */
export function thinkingRow(id: string, markdown = "reasoning"): FeedRow {
  return settledResponseRow(id, markdown, { thinking: true });
}

/** A settled INTERIM response row: the daemon proved a later row of its turn landed. */
export function interimRow(id: string, markdown = "meanwhile"): FeedRow {
  return settledResponseRow(id, markdown, { interim: true });
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

/** A SYNC merge bubble row, its fold decided BY the daemon unless said. */
export function mergeRow(
  id: string,
  folded = true,
  label = "branch → master",
  by: "daemon" | "reader" = "daemon",
  mode: "sync" | "async" = "sync",
): FeedRow {
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
              label: { text: label },
              runtime: { startedAtMs: 0n },
              fold: { folded, decidedBy: { case: by, value: {} } },
              mode: { mode: { case: mode, value: {} } },
            },
            result: { case: "update", value: {} },
          }),
        },
      },
    },
  });
}

/**
 * An ASYNC merge bubble row in RESULT's state, its fold as the daemon ships
 * an async merge's (folded) unless said.
 */
export function asyncMergeRow(
  id: string,
  result: "update" | "success" | "failed" = "update",
  folded = true,
  by: "daemon" | "reader" = "daemon",
): FeedRow {
  const row = mergeRow(id, folded, "branch → master", by, "async");
  if (row.row.case !== "activity" || row.row.value.unit.case !== "merge") throw new Error("not a merge row");
  const merge = row.row.value.unit.value;
  switch (result) {
    case "update":
      break;
    case "success":
      merge.result = { case: "success", value: create(FeedMergeSuccessSchema, { endedAtMs: 1n, commit: "a1b2c3d" }) };
      break;
    case "failed":
      merge.result = {
        case: "error",
        value: create(FeedMergeErrorSchema, { endedAtMs: 1n, reason: { case: "failed", value: { summary: "tests never passed" } } }),
      };
      break;
  }
  return row;
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
 * response, prompt or other bubble, with what the viewport does about it
 * (`center`, a step from Emacs, unless the shorthand says `hold`, a click).
 */
export type SelectionShape =
  | { none: "returnToTail" | "stay" | "hold" }
  | { response: string; viewport?: RowViewport }
  | { prompt: string; viewport?: RowViewport }
  | { bubble: string; viewport?: RowViewport };

/** What the viewport does about a newly selected row. */
type RowViewport = "center" | "hold";

/** The daemon's `FeedSelection` for SHAPE. */
export function selectionOf(shape: SelectionShape): FeedSelection {
  if ("none" in shape) {
    return create(FeedSelectionSchema, {
      selection: { case: "none", value: { viewport: { case: shape.none, value: {} } } },
    });
  }
  const viewport = { viewport: { case: shape.viewport ?? "center", value: {} } } as const;
  if ("response" in shape) {
    return create(FeedSelectionSchema, {
      selection: { case: "response", value: { row: feedId(shape.response), viewport } },
    });
  }
  if ("prompt" in shape) {
    return create(FeedSelectionSchema, {
      selection: { case: "prompt", value: { row: feedId(shape.prompt), viewport } },
    });
  }
  return create(FeedSelectionSchema, {
    selection: { case: "bubble", value: { row: feedId(shape.bubble), viewport } },
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
    chessBoard: stub("chessBoard"),
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
export function rowContext(ctx: AppContext, row: FeedRow, extra: Partial<RowContext> = {}): RowContext {
  return {
    ctx,
    feed: "root" as const,
    row,
    // A row drawn from a page: its title shows whole, as history does.
    arrival: "page",
    revealRow: async () => false,
    ...extra,
  };
}

/**
 * A row context for a card drawn from a page, so its title shows whole: what a
 * test of anything but the title's type-out hands `foldTitle`.
 */
export function pagedRowContext(): RowContext {
  return rowContext(harness().ctx, create(FeedRowSchema, {}));
}

/** Let every scripted answer settle; the router hands them back on timers. */
export async function settle(advance: (ms: number) => Promise<unknown>): Promise<void> {
  for (let i = 0; i < 30; i += 1) await advance(0);
}

/** Whether A is drawn before B in the document; throws when either is missing. */
export function drawnBefore(a: Element | null, b: Element | null): boolean {
  if (a === null || b === null) throw new Error("the drawing lacks an element the order is about");
  return (a.compareDocumentPosition(b) & Node.DOCUMENT_POSITION_FOLLOWING) !== 0;
}
