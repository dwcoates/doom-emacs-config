/**
 * THE FAKE DAEMON — a real Connect server the integration suite drives.
 *
 * It is a genuine HTTP/1.1 listener on 127.0.0.1 speaking the real Connect
 * protocol over the real generated `AgentRepl` service descriptor, so the app
 * under test exercises its own transport, its own codec, and its own stream
 * plumbing end to end. Nothing here mocks a client seam.
 *
 * Everything it serves is SCRIPTED. The driver API below is the only way state
 * changes: a test pushes a view, answers an rpc, or fails one, and the server
 * hands that exact message to whoever is listening. Anything a test has not
 * scripted answers with a DEFAULT HEALTHY message: every response carries its
 * `success` arm and every message is COMPLETE (every non-optional field set,
 * every oneof set), because the client refuses malformed views by contract and
 * an accidentally-empty fake would fail tests for the wrong reason.
 */
import { createServer, type IncomingMessage, type Server, type ServerResponse } from "node:http";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import {
  ScalarType,
  create,
  type DescField,
  type DescMessage,
  type MessageInitShape,
} from "@bufbuild/protobuf";
import { connectNodeAdapter } from "@connectrpc/connect-node";
import type { ConnectRouter } from "@connectrpc/connect";
import { Code, ConnectError } from "@connectrpc/connect";
// Connect's own spelling of a code, the one `PageSubscriptionFailed` carries.
import { codeToString } from "@connectrpc/connect/protocol-connect";

import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { FeedWatchTokenSchema } from "../../../proto/gen/ts/agentrepl/v1/feed_token_pb";
import type { FeedPage, FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FooterView } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import type { TopbarView } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { DaemonHoldTray } from "../../../proto/gen/ts/frontend/v1/daemon_hold_pb";
import type { DrainReason } from "../../../proto/gen/ts/agentrepl/v1/drain_reason_pb";
import {
  WatchDaemonResponseSchema,
  type WatchDaemonRequest,
  type WatchDaemonResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";
import {
  WatchFeedResponseSchema,
  type WatchFeedRequest,
  type WatchFeedResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_feed_pb";
import {
  WatchFooterResponseSchema,
  type WatchFooterRequest,
  type WatchFooterResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_footer_pb";
import {
  WatchTopbarResponseSchema,
  type WatchTopbarRequest,
  type WatchTopbarResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_topbar_pb";
import {
  WatchWorkspaceRosterResponseSchema,
  type WatchWorkspaceRosterRequest,
  type WatchWorkspaceRosterResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import {
  WatchDaemonHoldsResponseSchema,
  type WatchDaemonHoldsRequest,
  type WatchDaemonHoldsResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_holds_pb";
import {
  WatchWebWorkspaceResponseSchema,
  type WatchWebWorkspaceRequest,
  type WatchWebWorkspaceResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_web_workspace_pb";
import { OpenFeedResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_pb";
import { GetFeedPageResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_get_feed_page_pb";
import { SubmitPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { InterruptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { AnswerPermissionResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_permission_pb";
import { AnswerQuestionResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_question_pb";
import { AnswerColdGateResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_cold_gate_pb";
import { CreateWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_create_workspace_pb";
import { OpenWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import { CloseWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_workspace_pb";
import { KillWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_kill_workspace_pb";
import { NukeWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_nuke_workspace_pb";
import { MergeWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_merge_workspace_pb";
import { RestartWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_restart_workspace_pb";
import { SetWorkspacePriorityResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_priority_pb";
import { CreateTaskResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_create_task_pb";
import { UpdateTaskResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_task_pb";
import { AssignWorkspaceTaskResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_assign_workspace_task_pb";
import { SetModelResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_model_pb";
import { SetPermissionModeResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_permission_mode_pb";
import { SelectAccountResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_account_pb";
import { UpdateHeldPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_held_prompt_pb";
import { EditHeldPromptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_edit_held_prompt_pb";
import { AnswerHeldOfferResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_held_offer_pb";
import { UpdateShutdownScheduleResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_shutdown_schedule_pb";
import { UpdateMergeQueueResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_merge_queue_pb";
import { DaemonHealthResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_daemon_health_pb";
import { SessionHealthResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_session_health_pb";
import { ClientLogResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import { RegisterWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_register_workspace_pb";
import { SelectWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { AdoptHostWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_adopt_host_workspace_pb";
import { AdoptWebWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_adopt_web_workspace_pb";
import { OpenLoginResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_login_pb";
import { CloseLoginResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_login_pb";
import { OpenExternalResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import { RequestCommandSupportResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_request_command_support_pb";
import { OpenInEditorResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_in_editor_pb";
import { SendLoginInputResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_send_login_input_pb";
import {
  LoginTerminalOutputSchema,
  type LoginTerminalOutput,
  type WatchLoginTerminalRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_login_terminal_pb";
import { PromptOrigin } from "../../../proto/gen/ts/conversation/v1/prompt_origin_pb";
import type { WatchHostWorkspaceResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_host_workspace_pb";
import {
  PageAttachedSchema,
  PageFrameSchema,
  PageSubscriptionEndedSchema,
  WatchPageResponseSchema,
  type PageFrame,
  type SubscribePageRequest,
  type WatchPageResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_page_pb";
import {
  emptyFeedPage,
  emptyFooterView,
  emptyRoster,
  emptyTopbarView,
  emptyTray,
  hostWorkspacePush,
  shutdownAnnounced,
} from "./fixtures";

/** Every rpc the fake serves, by its generated lowerCamel method name. */
export type RpcName = keyof typeof AgentRepl.method;

/** The `$unknown` shape protobuf-es carries and re-serializes verbatim. */
const UNKNOWN_FIELD = { no: 999, wireType: 0, data: new Uint8Array([1]) } as const;

/** The workspace's own feed. Sub-feeds are addressed by their bubble's FeedId. */
export const ROOT_FEED = "root";
export type FeedKey = string;

/** One open server stream: a queue plus the parked reader waiting on it. */
class Channel<T> {
  private readonly queue: T[] = [];
  private waiting: ((r: IteratorResult<T>) => void) | undefined;
  private done = false;

  push(value: T): void {
    if (this.done) return;
    const waiting = this.waiting;
    if (waiting) {
      this.waiting = undefined;
      waiting({ value, done: false });
      return;
    }
    this.queue.push(value);
  }

  end(): void {
    this.done = true;
    const waiting = this.waiting;
    if (waiting) {
      this.waiting = undefined;
      waiting({ value: undefined as never, done: true });
    }
  }

  async *iterate(signal: AbortSignal): AsyncGenerator<T> {
    for (;;) {
      if (signal.aborted) return;
      if (this.queue.length > 0) {
        yield this.queue.shift() as T;
        continue;
      }
      if (this.done) return;
      const next = await new Promise<IteratorResult<T>>((resolve) => {
        this.waiting = resolve;
        signal.addEventListener("abort", () => resolve({ value: undefined as never, done: true }), {
          once: true,
        });
      });
      if (next.done) return;
      yield next.value;
    }
  }
}

/** A live stream registration, keyed so `liveStreams`/`endStream` can find it. */
interface Registration {
  rpc: RpcName;
  /** The workspace the stream was opened for; "" for the global streams. */
  workspace: string;
  /** The feed a WatchFeed stream tails; undefined for every other rpc. */
  feed?: FeedKey;
  channel: Channel<unknown>;
}

/** A recorded request, in arrival order, with the rpc that received it. */
export interface RecordedCall<Req = unknown> {
  rpc: RpcName;
  request: Req;
  atMs: number;
}

/**
 * The driver a test holds. Every method is synchronous state manipulation
 * except `start`/`stop` and the awaitable `nextCall`/`awaitStream`.
 */
export interface FakeDaemon {
  /** Boot the listener; resolves with the base url and the socket it listens on. */
  start(): Promise<{ baseUrl: string; socketPath: string }>;
  /** Shut the listener down and end every live stream. */
  stop(): Promise<void>;
  /**
   * The daemon's IDENTITY, once started. Throws before `start` resolves.
   *
   * NOT a dialable address: this listener speaks over a unix socket (see
   * `socketPath`), and every consumer reaches it by that path. The url exists
   * because the wire carries a daemon address as a string — a transfer
   * announces one, and the page draws it — and because a fetch still needs an
   * origin to form a request line. It is unique per daemon and resolves
   * nowhere, so a consumer that tried to dial it fails loudly rather than
   * reaching some other daemon.
   */
  readonly baseUrl: string;
  /** The unix socket this listener accepts on. Throws before `start`. */
  readonly socketPath: string;

  // --- scripted views: each setter also pushes to every live subscriber ----
  setFooter(workspace: string, view: FooterView): void;
  setTopbar(workspace: string, view: TopbarView): void;
  setRoster(roster: WorkspaceRoster): void;
  setTray(workspace: string, tray: DaemonHoldTray): void;
  setHostWorkspace(workspace: string, push: WatchHostWorkspaceResponse): void;

  // --- the feed universe --------------------------------------------------
  /** The page OpenFeed answers with, and GetFeedPage `first` returns. */
  setPage(workspace: string, feed: FeedKey, page: FeedPage): void;
  /** The page GetFeedPage `next` returns; falls back to `setPage` when unset. */
  setNextPage(workspace: string, feed: FeedKey, page: FeedPage): void;
  /** Deliver a row on every live WatchFeed tailing that feed. */
  pushRow(workspace: string, feed: FeedKey, row: FeedRow): void;
  /** The tokens OpenFeed has minted, oldest first, for one feed. */
  mintedTokens(workspace: string, feed: FeedKey): string[];

  // --- web-link and daemon-lifecycle pushes --------------------------------
  transfer(workspace: string, address: string): void;
  /** Name the session this workspace's page should attribute its logs to. */
  pushSessionIdentity(
    workspace: string,
    identity: { agentReplSessionId: string; claudeSessionId?: string },
  ): void;
  announceShutdown(init: Parameters<typeof shutdownAnnounced>[0]): void;
  scheduleDrain(atMs: bigint, reason: DrainReason): void;
  cancelDrain(): void;
  /**
   * Push WatchDaemon's `mutation_progress` — a TOP-LEVEL arm this bundle has no
   * case for, which is exactly what makes it the suite's forward-compat skew
   * probe: a newer daemon's arm must be skipped quietly, not drawn and not
   * filed as a bad frame.
   */
  pushMutationProgress(opId: string): void;

  // --- the login pty -------------------------------------------------------
  /** The buffer WatchLoginTerminal replays before any live byte. */
  setLoginScrollback(workspace: string, chunks: Uint8Array[]): void;
  /** Deliver live pty bytes on every attached WatchLoginTerminal. */
  pushLoginBytes(workspace: string, data: Uint8Array): void;
  /** Conclude the pty with the terminal `closed` frame (a legal stream end). */
  closeLoginTerminal(workspace: string): void;

  // --- scripted unary answers ---------------------------------------------
  /** Serve `response` for every later call of `rpc` (replaces the default). */
  answer(rpc: RpcName, response: unknown): void;
  /**
   * Serve a transport-level error for the NEXT call of `rpc` only. This is a
   * TRANSPORT death, not a refusal: the rpc never answered.
   */
  failNext(rpc: RpcName, errorMessage: string): void;
  /**
   * Serve `rpc`'s typed `<Rpc>Error` carrying ARM, built complete from the
   * schema. This is a domain REFUSAL: the rpc answered, and said no. Kept
   * apart from `failNext` because the two land differently in the app and the
   * refusals suite exists to tell them apart.
   */
  refuse(rpc: RpcName, arm: string): void;
  /** Every arm `refuse` accepts for `rpc`, read off the descriptor. */
  refusalArms(rpc: RpcName): string[];
  /** Put an unknown field on the next response or push of `rpc`. */
  injectUnknown(rpc: RpcName): void;
  /**
   * UNSET A REQUIRED FIELD on the next push of `rpc` — the second shape of a
   * malformed view, and the one `injectUnknown` cannot make.
   *
   * An unknown field is a NEWER daemon saying something extra; an unset
   * non-optional message field or an unset oneof is a daemon that composed the
   * view WRONG. The client's contract refuses both, but through different code
   * (`assertNoUnknownFields` versus `requireCase`/`requireMessage`), so a suite
   * that only ever injects unknowns leaves half of "TYPED ARMS, NO FALLBACKS"
   * untested.
   *
   * WHICH field is stripped is fixed per rpc by `FIELD_STRIPPERS`, and an rpc
   * with no stripper THROWS rather than quietly serving a healthy frame. The
   * strip copies down the path it edits, so a stored view is never corrupted
   * and the frame AFTER the poisoned one is the healthy original.
   */
  injectUnsetField(rpc: RpcName): void;
  /** The message-tree path `injectUnsetField` unsets for `rpc`. */
  unsetFieldPath(rpc: RpcName): string;

  // --- observation ---------------------------------------------------------
  calls<Req = unknown>(rpc: RpcName): Req[];
  /** Resolve with the next call of `rpc` not yet handed out by `nextCall`. */
  nextCall<Req = unknown>(rpc: RpcName): Promise<Req>;
  /** Every recorded call across every rpc, in arrival order. */
  log(): RecordedCall[];
  clearCalls(): void;

  // --- stream bookkeeping --------------------------------------------------
  liveStreams(rpc: RpcName, workspace?: string, feed?: FeedKey): number;
  /** Kill every matching stream WITHOUT a terminal frame (transport death). */
  endStream(rpc: RpcName, workspace?: string, feed?: FeedKey): void;
  /** Resolve once at least `count` streams of `rpc` are live. */
  awaitStream(rpc: RpcName, count?: number): Promise<void>;
  /**
   * Resolve once no live stream of `rpc` (optionally scoped to `workspace`
   * and/or `feed`) remains — the closing counterpart to `awaitStream`, for a
   * test that must observe a stream actually drop (e.g. after a client
   * abort) rather than poll `liveStreams` on a timer.
   */
  awaitStreamClosed(rpc: RpcName, workspace?: string, feed?: FeedKey): Promise<void>;

  // --- the page's one stream ----------------------------------------------
  /**
   * Every page id holding a `WatchPage` stream right now.
   *
   * A page holds ONE connection, so this is the count a leak shows up in: two
   * entries for one document means the mux opened a second stream.
   */
  attachedPages(): string[];
  /** Resolve once PAGE no longer holds its `WatchPage` stream. */
  awaitPageDetached(page: string): Promise<void>;
  /**
   * The live subscription ids on PAGE, in the order they were subscribed.
   *
   * THIS IS WHERE A LEAK IS VISIBLE. A collapsed bubble, a switched workspace
   * or a cancelled view that stopped drawing but never unsubscribed still
   * appears here, costing the daemon work for a reader that is gone.
   * Defaults to the only attached page when there is exactly one.
   */
  pageSubscriptions(page?: string): string[];
  /**
   * Kill the page's own stream WITHOUT a terminal frame — a dropped link.
   *
   * Every subscription riding it dies with it, which is what the client's
   * degraded state and its re-subscribe are the answer to.
   */
  endPageStream(page?: string): void;
}

const key = (workspace: string, feed: FeedKey): string => `${workspace} ${feed}`;

/** Attach the unknown field protobuf-es re-serializes verbatim. */
const withUnknown = <T extends object>(message: T): T => {
  (message as { $unknown?: unknown[] }).$unknown = [{ ...UNKNOWN_FIELD }];
  return message;
};

/**
 * HOW EACH RPC'S PUSH IS MADE INCOMPLETE, one required field per rpc.
 *
 * Each stripper returns a SHALLOW-COPIED message with exactly one field along
 * the named path unset. Copying rather than mutating matters: the view a
 * `set*` call stored is the same object the push carries, so an in-place strip
 * would poison every later push of that view and the "next frame renders"
 * assertion would be testing a broken fixture rather than the client.
 *
 * The three paths are the three shapes the contract names: an unset ONEOF on a
 * feed row (`FeedRow.row`), an unset non-optional MESSAGE field on a
 * whole-view push (`FooterStrip.status`), and an unset oneof on a REPEATED
 * element deep inside a view (`RosterRow.status`).
 */
const FIELD_STRIPPERS: Partial<Record<RpcName, { path: string; strip(message: object): object }>> = {
  watchFeed: {
    path: "FeedRow.row",
    strip: (message) => {
      const response = message as { row?: object };
      const row = required(response.row, "WatchFeedResponse.row");
      // A ONEOF is unset as `{ case: undefined }`: protobuf-es reads the
      // wrapper to serialize, and dropping it outright fails the codec
      // rather than producing the incomplete frame the test wants.
      return { ...response, row: { ...row, row: { case: undefined } } };
    },
  },
  watchFooter: {
    path: "FooterStrip.status",
    strip: (message) => {
      const response = message as { footer?: { strip?: object } };
      const footer = required(response.footer, "WatchFooterResponse.footer");
      const strip = required(footer.strip, "FooterView.strip");
      return { ...response, footer: { ...footer, strip: { ...strip, status: undefined } } };
    },
  },
  watchWorkspaceRoster: {
    path: "RosterRow.status",
    strip: (message) => {
      const response = message as { roster?: { repository?: { sections?: readonly object[] } } };
      const roster = required(response.roster, "WatchWorkspaceRosterResponse.roster");
      const repository = required(roster.repository, "WorkspaceRoster.repository");
      const sections = required(repository.sections, "RosterGrouping.sections");
      return {
        ...response,
        roster: {
          ...roster,
          repository: {
            ...repository,
            sections: sections.map((section) => {
              const held = section as { rows?: { rows?: readonly object[] } };
              const rows = required(held.rows, "RosterSection.rows");
              const list = required(rows.rows, "RosterRows.rows");
              return {
                ...held,
                rows: { ...rows, rows: list.map((row) => ({ ...row, status: { case: undefined } })) },
              };
            }),
          },
        },
      };
    },
  },
};

/**
 * The value at PATH, or a loud throw.
 *
 * A stripper that silently found nothing to strip would serve a HEALTHY frame
 * to a test asserting the client refused a broken one — a pass for the wrong
 * reason, which is the one outcome the fake exists to prevent.
 */
function required<T>(value: T | undefined, path: string): T {
  if (value === undefined) throw new Error(`the fake cannot strip ${path}: it is already unset`);
  return value;
}

/** The stripper for RPC, or a throw naming the rpcs that have one. */
function stripperFor(rpc: RpcName): { path: string; strip(message: object): object } {
  const stripper = FIELD_STRIPPERS[rpc];
  if (stripper === undefined) {
    throw new Error(
      `injectUnsetField has no stripper for ${rpc}; it knows [${Object.keys(FIELD_STRIPPERS).join(", ")}]`,
    );
  }
  return stripper;
}

/**
 * The streaming content types a watch request arrives with. The response
 * echoes the request's own type, which is what the adapter would have written.
 */
const STREAMING_CONTENT_TYPES = new Set([
  "application/connect+proto",
  "application/connect+json",
  "application/grpc-web+proto",
  "application/grpc-web+json",
  "application/grpc+proto",
  "application/grpc",
]);

/**
 * FLUSH THE RESPONSE HEAD AS SOON AS A WATCH IS ACCEPTED.
 *
 * The daemon flushes headers on accept, so a client can observe that its watch
 * is OPEN before any frame arrives — which matters because a standing stream
 * may legitimately push nothing for a long time, and "accepted" and "not yet
 * connected" must not look alike.
 *
 * connect-node writes the head lazily: it is emitted on the first frame, or at
 * the end, and a watch that pushes nothing therefore leaves the client with no
 * response head at all (measured: the head landed only when the stream ended).
 * So the fake writes it here, echoing the request's content type — the same
 * type the adapter would have written — and then neutralizes the adapter's own
 * later `writeHead`, which would otherwise throw ERR_HTTP_HEADERS_SENT.
 *
 * Unary requests are left alone: their head carries the response and there is
 * nothing to observe early.
 */
function flushHeadersOnAccept(req: IncomingMessage, res: ServerResponse): void {
  const contentType = req.headers["content-type"];
  if (typeof contentType !== "string" || !STREAMING_CONTENT_TYPES.has(contentType)) return;
  const writeHead = res.writeHead.bind(res);
  res.writeHead = ((...args: Parameters<ServerResponse["writeHead"]>) =>
    res.headersSent ? res : writeHead(...args)) as ServerResponse["writeHead"];
  res.writeHead(200, { "content-type": contentType });
  res.flushHeaders();
}

// ---------------------------------------------------------------------------
// TYPED REFUSALS, DERIVED FROM THE SCHEMAS
// ---------------------------------------------------------------------------

/**
 * The facts the cross-cutting arms carry, fixed so a suite can assert the
 * exact string it expects to see drawn at the call site.
 */
export const REFUSAL_FACTS: Readonly<Record<string, string>> = {
  registryDir: "/registry/elsewhere",
  address: "http://127.0.0.1:9999",
  detail: "the daemon said why",
  sink: "the durable log",
  path: "/no/such/path",
  ref: "origin/nope",
  name: "the-brief",
  text: "an unserved value",
  command: "/nope",
  url: "not-a-url",
  mode: "no-such-mode",
  reason: "the reason",
  cause: "the cause",
  summary: "the summary",
};

/** A deterministic string for a field the table above does not name. */
const refusalString = (fieldName: string): string => REFUSAL_FACTS[fieldName] ?? `${fieldName}-value`;

/**
 * Build a COMPLETE init for DESC: every field set, the first arm of every
 * oneof chosen, nested messages filled recursively.
 *
 * The client refuses a malformed view by contract, so a refusal the fake
 * serves half-built would fail a test for the wrong reason. Deriving the shape
 * from the descriptor rather than hand-writing ~40 error messages also means a
 * newly landed arm is servable the moment it lands.
 */
function completeInit(desc: DescMessage, depth = 0): Record<string, unknown> {
  const init: Record<string, unknown> = {};
  if (depth > 5) return init;
  const filledOneofs = new Set<string>();
  for (const field of desc.fields) {
    const oneof = field.oneof;
    if (oneof) {
      // The first field of a oneof is the arm the fake picks.
      if (filledOneofs.has(oneof.localName)) continue;
      filledOneofs.add(oneof.localName);
      init[oneof.localName] = { case: field.localName, value: fieldValue(field, depth) };
      continue;
    }
    init[field.localName] = fieldValue(field, depth);
  }
  return init;
}

/** A complete value for one field, by its kind. */
function fieldValue(field: DescField, depth: number): unknown {
  if (field.fieldKind === "list") return [];
  if (field.fieldKind === "map") return {};
  if (field.fieldKind === "message") return completeInit(field.message, depth + 1);
  if (field.fieldKind === "enum") {
    const values = field.enum.values;
    return (values.find((v) => v.number !== 0) ?? values[0])?.number ?? 0;
  }
  switch (field.scalar) {
    case ScalarType.STRING:
      return refusalString(field.localName);
    case ScalarType.BOOL:
      return true;
    case ScalarType.BYTES:
      return new Uint8Array([1]);
    case ScalarType.INT64:
    case ScalarType.UINT64:
    case ScalarType.SINT64:
    case ScalarType.FIXED64:
    case ScalarType.SFIXED64:
      return 1n;
    default:
      return 1;
  }
}

/** The `<Rpc>Error` descriptor and the name of its arm oneof, off the schema. */
function errorShapeOf(rpc: RpcName): { response: DescMessage; error: DescMessage; oneof: string } {
  const response = AgentRepl.method[rpc].output;
  const errorField = response.fields.find((f) => f.localName === "error" && f.fieldKind === "message");
  if (!errorField || errorField.fieldKind !== "message") {
    throw new Error(`${rpc} has no error field on its response`);
  }
  const error = errorField.message;
  // The oneof is spelled `cause` on most rpcs, `kind` on Interrupt and
  // `reason` on SubmitPrompt, so it is READ off the descriptor rather than
  // assumed — a wrong guess would silently build an empty error.
  const oneof = error.oneofs[0]?.localName;
  if (!oneof) throw new Error(`${error.typeName} declares no arm oneof`);
  return { response, error, oneof };
}

/** Every arm name `refuse` accepts for RPC, straight off the schema. */
export function refusalArmsOf(rpc: RpcName): string[] {
  const { error, oneof } = errorShapeOf(rpc);
  const declared = error.oneofs.find((o) => o.localName === oneof);
  return declared ? declared.fields.map((f) => f.localName) : [];
}

/** Build the complete `<Rpc>Response` carrying ARM's typed refusal. */
function buildRefusal(rpc: RpcName, arm: string): unknown {
  const { response, error, oneof } = errorShapeOf(rpc);
  const declared = error.oneofs.find((o) => o.localName === oneof);
  const field = declared?.fields.find((f) => f.localName === arm);
  if (!field || field.fieldKind !== "message") {
    throw new Error(
      `${error.typeName}.${oneof} has no arm ${JSON.stringify(arm)}; it has [${refusalArmsOf(rpc).join(", ")}]`,
    );
  }
  return create(response, {
    result: {
      case: "error",
      value: { [oneof]: { case: arm, value: completeInit(field.message) } },
    },
  });
}

/**
 * Distinguishes one fake daemon's identity url from another's within a worker.
 * A transfer test asserts the successor's address is DRAWN, so two daemons in
 * one test must not share a url.
 */
let daemonOrdinal = 0;
const nextDaemonOrdinal = (): number => (daemonOrdinal += 1);

export function createFakeDaemon(): FakeDaemon {
  const registrations = new Set<Registration>();
  const recorded: RecordedCall[] = [];
  const observed = new Map<RpcName, number>();
  const callWaiters: Array<{ rpc: RpcName; index: number; resolve: (request: unknown) => void }> = [];
  const streamWaiters: Array<{ rpc: RpcName; count: number; resolve: () => void }> = [];
  const streamClosedWaiters: Array<{
    rpc: RpcName;
    workspace?: string;
    feed?: FeedKey;
    resolve: () => void;
  }> = [];

  const footers = new Map<string, FooterView>();
  const topbars = new Map<string, TopbarView>();
  const trays = new Map<string, DaemonHoldTray>();
  const hosts = new Map<string, WatchHostWorkspaceResponse>();
  const pages = new Map<string, FeedPage>();
  const nextPages = new Map<string, FeedPage>();
  const tokens = new Map<string, string[]>();
  /** token value -> the feed it was minted for. WatchFeed refuses anything else. */
  const tokenFeeds = new Map<string, { workspace: string; feed: FeedKey }>();
  /** The pty buffer WatchLoginTerminal replays to a newly attached viewer. */
  const loginScrollback = new Map<string, Uint8Array[]>();
  /** Which workspace each known feed belongs to, for the submission check. */
  const feedOwners = new Map<FeedKey, string>();
  let roster: WorkspaceRoster = emptyRoster();

  const scripted = new Map<RpcName, unknown>();
  const failures = new Map<RpcName, string[]>();
  const unknowns = new Set<RpcName>();
  const unsets = new Set<RpcName>();

  let server: Server | undefined;
  let baseUrl = "";
  let socketPath = "";
  let socketDir = "";
  let mintCounter = 0;

  const countStreams = (rpc: RpcName, workspace?: string, feed?: FeedKey): number => {
    let n = 0;
    for (const reg of registrations) {
      if (reg.rpc !== rpc) continue;
      if (workspace !== undefined && reg.workspace !== workspace) continue;
      if (feed !== undefined && reg.feed !== feed) continue;
      n += 1;
    }
    return n;
  };

  const notifyStreamWaiters = (): void => {
    for (let i = streamWaiters.length - 1; i >= 0; i -= 1) {
      const waiter = streamWaiters[i];
      if (countStreams(waiter.rpc) >= waiter.count) {
        streamWaiters.splice(i, 1);
        waiter.resolve();
      }
    }
  };

  /** Wake every `awaitStreamClosed` waiter whose matching streams have hit zero. */
  const notifyStreamClosedWaiters = (): void => {
    for (let i = streamClosedWaiters.length - 1; i >= 0; i -= 1) {
      const waiter = streamClosedWaiters[i];
      if (countStreams(waiter.rpc, waiter.workspace, waiter.feed) === 0) {
        streamClosedWaiters.splice(i, 1);
        waiter.resolve();
      }
    }
  };

  const record = (rpc: RpcName, request: unknown): void => {
    recorded.push({ rpc, request, atMs: Date.now() });
    const index = recorded.filter((c) => c.rpc === rpc).length - 1;
    for (let i = callWaiters.length - 1; i >= 0; i -= 1) {
      const waiter = callWaiters[i];
      if (waiter.rpc === rpc && waiter.index === index) {
        callWaiters.splice(i, 1);
        waiter.resolve(request);
      }
    }
  };

  /** Throw for a scripted `failNext`, consuming one queued failure. */
  const consumeFailure = (rpc: RpcName): void => {
    const queue = failures.get(rpc);
    if (!queue || queue.length === 0) return;
    const message = queue.shift() as string;
    throw new ConnectError(message, Code.Unavailable);
  };

  /** Build the answer for `rpc`: the scripted one, else a default healthy one. */
  const answerFor = <Desc extends DescMessage>(
    rpc: RpcName,
    schema: Desc,
    fallback: MessageInitShape<Desc>,
  ): never => {
    consumeFailure(rpc);
    const supplied = scripted.get(rpc);
    const message = supplied ?? create(schema, fallback);
    if (unknowns.delete(rpc)) withUnknown(message);
    return message as never;
  };

  /**
   * Open a server stream: register it, hand the caller the async iterable, and
   * deregister on the client's cancellation.
   */
  const openStream = (
    rpc: RpcName,
    workspace: string,
    signal: AbortSignal,
    feed?: FeedKey,
  ): { channel: Channel<unknown>; iterate: () => AsyncGenerator<never> } => {
    const channel = new Channel<unknown>();
    const reg: Registration = { rpc, workspace, feed, channel };
    registrations.add(reg);
    notifyStreamWaiters();
    const iterate = async function* (): AsyncGenerator<never> {
      try {
        yield* channel.iterate(signal) as AsyncGenerator<never>;
      } finally {
        registrations.delete(reg);
        notifyStreamClosedWaiters();
      }
    };
    return { channel, iterate };
  };

  /**
   * Spend a pending `injectUnknown(rpc)` on `message`.
   *
   * The flag is consumed only when a message actually carries it, so arming an
   * injection before the stream is open still taints the FIRST frame that
   * reaches a reader rather than being swallowed by a push nobody received.
   */
  const taint = <T extends object>(rpc: RpcName, message: T): T => {
    const stripped = unsets.delete(rpc) ? (stripperFor(rpc).strip(message) as T) : message;
    return unknowns.delete(rpc) ? withUnknown(stripped) : stripped;
  };

  /** Push one value to every live stream of `rpc` matching workspace/feed. */
  const broadcast = (
    rpc: RpcName,
    workspace: string | undefined,
    feed: FeedKey | undefined,
    value: object,
  ): void => {
    const targets = [...registrations].filter((reg) => {
      if (reg.rpc !== rpc) return false;
      if (workspace !== undefined && reg.workspace !== workspace) return false;
      if (feed !== undefined && reg.feed !== feed) return false;
      return true;
    });
    if (targets.length === 0) return;
    const message = taint(rpc, value);
    for (const reg of targets) reg.channel.push(message);
  };

  // -------------------------------------------------------------------------
  // ONE SOURCE PER STANDING VIEW, SERVED TWO WAYS
  //
  // A browser page multiplexes every standing watch onto `WatchPage`, because
  // it holds about six connections per host over HTTP/1.1 and a
  // server-streaming call pins one for its whole life
  // (endpoint_watch_page.proto). The contract there is that A SUBSCRIPTION IS
  // EXACTLY THE STREAM IT REPLACES: the same request, the same replay, the same
  // pushes, the same refusal.
  //
  // So the fake keeps ONE generator per view and serves it from BOTH the
  // dedicated rpc and `SubscribePage`. A second implementation behind the mux
  // could answer a suite differently from the rpc it stands for, and the
  // suite would be proving the stand-in rather than the app.
  // -------------------------------------------------------------------------

  const rosterSource = (
    request: WatchWorkspaceRosterRequest,
    signal: AbortSignal,
  ): AsyncGenerator<object> => {
    record("watchWorkspaceRoster", request);
    consumeFailure("watchWorkspaceRoster");
    const { channel, iterate } = openStream("watchWorkspaceRoster", "", signal);
    channel.push(taint("watchWorkspaceRoster", create(WatchWorkspaceRosterResponseSchema, { roster })));
    return iterate();
  };

  const webWorkspaceSource = (
    request: WatchWebWorkspaceRequest,
    signal: AbortSignal,
  ): AsyncGenerator<object> => {
    record("watchWebWorkspace", request);
    consumeFailure("watchWebWorkspace");
    const workspace = request.workspace?.id ?? "";
    const { iterate } = openStream("watchWebWorkspace", workspace, signal);
    return iterate();
  };

  const daemonSource = (
    request: WatchDaemonRequest,
    signal: AbortSignal,
  ): AsyncGenerator<object> => {
    record("watchDaemon", request);
    consumeFailure("watchDaemon");
    const { iterate } = openStream("watchDaemon", "", signal);
    return iterate();
  };

  const topbarSource = (
    request: WatchTopbarRequest,
    signal: AbortSignal,
  ): AsyncGenerator<object> => {
    record("watchTopbar", request);
    consumeFailure("watchTopbar");
    const workspace = request.workspace?.id ?? "";
    const { channel, iterate } = openStream("watchTopbar", workspace, signal);
    channel.push(
      taint(
        "watchTopbar",
        create(WatchTopbarResponseSchema, { topbar: topbars.get(workspace) ?? emptyTopbarView() }),
      ),
    );
    return iterate();
  };

  const footerSource = (
    request: WatchFooterRequest,
    signal: AbortSignal,
  ): AsyncGenerator<object> => {
    record("watchFooter", request);
    consumeFailure("watchFooter");
    const workspace = request.workspace?.id ?? "";
    const { channel, iterate } = openStream("watchFooter", workspace, signal);
    channel.push(
      taint(
        "watchFooter",
        create(WatchFooterResponseSchema, { footer: footers.get(workspace) ?? emptyFooterView() }),
      ),
    );
    return iterate();
  };

  const holdsSource = (
    request: WatchDaemonHoldsRequest,
    signal: AbortSignal,
  ): AsyncGenerator<object> => {
    record("watchDaemonHolds", request);
    consumeFailure("watchDaemonHolds");
    const workspace = request.workspace?.id ?? "";
    const { channel, iterate } = openStream("watchDaemonHolds", workspace, signal);
    channel.push(
      taint(
        "watchDaemonHolds",
        create(WatchDaemonHoldsResponseSchema, { tray: trays.get(workspace) ?? emptyTray() }),
      ),
    );
    return iterate();
  };

  const feedSource = (
    request: WatchFeedRequest,
    signal: AbortSignal,
  ): AsyncGenerator<object> => {
    record("watchFeed", request);
    consumeFailure("watchFeed");
    const token = request.watch?.value ?? "";
    const target = tokenFeeds.get(token);
    if (!target) {
      throw new ConnectError(`unknown feed watch token: ${JSON.stringify(token)}`, Code.NotFound);
    }
    const { iterate } = openStream("watchFeed", target.workspace, signal, target.feed);
    return iterate();
  };

  const loginTerminalSource = (
    request: WatchLoginTerminalRequest,
    signal: AbortSignal,
  ): AsyncGenerator<object> => {
    record("watchLoginTerminal", request);
    consumeFailure("watchLoginTerminal");
    const workspace = request.workspace?.id ?? "";
    const { channel, iterate } = openStream("watchLoginTerminal", workspace, signal);
    // The scrollback replays FIRST, exactly as the daemon replays the pty's
    // buffer to a newly attached viewer, before any live byte arrives.
    for (const chunk of loginScrollback.get(workspace) ?? []) {
      channel.push(
        taint(
          "watchLoginTerminal",
          create(LoginTerminalOutputSchema, { output: { case: "bytes", value: { data: chunk } } }),
        ),
      );
    }
    return iterate();
  };

  /**
   * Open the source one `SubscribePage` arm stands for.
   *
   * The switch is exhaustive over the request oneof: a NEW arm in the proto
   * fails to compile here rather than becoming a silently unserved
   * subscription, which is the same rule the client's own mux follows.
   */
  const openSubscriptionSource = (
    request: SubscribePageRequest["request"],
    signal: AbortSignal,
  ): AsyncGenerator<object> => {
    switch (request.case) {
      case "roster":
        return rosterSource(request.value, signal);
      case "webWorkspace":
        return webWorkspaceSource(request.value, signal);
      case "daemon":
        return daemonSource(request.value, signal);
      case "topbar":
        return topbarSource(request.value, signal);
      case "footer":
        return footerSource(request.value, signal);
      case "holds":
        return holdsSource(request.value, signal);
      case "feed":
        return feedSource(request.value, signal);
      case "loginTerminal":
        return loginTerminalSource(request.value, signal);
      case undefined:
        throw new ConnectError(
          "SubscribePageRequest.request is required and no arm was set",
          Code.InvalidArgument,
        );
    }
  };

  // -------------------------------------------------------------------------
  // THE PAGE'S ONE STREAM
  // -------------------------------------------------------------------------

  /** One attached page: its outbound frames and the subscriptions riding them. */
  interface PageState {
    readonly outbound: Channel<WatchPageResponse>;
    /** Every live subscription, by its client-minted id, with its own abort. */
    readonly subscriptions: Map<string, AbortController>;
  }

  const pageStates = new Map<string, PageState>();
  const pageDetachWaiters = new Map<string, Array<() => void>>();

  /** Wake tests waiting for the server-side `WatchPage` finally to finish. */
  const notifyPageDetached = (page: string): void => {
    const waiters = pageDetachWaiters.get(page) ?? [];
    pageDetachWaiters.delete(page);
    for (const resolve of waiters) resolve();
  };

  /**
   * The page a control is about.
   *
   * NAMING NOTHING IS ONLY LEGAL WITH ONE PAGE ATTACHED. A harness mounts one
   * document, so the common case needs no id; two attached pages make the
   * default ambiguous and this THROWS rather than picking one, which would
   * make an assertion about a leak depend on map order.
   */
  const requirePage = (page: string | undefined, control: string): PageState => {
    if (page === undefined) {
      const attached = [...pageStates.keys()];
      if (attached.length !== 1) {
        throw new Error(
          `${control} was given no page and ${attached.length} are attached [${attached.join(", ")}]`,
        );
      }
      page = attached[0];
    }
    const state = pageStates.get(page);
    if (state === undefined) {
      throw new Error(`${control}: no stream is attached for page ${JSON.stringify(page)}`);
    }
    return state;
  };

  /** Wrap one source push as the frame addressing SUBSCRIPTION. */
  const pushFrame = (
    state: PageState,
    subscription: string,
    kind: NonNullable<SubscribePageRequest["request"]["case"]>,
    value: object,
  ): void => {
    state.outbound.push(
      create(WatchPageResponseSchema, {
        frame: {
          case: "push",
          value: create(PageFrameSchema, {
            subscription,
            // The arm and the value are correlated by the source that produced
            // them; the compiler cannot carry that through a generic build of
            // the oneof, and the switch above is what establishes it.
            payload: { case: kind, value } as PageFrame["payload"],
          }),
        },
      }),
    );
  };

  /**
   * Carry one subscription's remaining pushes onto the page's stream, then
   * announce its end.
   *
   * THE CLIENT'S OWN UNSUBSCRIBE ANNOUNCES NOTHING. `unsubscribePage` drops the
   * id from the map BEFORE aborting, so the check here sees a subscription the
   * client already retired and stays quiet — a client that asked for the end
   * does not need to be told, which is the proto's own rule.
   */
  const pumpSubscription = async (
    state: PageState,
    subscription: string,
    kind: NonNullable<SubscribePageRequest["request"]["case"]>,
    source: AsyncGenerator<object>,
    controller: AbortController,
  ): Promise<void> => {
    let ending: MessageInitShape<typeof PageSubscriptionEndedSchema>["how"];
    try {
      for (;;) {
        const step = await source.next();
        if (step.done === true) break;
        pushFrame(state, subscription, kind, step.value);
      }
      ending = { case: "sourceEnded", value: {} };
    } catch (err) {
      // THE DEDICATED RPC'S OWN ERROR, CARRIED AS DATA. A multiplexed
      // subscription has no status of its own to fail with, so the Connect
      // error its stream would have ended with rides an `ended` frame instead
      // and the page's stream stays open for its other subscriptions.
      const connectError = ConnectError.from(err);
      ending = {
        case: "failed",
        value: { code: codeToString(connectError.code), message: connectError.rawMessage },
      };
    } finally {
      if (state.subscriptions.get(subscription) === controller) {
        state.subscriptions.delete(subscription);
        state.outbound.push(endedFrame(subscription, ending));
      }
    }
  };

  /** One `ended` frame, saying HOW the subscription ended. */
  const endedFrame = (
    subscription: string,
    how: MessageInitShape<typeof PageSubscriptionEndedSchema>["how"],
  ): WatchPageResponse =>
    create(WatchPageResponseSchema, {
      frame: {
        case: "ended",
        value: create(PageSubscriptionEndedSchema, { subscription, how }),
      },
    });

  const routes = (router: ConnectRouter): void => {
    router.service(AgentRepl, {
      // ---- the feed -------------------------------------------------------
      submitPrompt(request) {
        record("submitPrompt", request);
        // `origin` and `workspace` are both REQUIRED by the contract, and an
        // unset enum and an absent message are the two shapes a proto3 client
        // can send without noticing. The real daemon rejects both, so the fake
        // does too: a suite that forgets one fails here rather than passing
        // against a lenient stub.
        if (request.origin === PromptOrigin.UNSPECIFIED) {
          throw new ConnectError(
            "SubmitPromptRequest.origin is required and was PROMPT_ORIGIN_UNSPECIFIED",
            Code.InvalidArgument,
          );
        }
        const submitter = request.workspace?.id;
        if (submitter === undefined || submitter === "") {
          throw new ConnectError(
            "SubmitPromptRequest.workspace is required and was absent",
            Code.InvalidArgument,
          );
        }
        // A `feed` addresses a bubble's composer, and a bubble belongs to
        // exactly one workspace. Submitting into another workspace's feed is
        // the identity-space confusion the four-id rule exists to prevent, so
        // it is refused rather than quietly accepted.
        const addressed = request.feed?.value;
        if (addressed !== undefined) {
          const owner = feedOwners.get(addressed);
          if (owner !== undefined && owner !== submitter) {
            throw new ConnectError(
              `SubmitPromptRequest.feed ${JSON.stringify(addressed)} belongs to workspace ` +
                `${JSON.stringify(owner)}, not ${JSON.stringify(submitter)}`,
              Code.InvalidArgument,
            );
          }
        }
        return answerFor("submitPrompt", SubmitPromptResponseSchema, {
          result: {
            case: "success",
            value: { outcome: { case: "turn", value: { turn: { value: "turn-1" } } } },
          },
        });
      },
      openFeed(request) {
        record("openFeed", request);
        consumeFailure("openFeed");
        const workspace = request.workspace?.id ?? "";
        const feed: FeedKey = request.feed?.value ?? ROOT_FEED;
        mintCounter += 1;
        const token = `watch-${mintCounter}`;
        const minted = tokens.get(key(workspace, feed)) ?? [];
        minted.push(token);
        tokens.set(key(workspace, feed), minted);
        tokenFeeds.set(token, { workspace, feed });
        feedOwners.set(feed, workspace);
        const message =
          scripted.get("openFeed") ??
          create(OpenFeedResponseSchema, {
            result: {
              case: "success",
              value: {
                page: pages.get(key(workspace, feed)) ?? emptyFeedPage(),
                watch: create(FeedWatchTokenSchema, { value: token }),
              },
            },
          });
        if (unknowns.delete("openFeed")) withUnknown(message);
        return message;
      },
      async *watchFeed(request, context) {
        yield* feedSource(request, context.signal) as AsyncGenerator<WatchFeedResponse>;
      },
      getFeedPage(request) {
        record("getFeedPage", request);
        consumeFailure("getFeedPage");
        const workspace = request.workspace?.id ?? "";
        const feed: FeedKey = request.feed?.value ?? ROOT_FEED;
        const wanted =
          request.page.case === "next"
            ? nextPages.get(key(workspace, feed)) ?? pages.get(key(workspace, feed))
            : pages.get(key(workspace, feed));
        const message =
          scripted.get("getFeedPage") ??
          create(GetFeedPageResponseSchema, {
            result: { case: "success", value: wanted ?? emptyFeedPage() },
          });
        if (unknowns.delete("getFeedPage")) withUnknown(message);
        return message;
      },
      interrupt(request) {
        record("interrupt", request);
        return answerFor("interrupt", InterruptResponseSchema, {
          result: { case: "success", value: { outcome: { case: "interruptedTurn", value: {} } } },
        });
      },
      answerPermission(request) {
        record("answerPermission", request);
        return answerFor("answerPermission", AnswerPermissionResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      answerQuestion(request) {
        record("answerQuestion", request);
        return answerFor("answerQuestion", AnswerQuestionResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      answerColdGate(request) {
        record("answerColdGate", request);
        return answerFor("answerColdGate", AnswerColdGateResponseSchema, {
          result: { case: "success", value: {} },
        });
      },

      // ---- the sidebar ----------------------------------------------------
      async *watchWorkspaceRoster(request, context) {
        yield* rosterSource(request, context.signal) as AsyncGenerator<WatchWorkspaceRosterResponse>;
      },
      createWorkspace(request) {
        record("createWorkspace", request);
        return answerFor("createWorkspace", CreateWorkspaceResponseSchema, {
          result: {
            case: "success",
            value: { workspace: { id: "ws-created", dir: "/tmp/ws-created" } },
          },
        });
      },
      openWorkspace(request) {
        record("openWorkspace", request);
        return answerFor("openWorkspace", OpenWorkspaceResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      closeWorkspace(request) {
        record("closeWorkspace", request);
        return answerFor("closeWorkspace", CloseWorkspaceResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      killWorkspace(request) {
        record("killWorkspace", request);
        return answerFor("killWorkspace", KillWorkspaceResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      nukeWorkspace(request) {
        record("nukeWorkspace", request);
        return answerFor("nukeWorkspace", NukeWorkspaceResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      mergeWorkspace(request) {
        record("mergeWorkspace", request);
        return answerFor("mergeWorkspace", MergeWorkspaceResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      restartWorkspace(request) {
        record("restartWorkspace", request);
        return answerFor("restartWorkspace", RestartWorkspaceResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      setWorkspacePriority(request) {
        record("setWorkspacePriority", request);
        return answerFor("setWorkspacePriority", SetWorkspacePriorityResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      createTask(request) {
        record("createTask", request);
        return answerFor("createTask", CreateTaskResponseSchema, {
          result: { case: "success", value: { task: { id: "task-created" } } },
        });
      },
      updateTask(request) {
        record("updateTask", request);
        return answerFor("updateTask", UpdateTaskResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      assignWorkspaceTask(request) {
        record("assignWorkspaceTask", request);
        return answerFor("assignWorkspaceTask", AssignWorkspaceTaskResponseSchema, {
          result: { case: "success", value: {} },
        });
      },

      // ---- topbar / footer / tray -----------------------------------------
      async *watchTopbar(request, context) {
        yield* topbarSource(request, context.signal) as AsyncGenerator<WatchTopbarResponse>;
      },
      setModel(request) {
        record("setModel", request);
        return answerFor("setModel", SetModelResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      setPermissionMode(request) {
        record("setPermissionMode", request);
        return answerFor("setPermissionMode", SetPermissionModeResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      selectAccount(request) {
        record("selectAccount", request);
        // THE ANSWER IS READ OFF THE SERVED OPTIONS, never a constant.
        // `SelectAccountSuccess.logged_in` is the client's cue to open that
        // root's login flow, so a stub that always said one thing would make
        // one of the two branches behind that cue untestable, and a root the
        // view never offered is `unknown_account` exactly as the daemon has
        // it — the client may only echo a `config_dir` it was served.
        const option = topbars
          .get(request.workspace?.id ?? "")
          ?.account?.options.find((served) => served.configDir === request.configDir);
        return answerFor(
          "selectAccount",
          SelectAccountResponseSchema,
          option === undefined
            ? { result: { case: "error", value: { cause: { case: "unknownAccount", value: {} } } } }
            : {
                result: {
                  case: "success",
                  value: { loggedIn: option.state.case === "loggedIn" },
                },
              },
        );
      },
      async *watchFooter(request, context) {
        yield* footerSource(request, context.signal) as AsyncGenerator<WatchFooterResponse>;
      },
      async *watchDaemonHolds(request, context) {
        yield* holdsSource(request, context.signal) as AsyncGenerator<WatchDaemonHoldsResponse>;
      },
      updateHeldPrompt(request) {
        record("updateHeldPrompt", request);
        return answerFor("updateHeldPrompt", UpdateHeldPromptResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      editHeldPrompt(request) {
        record("editHeldPrompt", request);
        return answerFor("editHeldPrompt", EditHeldPromptResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      answerHeldOffer(request) {
        record("answerHeldOffer", request);
        return answerFor("answerHeldOffer", AnswerHeldOfferResponseSchema, {
          result: { case: "success", value: {} },
        });
      },

      // ---- admin / diagnostics --------------------------------------------
      updateShutdownSchedule(request) {
        record("updateShutdownSchedule", request);
        return answerFor("updateShutdownSchedule", UpdateShutdownScheduleResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      updateMergeQueue(request) {
        record("updateMergeQueue", request);
        return answerFor("updateMergeQueue", UpdateMergeQueueResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      daemonHealth(request) {
        record("daemonHealth", request);
        return answerFor("daemonHealth", DaemonHealthResponseSchema, {
          result: { case: "success", value: { health: { case: "healthy", value: {} } } },
        });
      },
      sessionHealth(request) {
        record("sessionHealth", request);
        return answerFor("sessionHealth", SessionHealthResponseSchema, {
          result: { case: "success", value: { health: { case: "healthy", value: {} } } },
        });
      },
      clientLog(request) {
        record("clientLog", request);
        return answerFor("clientLog", ClientLogResponseSchema, {
          result: { case: "success", value: {} },
        });
      },

      // ---- host section (Emacs's, served so the fake is complete) ----------
      registerWorkspace(request) {
        record("registerWorkspace", request);
        return answerFor("registerWorkspace", RegisterWorkspaceResponseSchema, {
          result: {
            case: "success",
            value: { workspace: { id: "ws-registered", dir: "/tmp/ws-registered" } },
          },
        });
      },
      selectWorkspace(request) {
        record("selectWorkspace", request);
        return answerFor("selectWorkspace", SelectWorkspaceResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      async *watchHostWorkspace(request, context) {
        record("watchHostWorkspace", request);
        consumeFailure("watchHostWorkspace");
        const workspace = request.workspace?.id ?? "";
        const { channel, iterate } = openStream("watchHostWorkspace", workspace, context.signal);
        channel.push(taint("watchHostWorkspace", hosts.get(workspace) ?? hostWorkspacePush()));
        yield* iterate();
      },
      async *watchDaemon(request, context) {
        yield* daemonSource(request, context.signal) as AsyncGenerator<WatchDaemonResponse>;
      },
      adoptHostWorkspace(request) {
        record("adoptHostWorkspace", request);
        return answerFor("adoptHostWorkspace", AdoptHostWorkspaceResponseSchema, {
          result: { case: "success", value: {} },
        });
      },

      // ---- the page's one stream ------------------------------------------
      //
      // A page holds ONE connection and multiplexes every standing watch onto
      // it. These three verbs are what make that possible, and the fake serves
      // them from the same per-view sources the dedicated rpcs use, so a
      // subscription replays exactly what its own rpc replays.
      async *watchPage(request, context) {
        record("watchPage", request);
        consumeFailure("watchPage");
        const page = request.page;
        // TWO STREAMS FOR ONE PAGE ID IS A REFUSAL, not a replacement: the
        // second would leave the first's subscriptions addressable by a client
        // that no longer owns them.
        if (pageStates.has(page)) {
          throw new ConnectError(
            `page ${JSON.stringify(page)} already holds a stream`,
            Code.AlreadyExists,
          );
        }
        const state: PageState = {
          outbound: new Channel<WatchPageResponse>(),
          subscriptions: new Map(),
        };
        pageStates.set(page, state);
        // THE LATCH GOES FIRST AND EXACTLY ONCE. `SubscribePage` names a page
        // that must already be attached, so the client may not subscribe until
        // it has read this frame.
        state.outbound.push(
          create(WatchPageResponseSchema, {
            frame: { case: "attached", value: create(PageAttachedSchema, {}) },
          }),
        );
        try {
          yield* state.outbound.iterate(context.signal);
        } finally {
          // EVERY SUBSCRIPTION DIES WITH THE STREAM THAT CARRIED IT. Nothing
          // can address them any more, so leaving them running would be work
          // for a reader that no longer exists.
          for (const [, controller] of state.subscriptions) controller.abort();
          state.subscriptions.clear();
          if (pageStates.get(page) === state) {
            pageStates.delete(page);
            notifyPageDetached(page);
          }
        }
      },
      subscribePage(request) {
        record("subscribePage", request);
        consumeFailure("subscribePage");
        const state = pageStates.get(request.page);
        if (state === undefined) {
          throw new ConnectError(
            `no stream is attached for page ${JSON.stringify(request.page)}`,
            Code.FailedPrecondition,
          );
        }
        if (state.subscriptions.has(request.subscription)) {
          throw new ConnectError(
            `subscription ${JSON.stringify(request.subscription)} is already live on this page`,
            Code.AlreadyExists,
          );
        }
        const kind = request.request.case;
        if (kind === undefined) {
          throw new ConnectError(
            "SubscribePageRequest.request is required and no arm was set",
            Code.InvalidArgument,
          );
        }
        const controller = new AbortController();
        // THE SUBSCRIPTION EXISTS BEFORE THE ANSWER DOES, and that is why this
        // call is not an `await`. Each source registers with its publisher and
        // queues its replay SYNCHRONOUSLY — the same shape `openStream` has —
        // so a view published after this reply cannot be missed, and a view
        // that replays NOTHING (`daemon`, `webWorkspace`) still answers rather
        // than parking this unary on a first frame that may never come.
        //
        // A source that REFUSES (a scripted failure, an unknown feed token)
        // throws from here and refuses THIS call, exactly as the dedicated
        // rpc's own open would have.
        const source = openSubscriptionSource(request.request, controller.signal);
        state.subscriptions.set(request.subscription, controller);
        void pumpSubscription(state, request.subscription, kind, source, controller);
        return {};
      },
      unsubscribePage(request) {
        record("unsubscribePage", request);
        const state = pageStates.get(request.page);
        const controller = state?.subscriptions.get(request.subscription);
        // ENDING ONE THAT DOES NOT EXIST IS NOT A REFUSAL: the end is the state
        // the caller asked for, and a subscription already gone is that state.
        if (state !== undefined && controller !== undefined) {
          // Dropped BEFORE the abort, so the pump does not also announce this
          // ending as the source's own. THE END IS ONE FACT ON ONE WIRE
          // whoever asked for it, so the `unsubscribed` arm is announced here
          // rather than suppressed — the client's own bookkeeping is what
          // makes it harmless, not the daemon's silence.
          state.subscriptions.delete(request.subscription);
          controller.abort();
          state.outbound.push(endedFrame(request.subscription, { case: "unsubscribed", value: {} }));
        }
        return {};
      },

      // ---- the web link ---------------------------------------------------
      async *watchWebWorkspace(request, context) {
        yield* webWorkspaceSource(request, context.signal) as AsyncGenerator<WatchWebWorkspaceResponse>;
      },
      adoptWebWorkspace(request) {
        record("adoptWebWorkspace", request);
        return answerFor("adoptWebWorkspace", AdoptWebWorkspaceResponseSchema, {
          result: { case: "success", value: {} },
        });
      },

      // ---- the login pty --------------------------------------------------
      openLogin(request) {
        record("openLogin", request);
        return answerFor("openLogin", OpenLoginResponseSchema, {
          result: { case: "success", value: { configDir: "/tmp/config" } },
        });
      },
      async *watchLoginTerminal(request, context) {
        yield* loginTerminalSource(request, context.signal) as AsyncGenerator<LoginTerminalOutput>;
      },
      sendLoginInput(request) {
        record("sendLoginInput", request);
        return answerFor("sendLoginInput", SendLoginInputResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      requestCommandSupport(request) {
        record("requestCommandSupport", request);
        return answerFor("requestCommandSupport", RequestCommandSupportResponseSchema, {
          result: {
            case: "success",
            value: { workspace: { id: "ws-support", dir: "/tmp/ws-support" } },
          },
        });
      },
      openInEditor(request) {
        record("openInEditor", request);
        return answerFor("openInEditor", OpenInEditorResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      closeLogin(request) {
        record("closeLogin", request);
        return answerFor("closeLogin", CloseLoginResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
      openExternal(request) {
        record("openExternal", request);
        return answerFor("openExternal", OpenExternalResponseSchema, {
          result: { case: "success", value: {} },
        });
      },
    });
  };

  return {
    async start() {
      const handler = connectNodeAdapter({ routes });
      const listener = createServer((req, res) => {
        flushHeadersOnAccept(req, res);
        handler(req, res);
      });
      // A UNIX SOCKET, NOT A LOOPBACK PORT, and that is the whole point.
      //
      // Every daemon used to be a fresh 127.0.0.1 listener, so every request
      // the app made burned an ephemeral port that then sat in TIME_WAIT for
      // an MSL. A run is ~1600 tests, each booting a daemon and opening
      // several standing streams; a few of those runs at once walked the
      // 49152-65535 range dry and `connect` started answering EADDRNOTAVAIL,
      // which reaches the page as a bare "fetch failed" and fails the boot
      // adoption. A unix socket consumes no port at all, so that exhaustion
      // is not merely unlikely here — it is unrepresentable.
      socketDir = mkdtempSync(join(tmpdir(), "agent-repl-fake-"));
      socketPath = join(socketDir, "d.sock");
      await new Promise<void>((resolve) => listener.listen(socketPath, resolve));
      server = listener;
      baseUrl = `http://fake-daemon-${nextDaemonOrdinal()}.invalid`;
      return { baseUrl, socketPath };
    },
    async stop() {
      for (const reg of registrations) reg.channel.end();
      registrations.clear();
      const listener = server;
      server = undefined;
      if (!listener) return;
      listener.closeAllConnections();
      await new Promise<void>((resolve, reject) =>
        listener.close((error) => (error ? reject(error) : resolve())),
      );
      // The socket file outlives close(); a run leaks ~1600 of them otherwise.
      if (socketDir) rmSync(socketDir, { recursive: true, force: true });
      socketDir = "";
      socketPath = "";
    },
    get baseUrl() {
      if (!baseUrl) throw new Error("fake daemon: start() has not resolved");
      return baseUrl;
    },
    get socketPath() {
      if (!socketPath) throw new Error("fake daemon: start() has not resolved");
      return socketPath;
    },

    setFooter(workspace, view) {
      footers.set(workspace, view);
      broadcast("watchFooter", workspace, undefined, create(WatchFooterResponseSchema, { footer: view }));
    },
    setTopbar(workspace, view) {
      topbars.set(workspace, view);
      broadcast("watchTopbar", workspace, undefined, create(WatchTopbarResponseSchema, { topbar: view }));
    },
    setRoster(next) {
      roster = next;
      broadcast(
        "watchWorkspaceRoster",
        undefined,
        undefined,
        create(WatchWorkspaceRosterResponseSchema, { roster: next }),
      );
    },
    setTray(workspace, tray) {
      trays.set(workspace, tray);
      broadcast("watchDaemonHolds", workspace, undefined, create(WatchDaemonHoldsResponseSchema, { tray }));
    },
    setHostWorkspace(workspace, push) {
      hosts.set(workspace, push);
      broadcast("watchHostWorkspace", workspace, undefined, push);
    },

    setPage(workspace, feed, page) {
      pages.set(key(workspace, feed), page);
      feedOwners.set(feed, workspace);
    },
    setNextPage(workspace, feed, page) {
      nextPages.set(key(workspace, feed), page);
    },
    pushRow(workspace, feed, row) {
      broadcast("watchFeed", workspace, feed, create(WatchFeedResponseSchema, { row }));
    },
    mintedTokens(workspace, feed) {
      return [...(tokens.get(key(workspace, feed)) ?? [])];
    },

    pushSessionIdentity(workspace, identity) {
      broadcast(
        "watchWebWorkspace",
        workspace,
        undefined,
        create(WatchWebWorkspaceResponseSchema, {
          push: {
            case: "sessionIdentity",
            value: {
              agentReplSessionId: identity.agentReplSessionId,
              claudeSessionId: identity.claudeSessionId ?? "",
            },
          },
        }),
      );
    },
    transfer(workspace, address) {
      broadcast(
        "watchWebWorkspace",
        workspace,
        undefined,
        create(WatchWebWorkspaceResponseSchema, { push: { case: "transferred", value: { address } } }),
      );
    },
    announceShutdown(init) {
      broadcast("watchDaemon", undefined, undefined, shutdownAnnounced(init));
    },
    scheduleDrain(atMs, reason) {
      broadcast(
        "watchDaemon",
        undefined,
        undefined,
        create(WatchDaemonResponseSchema, { push: { case: "drainScheduled", value: { atMs, reason } } }),
      );
    },
    cancelDrain() {
      broadcast(
        "watchDaemon",
        undefined,
        undefined,
        create(WatchDaemonResponseSchema, { push: { case: "drainCancelled", value: {} } }),
      );
    },
    pushMutationProgress(opId) {
      broadcast(
        "watchDaemon",
        undefined,
        undefined,
        create(WatchDaemonResponseSchema, {
          push: { case: "mutationProgress", value: { opId } },
        }),
      );
    },

    setLoginScrollback(workspace, chunks) {
      loginScrollback.set(workspace, chunks);
    },
    pushLoginBytes(workspace, data) {
      broadcast(
        "watchLoginTerminal",
        workspace,
        undefined,
        create(LoginTerminalOutputSchema, { output: { case: "bytes", value: { data } } }),
      );
    },
    closeLoginTerminal(workspace) {
      broadcast(
        "watchLoginTerminal",
        workspace,
        undefined,
        create(LoginTerminalOutputSchema, { output: { case: "closed", value: {} } }),
      );
      for (const reg of [...registrations]) {
        if (reg.rpc !== "watchLoginTerminal" || reg.workspace !== workspace) continue;
        registrations.delete(reg);
        reg.channel.end();
      }
      notifyStreamClosedWaiters();
    },

    answer(rpc, response) {
      scripted.set(rpc, response);
    },
    failNext(rpc, errorMessage) {
      const queue = failures.get(rpc) ?? [];
      queue.push(errorMessage);
      failures.set(rpc, queue);
    },
    refuse(rpc, arm) {
      scripted.set(rpc, buildRefusal(rpc, arm));
    },
    refusalArms(rpc) {
      return refusalArmsOf(rpc);
    },
    injectUnknown(rpc) {
      unknowns.add(rpc);
    },
    injectUnsetField(rpc) {
      // Resolved EAGERLY so an rpc with no stripper fails at the arrange step,
      // where the test can see it, rather than inside a push nobody awaits.
      stripperFor(rpc);
      unsets.add(rpc);
    },
    unsetFieldPath(rpc) {
      return stripperFor(rpc).path;
    },

    calls<Req>(rpc: RpcName): Req[] {
      return recorded.filter((c) => c.rpc === rpc).map((c) => c.request as Req);
    },
    nextCall<Req>(rpc: RpcName): Promise<Req> {
      const index = observed.get(rpc) ?? 0;
      observed.set(rpc, index + 1);
      const already = recorded.filter((c) => c.rpc === rpc);
      if (already.length > index) return Promise.resolve(already[index].request as Req);
      return new Promise<Req>((resolve) =>
        callWaiters.push({ rpc, index, resolve: resolve as (r: unknown) => void }),
      );
    },
    log() {
      return [...recorded];
    },
    clearCalls() {
      recorded.length = 0;
      observed.clear();
    },

    liveStreams(rpc, workspace, feed) {
      return countStreams(rpc, workspace, feed);
    },
    endStream(rpc, workspace, feed) {
      for (const reg of [...registrations]) {
        if (reg.rpc !== rpc) continue;
        if (workspace !== undefined && reg.workspace !== workspace) continue;
        if (feed !== undefined && reg.feed !== feed) continue;
        registrations.delete(reg);
        reg.channel.end();
      }
      notifyStreamClosedWaiters();
    },
    awaitStream(rpc, count = 1) {
      if (countStreams(rpc) >= count) return Promise.resolve();
      return new Promise<void>((resolve) => streamWaiters.push({ rpc, count, resolve }));
    },
    awaitStreamClosed(rpc, workspace, feed) {
      if (countStreams(rpc, workspace, feed) === 0) return Promise.resolve();
      return new Promise<void>((resolve) => streamClosedWaiters.push({ rpc, workspace, feed, resolve }));
    },
    attachedPages() {
      return [...pageStates.keys()];
    },
    awaitPageDetached(page) {
      if (!pageStates.has(page)) return Promise.resolve();
      return new Promise<void>((resolve) => {
        const waiters = pageDetachWaiters.get(page) ?? [];
        waiters.push(resolve);
        pageDetachWaiters.set(page, waiters);
      });
    },
    pageSubscriptions(page) {
      return [...requirePage(page, "pageSubscriptions").subscriptions.keys()];
    },
    endPageStream(page) {
      requirePage(page, "endPageStream").outbound.end();
    },
  };
}
