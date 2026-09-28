/**
 * test/integration-support/client.ts — shim.v1 clients, and the request/stream
 * vocabulary the suites share.
 *
 * # Both dialects, one socket
 *
 * The shim serves HTTP/1.1 and h2c on the SAME unix socket (`service/server.ts`
 * sniffs the HTTP/2 preface). The two are reached differently and the
 * difference is a trap worth encoding once: HTTP/1.1 takes `socketPath`, while
 * http2 IGNORES `socketPath` silently and needs `createConnection`. A test that
 * passed `socketPath` for h2 would dial `http://shim:80` over TCP and fail with
 * a DNS error that says nothing about the shim.
 *
 * # Streams are pulled, never awaited whole
 *
 * A standing stream never ends, so `for await` over one never returns. Every
 * stream here is wrapped in {@link Stream}, which pulls ONE frame at a time and
 * cancels the call through an `AbortController` at close — the same shape the
 * daemon uses, and the only one that terminates.
 */
import { create, type DescMessage, type MessageInitShape } from "@bufbuild/protobuf";
import { Code, ConnectError, createClient, type Client } from "@connectrpc/connect";
import { createConnectTransport } from "@connectrpc/connect-node";
import { connect as netConnect } from "node:net";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { FAKE_DEFAULT_MODEL } from "../../src/fake/catalogs.js";

/**
 * The model every request names by default.
 *
 * The MOCK's own catalog, not a plausible-looking real model id: a session
 * started on a model the catalog does not carry would be refused
 * `model_not_in_catalog`, and every test would then be exercising that refusal
 * by accident.
 */
export const DEFAULT_MODEL = FAKE_DEFAULT_MODEL;

/** A model id no catalog carries — the `model_not_in_catalog` subject. */
export const UNCATALOGED_MODEL = "model-nobody-offers";

/** A shim.v1 client. */
export type ShimClient = Client<typeof shimv1.Shim>;

/** The one socket, reached in both dialects. */
export interface ShimClients {
  /** Connect over HTTP/1.1 (`nodeOptions.socketPath`). */
  readonly h1: ShimClient;
  /** Connect over h2c (`nodeOptions.createConnection`). */
  readonly h2: ShimClient;
}

/** Build both clients for one shim socket. */
export function createShimClients(socketPath: string): ShimClients {
  return {
    h1: createClient(
      shimv1.Shim,
      createConnectTransport({
        httpVersion: "1.1",
        baseUrl: "http://shim",
        nodeOptions: { socketPath },
      }),
    ),
    h2: createClient(
      shimv1.Shim,
      createConnectTransport({
        httpVersion: "2",
        baseUrl: "http://shim",
        // socketPath is silently ignored by http2.connect; createConnection is
        // the only way to point an h2 session at a unix socket.
        nodeOptions: { createConnection: () => netConnect(socketPath) },
      }),
    ),
  };
}

// ---------------------------------------------------------------------------
// stream handling
// ---------------------------------------------------------------------------

/** The end of a bounded stream, as a value rather than an exception. */
export const STREAM_END = Symbol("stream-end");

/** A pulled server stream with a cancellable call behind it. */
export class Stream<T> {
  private readonly iterator: AsyncIterator<T>;
  private readonly seen: T[] = [];
  private ended = false;

  constructor(
    iterable: AsyncIterable<T>,
    private readonly controller: AbortController,
  ) {
    this.iterator = iterable[Symbol.asyncIterator]();
  }

  /** Every frame pulled so far, in order. */
  frames(): T[] {
    return [...this.seen];
  }

  /** Whether the producer has concluded the stream. */
  isEnded(): boolean {
    return this.ended;
  }

  /** The next frame, or {@link STREAM_END} when the producer concluded. */
  async nextOrEnd(): Promise<T | typeof STREAM_END> {
    const result = await this.iterator.next();
    if (result.done === true) {
      this.ended = true;
      return STREAM_END;
    }
    this.seen.push(result.value);
    return result.value;
  }

  /** The next frame; a concluded stream is a failure, not an absence. */
  async next(): Promise<T> {
    const frame = await this.nextOrEnd();
    if (frame === STREAM_END) {
      throw new Error("the stream concluded while a frame was expected");
    }
    return frame;
  }

  /** Pull until `predicate` matches, returning the matching frame. */
  async until(predicate: (frame: T) => boolean): Promise<T> {
    for (;;) {
      const frame = await this.next();
      if (predicate(frame)) return frame;
    }
  }

  /** Pull until the producer concludes, returning every frame it sent. */
  async drain(): Promise<T[]> {
    for (;;) {
      const frame = await this.nextOrEnd();
      if (frame === STREAM_END) return this.frames();
    }
  }

  /**
   * Cancel the call.
   *
   * Cancelling and never merely closing: connect's stream close DRAINS the
   * body, which on a standing stream never finishes (the standing-stream
   * transport ruling).
   */
  close(): void {
    this.controller.abort();
  }
}

/**
 * A WatchSession stream with the `session_started` RE-ANNOUNCEMENT dropped.
 *
 * The re-announcement is sent once per watch, right after the opening
 * diagnostics (landing 7). A suite reading session FACTS is not reading it, and
 * a helper that let it through would make every such suite restate the frame
 * order. `test/integration/session.test.ts` asserts the re-announcement itself
 * through the unfiltered stream.
 */
export function openSessionUpdates(
  open: (options: { signal: AbortSignal }) => AsyncIterable<shimv1.WatchSessionResponse>,
): Stream<shimv1.WatchSessionResponse> {
  const controller = new AbortController();
  // THE CALL IS MADE HERE, NOT INSIDE THE GENERATOR. A generator body does not
  // run until its first `next()`, and a watch that only subscribed then would
  // miss every fact pushed between the open and the first pull — which is
  // exactly what a suite that opens its watch before prompting relies on.
  const source = open({ signal: controller.signal });
  return new Stream(
    (async function* filtered(): AsyncIterable<shimv1.WatchSessionResponse> {
      for await (const frame of source) {
        if (frame.frame.case === "sessionStarted") continue;
        yield frame;
      }
    })(),
    controller,
  );
}

/** Open a server stream through `open`, wired to its own abort controller. */
export function openStream<T>(
  open: (options: { signal: AbortSignal }) => AsyncIterable<T>,
): Stream<T> {
  const controller = new AbortController();
  return new Stream(open({ signal: controller.signal }), controller);
}

/**
 * The first history entry matching `predicate` on a JUST-OPENED WatchAgent
 * stream, searched on the OPENING PAGE and then on the tail.
 *
 * A row written before the watch opened is on the PAGE, and the shim never
 * serves it again as a tail entry (the store pins the tail at the open). So a
 * wait on the tail alone is a race against the store writer rather than a wait
 * on the row: whenever the writer's batch lands before `OpenAgentSession`, the
 * row is on the page and the tail wait never returns.
 *
 * The stream must not have been pulled yet, because its next frame has to be
 * the page; a stream whose page was already consumed refuses loudly here
 * instead of silently tailing past it.
 */
export async function awaitAgentEntry(
  stream: Stream<shimv1.WatchAgentResponse>,
  predicate: (entry: conversationv1.HistoryEntryAt) => boolean,
): Promise<conversationv1.HistoryEntryAt> {
  const opening = await stream.next();
  if (opening.frame.case !== "page") {
    throw new Error(
      `awaitAgentEntry: expected the opening page, got ${opening.frame.case ?? "an unset oneof"}`,
    );
  }
  const onPage = opening.frame.value.entries.find(predicate);
  if (onPage !== undefined) return onPage;
  for (;;) {
    const frame = await stream.next();
    if (frame.frame.case === "entry" && predicate(frame.frame.value)) return frame.frame.value;
  }
}

// ---------------------------------------------------------------------------
// refusal helpers
// ---------------------------------------------------------------------------

/** The Connect code an rpc refused with, or `"resolved"` when it did not. */
export async function connectCode(call: Promise<unknown>): Promise<Code | "resolved"> {
  try {
    await call;
    return "resolved";
  } catch (err) {
    return ConnectError.from(err).code;
  }
}

/** The Connect code a STREAM OPEN refused with, or `"resolved"` when it did not. */
export async function streamOpenCode<T>(stream: Stream<T>): Promise<Code | "resolved"> {
  try {
    await stream.nextOrEnd();
    return "resolved";
  } catch (err) {
    return ConnectError.from(err).code;
  }
}

// ---------------------------------------------------------------------------
// request vocabulary
// ---------------------------------------------------------------------------

/** `conversation.v1.AgentModel` by name. */
export function model(name: string): conversationv1.AgentModel {
  return create(conversationv1.AgentModelSchema, { name });
}

/** The permission-mode arms, by their oneof field name. */
export type PermissionModeArm =
  | "default"
  | "acceptEdits"
  | "bypass"
  | "plan"
  | "dontAsk"
  | "auto";

const PERMISSION_MODE_SCHEMAS: Record<PermissionModeArm, DescMessage> = {
  default: conversationv1.AgentPermissionModeDefaultSchema,
  acceptEdits: conversationv1.AgentPermissionModeAcceptEditsSchema,
  bypass: conversationv1.AgentPermissionModeBypassSchema,
  plan: conversationv1.AgentPermissionModePlanSchema,
  dontAsk: conversationv1.AgentPermissionModeDontAskSchema,
  auto: conversationv1.AgentPermissionModeAutoSchema,
};

/** `conversation.v1.AgentPermissionMode` with the named arm set. */
export function permissionMode(arm: PermissionModeArm): conversationv1.AgentPermissionMode {
  return create(conversationv1.AgentPermissionModeSchema, {
    mode: { case: arm, value: create(PERMISSION_MODE_SCHEMAS[arm], {}) },
  } as MessageInitShape<typeof conversationv1.AgentPermissionModeSchema>);
}

/** A fresh-start request. */
export function freshSession(
  modelName: string = DEFAULT_MODEL,
  mode: PermissionModeArm = "default",
): shimv1.StartSessionRequest {
  return create(shimv1.StartSessionRequestSchema, {
    source: {
      case: "fresh",
      value: create(shimv1.StartSessionFreshSchema, {
        model: model(modelName),
        permissionMode: permissionMode(mode),
      }),
    },
  });
}

/** A resume request, optionally naming a cold remediation. */
export function resumeSession(
  vendorSessionId: string,
  coldRemediation?: conversationv1.SessionColdRemediation,
): shimv1.StartSessionRequest {
  return create(shimv1.StartSessionRequestSchema, {
    source: {
      case: "resume",
      value: create(shimv1.StartSessionResumeSchema, {
        vendorSessionId,
        ...(coldRemediation === undefined ? {} : { coldRemediation }),
      }),
    },
  });
}

/** `SessionColdRemediation{pay}`. */
export function remediationPay(): conversationv1.SessionColdRemediation {
  return create(conversationv1.SessionColdRemediationSchema, {
    remediation: { case: "pay", value: create(conversationv1.SessionColdPaySchema, {}) },
  });
}

/** `SessionColdRemediation{clear}`. */
export function remediationClear(): conversationv1.SessionColdRemediation {
  return create(conversationv1.SessionColdRemediationSchema, {
    remediation: { case: "clear", value: create(conversationv1.SessionColdClearSchema, {}) },
  });
}

/** `SessionColdRemediation{compact}`. */
export function remediationCompact(
  modelName: string = DEFAULT_MODEL,
  scope: conversationv1.SessionCompactScope = conversationv1.SessionCompactScope.ALL,
): conversationv1.SessionColdRemediation {
  return create(conversationv1.SessionColdRemediationSchema, {
    remediation: {
      case: "compact",
      value: create(conversationv1.SessionColdCompactSchema, { model: model(modelName), scope }),
    },
  });
}

/** `conversation.v1.UserSaid` carrying one text block. */
export function said(text: string): conversationv1.UserSaid {
  return create(conversationv1.UserSaidSchema, {
    content: create(conversationv1.UserContentSchema, {
      blocks: [
        create(conversationv1.UserContentBlockSchema, {
          block: { case: "text", value: create(conversationv1.TextBlockSchema, { text }) },
        }),
      ],
    }),
  });
}

/** `conversation.v1.TurnId`. */
export function turnId(value: string): conversationv1.TurnId {
  return create(conversationv1.TurnIdSchema, { value });
}

/** `conversation.v1.AgentId`. */
export function agentId(value: string): conversationv1.AgentId {
  return create(conversationv1.AgentIdSchema, { value });
}

/** `conversation.v1.AgentActivityId`. */
export function activityId(value: string): conversationv1.AgentActivityId {
  return create(conversationv1.AgentActivityIdSchema, { value });
}

/** `conversation.v1.DetachedWorkId`. */
export function workId(value: string): conversationv1.DetachedWorkId {
  return create(conversationv1.DetachedWorkIdSchema, { value });
}

/** `conversation.v1.HistoryPointer`. */
export function pointer(value: string): conversationv1.HistoryPointer {
  return create(conversationv1.HistoryPointerSchema, { value });
}

/** A StartTurn request; `pageSize` defaults to a legal, non-zero budget. */
export function startTurnRequest(init: {
  readonly turn: string;
  readonly text: string;
  readonly pageSize?: number;
  readonly origin?: conversationv1.PromptOrigin;
  readonly knownThrough?: conversationv1.HistoryPointer;
}): shimv1.StartTurnRequest {
  return create(shimv1.StartTurnRequestSchema, {
    turn: turnId(init.turn),
    said: said(init.text),
    origin: init.origin ?? conversationv1.PromptOrigin.USER_SENT,
    pageSize: init.pageSize ?? 50,
    ...(init.knownThrough === undefined ? {} : { knownThrough: init.knownThrough }),
  });
}

/** A WatchAgent request. */
export function watchAgentRequest(init: {
  readonly target?: conversationv1.AgentId;
  readonly pageSize?: number;
  readonly knownThrough?: conversationv1.HistoryPointer;
} = {}): shimv1.WatchAgentRequest {
  return create(shimv1.WatchAgentRequestSchema, {
    ...(init.target === undefined ? {} : { target: init.target }),
    pageSize: init.pageSize ?? 50,
    ...(init.knownThrough === undefined ? {} : { knownThrough: init.knownThrough }),
  });
}

/** A ReadHistory request positioned at the newest page. */
export function readHistoryFirst(init: {
  readonly target?: conversationv1.AgentId;
  readonly pageSize?: number;
} = {}): shimv1.ReadHistoryRequest {
  return create(shimv1.ReadHistoryRequestSchema, {
    ...(init.target === undefined ? {} : { target: init.target }),
    pageSize: init.pageSize ?? 50,
    position: { case: "first", value: create(shimv1.ReadHistoryFirstSchema, {}) },
  });
}

/** A ReadHistory request walking older than `after`. */
export function readHistoryAfter(
  after: conversationv1.HistoryPointer,
  init: { readonly target?: conversationv1.AgentId; readonly pageSize?: number } = {},
): shimv1.ReadHistoryRequest {
  return create(shimv1.ReadHistoryRequestSchema, {
    ...(init.target === undefined ? {} : { target: init.target }),
    pageSize: init.pageSize ?? 50,
    position: { case: "after", value: after },
  });
}

/** `ReadHistory{through}`: the book as it stood at an instant. */
export function readHistoryThrough(
  atMs: bigint,
  init: { readonly target?: conversationv1.AgentId; readonly pageSize?: number } = {},
): shimv1.ReadHistoryRequest {
  return create(shimv1.ReadHistoryRequestSchema, {
    ...(init.target === undefined ? {} : { target: init.target }),
    pageSize: init.pageSize ?? 50,
    position: { case: "through", value: create(conversationv1.ConversationThroughSchema, { atMs }) },
  });
}

/** `UpdateAgent{stop}`. */
export function stopAgent(target?: conversationv1.AgentId): shimv1.UpdateAgentRequest {
  return create(shimv1.UpdateAgentRequestSchema, {
    ...(target === undefined ? {} : { target }),
    input: create(conversationv1.AgentInputSchema, {
      input: { case: "stop", value: create(conversationv1.AgentStopSchema, {}) },
    }),
  });
}

/** `UpdateAgent{answer{permission_decision}}` allowing once. */
export function allowOnce(
  ask: conversationv1.AgentPermissionId,
  target?: conversationv1.AgentId,
): shimv1.UpdateAgentRequest {
  return permissionDecision(
    create(conversationv1.AgentPermissionDecisionSchema, {
      ask,
      decision: {
        case: "allowed",
        value: create(conversationv1.AgentPermissionAllowedSchema, {
          scope: {
            case: "once",
            value: create(conversationv1.AgentPermissionAllowedOnceSchema, {}),
          },
        }),
      },
    }),
    target,
  );
}

/** `UpdateAgent{answer{permission_decision}}` allowing a standing grant. */
export function allowStanding(
  ask: conversationv1.AgentPermissionId,
  standing: conversationv1.AgentPermissionStanding,
  target?: conversationv1.AgentId,
): shimv1.UpdateAgentRequest {
  return permissionDecision(
    create(conversationv1.AgentPermissionDecisionSchema, {
      ask,
      decision: {
        case: "allowed",
        value: create(conversationv1.AgentPermissionAllowedSchema, {
          scope: {
            case: "standing",
            value: create(conversationv1.AgentPermissionAllowedStandingSchema, { standing }),
          },
        }),
      },
    }),
    target,
  );
}

/** `UpdateAgent{answer{permission_decision}}` denying, with the user's message. */
export function denyPermission(
  ask: conversationv1.AgentPermissionId,
  message: string,
  target?: conversationv1.AgentId,
): shimv1.UpdateAgentRequest {
  return permissionDecision(
    create(conversationv1.AgentPermissionDecisionSchema, {
      ask,
      decision: {
        case: "denied",
        value: create(conversationv1.AgentPermissionDeniedByUserSchema, { message }),
      },
    }),
    target,
  );
}

function permissionDecision(
  decision: conversationv1.AgentPermissionDecision,
  target?: conversationv1.AgentId,
): shimv1.UpdateAgentRequest {
  return create(shimv1.UpdateAgentRequestSchema, {
    ...(target === undefined ? {} : { target }),
    input: create(conversationv1.AgentInputSchema, {
      input: {
        case: "answer",
        value: create(conversationv1.AgentAnswerSchema, {
          answer: { case: "permissionDecision", value: decision },
        }),
      },
    }),
  });
}

/** One question's selection: the question text, its chosen labels, free text. */
export interface QuestionSelection {
  readonly question: string;
  readonly labels: readonly string[];
  readonly freeText?: string;
}

/** `UpdateAgent{answer{question_answer}}` echoing the batch's own values. */
export function answerQuestion(
  ask: conversationv1.AgentQuestionId,
  selections: readonly QuestionSelection[],
  target?: conversationv1.AgentId,
): shimv1.UpdateAgentRequest {
  return create(shimv1.UpdateAgentRequestSchema, {
    ...(target === undefined ? {} : { target }),
    input: create(conversationv1.AgentInputSchema, {
      input: {
        case: "answer",
        value: create(conversationv1.AgentAnswerSchema, {
          answer: {
            case: "questionAnswer",
            value: create(conversationv1.AgentQuestionAnswerSchema, {
              ask,
              answers: create(conversationv1.AgentQuestionAnswersSchema, {
                answers: selections.map((selection) =>
                  create(conversationv1.AgentQuestionSelectionSchema, {
                    question: create(conversationv1.AgentQuestionTextSchema, {
                      text: selection.question,
                    }),
                    chosen: selection.labels.map((label) =>
                      create(conversationv1.AgentQuestionChoiceSchema, {
                        label: create(conversationv1.AgentQuestionOptionLabelSchema, { label }),
                      }),
                    ),
                    ...(selection.freeText === undefined
                      ? {}
                      : {
                          freeText: create(conversationv1.AgentQuestionFreeTextSchema, {
                            text: selection.freeText,
                          }),
                        }),
                  }),
                ),
              }),
            }),
          },
        }),
      },
    }),
  });
}

/** `UpdateAgent{prompt}` — a message to an EXISTING agent, never the turn. */
export function promptAgent(
  text: string,
  target: conversationv1.AgentId,
): shimv1.UpdateAgentRequest {
  return create(shimv1.UpdateAgentRequestSchema, {
    target,
    input: create(conversationv1.AgentInputSchema, {
      input: { case: "prompt", value: said(text) },
    }),
  });
}
