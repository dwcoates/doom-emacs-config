/**
 * pageStreams — THE ONE CONNECTION THIS PAGE HOLDS.
 *
 * A BROWSER CANNOT HOLD MANY STANDING STREAMS AT ONCE, and that is not a
 * tuning question. The daemon serves h2c, but no browser negotiates cleartext
 * HTTP/2, so this page talks HTTP/1.1 — which caps it at about SIX connections
 * per host. A server-streaming Connect call pins one for its whole life, and
 * this webapp held six (`WatchWorkspaceRoster`, `WatchWebWorkspace`,
 * `WatchDaemon`, `WatchTopbar`, `WatchFooter`, `WatchDaemonHolds`) before its
 * feed tail was even opened. The seventh did NOT fail: it QUEUED, forever, with
 * no request on the wire, no error and no card — so `WatchFeed` was never
 * issued and no row the workspace produced after the page loaded was ever
 * drawn. Reloading appeared to fix it only because a fresh `OpenFeed` page
 * happened to contain the rows. Every expanded subagent bubble opens another
 * feed tail, so the count grew with the conversation and no fixed budget could
 * have contained it.
 *
 * SO THE PAGE OPENS ONE STREAM AND NOTHING ELSE OPENS ANY. `WatchPage` is it;
 * `SubscribePage` and `UnsubscribePage` are unary and hold no connection
 * between calls. Exceeding the cap is not something a component can now get
 * wrong — there is no second stream in the protocol for it to open.
 *
 * A SUBSCRIPTION LOOKS EXACTLY LIKE THE STREAM IT REPLACES. `watch()` yields
 * the same `Watch*Response` the dedicated rpc yielded, so every caller is still
 * an ordinary `watchStream` with its own backoff, its own `daemon_unreachable`
 * window and its own reopen. Nothing about a component's stream contract
 * changed except which socket carries it.
 *
 * THE ATTACHMENT IS A LATCH AND IT ALWAYS SETTLES. `watch()` awaits the page's
 * attachment before subscribing, because the daemon refuses a subscription for
 * a page it has not registered. The promise RESOLVES on the daemon's `attached`
 * frame and REJECTS when the page's stream ends without one — it never merely
 * stays pending, or this module would have reinvented the silent hang it exists
 * to remove. A rejection reaches the caller's own `watchStream` as an ordinary
 * open failure, which files its card and retries on backoff, by which time the
 * page's stream has reopened and re-attached.
 */
import type { MessageInitShape } from "@bufbuild/protobuf";
import { log } from "../log.js";
import {
  SubscribePageRequestSchema,
  SubscribePageResponseSchema,
  WatchPageResponseSchema,
  type PageFrame,
  type SubscribePageRequest,
  type WatchPageResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_page_pb.js";
import { MalformedView } from "./malformed.js";
import { watchStream, type StreamContext, type StreamHandle } from "./streams.js";
import { callUnary } from "./unary.js";

/** The slice of the context this module needs; the whole AppContext fits. */
export interface PageStreamContext extends StreamContext {
  /** Whether the page has gone quiet (the workspace moved to a successor). */
  isQuiesced(): boolean;
}

/** The `SubscribePageRequest.request` arms, which are also the `PageFrame` arms. */
export type PageWatchKind = NonNullable<SubscribePageRequest["request"]["case"]>;

/**
 * The `request` oneof as a CALLER writes it: the init shape, so a component
 * passes the same plain object literal it passed the dedicated rpc.
 */
type SubscribeRequestInit = NonNullable<
  MessageInitShape<typeof SubscribePageRequestSchema>["request"]
>;

/** The request one arm takes. */
export type PageWatchRequest<K extends PageWatchKind> = Extract<
  SubscribeRequestInit,
  { case: K }
>["value"];

/** The response one arm pushes. */
export type PageWatchResponse<K extends PageWatchKind> = NonNullable<
  Extract<PageFrame["payload"], { case: K }>["value"]
>;

export interface PageStreams {
  /**
   * Open one subscription on the page's stream.
   *
   * The iterable ends when the subscription ends — the daemon finished it, the
   * page's own stream died, or SIGNAL aborted — which is exactly the ending
   * `watchStream` already knows how to read.
   */
  watch<K extends PageWatchKind>(
    kind: K,
    request: PageWatchRequest<K>,
    signal: AbortSignal,
  ): AsyncIterable<PageWatchResponse<K>>;
  /** The id this document minted for itself; it rides every page-mux record. */
  readonly page: string;
}

export interface PageStreamsHandle extends PageStreams {
  /** Cancel the page's own stream. Idempotent. */
  cancel(): void;
}

/** What a subscription's open throws when the page's stream is not up. */
export class PageDetached extends Error {
  constructor(page: string) {
    super(`the page ${page} holds no stream on this daemon`);
    this.name = "PageDetached";
  }
}

/**
 * A subscription's inbox: pushes in, one at a time, until it closes.
 *
 * It is UNBOUNDED on purpose. The daemon's writer blocks until this page's
 * socket takes a frame, so the only thing that can pile up here is what the
 * socket has already delivered and a consumer has not yet drawn — and dropping
 * one of those would lose a row nothing would ever resend.
 */
class FrameQueue<T> {
  private readonly waiting: T[] = [];
  private wake: (() => void) | null = null;
  private closed = false;

  push(value: T): void {
    if (this.closed) return;
    this.waiting.push(value);
    this.wake?.();
  }

  close(): void {
    if (this.closed) return;
    this.closed = true;
    this.wake?.();
  }

  async *drain(): AsyncGenerator<T> {
    for (;;) {
      while (this.waiting.length > 0) {
        yield this.waiting.shift() as T;
      }
      if (this.closed) return;
      await new Promise<void>((resolve) => {
        this.wake = () => {
          this.wake = null;
          resolve();
        };
      });
    }
  }
}

/** One generation of the page's stream, and whether it is registered yet. */
interface Attachment {
  /** Resolves once the daemon has registered the page; rejects when it will not. */
  readonly ready: Promise<void>;
  resolve(): void;
  reject(cause: unknown): void;
  settled: boolean;
}

function newAttachment(): Attachment {
  let resolveFn: () => void = () => undefined;
  let rejectFn: (cause: unknown) => void = () => undefined;
  const ready = new Promise<void>((resolve, reject) => {
    resolveFn = resolve;
    rejectFn = reject;
  });
  // A rejection nobody is awaiting YET must not surface as an unhandled
  // rejection: the whole point of this promise is that it may be awaited later.
  void ready.catch(() => undefined);
  const attachment: Attachment = {
    ready,
    settled: false,
    resolve(): void {
      if (attachment.settled) return;
      attachment.settled = true;
      resolveFn();
    },
    reject(cause: unknown): void {
      if (attachment.settled) return;
      attachment.settled = true;
      rejectFn(cause);
    },
  };
  return attachment;
}

/**
 * Open this page's one stream and answer the handle every component subscribes
 * through. PAGE is the id this document minted for itself.
 */
export function startPageStreams(ctx: PageStreamContext, page: string): PageStreamsHandle {
  const subscriptions = new Map<string, FrameQueue<PageFrame>>();
  let attachment = newAttachment();
  let nextId = 0;

  // THE PAGE'S OWN STREAM IS AN ORDINARY STANDING STREAM, so it gets the
  // ordinary machinery: an ending it did not ask for is a transport failure,
  // reported once and reopened on backoff. Every subscription on it ends when
  // it does, and each of those callers reopens on its own backoff — finding a
  // re-attached page by the time it does.
  const stream: StreamHandle = watchStream<WatchPageResponse>(ctx, {
    name: "WatchPage",
    schema: WatchPageResponseSchema,
    open: (client, signal) => client.watchPage({ page }, { signal }),
    onPush: (response) => routeFrame(response),
    onEnd: () => detach(),
  });

  return {
    page,
    watch,
    cancel(): void {
      stream.cancel();
      detach();
    },
  };

  /** Draw one frame off the page's stream onto the subscription it addresses. */
  function routeFrame(response: WatchPageResponse): void {
    const frame = response.frame;
    switch (frame.case) {
      case "attached":
        log("info", "the page's one standing stream is attached", {
          operation: "rpc.page-attached",
          context: { page },
        });
        attachment.resolve();
        return;
      case "push": {
        const queue = subscriptions.get(frame.value.subscription);
        if (queue === undefined) {
          // A push for a subscription this page has already dropped. It is the
          // ordinary race between `UnsubscribePage` and the frames already on
          // the wire, not a fault: the daemon stops on its own.
          log("debug", "a push arrived for a subscription this page had already ended", {
            operation: "rpc.page-push-unclaimed",
            context: { page, subscription: frame.value.subscription },
          });
          return;
        }
        queue.push(frame.value);
        return;
      }
      case "ended": {
        const queue = subscriptions.get(frame.value.subscription);
        if (queue === undefined) return;
        log("info", "a page subscription was ended by the daemon", {
          operation: "rpc.page-subscription-ended",
          context: { page, subscription: frame.value.subscription },
        });
        queue.close();
        return;
      }
      default:
        throw new MalformedView(
          "WatchPageResponse.frame",
          "the page's stream sent a frame with no arm set",
        );
    }
  }

  /**
   * The page's stream ended: every subscription on it ends with it, and the
   * next `watch()` waits on the NEXT generation's attachment rather than on a
   * promise that has already settled.
   */
  function detach(): void {
    attachment.reject(new PageDetached(page));
    for (const [, queue] of subscriptions) queue.close();
    subscriptions.clear();
    attachment = newAttachment();
  }

  async function* watch<K extends PageWatchKind>(
    kind: K,
    request: PageWatchRequest<K>,
    signal: AbortSignal,
  ): AsyncGenerator<PageWatchResponse<K>> {
    // The generation this subscription belongs to, read BEFORE the await so a
    // detach during it cannot leave this subscription believing it joined a
    // stream it never did.
    const generation = attachment;
    await generation.ready;
    if (signal.aborted) return;

    const id = `${kind}-${++nextId}`;
    const queue = new FrameQueue<PageFrame>();
    subscriptions.set(id, queue);
    const stopOnAbort = (): void => queue.close();
    signal.addEventListener("abort", stopOnAbort, { once: true });
    try {
      await callUnary(
        ctx,
        "SubscribePage",
        (client) =>
          client.subscribePage(
            {
              page,
              subscription: id,
              // The arm and its value are correlated by K, which the compiler
              // cannot carry through a generic construction of the oneof.
              request: { case: kind, value: request } as SubscribeRequestInit,
            },
            { signal },
          ),
        SubscribePageResponseSchema,
      );
      for await (const frame of queue.drain()) {
        if (signal.aborted) return;
        yield payloadOf(kind, frame);
      }
    } finally {
      signal.removeEventListener("abort", stopOnAbort);
      subscriptions.delete(id);
      // TELL THE DAEMON ONLY WHILE THERE IS A PAGE TO TELL. A detached page's
      // subscriptions are already gone daemon-side with the stream that owned
      // them, so the unary would be work nobody has to do.
      if (generation === attachment) void unsubscribe(id);
    }
  }

  /** End one subscription daemon-side. */
  async function unsubscribe(id: string): Promise<void> {
    try {
      await ctx.client.unsubscribePage({ page, subscription: id });
    } catch (err) {
      // The page is going away, or already has. It is RECORDED and not
      // reported: the daemon drops every subscription a departed page held, so
      // there is nothing here for a user to see or for this page to fix.
      log("debug", "a page subscription could not be ended on the daemon", {
        operation: "rpc.page-unsubscribe-failed",
        context: { page, subscription: id, cause: String(err) },
      });
    }
  }
}

/**
 * Read one frame's payload as the arm its subscription asked for.
 *
 * A MISMATCH IS ONE UNREADABLE FRAME, not a broken link: the daemon addressed
 * this subscription with a payload of another kind, which the caller's own
 * `watchStream` skips and files while the stream keeps running.
 */
function payloadOf<K extends PageWatchKind>(kind: K, frame: PageFrame): PageWatchResponse<K> {
  if (frame.payload.case !== kind || frame.payload.value === undefined) {
    throw new MalformedView(
      "PageFrame.payload",
      `subscription ${frame.subscription} asked for ${kind} and was sent ${String(frame.payload.case)}`,
    );
  }
  return frame.payload.value as PageWatchResponse<K>;
}
