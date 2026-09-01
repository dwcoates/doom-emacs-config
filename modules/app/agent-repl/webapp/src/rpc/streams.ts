/**
 * watchStream — every `Watch*` stream the webapp opens runs through here.
 *
 * STREAMS ARE STANDING. A component stream never concludes on its own: a
 * turn's end arrives as a `FeedTurnEnded` ROW, a session going quiet is a
 * footer state, and there are no keepalive frames anywhere in the contract. A
 * stream therefore ends for exactly one legitimate reason — this client
 * cancelled it (a bubble collapsed, a view was disposed). ANY OTHER ENDING IS
 * A TRANSPORT FAILURE, including a clean conclusion with no error at all, and
 * is reported and reopened. That equivalence is what keeps a dropped pipe
 * distinguishable from a real ending instead of reading as "the work stopped".
 *
 * A BAD FRAME SKIPS, A BAD LINK REOPENS. The two failures are different and
 * are treated differently:
 *   - a push that fails the strict check, or that `onPush` refuses, is ONE
 *     unreadable frame: it is logged, filed as `frame_undecodable` (the user
 *     is told conversation may be missing, which is why it is a card and not a
 *     log line), and SKIPPED. The stream keeps running, because the next frame
 *     is very probably fine and tearing the view down would lose the rest of
 *     the conversation over one row.
 *   - a stream that ended is the LINK: `daemon_unreachable` is filed and the
 *     stream reopens with backoff. That card is window-shaped and is retracted
 *     on the first successful push, so a flapping link leaves no ghost.
 *
 * REOPENING IS THE ONLY RECOVERY, and it needs no resume token. Resolver state
 * is daemon-memory and every push is a WHOLE view, so a reopened stream's
 * first push is the current truth; there are no fences, no epochs and no gap
 * to close.
 *
 * EVERY HANDLE RE-REGISTERS ON A NEW CLIENT. The graceful-rollout path hands
 * the page a new daemon to adopt, so each handle subscribes to
 * `ctx.onClientReplaced` and restarts itself on whatever `ctx.client` is by
 * then. The context's listener set IS the registry — a second one beside it
 * could disagree with it.
 */
import type { DescMessage, Message } from "@bufbuild/protobuf";
import { ConnectError } from "@connectrpc/connect";
import { log } from "../log.js";
import { daemonUnreachable, frameUndecodable, type FailureSink } from "../failure/sink.js";
import { MalformedView, isMalformedView } from "./malformed.js";
import { assertNoUnknownFields } from "./strict.js";
import type { AgentReplClient } from "./client.js";

/** How a stream's run finished. `cancelled` is the only legitimate one. */
export type StreamEnd =
  | { kind: "cancelled" }
  | { kind: "transport_failure"; error: unknown }
  | { kind: "producer_ended" };

export interface StreamHandle {
  /** Abort the stream and stop reopening it. Idempotent. */
  cancel(): void;
}

/** The slice of the AppContext a stream needs; the whole context fits. */
export interface StreamContext {
  readonly client: AgentReplClient;
  readonly failures: FailureSink;
  onClientReplaced(fn: () => void): () => void;
  /**
   * The page going quiet, which every standing stream must obey.
   *
   * THE HANDLE CANCELS ITSELF rather than waiting to be told: the workspace has
   * moved to a successor this page will never dial, so reopening would file a
   * `daemon_unreachable` card every backoff for a link that is correctly gone.
   */
  onQuiesced(fn: () => void): () => void;
}

export interface WatchStreamOptions<Res extends Message> {
  /** The rpc's own name ("WatchFooter"); rides every log record. */
  name: string;
  /** The RESPONSE schema the strict check walks on every push. */
  schema: DescMessage;
  /** Open the stream. The signal is aborted on cancel and on reopen. */
  open: (client: AgentReplClient, signal: AbortSignal) => AsyncIterable<Res>;
  /** Draw the push. A MalformedView thrown here skips the frame. */
  onPush: (res: Res) => void;
  /** Observe how a run finished. Never required. */
  onEnd?: (end: StreamEnd) => void;
  /**
   * Called on the first push AFTER a run that ended without our cancel.
   *
   * The link coming back is a fact only this loop can observe, and the
   * lifecycle needs it: an announced outage's banner comes down when the
   * daemon is answering again, not when its own countdown says it should be.
   */
  onReconnected?: () => void;
  /** Injected by tests running on fake timers. */
  backoff?: BackoffOptions;
}

export interface BackoffOptions {
  /** The first wait after a failure. */
  initialMs?: number;
  /** The ceiling the doubling stops at. */
  maxMs?: number;
}

/** 250 ms, doubling, capped at 5 s. */
export const DEFAULT_BACKOFF_INITIAL_MS = 250;
export const DEFAULT_BACKOFF_MAX_MS = 5000;

/** How much of an unreadable frame the card quotes. */
const FRAME_HEAD_LIMIT = 200;

/**
 * Open a standing stream and keep it open.
 *
 * Returns immediately; the run loop lives on its own promise chain. The handle
 * is the only way to stop it.
 */
export function watchStream<Res extends Message>(
  ctx: StreamContext,
  opts: WatchStreamOptions<Res>,
): StreamHandle {
  const initialMs = opts.backoff?.initialMs ?? DEFAULT_BACKOFF_INITIAL_MS;
  const maxMs = opts.backoff?.maxMs ?? DEFAULT_BACKOFF_MAX_MS;

  let cancelled = false;
  let controller = new AbortController();
  let backoffMs = initialMs;
  /** Whether a daemon_unreachable card of ours is standing. */
  let unreachableFiled = false;
  /** Resolves the current backoff wait early when the stream is cancelled. */
  let wakeFromBackoff: (() => void) | null = null;

  log("debug", `opening the ${opts.name} stream`, {
    operation: "rpc.stream-open",
    context: { rpc: opts.name },
  });

  /** Filled in below; the quiet subscription cancels through the handle. */
  let cancelSelf = (): void => undefined;
  /** Set when the page was ALREADY quiet as this stream was being built. */
  let quiesceRequested = false;
  const unsubscribeFromQuiesce = ctx.onQuiesced(() => {
    if (cancelled) return;
    quiesceRequested = true;
    log("info", `${opts.name} is stopping: the workspace moved to another daemon`, {
      operation: "rpc.stream-quiesced",
      context: { rpc: opts.name },
    });
    cancelSelf();
  });

  const unsubscribeFromClientReplacement = ctx.onClientReplaced(() => {
    if (cancelled) return;
    // The page adopted a new daemon. Abort the run against the old client; the
    // loop reopens on `ctx.client`, which is already the new one.
    log("info", `${opts.name} is reopening on the adopted daemon`, {
      operation: "rpc.stream-client-replaced",
      context: { rpc: opts.name },
    });
    backoffMs = initialMs;
    controller.abort();
    wakeFromBackoff?.();
  });

  /**
   * One push. A refusal — from the strict check or from the drawing — costs
   * this frame and nothing else.
   */
  const consume = (response: Res): void => {
    try {
      assertNoUnknownFields(opts.schema, response);
      opts.onPush(response);
    } catch (err) {
      if (!isMalformedView(err)) throw err;
      reportUndecodable(err);
      return;
    }
    // A frame arrived and was drawn, so the link is up. Retract the window and
    // re-arm the backoff, or a link that flaps every few minutes would creep
    // to the ceiling and stay there.
    if (unreachableFiled) {
      log("info", `${opts.name} is receiving again; retracting the unreachable card`, {
        operation: "rpc.stream-recovered",
        context: { rpc: opts.name },
      });
      ctx.failures.retract("daemonUnreachable");
      unreachableFiled = false;
      opts.onReconnected?.();
    }
    backoffMs = initialMs;
  };

  const reportUndecodable = (err: MalformedView): void => {
    log("error", `a ${opts.name} push could not be read and was skipped: ${err.detail}`, {
      operation: "rpc.stream-frame-undecodable",
      context: { rpc: opts.name, path: err.path, cause: err.detail },
    });
    ctx.failures.report(
      frameUndecodable(err.detail, `${opts.schema.typeName} at ${err.path}`.slice(0, FRAME_HEAD_LIMIT)),
    );
  };

  const reportUnreachable = (end: StreamEnd): void => {
    const { closeCode, closeReason } = closeEvidence(end);
    log("error", `the ${opts.name} stream ended without being cancelled; reopening`, {
      operation: "rpc.stream-transport-failure",
      context: { rpc: opts.name, end: end.kind, close_code: closeCode, close_reason: closeReason, backoff_ms: backoffMs },
    });
    ctx.failures.report(daemonUnreachable(closeCode, closeReason));
    unreachableFiled = true;
  };

  /** Wait out the backoff, returning early if the stream is cancelled. */
  const waitBackoff = (ms: number): Promise<void> =>
    new Promise<void>((resolve) => {
      const timer = setTimeout(() => {
        wakeFromBackoff = null;
        resolve();
      }, ms);
      wakeFromBackoff = () => {
        clearTimeout(timer);
        wakeFromBackoff = null;
        resolve();
      };
    });

  const run = async (): Promise<void> => {
    while (!cancelled) {
      controller = new AbortController();
      let end: StreamEnd;
      try {
        for await (const response of opts.open(ctx.client, controller.signal)) {
          if (cancelled) break;
          consume(response);
        }
        end = cancelled ? { kind: "cancelled" } : { kind: "producer_ended" };
      } catch (err) {
        end = cancelled ? { kind: "cancelled" } : { kind: "transport_failure", error: err };
      }
      opts.onEnd?.(end);
      if (cancelled) return;
      // producer_ended and transport_failure are the SAME condition here: a
      // standing stream that ended without our cancel. The arms differ only so
      // an onEnd observer can tell a clean conclusion from a thrown one.
      reportUnreachable(end);
      await waitBackoff(backoffMs);
      if (cancelled) return;
      backoffMs = Math.min(backoffMs * 2, maxMs);
    }
  };

  const handle: StreamHandle = {
    cancel(): void {
      if (cancelled) return;
      cancelled = true;
      log("debug", `cancelling the ${opts.name} stream`, {
        operation: "rpc.stream-cancel",
        context: { rpc: opts.name },
      });
      unsubscribeFromClientReplacement();
      unsubscribeFromQuiesce();
      controller.abort();
      wakeFromBackoff?.();
    },
  };
  cancelSelf = () => handle.cancel();
  // A page that went quiet BEFORE this stream was opened must not get a run
  // loop at all: `onQuiesced` fires immediately in that case, when `cancelSelf`
  // was still the placeholder, so the flag it set is honored here instead.
  if (quiesceRequested) {
    handle.cancel();
    return handle;
  }
  void run();
  return handle;
}

/**
 * The close code and reason a `daemon_unreachable` card carries.
 *
 * A ConnectError's numeric code stands in for the socket close code the arm
 * was named for: the card's job is to distinguish a deliberate close from a
 * dropped one, and the Connect code is what carries that distinction over
 * HTTP. A producer that simply concluded gives neither, so the card reports
 * code 0 and says so in the reason rather than inventing evidence.
 */
function closeEvidence(end: StreamEnd): { closeCode: number; closeReason: string } {
  if (end.kind !== "transport_failure") {
    return { closeCode: 0, closeReason: "the stream concluded without an error" };
  }
  const connectError = ConnectError.from(end.error);
  return { closeCode: connectError.code, closeReason: connectError.rawMessage };
}
