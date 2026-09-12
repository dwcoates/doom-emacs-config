/**
 * service/server.ts — the one listener the shim binds.
 *
 * # One socket, both HTTP versions — and why not `allowHTTP1`
 *
 * The daemon dials Connect over h2c; a human debugging the socket dials
 * HTTP/1.1 with `curl`, and the Connect protocol runs happily over either. The
 * prescribed mechanism for serving both was
 * `http2.createServer({ allowHTTP1: true })`, and it does not work: `allowHTTP1`
 * is honored only by `createSecureServer`, where the version is chosen by TLS
 * ALPN. On a CLEARTEXT http2 server the option is accepted and ignored, and an
 * HTTP/1.1 request dies with "Parse Error: Expected HTTP/, RTSP/ or ICE/"
 * (verified on this Node). A unix socket has no ALPN, so there is nothing to
 * negotiate with.
 *
 * What replaces it is the thing ALPN would have done, done by hand: the
 * listener is a plain `net` server that PEEKS at the first bytes of each
 * connection and hands the socket to whichever server speaks its dialect. The
 * HTTP/2 connection preface is a fixed 24-byte string that no HTTP/1.1 request
 * line can begin with, so the discrimination is exact rather than heuristic —
 * see {@link sniffProtocol}. Both servers run the SAME Connect handler, so the
 * two paths cannot diverge in behavior.
 *
 * # Standing streams: the head goes out ON ACCEPT
 *
 * connect-node writes a response head LAZILY — for a server stream that has
 * pushed nothing yet, `writeHead` fires only when the stream ENDS. A Go client
 * surfaces a server-stream refusal at its first `Receive`, so a stream that has
 * accepted but not yet spoken is indistinguishable at the client from one that
 * was refused, and the daemon's bring-up blocks on a WatchSession that is
 * perfectly healthy and merely quiet.
 *
 * So the head is written HERE, before the Connect adapter sees the request:
 * a streaming content type gets `200` with the request's own content type
 * echoed back, `flushHeaders()`, and then `writeHead` rebound to a no-op so the
 * adapter's later call cannot raise ERR_HTTP_HEADERS_SENT. Unary requests are
 * untouched — they have a real status to report and nothing to gain from an
 * early head. A streaming verb that REFUSES still reports its refusal, because
 * the Connect protocol carries a stream's error in its end-of-stream frame,
 * not in the head.
 *
 * # Stale sockets, and why a LIVE one is refused
 *
 * A unix socket path outlives the process that bound it: a shim killed with
 * SIGKILL leaves the file behind and `listen` on it fails EADDRINUSE forever
 * after, so a stale file is unlinked. But "unlink whatever is there" is exactly
 * how a healthy shim gets silently disconnected from its daemon — the file
 * would vanish out from under a live listener, which keeps running and
 * receiving nothing. The two cases are told apart by DIALING the path first: a
 * refused connection means nobody is listening (stale, unlink it), a successful
 * one means somebody is (refuse to start), ENOENT means the path is free.
 */
import { connectNodeAdapter } from "@connectrpc/connect-node";
import type { ConnectRouter } from "@connectrpc/connect";
import { connect, createServer as createNetServer, type Socket } from "node:net";
import { unlinkSync } from "node:fs";
import http from "node:http";
import http2 from "node:http2";
import { bindLog } from "../log.js";

const LOGGER = bindLog({ component: "shim-server", operation: "shim.service.server" });

/**
 * The HTTP/2 connection preface (RFC 9113 §3.4), which every h2c client sends
 * before anything else. No HTTP/1.1 request line can begin with it: `PRI` is
 * not a method any HTTP/1.1 client sends, which is precisely why the preface
 * was chosen to look like one.
 */
export const HTTP2_PREFACE = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n";

/** The shortest prefix that already decides the question. */
const PREFACE_DECIDING_PREFIX = "PRI * HTTP/2.0";

/** What the first bytes of a connection say about which server should take it. */
type SniffedProtocol = "h2" | "h1" | "need-more";

/**
 * Decide, from the bytes seen so far, which dialect a connection speaks.
 *
 * `need-more` is a real answer and not a failure: a client may deliver the
 * preface in pieces, and guessing HTTP/1.1 from an incomplete prefix would send
 * a perfectly good h2c connection to a server that cannot parse it.
 */
export function sniffProtocol(seen: Buffer): SniffedProtocol {
  const comparable = Math.min(seen.length, PREFACE_DECIDING_PREFIX.length);
  if (seen.subarray(0, comparable).toString("latin1") !== PREFACE_DECIDING_PREFIX.slice(0, comparable)) {
    return "h1";
  }
  return comparable === PREFACE_DECIDING_PREFIX.length ? "h2" : "need-more";
}

/**
 * The server-streaming verbs, by the path Connect routes them on.
 *
 * Named rather than derived because the exit path needs the answer BEFORE a
 * response has any headers to inspect, and a wrong answer here is the
 * difference between a bounded wait and a five-second one on every kill.
 */
const STREAMING_RPCS: readonly string[] = [
  "WatchSession",
  "WatchAgent",
  "WatchBash",
  "WatchWorkflow",
];

/** Does this request path name a server-streaming rpc? */
export function isStreamingPath(url: string): boolean {
  const rpc = url.split("/").pop() ?? "";
  return STREAMING_RPCS.includes(rpc);
}

/**
 * The two h2 reset codes this server has to tell apart (RFC 9113 s7).
 *
 * `NO_ERROR` on a stream is an ordinary end. `CANCEL` is THE PEER saying it is
 * done with a stream this side was still serving — the daemon dropping its
 * standing `WatchSession` when it gives up an adoption, or a consumer closing
 * a tail — and nothing on this side failed.
 */
const H2_NO_ERROR = 0;
const H2_CANCEL = 8;

/**
 * Record how one h2c stream ended.
 *
 * A CANCEL IS EXPLAINED, NEVER MOURNED. Reporting the peer's own cancel as an
 * error made every ordinary consumer departure look like a transport defect of
 * this server's, and buried the resets that ARE defects among them. The cancel
 * is still RECORDED — a standing stream ending early is a fact the log is the
 * only place to learn it — but it is recorded as the cancel it is, with the
 * reason named, and only a reset this side cannot account for stays an error.
 *
 * AND IT IS NOT A WARNING EITHER, which is where that reasoning stopped one
 * level short. `CANCEL` is the one reset code whose whole meaning is that the
 * peer decided: RFC 9113 §7 defines it as the endpoint no longer needing the
 * stream. Every workspace the daemon closes cancels its two standing watches on
 * the way out, so a warning here is a warning for an ordinary close — and a
 * warning that fires on every ordinary close tells nobody anything, while
 * costing a realtest run whose harvest bar admits no WARN at all. The record
 * stays, at the level a deliberate departure warrants.
 *
 * A PEER THAT VANISHED MID-SERVE IS A DIFFERENT THING and is untouched: any
 * reset code that is neither `NO_ERROR` nor `CANCEL` is still an ERROR, because
 * this side cannot account for it.
 *
 * Exported so the classification is testable for the codes no suite can
 * provoke from a real peer.
 */
export function recordStreamReset(
  rstCode: number | undefined,
  streamId: number | undefined,
  path: string | undefined,
): void {
  if (rstCode === undefined || rstCode === H2_NO_ERROR) return;
  if (rstCode === H2_CANCEL) {
    LOGGER.info(
      {
        stream_id: streamId,
        rst_code: rstCode,
        path,
        reason: "the peer cancelled the stream; it stopped consuming what this server was serving",
      },
      "an h2c stream was cancelled by its peer; what it was serving did not finish",
    );
    return;
  }
  LOGGER.error(
    { stream_id: streamId, rst_code: rstCode, path, detail: "the h2c stream ended before its response completed" },
    "an h2c stream ended with a reset code; whatever it was serving did not finish",
  );
}

/**
 * How long ONE connection has to stop producing bytes before the exit cuts it
 * anyway.
 *
 * A LAST RESORT, never the mechanism, exactly like the quiet-drain budget: the
 * ordinary case settles in two event-loop turns and spends none of this. It
 * exists so a connection that never stops writing cannot keep a killed shim
 * alive. One second is three orders of magnitude over the work it covers —
 * pushing one unary Connect response, tens of bytes, down a unix socket.
 */
export const SOCKET_QUIET_BUDGET_MS = 1_000;

/**
 * Cut a connection once it has stopped producing bytes.
 *
 * WHY THIS IS NOT A BARE `socket.destroy()`, WHICH IS WHAT IT USED TO BE.
 * {@link ShimServer.quiet} waits for each response's own `close` event, and
 * that event is NOT "the answer is on the wire". Over h2c — the only dialect
 * the daemon dials — a response's close means its frames were handed to the
 * http2 session; the session serializes and writes them on a LATER turn of the
 * event loop. Destroying the socket in the same breath threw them away, and
 * neither `session.state.outboundQueueSize` nor `socket.writableLength`
 * reported anything pending at that moment (both measured at 0 while 9 bytes
 * of the answer had yet to be written), so there is nothing to ask: the only
 * honest question is whether the connection has gone quiet.
 *
 * What it cost: the daemon read `unexpected EOF` off a `KillSession` the shim
 * had in fact performed, recorded "the session kill did not answer" against an
 * orderly stand-down, and failed the e2e suite's warning sweep with it —
 * 6 of 36 full runs, always on the handover that stands a merged workspace's
 * shim down.
 *
 * THE WAIT IS QUIESCENCE, NOT A DELAY. Each pass yields the event loop and
 * then asks the socket whether anything new left it and whether anything is
 * still queued. It ends the moment a whole turn passes with neither — two
 * turns in the ordinary case, measured — and the budget bounds only a
 * connection that never stops.
 */
async function cutWhenQuiet(socket: Socket, budgetMs: number): Promise<void> {
  if (socket.destroyed) return;
  let spent = false;
  const budget = setTimeout(() => {
    spent = true;
  }, budgetMs);
  // The budget must not itself hold the process open: it is a backstop on work
  // the event loop is already doing.
  budget.unref();
  let written = -1;
  while (!spent && !socket.destroyed && (socket.bytesWritten !== written || socket.writableLength > 0)) {
    written = socket.bytesWritten;
    await new Promise<void>((resolve) => {
      setImmediate(resolve);
    });
  }
  clearTimeout(budget);
  if (spent) {
    LOGGER.error(
      {
        budget_ms: budgetMs,
        pending_bytes: socket.writableLength,
        bytes_written: socket.bytesWritten,
        detail: "the connection remained active until its exit budget expired",
      },
      "a connection never went quiet before the exit cut it; an answer may not have reached the daemon",
    );
  }
  socket.destroy();
}

/** A bound listener, and the way to stop it. */
export interface ShimServer {
  /** The path the listener is bound to. */
  readonly socketPath: string;
  /** Stop accepting, cut every open connection, and remove the socket file. */
  close(): Promise<void>;
  /**
   * Resolve once no response is still being written.
   *
   * The exit after `KillSession` needs this: {@link close} DESTROYS every open
   * socket in the same tick, which would cut the KillSession response itself
   * off the wire and surface to the daemon as a hang-up rather than as the
   * answer it asked for. Waiting on the responses themselves — each one's own
   * `close` event — is the only statement of "the daemon has its answer" that
   * is not a guess about how many ticks serialization takes.
   *
   * `budgetMs` is a LAST RESORT, not the mechanism: a stream that never ends
   * must not be able to keep a killed shim alive forever, so exceeding it is
   * logged at ERROR and the exit proceeds.
   */
  quiet(budgetMs: number): Promise<void>;
}

/** What a probe of an existing socket path concluded. */
type SocketProbe = "free" | "stale" | "live";

/**
 * Decide whether a socket path may be bound.
 *
 * Exported because it carries the whole stale-vs-live judgement, which is the
 * part worth testing on its own; {@link serve} only acts on the verdict.
 */
export function probeSocket(socketPath: string): Promise<SocketProbe> {
  return new Promise<SocketProbe>((resolve) => {
    const probe: Socket = connect({ path: socketPath });
    const settle = (verdict: SocketProbe): void => {
      probe.removeAllListeners();
      probe.destroy();
      resolve(verdict);
    };
    probe.once("connect", () => settle("live"));
    probe.once("error", (err: NodeJS.ErrnoException) => {
      // ENOENT: nothing is there at all. ECONNREFUSED: a socket FILE is there
      // with no process behind it — the SIGKILLed-predecessor case.
      settle(err.code === "ENOENT" ? "free" : "stale");
    });
  });
}

/** Remove a socket file left behind by a dead predecessor, or by ourselves. */
function unlinkSocketFile(socketPath: string, why: string): void {
  try {
    unlinkSync(socketPath);
    LOGGER.debug({ socket_path: socketPath, why }, `removed the shim socket file (${why})`);
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") return;
    LOGGER.error(
      { socket_path: socketPath, why, cause: err },
      "could not remove the shim socket file",
    );
    throw err;
  }
}

/**
 * Bind the shim's socket and start serving the given routes.
 *
 * Resolves once the listener is accepting, so whoever awaits this has a
 * dialable shim: the daemon may connect the instant the promise settles.
 */
export async function serve(
  socketPath: string,
  routes: (router: ConnectRouter) => void,
): Promise<ShimServer> {
  const verdict = await probeSocket(socketPath);
  if (verdict === "live") {
    LOGGER.error(
      { socket_path: socketPath, detail: "another live shim already owns the socket" },
      "refusing to bind: another shim is already listening on this socket",
    );
    throw new Error(
      `shim: ${socketPath} already has a live listener; refusing to start a second shim on one session socket`,
    );
  }
  if (verdict === "stale") unlinkSocketFile(socketPath, "stale predecessor");

  // RESPONSE COMPRESSION IS OFF, AND IT HAS TO BE.
  //
  // The early-head ruling has this server write its own response head the
  // moment it accepts a stream, which means the head goes out BEFORE the
  // adapter has decided anything about the body — including whether to compress
  // it. The adapter's own later `writeHead`, the one that would have carried
  // `connect-content-encoding`, is necessarily absorbed. A compressed envelope
  // would then reach a client that was never told how to read it, and the
  // client says so: "received compressed envelope, but do not know how to
  // decompress".
  //
  // It bit only once a payload crossed the default 1 KiB threshold, so early
  // streams worked and later ones — reading a book that had grown — did not,
  // which is the worst shape this failure could have taken.
  //
  // The two properties cannot both hold, and the early head is the one with a
  // ruling behind it: acceptance must be observable before the first frame. So
  // the server never compresses a response. `acceptCompression` is deliberately
  // left alone — the server still UNDERSTANDS a compressed REQUEST, which no
  // head of ours is involved in — and the client's RESPONSE accept list is
  // declined per stream in `flushStreamHead`, so the adapter never negotiates a
  // response encoding our head could not carry.
  const served = withEarlyStreamHeaders(
    connectNodeAdapter({ routes, compressMinBytes: Number.MAX_SAFE_INTEGER }),
  );
  // In-flight UNARY responses, counted so the exit path can wait for the wire
  // to go quiet. `close` and not `finish`: an aborted response never finishes,
  // and a counter that only came down on success would wedge the exit.
  //
  // The server-streaming verbs are deliberately excluded. A `WatchAgent` tail
  // never concludes on its own — that is its contract — so counting it would
  // make every `KillSession` wait out the whole budget before exiting, turning
  // a bounded last resort into the ordinary path.
  let inFlight = 0;
  const quietWaiters = new Set<() => void>();
  const handler: typeof served = (request, response) => {
    if (isStreamingPath(request.url ?? "")) {
      served(request, response);
      return;
    }
    inFlight += 1;
    response.once("close", () => {
      inFlight -= 1;
      if (inFlight > 0) return;
      for (const waiter of quietWaiters) waiter();
      quietWaiters.clear();
    });
    served(request, response);
  };
  // Neither of these ever LISTENS. They exist to own a connection's protocol
  // state machine; the net server below feeds them sockets directly.
  const h1 = http.createServer(handler);
  const h2 = http2.createServer(handler);
  h1.on("clientError", (err, socket) => {
    // warn: a defect because a client connection failed before the server could identify its request.
    LOGGER.warn({ cause: err }, "an HTTP/1.1 client connection failed before a request was read");
    socket.destroy();
  });
  h2.on("sessionError", (err) => {
    // warn: a defect because an h2c transport session failed while the listener continued serving.
    LOGGER.warn({ cause: err }, "an h2c session failed");
  });
  // EVERY STREAM THIS SERVER RESETS SAYS SO. Without this the shim was silent
  // about its own h2 layer cutting a stream: the daemon recorded "a standing
  // stream ended without the session ending" and opened a `link_severed`
  // fault, and the shim's log — the only place that could say whether the
  // reset came from here — held nothing at all. `rstCode` names the code the
  // stream ended with; `NGHTTP2_NO_ERROR` is an ordinary end and is not
  // recorded, and {@link recordStreamReset} decides whether what is left is
  // the peer's own cancel or this server's defect.
  h2.on("session", (session) => {
    session.on("frameError", (type, code, id) => {
      LOGGER.error(
        { frame_type: type, error_code: code, stream_id: id, detail: "the h2c transport could not send a frame" },
        "an h2c frame could not be sent",
      );
    });
    session.on("stream", (stream, headers) => {
      const path = headers[":path"];
      stream.once("close", () => {
        recordStreamReset(stream.rstCode, stream.id, path);
      });
    });
  });

  const open = new Set<Socket>();
  const listener = createNetServer((socket) => {
    open.add(socket);
    socket.once("close", () => open.delete(socket));
    routeByPreface(socket, h1, h2);
  });

  await new Promise<void>((resolve, reject) => {
    const onError = (err: Error): void => {
      LOGGER.error({ socket_path: socketPath, cause: err }, "the shim listener failed to bind");
      reject(err);
    };
    listener.once("error", onError);
    listener.listen({ path: socketPath }, () => {
      listener.removeListener("error", onError);
      resolve();
    });
  });

  // A listener error AFTER bind is not a bind failure, and an unhandled 'error'
  // on a server is an uncaught exception that would kill the shim.
  listener.on("error", (err: Error) => {
    LOGGER.error({ socket_path: socketPath, cause: err }, "the shim listener raised an error while serving");
  });

  LOGGER.info({ socket_path: socketPath, probe: verdict }, "shim.v1 listener bound and accepting");

  return {
    socketPath,
    quiet: async (budgetMs: number): Promise<void> => {
      if (inFlight === 0) return;
      await new Promise<void>((resolve) => {
        const timer = setTimeout(() => {
          quietWaiters.delete(settle);
          LOGGER.error(
            { socket_path: socketPath, in_flight: inFlight, budget_ms: budgetMs, detail: "responses remained open when the quiet budget expired" },
            "gave up waiting for the wire to go quiet; ending the process with responses still open",
          );
          resolve();
        }, budgetMs);
        const settle = (): void => {
          clearTimeout(timer);
          resolve();
        };
        quietWaiters.add(settle);
      });
    },
    close: async (): Promise<void> => {
      // THE ORDER MATTERS. `close()` stops accepting and then waits for every
      // open connection to end, and this shim's connections do not end on their
      // own: a standing WatchSession never concludes, and an h2c session is held
      // open by its client. Waiting first would hang the stand-down forever, so
      // the open sockets are cut without waiting for the peer.
      //
      // CUT, BUT ONLY ONCE WHAT IS ALREADY OWED HAS LEFT: see {@link cutWhenQuiet}.
      const stopped = new Promise<void>((resolve) => listener.close(() => resolve()));
      const cutting = [...open].map((socket) => cutWhenQuiet(socket, SOCKET_QUIET_BUDGET_MS));
      open.clear();
      await Promise.all(cutting);
      await stopped;
      h2.close();
      h1.close();
      unlinkSocketFile(socketPath, "listener closed");
      LOGGER.info({ socket_path: socketPath }, "shim.v1 listener closed and its socket removed");
    },
  };
}

/**
 * The request content types that mean "this is a stream".
 *
 * The Connect, gRPC-Web and gRPC streaming framings, all of which the router
 * accepts. A unary Connect request is `application/proto` or
 * `application/json` and is deliberately absent: it has a real status to
 * report, and committing to 200 before the handler ran would throw that away.
 */
const STREAMING_CONTENT_TYPES: ReadonlySet<string> = new Set([
  "application/connect+proto",
  "application/connect+json",
  "application/grpc-web+proto",
  "application/grpc-web+json",
  "application/grpc+proto",
  "application/grpc",
]);

/** Whether a request's content type means the response is a stream. */
export function isStreamingContentType(contentType: string | undefined): boolean {
  if (contentType === undefined) return false;
  return STREAMING_CONTENT_TYPES.has((contentType.split(";")[0] ?? "").trim().toLowerCase());
}

/**
 * The request and response shapes both dialects present to a Node handler.
 *
 * Structural rather than the two Node unions: `http.ServerResponse` and
 * `http2.Http2ServerResponse` declare incompatible `writeHead` overloads, so a
 * union of them has no callable signature at all. This names exactly the three
 * members this wrapper touches, which BOTH satisfy.
 */
interface StreamableRequest {
  readonly headers: Record<string, string | string[] | undefined>;
}

/**
 * The request headers that offer the server a RESPONSE compression to use.
 *
 * Both dialects' spellings, because the router accepts both and a spelling
 * missed here is a negotiation that still happens behind our back.
 */
const ACCEPT_ENCODING_HEADERS = ["connect-accept-encoding", "grpc-accept-encoding"] as const;
interface StreamableResponse {
  readonly headersSent: boolean;
  // BOTH HEADER SHAPES. `writeHead` accepts an object or a flat array, the
  // adapter may use either, and a signature that admitted only one would let
  // the encoding guard below be typed past rather than satisfied.
  writeHead(status: number, headers: Record<string, string> | string[]): unknown;
  flushHeaders?: () => void;
}
type NodeHandler = (request: never, response: never) => void;

/**
 * Send the response head the moment a streaming request is accepted.
 *
 * Wraps the Connect adapter rather than patching it: the adapter's own
 * `writeHead` is lazy by design, and the only place that can beat it is the
 * layer above. See the note at the top of this file for why a quiet stream is
 * otherwise indistinguishable from a refused one.
 */
function withEarlyStreamHeaders<H extends NodeHandler>(handler: H): H {
  const wrapped = (request: StreamableRequest, response: StreamableResponse): void => {
    flushStreamHead(request, response);
    (handler as unknown as (request: StreamableRequest, response: StreamableResponse) => void)(
      request,
      response,
    );
  };
  return wrapped as unknown as H;
}

/**
 * Write and flush the head for a streaming request, exactly once.
 *
 * Separated from the wrapper so a suite can drive it with a plain object and
 * assert the three effects — head written, headers flushed, later writeHead
 * absorbed — without a socket.
 */
export function flushStreamHead(request: StreamableRequest, response: StreamableResponse): boolean {
  const header = request.headers["content-type"];
  const contentType = Array.isArray(header) ? header[0] : header;
  if (!isStreamingContentType(contentType) || response.headersSent) return false;
  // DECLINE THE RESPONSE COMPRESSION BEFORE THE ADAPTER NEGOTIATES IT.
  //
  // The adapter answers the client's accept list by setting
  // `Connect-Content-Encoding` on the response header — a decision it makes
  // when the handler is invoked, which is AFTER this head has gone out, so the
  // announcement can never reach the client and the guard below fired on every
  // healthy accept. Nothing was actually compressed (`compressMinBytes` is
  // MAX_SAFE_INTEGER), so the header was a promise about a body that was never
  // made good on either way.
  //
  // ONE FACT, ONE SOURCE: this server never compresses a response, so the
  // accept list is not negotiable and is rewritten to `identity` here rather
  // than re-deriving the adapter's negotiation into our own head. The REQUEST's
  // own `connect-content-encoding` is untouched — a compressed request is still
  // understood, and no head of ours is involved in reading one.
  for (const name of ACCEPT_ENCODING_HEADERS) {
    if (request.headers[name] === undefined) continue;
    request.headers[name] = "identity";
  }
  response.writeHead(200, { "content-type": contentType as string });
  if (typeof response.flushHeaders === "function") response.flushHeaders();
  // The adapter WILL call writeHead again. Once the head is out that is an
  // ERR_HTTP_HEADERS_SENT throw, so the call is absorbed rather than allowed to
  // fail a healthy stream.
  //
  // ABSORBED, NOT IGNORED. Anything the adapter meant to say about the body's
  // ENCODING is a statement the client needs and our head did not make, so a
  // non-identity encoding here means the stream we are about to send is
  // undecodable. The server is configured never to compress, so this can only
  // fire if that configuration is lost — and a loud record is the difference
  // between finding it here and finding it as an unreadable stream in the
  // daemon.
  response.writeHead = ((...args: unknown[]): unknown => {
    const encoding = encodingAnnouncedBy(args);
    if (encoding !== undefined && encoding !== "identity") {
      LOGGER.error(
        { content_type: contentType, content_encoding: encoding, detail: "the flushed response head omitted the adapter's later encoding" },
        "the adapter tried to announce a response encoding AFTER the stream head was flushed; the client cannot be told, so this stream would be undecodable",
      );
    }
    return response;
  });
  LOGGER.logVerbose({ content_type: contentType }, "flushed the response head on accepting a stream");
  return true;
}

/**
 * The body encoding a `writeHead` call announces, if it announces one.
 *
 * `writeHead` takes its headers as the last argument, either as an object or as
 * a flat array; both shapes are read, because a shape this missed would be a
 * silent gap in the guard above.
 */
function encodingAnnouncedBy(args: readonly unknown[]): string | undefined {
  const names = ["connect-content-encoding", "grpc-encoding", "content-encoding"];
  for (const arg of args) {
    if (arg === null || typeof arg !== "object") continue;
    if (Array.isArray(arg)) {
      for (let index = 0; index + 1 < arg.length; index += 2) {
        const key = String(arg[index]).toLowerCase();
        if (names.includes(key)) return String(arg[index + 1]);
      }
      continue;
    }
    for (const [key, value] of Object.entries(arg as Record<string, unknown>)) {
      if (names.includes(key.toLowerCase())) return String(value);
    }
  }
  return undefined;
}

/**
 * Peek at a connection's first bytes and hand it to the server that speaks its
 * dialect.
 *
 * The bytes are put BACK with `unshift` before the socket is handed over, so
 * the receiving server sees the connection exactly as it arrived — the peek is
 * invisible to it.
 */
function routeByPreface(socket: Socket, h1: http.Server, h2: http2.Http2Server): void {
  let seen = Buffer.alloc(0);
  const onReadable = (): void => {
    const chunk: Buffer | null = socket.read() as Buffer | null;
    if (chunk === null) return;
    seen = Buffer.concat([seen, chunk]);
    const verdict = sniffProtocol(seen);
    if (verdict === "need-more") return;
    socket.removeListener("readable", onReadable);
    socket.unshift(seen);
    LOGGER.logVerbose({ protocol: verdict }, "routed an inbound connection by its HTTP/2 preface");
    if (verdict === "h2") h2.emit("connection", socket);
    else h1.emit("connection", socket);
  };
  socket.on("readable", onReadable);
}
