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
export type SniffedProtocol = "h2" | "h1" | "need-more";

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

/** A bound listener, and the way to stop it. */
export interface ShimServer {
  /** The path the listener is bound to. */
  readonly socketPath: string;
  /** Stop accepting, cut every open connection, and remove the socket file. */
  close(): Promise<void>;
}

/** What a probe of an existing socket path concluded. */
export type SocketProbe = "free" | "stale" | "live";

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
    LOGGER.log({ socket_path: socketPath, why }, `removed the shim socket file (${why})`);
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") return;
    LOGGER.log(
      { level: "error", socket_path: socketPath, why, cause: err },
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
    LOGGER.log(
      { level: "error", socket_path: socketPath },
      "refusing to bind: another shim is already listening on this socket",
    );
    throw new Error(
      `shim: ${socketPath} already has a live listener; refusing to start a second shim on one session socket`,
    );
  }
  if (verdict === "stale") unlinkSocketFile(socketPath, "stale predecessor");

  const handler = connectNodeAdapter({ routes });
  // Neither of these ever LISTENS. They exist to own a connection's protocol
  // state machine; the net server below feeds them sockets directly.
  const h1 = http.createServer(handler);
  const h2 = http2.createServer(handler);
  h1.on("clientError", (err, socket) => {
    LOGGER.log({ level: "warn", cause: err }, "an HTTP/1.1 client connection failed before a request was read");
    socket.destroy();
  });
  h2.on("sessionError", (err) => {
    LOGGER.log({ level: "warn", cause: err }, "an h2c session failed");
  });

  const open = new Set<Socket>();
  const listener = createNetServer((socket) => {
    open.add(socket);
    socket.once("close", () => open.delete(socket));
    routeByPreface(socket, h1, h2);
  });

  await new Promise<void>((resolve, reject) => {
    const onError = (err: Error): void => {
      LOGGER.log({ level: "error", socket_path: socketPath, cause: err }, "the shim listener failed to bind");
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
    LOGGER.log({ level: "error", socket_path: socketPath, cause: err }, "the shim listener raised an error while serving");
  });

  LOGGER.log({ socket_path: socketPath, probe: verdict }, "shim.v1 listener bound and accepting");

  return {
    socketPath,
    close: async (): Promise<void> => {
      // THE ORDER MATTERS. `close()` stops accepting and then waits for every
      // open connection to end, and this shim's connections do not end on their
      // own: a standing WatchSession never concludes, and an h2c session is held
      // open by its client. Waiting first would hang the stand-down forever, so
      // the open sockets are cut inside the same tick that closes the listener.
      await new Promise<void>((resolve) => {
        listener.close(() => resolve());
        for (const socket of open) socket.destroy();
        open.clear();
      });
      h2.close();
      h1.close();
      unlinkSocketFile(socketPath, "listener closed");
      LOGGER.log({ socket_path: socketPath }, "shim.v1 listener closed and its socket removed");
    },
  };
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
