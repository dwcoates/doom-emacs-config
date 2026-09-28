/**
 * store/client.ts — the shim's dial-out to the store over its unix socket.
 *
 * # Why an interface rather than the generated client directly
 *
 * Everything above this line (the writer's retry buffer, the reader that serves
 * history, the reconciler that resolves live work at session start) needs to be
 * unit-testable WITHOUT a store, and a generated Connect client is not
 * implementable by hand — it carries call options, headers and abort signals on
 * every method. {@link StoreClient} is the seven verbs as the shim uses them,
 * so a test substitutes an object literal and a caller cannot accidentally
 * depend on transport details that would then have to be faked.
 *
 * # HTTP/1.1, deliberately
 *
 * Server streaming works fine over HTTP/1.1 (the Connect protocol frames it in
 * a chunked body), and `http.request` accepts a `socketPath` option, so a unix
 * socket is one option away. `http2.connect` has NO `socketPath` — it wants a
 * `createConnection` factory — so an h2c client that passes `socketPath`
 * silently dials the AUTHORITY over TCP and fails Unavailable against a host
 * that was never listening. HTTP/1.1 removes that trap entirely.
 *
 * The base URL is a placeholder: with `socketPath` set, the authority is never
 * resolved, and only the path of each request is used.
 */
import { createClient, type Client } from "@connectrpc/connect";
import { createConnectTransport } from "@connectrpc/connect-node";
import { bindLog } from "../log.js";
import { storev1 } from "../proto.js";

const LOGGER = bindLog({ component: "shim-store-client", operation: "shim.store.client" });

/**
 * The authority every store request nominally addresses.
 *
 * Never resolved — `socketPath` decides where the bytes go — but Connect needs
 * a syntactically valid base URL to build request paths from.
 */
export const STORE_BASE_URL = "http://store";

/**
 * The store, as the shim uses it: one method per store.v1 rpc.
 *
 * `watchAgentSession` is the only stream. Its OPEN can be refused — an unknown
 * or already-consumed token, or a store that restarted and forgot it — and that
 * refusal arrives as a Connect `NotFound` at the transport, because a stream
 * has no failure message to carry one.
 */
export interface StoreClient {
  /** Open one agent's reading session: the first page plus the watch token. */
  openAgentSession(
    request: storev1.OpenAgentSessionRequest,
  ): Promise<storev1.OpenAgentSessionResponse>;
  /**
   * The pure tail of an opened reading session. Throws NotFound on a refused open.
   *
   * `signal` IS HOW A TAIL ENDS, and it is not optional in practice: Connect's
   * stream close DRAINS the response body, which never completes on a STANDING
   * stream — so a reader that merely stopped iterating would block forever.
   * Cancelling the call is the only way out, and every teardown path takes it.
   */
  watchAgentSession(
    request: storev1.WatchAgentSessionRequest,
    signal?: AbortSignal,
  ): AsyncIterable<storev1.WatchAgentSessionResponse>;
  /**
   * One detached shell run's lifecycle rows: every stored row in write order,
   * then the tail, ending after the terminal.
   *
   * THE ONE READ PATH TO THE BASH TABLE, and the reason a detached shell's
   * output can come entirely from the sidecar and still be served by the shim's
   * own WatchBash. A run with no stored row is a refused open — a Connect
   * `NotFound` at the transport, like every other watch here. `signal` ends it,
   * per the standing-stream rule.
   */
  watchBashRun(
    request: storev1.WatchBashRunRequest,
    signal?: AbortSignal,
  ): AsyncIterable<storev1.WatchBashRunResponse>;
  /** An OLDER page of one book, walking down from a served pointer. */
  readAgentPage(request: storev1.ReadAgentPageRequest): Promise<storev1.ReadAgentPageResponse>;
  /** One workflow run's stored state. Answers Unimplemented this wave. */
  getWorkflow(request: storev1.GetWorkflowRequest): Promise<storev1.GetWorkflowResponse>;
  /** The sidecar's persisted file cursors. */
  getSidecarCursors(
    request: storev1.GetSidecarCursorsRequest,
  ): Promise<storev1.GetSidecarCursorsResponse>;
  /** The open obligations: everything started and never concluded, per the record. */
  getLiveWork(request: storev1.GetLiveWorkRequest): Promise<storev1.GetLiveWorkResponse>;
  /** Which agent of the caller's lineage a vendor task locator names. */
  getAgentByVendorTask(
    request: storev1.GetAgentByVendorTaskRequest,
  ): Promise<storev1.GetAgentByVendorTaskResponse>;
  /** One batch, durable or nothing, cursor advance in the same transaction. */
  writeBatch(request: storev1.WriteBatchRequest): Promise<storev1.WriteBatchResponse>;
}

/** The generated Connect client, before it is narrowed to {@link StoreClient}. */
type GeneratedStoreClient = Client<typeof storev1.ShimStore>;

/** Record both sides of one unary store round-trip without owning its failures. */
async function unaryRoundTrip<T>(rpc: string, act: () => Promise<T>): Promise<T> {
  LOGGER.debug({ rpc, boundary: "entered" }, `calling store.v1.${rpc}`);
  const response = await act();
  LOGGER.debug({ rpc, boundary: "completed" }, `completed store.v1.${rpc}`);
  return response;
}

/** Record both sides of one streaming store round-trip without owning its failures. */
async function* streamingRoundTrip<T>(rpc: string, frames: () => AsyncIterable<T>): AsyncIterable<T> {
  LOGGER.debug({ rpc, boundary: "entered" }, `calling store.v1.${rpc}`);
  yield* frames();
  LOGGER.debug({ rpc, boundary: "completed" }, `completed store.v1.${rpc} stream`);
}

/** Build the generated Connect client for a store listening on `socketPath`. */
function createStoreTransportClient(socketPath: string): GeneratedStoreClient {
  if (socketPath === "") {
    throw new Error("shim store client: a store socket path is required");
  }
  LOGGER.debug({ store_socket: socketPath }, "creating the store.v1 client over its unix socket");
  return createClient(
    storev1.ShimStore,
    createConnectTransport({
      httpVersion: "1.1",
      baseUrl: STORE_BASE_URL,
      nodeOptions: { socketPath },
    }),
  );
}

/**
 * The store client the rest of the shim uses.
 *
 * A thin adaptation of the generated client onto {@link StoreClient}: it drops
 * the call-options parameter nothing here passes, which is what makes the
 * interface implementable by a test fake.
 */
export function createStoreClient(socketPath: string): StoreClient {
  const client = createStoreTransportClient(socketPath);
  return {
    openAgentSession: (request) => unaryRoundTrip("OpenAgentSession", () => client.openAgentSession(request)),
    watchAgentSession: (request, signal) => streamingRoundTrip("WatchAgentSession", () => client.watchAgentSession(request, { signal })),
    watchBashRun: (request, signal) => streamingRoundTrip("WatchBashRun", () => client.watchBashRun(request, { signal })),
    readAgentPage: (request) => unaryRoundTrip("ReadAgentPage", () => client.readAgentPage(request)),
    getWorkflow: (request) => unaryRoundTrip("GetWorkflow", () => client.getWorkflow(request)),
    getSidecarCursors: (request) => unaryRoundTrip("GetSidecarCursors", () => client.getSidecarCursors(request)),
    getLiveWork: (request) => unaryRoundTrip("GetLiveWork", () => client.getLiveWork(request)),
    getAgentByVendorTask: (request) =>
      unaryRoundTrip("GetAgentByVendorTask", () => client.getAgentByVendorTask(request)),
    writeBatch: (request) => unaryRoundTrip("WriteBatch", () => client.writeBatch(request)),
  };
}
