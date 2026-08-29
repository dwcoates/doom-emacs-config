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
  /** The pure tail of an opened reading session. Throws NotFound on a refused open. */
  watchAgentSession(
    request: storev1.WatchAgentSessionRequest,
  ): AsyncIterable<storev1.WatchAgentSessionResponse>;
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
  /** One batch, durable or nothing, cursor advance in the same transaction. */
  writeBatch(request: storev1.WriteBatchRequest): Promise<storev1.WriteBatchResponse>;
}

/** The generated Connect client, before it is narrowed to {@link StoreClient}. */
export type GeneratedStoreClient = Client<typeof storev1.ShimStore>;

/** Build the generated Connect client for a store listening on `socketPath`. */
export function createStoreTransportClient(socketPath: string): GeneratedStoreClient {
  if (socketPath === "") {
    throw new Error("shim store client: a store socket path is required");
  }
  LOGGER.log({ store_socket: socketPath }, "creating the store.v1 client over its unix socket");
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
    openAgentSession: (request) => client.openAgentSession(request),
    watchAgentSession: (request) => client.watchAgentSession(request),
    readAgentPage: (request) => client.readAgentPage(request),
    getWorkflow: (request) => client.getWorkflow(request),
    getSidecarCursors: (request) => client.getSidecarCursors(request),
    getLiveWork: (request) => client.getLiveWork(request),
    writeBatch: (request) => client.writeBatch(request),
  };
}
