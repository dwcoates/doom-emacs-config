/**
 * test/integration-support/raw.ts — dial the shim BELOW the Connect client.
 *
 * # Why a raw dial exists at all
 *
 * One obligation cannot be observed through a client: the standing-stream
 * transport ruling says the shim FLUSHES RESPONSE HEADERS the moment it accepts
 * a `WatchSession`/`WatchAgent`/`WatchBash`, so acceptance is observable before
 * the first frame. A Connect client hands back an async iterable and hides the
 * head entirely — from up there, "accepted" and "sent a frame" are the same
 * event. Reaching for the raw response means the two are separately
 * observable: `headersAt` is stamped when the head arrives, `firstByteAt` when
 * the first body byte does, and `bodyBeforeHeaders` records whether any byte
 * had already arrived when the head landed.
 *
 * # The framing, by hand
 *
 * The Connect streaming protocol frames each message as a 5-byte prefix (one
 * flags byte, then a big-endian uint32 length) followed by the message bytes.
 * The request body is built from the SAME generated schema the shim serves, so
 * this dial cannot ask for a contract the shim does not have — only the
 * transport is hand-rolled, never the message.
 */
import { toBinary, type DescMessage, type MessageShape } from "@bufbuild/protobuf";
import http2 from "node:http2";
import { connect as netConnect } from "node:net";

/** The content type of a Connect server-stream call over either HTTP version. */
export const CONNECT_STREAM_CONTENT_TYPE = "application/connect+proto";

/** Frame one message the way the Connect streaming protocol does. */
export function envelope(bytes: Uint8Array): Buffer {
  const prefix = Buffer.alloc(5);
  prefix.writeUInt8(0, 0);
  prefix.writeUInt32BE(bytes.length, 1);
  return Buffer.concat([prefix, Buffer.from(bytes)]);
}

/** What a raw stream open observed, in the order it observed it. */
export interface RawStreamOpen {
  /** The HTTP status of the response head. */
  readonly status: number;
  /** The head's content type, which says the stream was accepted as one. */
  readonly contentType: string;
  /** `process.hrtime.bigint()` when the head arrived. */
  readonly headersAt: bigint;
  /** When the first body byte arrived, or null when none did. */
  readonly firstByteAt: bigint | null;
  /**
   * Whether any body byte had ALREADY arrived when the head was observed.
   *
   * This is the assertion that matters: a server that withheld its head until
   * it had a frame to send would make the two indistinguishable, and a server
   * that flushed on accept cannot have body bytes before its own head.
   */
  readonly bodyBeforeHeaders: boolean;
}

/**
 * Open a server stream over HTTP/1.1 and report the head/body ordering.
 *
 * DIALED AT THE SOCKET, not through `http.request`. Node's client parses the
 * response and hands back a stream whose `data` events are already past the
 * head, so from up there "the head arrived alone" and "the head arrived glued
 * to a frame" are indistinguishable — which is why an earlier version of this
 * helper simply reported `bodyBeforeHeaders: false` and asserted nothing.
 *
 * On HTTP/1.1 the head and the body share one byte stream, so the observable
 * is WHICH CHUNK completes the head: a server that flushed on accept writes the
 * head by itself and the chunk carrying `\r\n\r\n` ends there, while a server
 * that withheld its head until it had something to say writes the head and the
 * first frame together. That is the same question `rawStreamOpenH2` answers by
 * comparing two events, asked in the shape HTTP/1.1 makes it available.
 *
 * Resolves once the first body byte has arrived (or the response ended), which
 * is the point at which both stamps exist. The socket is destroyed before
 * resolving: a standing stream never ends, and leaving it open would hold the
 * shim's listener past the test.
 */
export function rawStreamOpenH1(
  socketPath: string,
  procedure: string,
  body: Buffer,
): Promise<RawStreamOpen> {
  return new Promise<RawStreamOpen>((resolve, reject) => {
    const socket = netConnect(socketPath);
    socket.once("error", reject);

    let buffered = Buffer.alloc(0);
    let headersAt: bigint | null = null;
    let firstByteAt: bigint | null = null;
    let status = 0;
    let contentType = "";
    let bodyBeforeHeaders = false;
    let settled = false;

    const settle = (): void => {
      if (settled || headersAt === null) return;
      settled = true;
      socket.destroy();
      resolve({ status, contentType, headersAt, firstByteAt, bodyBeforeHeaders });
    };

    socket.on("data", (chunk: Buffer) => {
      const at = process.hrtime.bigint();
      if (headersAt === null) {
        buffered = Buffer.concat([buffered, chunk]);
        const boundary = buffered.indexOf("\r\n\r\n");
        if (boundary < 0) return;
        headersAt = at;
        const head = buffered.subarray(0, boundary).toString("latin1").split("\r\n");
        status = Number(head[0]?.split(" ")[1] ?? 0);
        for (const line of head.slice(1)) {
          const colon = line.indexOf(":");
          if (colon < 0) continue;
          if (line.slice(0, colon).toLowerCase() === "content-type") {
            contentType = line.slice(colon + 1).trim();
          }
        }
        // THE ASSERTION THIS HELPER EXISTS FOR: body bytes riding in the very
        // chunk that completed the head mean the server had already produced a
        // frame when it finally flushed.
        const trailing = buffered.length - (boundary + 4);
        if (trailing > 0) {
          bodyBeforeHeaders = true;
          firstByteAt = at;
          settle();
          return;
        }
        return;
      }
      if (firstByteAt === null) firstByteAt = at;
      settle();
    });
    socket.once("end", settle);
    socket.once("close", settle);

    socket.write(
      [
        `POST ${procedure} HTTP/1.1`,
        "host: shim",
        `content-type: ${CONNECT_STREAM_CONTENT_TYPE}`,
        "connect-protocol-version: 1",
        `content-length: ${String(body.length)}`,
        "",
        "",
      ].join("\r\n"),
    );
    socket.write(body);
  });
}

/** The same observation over h2c, where the head is its own event. */
export function rawStreamOpenH2(
  socketPath: string,
  procedure: string,
  body: Buffer,
): Promise<RawStreamOpen> {
  return new Promise<RawStreamOpen>((resolve, reject) => {
    // socketPath is silently ignored by http2.connect; createConnection is the
    // only way to reach a unix socket with an h2 session.
    const session = http2.connect("http://shim", {
      createConnection: () => netConnect(socketPath),
    });
    session.once("error", reject);
    const stream = session.request({
      [http2.constants.HTTP2_HEADER_METHOD]: "POST",
      [http2.constants.HTTP2_HEADER_PATH]: procedure,
      "content-type": CONNECT_STREAM_CONTENT_TYPE,
      "connect-protocol-version": "1",
    });
    let sawData = false;
    let firstByteAt: bigint | null = null;
    stream.on("data", () => {
      if (firstByteAt === null) firstByteAt = process.hrtime.bigint();
      sawData = true;
      finish();
    });
    stream.once("response", (headers) => {
      const headersAt = process.hrtime.bigint();
      const bodyBeforeHeaders = sawData;
      settleWith(headers, headersAt, bodyBeforeHeaders);
    });
    stream.once("error", reject);

    let head: {
      readonly status: number;
      readonly contentType: string;
      readonly headersAt: bigint;
      readonly bodyBeforeHeaders: boolean;
    } | null = null;
    let done = false;

    function settleWith(
      headers: http2.IncomingHttpHeaders,
      headersAt: bigint,
      bodyBeforeHeaders: boolean,
    ): void {
      head = {
        status: Number(headers[http2.constants.HTTP2_HEADER_STATUS] ?? 0),
        contentType: String(headers["content-type"] ?? ""),
        headersAt,
        bodyBeforeHeaders,
      };
      finish();
    }

    function finish(): void {
      if (done || head === null || firstByteAt === null) return;
      done = true;
      stream.destroy();
      session.destroy();
      resolve({ ...head, firstByteAt });
    }

    stream.once("close", () => {
      if (done || head === null) return;
      done = true;
      session.destroy();
      resolve({ ...head, firstByteAt: null });
    });
    stream.end(body);
  });
}

/** Serialize a request message for a raw dial, from its own generated schema. */
export function rawBody<Desc extends DescMessage>(
  schema: Desc,
  message: MessageShape<Desc>,
): Buffer {
  return envelope(toBinary(schema, message));
}
