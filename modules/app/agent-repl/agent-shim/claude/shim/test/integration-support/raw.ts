/**
 * test/integration-support/raw.ts — dial the shim BELOW the Connect client.
 *
 * # Why a raw dial exists at all
 *
 * One obligation cannot be observed through a client: the standing-stream
 * transport ruling says the shim FLUSHES RESPONSE HEADERS the moment it accepts
 * a `WatchSession`/`WatchAgent`/`WatchBash`. A Connect client hands back an
 * async iterable and hides the head entirely — from up there, "accepted" and
 * "sent a frame" are the same event.
 *
 * The raw dial splits them, but WHAT it can split depends on the transport:
 *
 * - over h2c the head is its own HEADERS frame, so `rawStreamOpenH2` observes
 *   the ORDERING directly — `headersAt`, `firstByteAt`, and `bodyBeforeHeaders`;
 * - over HTTP/1.1 the head and the first frame share one byte stream and can
 *   land in one read, so ordering is NOT an observable there. `rawHeadH1`
 *   instead witnesses what h1 does decide: the head alone parses and carries
 *   acceptance, with no frame consulted.
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

/** What a raw h2c stream open observed, in the order it observed it. */
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

/** A response head observed on its own, with no frame consulted. */
export interface RawStreamHead {
  /** The HTTP status of the response head. */
  readonly status: number;
  /** The head's content type, which says the stream was accepted as one. */
  readonly contentType: string;
  /** Every header of the head, lower-cased names to values. */
  readonly headers: ReadonlyMap<string, string>;
  /** `process.hrtime.bigint()` when the head was complete. */
  readonly headersAt: bigint;
}

/**
 * Open a server stream over HTTP/1.1 and report the RESPONSE HEAD ALONE.
 *
 * WHAT HTTP/1.1 CAN AND CANNOT WITNESS. The head and the body share one byte
 * stream and one TCP window, so a head flushed on accept and a head written
 * together with the first frame can land in the same read — head-before-body
 * ORDERING is simply not an observable over h1, and asserting it is a coin
 * toss under load. The h2c sibling keeps that ordering claim, because there
 * the head is its own HEADERS frame and its own event.
 *
 * What h1 DOES witness is the claim the standing-stream transport ruling
 * actually rests on: ACCEPTANCE IS DECIDABLE FROM THE HEAD ALONE. This helper
 * resolves the moment the header block is complete — it never waits for a body
 * byte, and never reads one to decide anything. If the shim withheld its head
 * until it had a verdict to report, or reported the verdict as an HTTP-level
 * refusal, this promise would either not resolve or resolve to a head that does
 * not say "accepted". Resolving with a 200 and a Connect stream content type is
 * the whole acceptance answer, with zero frames consulted.
 *
 * DIALED AT THE SOCKET, not through `http.request`: Node's client would parse
 * the head for us and buffer past it, which is exactly the boundary under test.
 * The socket is destroyed before resolving — a standing stream never ends, and
 * leaving it open would hold the shim's listener past the test.
 */
export function rawHeadH1(
  socketPath: string,
  procedure: string,
  body: Buffer,
): Promise<RawStreamHead> {
  return new Promise<RawStreamHead>((resolve, reject) => {
    const socket = netConnect(socketPath);
    socket.once("error", reject);

    let buffered = Buffer.alloc(0);
    let settled = false;

    socket.on("data", (chunk: Buffer) => {
      if (settled) return;
      const at = process.hrtime.bigint();
      buffered = Buffer.concat([buffered, chunk]);
      const boundary = buffered.indexOf("\r\n\r\n");
      if (boundary < 0) return;
      settled = true;

      const lines = buffered.subarray(0, boundary).toString("latin1").split("\r\n");
      const headers = new Map<string, string>();
      for (const line of lines.slice(1)) {
        const colon = line.indexOf(":");
        if (colon < 0) continue;
        headers.set(line.slice(0, colon).toLowerCase().trim(), line.slice(colon + 1).trim());
      }
      socket.destroy();
      resolve({
        status: Number(lines[0]?.split(" ")[1] ?? 0),
        contentType: headers.get("content-type") ?? "",
        headers,
        headersAt: at,
      });
    });
    socket.once("close", () => {
      if (!settled) reject(new Error("the connection closed before a complete response head"));
    });

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

/**
 * Observe head-versus-first-frame ORDERING over h2c, where the head is its own
 * HEADERS frame and therefore its own event — the one transport on which the
 * standing-stream flush is observable as an ordering, rather than only as an
 * acceptance decidable from the head (see `rawHeadH1`).
 */
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
