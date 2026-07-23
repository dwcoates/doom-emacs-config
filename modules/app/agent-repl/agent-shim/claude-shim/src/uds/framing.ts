/**
 * Length-prefixed protobuf framing for every agent-shim UDS hop, plus the
 * message-multiplexing envelope layered on top of it. This is the TypeScript
 * twin of the Go `agentrepl/wire` package (agent-shim/wire/wire.go) and MUST
 * keep byte-for-byte parity with it: a 4-byte big-endian length prefix
 * followed by exactly that many payload bytes, one serialized message per
 * frame.
 *
 * SEMANTICS (mirrored from wire.go, deliberately identical):
 * - A length prefix greater than {@link MAX_FRAME} is a protocol violation:
 *   it surfaces as a loud error and POISONS the decoder. A corrupted stream
 *   cannot be trusted after a bad length, so there is no resync — every
 *   subsequent read fails.
 * - Clean EOF is reported ONLY at a frame boundary (the peer closed between
 *   frames). A stream that ends mid-header or mid-payload is a truncation and
 *   raises {@link UnexpectedEofError}, never a silent short read.
 * - A zero-length frame is legal (an all-defaults protobuf serializes to zero
 *   bytes); it decodes to an empty payload exactly as wire.go's ReadFrame
 *   does. wire.go rejects neither, and neither do we.
 *
 * MULTIPLEXING: several distinct protobuf message types share one connection
 * in each direction (e.g. SubmitPrompt, Ack, Event, Heartbeat on the
 * shim↔daemon socket). Serialized protobufs are not self-describing, so each
 * frame payload is a serialized google.protobuf.Any wrapping one core
 * message. The receiver dispatches on the Any type URL
 * (`type.googleapis.com/agentshim.core.v1.<Message>`). Any is the
 * protobuf-native discriminator and is unpackable identically from Go, which
 * is why the daemon-side client (G7) MUST use the same Any envelope. See the
 * G5 report for this contract.
 */
import net from "node:net";
import { create, fromBinary, toBinary } from "@bufbuild/protobuf";
import type { DescMessage, Message, MessageShape } from "@bufbuild/protobuf";
import { AnySchema, anyIs, anyPack, anyUnpack } from "@bufbuild/protobuf/wkt";
import type { Any } from "@bufbuild/protobuf/wkt";
import { shimLog } from "./log.js";

/** Maximum single-frame payload. Identical to wire.go's `MaxFrame`. */
export const MAX_FRAME = 32 << 20; // 32 MiB

/** A length prefix exceeded {@link MAX_FRAME}; twin of wire.ErrFrameTooLarge. */
export class FrameTooLargeError extends Error {
  constructor(claimed: number) {
    super(`wire: frame exceeds MaxFrame: header claims ${claimed} bytes`);
    this.name = "FrameTooLargeError";
  }
}

/** The stream ended mid-frame; twin of wire's io.ErrUnexpectedEOF path. */
export class UnexpectedEofError extends Error {
  constructor(buffered: number) {
    super(`wire: unexpected EOF: ${buffered} buffered bytes did not complete a frame`);
    this.name = "UnexpectedEofError";
  }
}

/** Encode one length-prefixed frame carrying `payload`; twin of WriteFrame. */
export function encodeFrame(payload: Uint8Array): Uint8Array {
  if (payload.length > MAX_FRAME) {
    throw new FrameTooLargeError(payload.length);
  }
  const out = Buffer.allocUnsafe(4 + payload.length);
  out.writeUInt32BE(payload.length, 0);
  out.set(payload, 4);
  return out;
}

/**
 * Incremental frame reader. Sockets deliver arbitrary byte chunks, so this
 * buffers across chunks and yields whole frames, matching wire.ReadFrame's
 * boundary and error semantics. Once poisoned by an over-size length it stays
 * poisoned (no resync past corruption).
 */
export class FrameDecoder {
  private buf: Buffer = Buffer.alloc(0);
  private poison: Error | null = null;

  /**
   * Feed one chunk; return every complete frame now available (possibly
   * none). Throws {@link FrameTooLargeError} on an over-size length prefix,
   * after which the decoder is permanently poisoned.
   */
  push(chunk: Uint8Array): Uint8Array[] {
    if (this.poison) throw this.poison;
    this.buf = this.buf.length === 0 ? Buffer.from(chunk) : Buffer.concat([this.buf, chunk]);
    const frames: Uint8Array[] = [];
    for (;;) {
      if (this.buf.length < 4) break; // header incomplete
      const n = this.buf.readUInt32BE(0);
      if (n > MAX_FRAME) {
        this.poison = new FrameTooLargeError(n);
        throw this.poison;
      }
      if (this.buf.length < 4 + n) break; // payload incomplete
      frames.push(Uint8Array.prototype.slice.call(this.buf, 4, 4 + n));
      this.buf = this.buf.subarray(4 + n);
    }
    return frames;
  }

  /**
   * Signal end-of-stream. Clean only at a frame boundary; leftover bytes
   * (a partial header or payload) are a truncation and raise
   * {@link UnexpectedEofError}.
   */
  end(): void {
    if (this.poison) throw this.poison;
    if (this.buf.length !== 0) {
      throw new UnexpectedEofError(this.buf.length);
    }
  }
}

// ---------------------------------------------------------------------------
// Any-envelope multiplexing
// ---------------------------------------------------------------------------

/** Pack one message into a serialized Any-in-a-frame ready for the wire. */
export function encodeMessage<Desc extends DescMessage>(
  schema: Desc,
  message: MessageShape<Desc>,
): Uint8Array {
  return encodeFrame(toBinary(AnySchema, anyPack(schema, message)));
}

/** Decode a frame payload back into a google.protobuf.Any for dispatch. */
export function decodeEnvelope(payload: Uint8Array): Any {
  return fromBinary(AnySchema, payload);
}

/** True when `any` carries a message of `schema`. */
export function envelopeIs(any: Any, schema: DescMessage): boolean {
  return anyIs(any, schema);
}

/** Unpack `any` as `schema`, or undefined if it carries a different type. */
export function unpackAs<Desc extends DescMessage>(
  any: Any,
  schema: Desc,
): MessageShape<Desc> | undefined {
  return anyUnpack(any, schema);
}

/** The bare type name inside an Any's type URL (for loud logs / dispatch). */
export function envelopeType(any: Any): string {
  const url = any.typeUrl;
  const slash = url.lastIndexOf("/");
  return slash >= 0 ? url.slice(slash + 1) : url;
}

// ---------------------------------------------------------------------------
// MessageConn — a socket wrapped as a typed message channel
// ---------------------------------------------------------------------------

export interface MessageConnHandlers {
  /** One inbound message (decoded to an Any; dispatch via {@link unpackAs}). */
  onMessage(message: Any): void;
  /**
   * The connection closed. `err` is set on a truncation (mid-frame EOF),
   * corrupt frame, or socket error, and null on a clean close between frames.
   * Fires exactly once.
   */
  onClose(err: Error | null): void;
}

/**
 * Binds a connected `net.Socket` to the framed Any-envelope protocol: decodes
 * inbound frames into messages, encodes outbound messages, and reports close
 * with truncation/corruption surfaced loudly. It never buffers unsent
 * outbound data of its own beyond the socket's kernel buffer.
 */
export class MessageConn {
  private readonly decoder = new FrameDecoder();
  private closeErr: Error | null = null;
  private closed = false;

  constructor(
    private readonly socket: net.Socket,
    private readonly handlers: MessageConnHandlers,
    private readonly component: string,
  ) {
    socket.on("data", (chunk: Buffer) => this.onData(chunk));
    socket.on("end", () => this.onEnd());
    socket.on("error", (err: Error) => this.onError(err));
    socket.on("close", () => this.onCloseEvent());
  }

  /** Serialize and write one message. No-op with a loud log if not writable. */
  send<Desc extends DescMessage>(schema: Desc, message: MessageShape<Desc>): void {
    if (this.closed || this.socket.destroyed || !this.socket.writable) {
      shimLog(this.component, {}, `dropping outbound message: connection not writable`);
      return;
    }
    this.socket.write(encodeMessage(schema, message));
  }

  /** Tear down the connection (a deliberate local close, not an error). */
  close(): void {
    this.socket.destroy();
  }

  private onData(chunk: Buffer): void {
    let frames: Uint8Array[];
    try {
      frames = this.decoder.push(chunk);
    } catch (err) {
      shimLog(this.component, {}, `frame decode failed (stream poisoned): ${errMsg(err)}`);
      this.closeErr = asError(err);
      this.socket.destroy();
      return;
    }
    for (const frame of frames) {
      let any: Any;
      try {
        any = decodeEnvelope(frame);
      } catch (err) {
        shimLog(this.component, {}, `envelope decode failed on a ${frame.length}-byte frame: ${errMsg(err)}`);
        this.closeErr = asError(err);
        this.socket.destroy();
        return;
      }
      this.handlers.onMessage(any);
    }
  }

  private onEnd(): void {
    // Peer half-closed. A clean close lands exactly at a frame boundary; a
    // partial frame in the buffer is a truncation the contract surfaces.
    try {
      this.decoder.end();
    } catch (err) {
      shimLog(this.component, {}, `stream truncated mid-frame: ${errMsg(err)}`);
      this.closeErr = asError(err);
    }
  }

  private onError(err: Error): void {
    // ECONNRESET etc. Recorded (not thrown): 'close' delivers the verdict.
    if (this.closeErr === null) this.closeErr = err;
  }

  private onCloseEvent(): void {
    if (this.closed) return;
    this.closed = true;
    this.handlers.onClose(this.closeErr);
  }
}

function asError(err: unknown): Error {
  return err instanceof Error ? err : new Error(String(err));
}

function errMsg(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}

/** Re-export the low-level primitives tests and callers lean on. */
export { create, toBinary, fromBinary };
export type { Any, Message };
