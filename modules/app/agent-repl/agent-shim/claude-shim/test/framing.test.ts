import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FrameDecoder,
  FrameTooLargeError,
  MAX_FRAME,
  MessageConn,
  UnexpectedEofError,
  decodeEnvelope,
  encodeFrame,
  encodeMessage,
  envelopeType,
  unpackAs,
} from "../src/uds/framing.js";
import type { Any } from "../src/uds/framing.js";
import {
  AckSchema,
  ShimHelloSchema,
  SubmitPromptSchema,
} from "../src/uds/proto.js";
import { socketPair } from "./uds-harness.js";

describe("encodeFrame", () => {
  it("prefixes a 4-byte big-endian length", () => {
    // Arrange
    const payload = new Uint8Array([1, 2, 3]);
    // Act
    const frame = encodeFrame(payload);
    // Assert
    expect(Array.from(frame)).toEqual([0, 0, 0, 3, 1, 2, 3]);
  });

  it("encodes a zero-length payload as a bare header", () => {
    // Arrange / Act
    const frame = encodeFrame(new Uint8Array(0));
    // Assert
    expect(Array.from(frame)).toEqual([0, 0, 0, 0]);
  });

  it("throws FrameTooLargeError above MAX_FRAME", () => {
    // Arrange
    const oversize = new Uint8Array(MAX_FRAME + 1);
    // Act / Assert
    expect(() => encodeFrame(oversize)).toThrow(FrameTooLargeError);
  });
});

describe("FrameDecoder", () => {
  it("decodes a single whole frame", () => {
    // Arrange
    const dec = new FrameDecoder();
    // Act
    const frames = dec.push(encodeFrame(new Uint8Array([9, 8, 7])));
    // Assert
    expect(frames.map((f) => Array.from(f))).toEqual([[9, 8, 7]]);
  });

  it("reassembles a frame split across chunks", () => {
    // Arrange
    const dec = new FrameDecoder();
    const whole = encodeFrame(new Uint8Array([1, 2, 3, 4]));
    // Act: split mid-header and mid-payload
    const r1 = dec.push(whole.subarray(0, 2));
    const r2 = dec.push(whole.subarray(2, 6));
    const r3 = dec.push(whole.subarray(6));
    // Assert
    expect(r1).toEqual([]);
    expect(r2).toEqual([]);
    expect(r3.map((f) => Array.from(f))).toEqual([[1, 2, 3, 4]]);
  });

  it("yields multiple frames from one chunk", () => {
    // Arrange
    const dec = new FrameDecoder();
    const buf = Buffer.concat([
      encodeFrame(new Uint8Array([1])),
      encodeFrame(new Uint8Array([2, 2])),
    ]);
    // Act
    const frames = dec.push(buf);
    // Assert
    expect(frames.map((f) => Array.from(f))).toEqual([[1], [2, 2]]);
  });

  it("treats a zero-length frame as a valid empty payload", () => {
    // Arrange
    const dec = new FrameDecoder();
    // Act
    const frames = dec.push(new Uint8Array([0, 0, 0, 0]));
    // Assert
    expect(frames.map((f) => Array.from(f))).toEqual([[]]);
  });

  it("throws FrameTooLargeError on an over-size length prefix", () => {
    // Arrange
    const dec = new FrameDecoder();
    const hdr = Buffer.alloc(4);
    hdr.writeUInt32BE(MAX_FRAME + 1, 0);
    // Act / Assert
    expect(() => dec.push(hdr)).toThrow(FrameTooLargeError);
  });

  it("stays poisoned after an over-size length (no resync)", () => {
    // Arrange
    const dec = new FrameDecoder();
    const hdr = Buffer.alloc(4);
    hdr.writeUInt32BE(MAX_FRAME + 1, 0);
    try {
      dec.push(hdr);
    } catch {
      /* first throw expected */
    }
    // Act / Assert: a subsequent, perfectly valid frame still throws
    expect(() => dec.push(encodeFrame(new Uint8Array([1])))).toThrow(FrameTooLargeError);
  });

  it("end() at a frame boundary is a clean close", () => {
    // Arrange
    const dec = new FrameDecoder();
    dec.push(encodeFrame(new Uint8Array([1, 2])));
    // Act / Assert
    expect(() => dec.end()).not.toThrow();
  });

  it("end() mid-frame raises UnexpectedEofError", () => {
    // Arrange
    const dec = new FrameDecoder();
    dec.push(encodeFrame(new Uint8Array([1, 2, 3])).subarray(0, 5)); // header + 1 of 3
    // Act / Assert
    expect(() => dec.end()).toThrow(UnexpectedEofError);
  });
});

describe("Any envelope multiplexing", () => {
  it("round-trips a message through encodeMessage/decodeEnvelope/unpackAs", () => {
    // Arrange
    const hello = create(ShimHelloSchema, { sessionId: "s1", vendor: "claude" });
    // Act
    const frame = encodeMessage(ShimHelloSchema, hello);
    const any = decodeEnvelope(frame.subarray(4)); // strip the length prefix
    const back = unpackAs(any, ShimHelloSchema);
    // Assert
    expect(back?.sessionId).toBe("s1");
    expect(back?.vendor).toBe("claude");
  });

  it("stamps the Go-registry type URL as the discriminator", () => {
    // Arrange / Act
    const any = decodeEnvelope(
      encodeMessage(SubmitPromptSchema, create(SubmitPromptSchema, { requestId: "r" })).subarray(4),
    );
    // Assert: this string MUST match the Go proto registry name (G7 interop)
    expect(any.typeUrl).toBe("type.googleapis.com/agentshim.core.v1.SubmitPrompt");
    expect(envelopeType(any)).toBe("agentshim.core.v1.SubmitPrompt");
  });

  it("distinguishes message types by their type URL", () => {
    // Arrange
    const ackAny = decodeEnvelope(
      encodeMessage(AckSchema, create(AckSchema, { requestId: "r" })).subarray(4),
    );
    // Act / Assert: an Ack does not unpack as a SubmitPrompt
    expect(unpackAs(ackAny, AckSchema)).toBeDefined();
    expect(unpackAs(ackAny, SubmitPromptSchema)).toBeUndefined();
  });
});

describe("MessageConn", () => {
  it("round-trips a message between the two socket ends", async () => {
    // Arrange
    const pair = await socketPair();
    const received: Any[] = [];
    const conn = new MessageConn(
      pair.a,
      { onMessage: (m) => received.push(m), onClose: () => {} },
      "test",
    );
    const peer = new MessageConn(pair.b, { onMessage: () => {}, onClose: () => {} }, "peer");
    // Act
    peer.send(ShimHelloSchema, create(ShimHelloSchema, { sessionId: "abc" }));
    await vi_until(() => received.length === 1);
    // Assert
    expect(unpackAs(received[0]!, ShimHelloSchema)?.sessionId).toBe("abc");
    // Cleanup
    conn.close();
    peer.close();
    pair.close();
  });

  it("reports a clean close with a null error", async () => {
    // Arrange
    const pair = await socketPair();
    let closeErr: Error | null | undefined;
    const conn = new MessageConn(
      pair.a,
      { onMessage: () => {}, onClose: (err) => (closeErr = err) },
      "test",
    );
    // Act: peer ends cleanly at a frame boundary
    pair.b.end();
    await vi_until(() => closeErr !== undefined);
    // Assert
    expect(closeErr).toBeNull();
    conn.close();
    pair.close();
  });
});

// local helper to avoid importing the harness's until under a clashing name
async function vi_until(pred: () => boolean): Promise<void> {
  for (let i = 0; i < 2000; i++) {
    if (pred()) return;
    await new Promise<void>((resolve) => setImmediate(resolve));
  }
  throw new Error("vi_until: predicate never held");
}
