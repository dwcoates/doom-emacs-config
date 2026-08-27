/**
 * The length-prefixed frame codec, which is transport and NOT schema.
 *
 * THE ENVELOPE AND CONNECTION SUITES WERE DELETED, NOT ADAPTED. They framed
 * `Ack`, `ShimHello` and `SubmitPrompt` -- `protocol.v1` messages that no
 * longer exist -- and picking a replacement exemplar out of `shim.v1` would be
 * deciding what the new envelope carries, which is a schema decision this
 * suite has no standing to make. `MessageConn`, `encodeMessage`,
 * `decodeEnvelope`, `envelopeType` and `unpackAs` are consequently unexercised
 * until `shim.v1` has an implementation to frame.
 */
import { describe, expect, it } from "vitest";
import {
  FrameDecoder,
  FrameTooLargeError,
  MAX_FRAME,
  UnexpectedEofError,
  encodeFrame,
} from "../src/uds/framing.js";


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
