import { describe, expect, it } from "vitest";
import { parseFrame } from "../src/protocol.js";

describe("parseFrame", () => {
  it("parses a known frame into envelope and typed frame", () => {
    // Arrange
    const data = JSON.stringify({
      type: "text-delta",
      seq: 3,
      ts: "T",
      session_id: "s1",
      block_id: "b1",
      text: "x",
    });
    // Act
    const { envelope, frame } = parseFrame(data);
    // Assert
    expect(envelope.seq).toBe(3);
    expect(frame).toMatchObject({ type: "text-delta", block_id: "b1" });
  });

  it("recognizes the interrupt frame as a known type", () => {
    // Arrange
    const data = JSON.stringify({ type: "interrupt", seq: 4, ts: "T", session_id: "s1" });
    // Act
    const { frame } = parseFrame(data);
    // Assert — a known frame parses non-null (unknown types return null).
    expect(frame).toMatchObject({ type: "interrupt" });
  });

  it("recognizes the assistant-error frame as a known type", () => {
    // Arrange
    const data = JSON.stringify({
      type: "assistant-error",
      seq: 5,
      ts: "T",
      session_id: "s1",
      message_id: "m1",
      error: "rate_limit",
    });
    // Act
    const { frame } = parseFrame(data);
    // Assert
    expect(frame).toMatchObject({ type: "assistant-error", message_id: "m1", error: "rate_limit" });
  });

  it("recognizes the task-summary frame as a known type", () => {
    // Arrange
    const data = JSON.stringify({
      type: "task-summary",
      seq: 6,
      ts: "T",
      session_id: "s1",
      summary: "Widget cache is being built.",
    });
    // Act
    const { frame } = parseFrame(data);
    // Assert
    expect(frame).toMatchObject({ type: "task-summary", summary: "Widget cache is being built." });
  });

  it("returns a null frame but a valid envelope for unknown types", () => {
    // Arrange
    const data = JSON.stringify({ type: "hologram", seq: 9, ts: "T", session_id: "s1" });
    // Act
    const { envelope, frame } = parseFrame(data);
    // Assert
    expect(frame).toBeNull();
    expect(envelope.seq).toBe(9);
  });

  it("throws when the type discriminator is missing", () => {
    // Arrange
    const data = JSON.stringify({ seq: 1 });
    // Act + Assert
    expect(() => parseFrame(data)).toThrow(/discriminator/);
  });

  it("throws when seq is missing", () => {
    // Arrange
    const data = JSON.stringify({ type: "result" });
    // Act + Assert
    expect(() => parseFrame(data)).toThrow(/seq/);
  });

  it("throws on invalid JSON", () => {
    // Arrange + Act + Assert
    expect(() => parseFrame("{nope")).toThrow();
  });
});
