import { describe, expect, it } from "vitest";
import {
  ProtocolError,
  ShimEvent,
  decodeCommandLine,
  encodeEvent,
} from "../src/protocol.js";

describe("decodeCommandLine", () => {
  it("decodes a user-message with string content", () => {
    // Arrange
    const line = JSON.stringify({
      type: "user-message",
      request_id: "r1",
      content: "hello",
    });
    // Act
    const cmd = decodeCommandLine(line);
    // Assert
    expect(cmd).toEqual({ type: "user-message", request_id: "r1", content: "hello" });
  });

  it("decodes a user-message with content blocks", () => {
    // Arrange
    const line = JSON.stringify({
      type: "user-message",
      request_id: "r1",
      content: [{ type: "text", text: "hi" }],
    });
    // Act
    const cmd = decodeCommandLine(line);
    // Assert
    expect(cmd).toMatchObject({ content: [{ type: "text", text: "hi" }] });
  });

  it("returns null for an unknown frame type (forward compatibility)", () => {
    // Arrange
    const line = JSON.stringify({ type: "from-the-future", request_id: "r1" });
    // Act
    const cmd = decodeCommandLine(line);
    // Assert
    expect(cmd).toBeNull();
  });

  it("throws ProtocolError on invalid JSON", () => {
    // Arrange
    const line = "{nope";
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(ProtocolError);
  });

  it("throws ProtocolError on a non-object frame", () => {
    // Arrange
    const line = JSON.stringify([1, 2, 3]);
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(ProtocolError);
  });

  it("throws ProtocolError when type is missing", () => {
    // Arrange
    const line = JSON.stringify({ request_id: "r1" });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/discriminator/);
  });

  it("throws ProtocolError when request_id is missing", () => {
    // Arrange
    const line = JSON.stringify({ type: "interrupt" });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/request_id/);
  });

  it("throws ProtocolError on user-message with invalid content", () => {
    // Arrange
    const line = JSON.stringify({ type: "user-message", request_id: "r1", content: 42 });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/content/);
  });

  it("throws ProtocolError on permission-decision with bad behavior", () => {
    // Arrange
    const line = JSON.stringify({
      type: "permission-decision",
      request_id: "r1",
      decision: { behavior: "maybe" },
    });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/behavior/);
  });

  it("throws ProtocolError on a deny decision without a message", () => {
    // Arrange
    const line = JSON.stringify({
      type: "permission-decision",
      request_id: "r1",
      decision: { behavior: "deny" },
    });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/message/);
  });

  it("throws ProtocolError on set-permission-mode with an invalid mode", () => {
    // Arrange
    const line = JSON.stringify({
      type: "set-permission-mode",
      request_id: "r1",
      mode: "yolo",
    });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/invalid mode/);
  });
});

describe("encodeEvent", () => {
  it("produces one newline-terminated JSON line", () => {
    // Arrange
    const evt: ShimEvent = {
      type: "ack",
      session_id: "s1",
      request_id: "r1",
    };
    // Act
    const line = encodeEvent(evt);
    // Assert
    expect(line.endsWith("\n")).toBe(true);
    expect(JSON.parse(line)).toEqual(evt);
  });
});
