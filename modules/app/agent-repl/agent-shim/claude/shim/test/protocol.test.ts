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

  it("accepts the CLI-era permission modes the topbar offers", () => {
    // Arrange — `auto` is a real, selectable mode. The decoder used to
    // carry its own 4-mode copy of the enum and rejected all four
    // CLI-era modes, so picking one in the GUI produced a bad_command
    // and silently left the mode unchanged.
    const line = JSON.stringify({
      type: "set-permission-mode",
      request_id: "r1",
      mode: "auto",
    });
    // Act
    const cmd = decodeCommandLine(line);
    // Assert
    expect(cmd).toMatchObject({ type: "set-permission-mode", mode: "auto" });
  });

  it("decodes a set-model command", () => {
    // Arrange
    const line = JSON.stringify({
      type: "set-model",
      request_id: "r1",
      model: "claude-opus-4-5",
    });
    // Act
    const cmd = decodeCommandLine(line);
    // Assert
    expect(cmd).toEqual({
      type: "set-model",
      request_id: "r1",
      model: "claude-opus-4-5",
    });
  });

  it("throws ProtocolError on set-model with an empty model", () => {
    // Arrange — empty is a caller who forgot to say which model, not a
    // request for the default one.
    const line = JSON.stringify({ type: "set-model", request_id: "r1", model: "" });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/non-empty model/);
  });

  it("throws the same ProtocolError on a synthetic model update", () => {
    // Arrange — updates require a real target. The marker has the same
    // invalid-update semantics as the empty string.
    const line = JSON.stringify({
      type: "set-model",
      request_id: "r1",
      model: "<synthetic>",
    });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/non-empty model/);
  });

  it("throws ProtocolError on set-model with no model field", () => {
    // Arrange
    const line = JSON.stringify({ type: "set-model", request_id: "r1" });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/non-empty model/);
  });

  it("decodes a refresh-commands command", () => {
    // Arrange — it carries nothing but its request_id.
    const line = JSON.stringify({ type: "refresh-commands", request_id: "r1" });
    // Act
    const cmd = decodeCommandLine(line);
    // Assert
    expect(cmd).toEqual({ type: "refresh-commands", request_id: "r1" });
  });

  it("throws ProtocolError on refresh-commands with no request_id", () => {
    // Arrange — the ack is correlated by request_id, so a refresh without
    // one could never be acknowledged.
    const line = JSON.stringify({ type: "refresh-commands" });
    // Act + Assert
    expect(() => decodeCommandLine(line)).toThrow(/missing request_id/);
  });

  it("encodes a commands event with its slash-command list intact", () => {
    // Arrange
    const evt: ShimEvent = {
      type: "commands",
      session_id: "s1",
      commands: [{ name: "compact", description: "summarize", argumentHint: "<how>" }],
    };
    // Act
    const line = encodeEvent(evt);
    // Assert
    expect(JSON.parse(line)).toEqual(evt);
  });

});
