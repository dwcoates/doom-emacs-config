// The host protocol's message readers: each answers a message of ours, or null.
// See src/host/protocol.ts.
import { describe, expect, it } from "vitest";

import { HOST_CHANNEL, onHostChannel, parseHostToPage, parsePageToHost } from "../../src/host/protocol.js";

describe("parseHostToPage", () => {
  it.each([
    ["presence", { channel: HOST_CHANNEL, kind: "presence", shown: true, warm: true }],
    ["inputBackground", { channel: HOST_CHANNEL, kind: "inputBackground", background: "#123456" }],
    ["gateDock with a background", { channel: HOST_CHANNEL, kind: "gateDock", pixels: 120, background: "#000" }],
    ["gateDock without a background", { channel: HOST_CHANNEL, kind: "gateDock", pixels: 120, background: null }],
    ["dismissDigest", { channel: HOST_CHANNEL, kind: "dismissDigest" }],
  ])("reads a %s message", (_name, data) => {
    // Arrange / Act
    const message = parseHostToPage(data);

    // Assert
    expect(message).toEqual(data);
  });

  it.each([
    ["another channel", { channel: "other", kind: "presence", shown: true, warm: true }],
    ["a non-object", "presence"],
    ["null", null],
    ["an unknown kind", { channel: HOST_CHANNEL, kind: "teleport" }],
    ["presence without a boolean", { channel: HOST_CHANNEL, kind: "presence", shown: "yes", warm: true }],
    ["presence without its warmth", { channel: HOST_CHANNEL, kind: "presence", shown: true }],
    ["an inputBackground without a string", { channel: HOST_CHANNEL, kind: "inputBackground", background: 3 }],
    ["a gateDock with a non-finite height", { channel: HOST_CHANNEL, kind: "gateDock", pixels: Infinity, background: null }],
    ["a gateDock with a numeric background", { channel: HOST_CHANNEL, kind: "gateDock", pixels: 10, background: 4 }],
  ])("refuses %s", (_name, data) => {
    // Arrange / Act
    const message = parseHostToPage(data);

    // Assert
    expect(message).toBeNull();
  });
});

describe("parsePageToHost", () => {
  it.each([
    ["hello", { channel: HOST_CHANNEL, kind: "hello" }],
    ["drawn page", { channel: HOST_CHANNEL, kind: "drawn", what: "page" }],
    ["drawn refused", { channel: HOST_CHANNEL, kind: "drawn", what: "refused" }],
    ["selection", { channel: HOST_CHANNEL, kind: "selection", text: "abc", focused: true }],
    ["selection with no selection object", { channel: HOST_CHANNEL, kind: "selection", text: null, focused: false }],
    ["editing", { channel: HOST_CHANNEL, kind: "editing", editing: true }],
  ])("reads a %s message", (_name, data) => {
    // Arrange / Act
    const message = parsePageToHost(data);

    // Assert
    expect(message).toEqual(data);
  });

  it.each([
    ["another channel", { channel: "other", kind: "hello" }],
    ["an unknown kind", { channel: HOST_CHANNEL, kind: "wave" }],
    ["a drawn of an unknown sort", { channel: HOST_CHANNEL, kind: "drawn", what: "half" }],
    ["a selection without focus", { channel: HOST_CHANNEL, kind: "selection", text: "abc" }],
    ["an editing without a boolean", { channel: HOST_CHANNEL, kind: "editing", editing: 1 }],
  ])("refuses %s", (_name, data) => {
    // Arrange / Act
    const message = parsePageToHost(data);

    // Assert
    expect(message).toBeNull();
  });
});

describe("onHostChannel", () => {
  it("is true for any message on the channel, readable or not", () => {
    expect(onHostChannel({ channel: HOST_CHANNEL, kind: "anything" })).toBe(true);
  });

  it("is false for a message on another channel", () => {
    expect(onHostChannel({ channel: "vite", kind: "hello" })).toBe(false);
  });
});
