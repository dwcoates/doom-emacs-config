// The host protocol's message readers: each answers a message of ours, or null.
// See src/host/protocol.ts.
import { describe, expect, it } from "vitest";

import { HOST_CHANNEL, onHostChannel, parseHostToPage, parsePageToHost } from "../../src/host/protocol.js";

describe("parseHostToPage", () => {
  it.each([
    ["presence", { channel: HOST_CHANNEL, kind: "presence", shown: true }],
    ["presence a switch made", { channel: HOST_CHANNEL, kind: "presence", shown: true, switchStartedAtMs: 1_700_000_000_123 }],
    ["inputBackground", { channel: HOST_CHANNEL, kind: "inputBackground", background: "#123456" }],
    ["gateDock with a background", { channel: HOST_CHANNEL, kind: "gateDock", pixels: 120, background: "#000" }],
    ["gateDock without a background", { channel: HOST_CHANNEL, kind: "gateDock", pixels: 120, background: null }],
    ["dismissDigest", { channel: HOST_CHANNEL, kind: "dismissDigest" }],
    ["sidebarDock", { channel: HOST_CHANNEL, kind: "sidebarDock", docked: true }],
    ["logLevel opening a window", { channel: HOST_CHANNEL, kind: "logLevel", level: "debug", untilMs: 1_060_000 }],
    ["logLevel closing a window", { channel: HOST_CHANNEL, kind: "logLevel", level: "info", untilMs: null }],
    ["webhostFailures with none", { channel: HOST_CHANNEL, kind: "webhostFailures", failures: [] }],
    [
      "webhostFailures with a row",
      {
        channel: HOST_CHANNEL,
        kind: "webhostFailures",
        failures: [{ arm: "daemonUnreachable", headline: "lost the connection", evidence: [["close", "1006"]] }],
      },
    ],
  ])("reads a %s message", (_name, data) => {
    // Arrange / Act
    const message = parseHostToPage(data);

    // Assert
    expect(message).toEqual(data);
  });

  it.each([
    ["another channel", { channel: "other", kind: "presence", shown: true }],
    ["a non-object", "presence"],
    ["null", null],
    ["an unknown kind", { channel: HOST_CHANNEL, kind: "teleport" }],
    ["presence without a boolean", { channel: HOST_CHANNEL, kind: "presence", shown: "yes" }],
    ["presence with a switch start that is not a number", { channel: HOST_CHANNEL, kind: "presence", shown: true, switchStartedAtMs: "now" }],
    ["presence with a non-finite switch start", { channel: HOST_CHANNEL, kind: "presence", shown: true, switchStartedAtMs: NaN }],
    ["an inputBackground without a string", { channel: HOST_CHANNEL, kind: "inputBackground", background: 3 }],
    ["a gateDock with a non-finite height", { channel: HOST_CHANNEL, kind: "gateDock", pixels: Infinity, background: null }],
    ["a sidebarDock without a boolean", { channel: HOST_CHANNEL, kind: "sidebarDock", docked: 1 }],
    ["a logLevel naming no level", { channel: HOST_CHANNEL, kind: "logLevel", level: "trace", untilMs: 1 }],
    ["a logLevel window with no end", { channel: HOST_CHANNEL, kind: "logLevel", level: "debug", untilMs: null }],
    ["an info logLevel with an end", { channel: HOST_CHANNEL, kind: "logLevel", level: "info", untilMs: 1 }],
    ["webhostFailures without a list", { channel: HOST_CHANNEL, kind: "webhostFailures", failures: {} }],
    [
      "webhostFailures with an arm no frontend mints",
      { channel: HOST_CHANNEL, kind: "webhostFailures", failures: [{ arm: "shimCrashed", headline: "h", evidence: [] }] },
    ],
    [
      "webhostFailures with a malformed evidence row",
      { channel: HOST_CHANNEL, kind: "webhostFailures", failures: [{ arm: "bootFailed", headline: "h", evidence: [["only"]] }] },
    ],
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
    ["gateBand", { channel: HOST_CHANNEL, kind: "gateBand", pixels: 120 }],
    ["gateBand with no band", { channel: HOST_CHANNEL, kind: "gateBand", pixels: 0 }],
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
    ["a gateBand below zero", { channel: HOST_CHANNEL, kind: "gateBand", pixels: -1 }],
    ["a gateBand that is not finite", { channel: HOST_CHANNEL, kind: "gateBand", pixels: NaN }],
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
