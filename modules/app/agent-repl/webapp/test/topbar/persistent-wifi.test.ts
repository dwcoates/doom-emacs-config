// @vitest-environment jsdom
//
// The persistent-wifi chip. One test per arm of each oneof, the unassigned
// (unread) case of each, the tooltip, and the arm this bundle cannot name.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarPersistentWifiSchema,
  type TopbarPersistentWifi,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { drawTopbarPersistentWifi } from "../../src/topbar/persistent-wifi.js";

/** A chip with the given arms and a tooltip. */
function chip(
  wifi: TopbarPersistentWifi["wifi"],
  mode: TopbarPersistentWifi["mode"],
): TopbarPersistentWifi {
  return { ...create(TopbarPersistentWifiSchema, { tooltip: { text: "the daemon's words" } }), wifi, mode };
}

const joined = { case: "joined", value: {} } as TopbarPersistentWifi["wifi"];
const notJoined = { case: "notJoined", value: {} } as TopbarPersistentWifi["wifi"];
const on = { case: "on", value: {} } as TopbarPersistentWifi["mode"];
const off = { case: "off", value: {} } as TopbarPersistentWifi["mode"];
const unset = { case: undefined };

describe("drawTopbarPersistentWifi", () => {
  it("marks a joined network for the green glyph", () => {
    expect(drawTopbarPersistentWifi(chip(joined, off)).getAttribute("data-wifi")).toBe("joined");
  });

  it("marks no network for the red glyph", () => {
    expect(drawTopbarPersistentWifi(chip(notJoined, off)).getAttribute("data-wifi")).toBe("not-joined");
  });

  it("marks an unread network as unknown", () => {
    expect(drawTopbarPersistentWifi(chip(unset as never, off)).getAttribute("data-wifi")).toBe("unknown");
  });

  it("marks the mode on for the blue disc", () => {
    expect(drawTopbarPersistentWifi(chip(notJoined, on)).getAttribute("data-mode")).toBe("on");
  });

  it("marks the mode off for no disc", () => {
    expect(drawTopbarPersistentWifi(chip(joined, off)).getAttribute("data-mode")).toBe("off");
  });

  it("marks an unread mode as unknown", () => {
    expect(drawTopbarPersistentWifi(chip(joined, unset as never)).getAttribute("data-mode")).toBe("unknown");
  });

  it("carries the daemon's tooltip verbatim", () => {
    expect(drawTopbarPersistentWifi(chip(joined, on)).title).toBe("the daemon's words");
  });

  it("draws the wifi glyph", () => {
    expect(drawTopbarPersistentWifi(chip(joined, on)).querySelector("svg.topbar-wifi-glyph")).not.toBeNull();
  });

  it("refuses a wifi arm this bundle cannot name", () => {
    expect(() => drawTopbarPersistentWifi(chip({ case: "radioOff", value: {} } as never, on))).toThrow(MalformedView);
  });

  it("refuses a mode arm this bundle cannot name", () => {
    expect(() => drawTopbarPersistentWifi(chip(joined, { case: "half", value: {} } as never))).toThrow(MalformedView);
  });

  it("refuses a chip with no tooltip", () => {
    const bare = { ...create(TopbarPersistentWifiSchema, {}), wifi: joined, mode: on };
    expect(() => drawTopbarPersistentWifi(bare)).toThrow(MalformedView);
  });
});
