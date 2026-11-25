// @vitest-environment jsdom
// The popup channel stand-ins. See test/host/popup-channel.ts.
import { describe, expect, it } from "vitest";

import { fakePopupChannel, quietPopupChannel, recordingPopupMirror } from "./popup-channel.js";

describe("fakePopupChannel", () => {
  it("records what is posted", () => {
    const { channel, posted } = fakePopupChannel();
    channel.postMessage({ kind: "hide", key: "ws-a" });
    expect(posted).toEqual([{ kind: "hide", key: "ws-a" }]);
  });

  it("delivers to every listener", () => {
    const { channel, deliver } = fakePopupChannel();
    const seen: unknown[] = [];
    channel.addEventListener("message", (event) => seen.push(event.data));
    deliver("hello");
    expect(seen).toEqual(["hello"]);
  });

  it("delivers nothing to a removed listener", () => {
    const { channel, deliver } = fakePopupChannel();
    const seen: unknown[] = [];
    const listener = (event: MessageEvent): void => void seen.push(event.data);
    channel.addEventListener("message", listener);
    channel.removeEventListener("message", listener);
    deliver("hello");
    expect(seen).toEqual([]);
  });
});

describe("quietPopupChannel", () => {
  it("takes a post without complaint", () => {
    expect(() => quietPopupChannel().postMessage({ kind: "hide", key: "ws-a" })).not.toThrow();
  });
});

describe("recordingPopupMirror", () => {
  it("records each thing it is told, by kind and key", () => {
    const told: string[] = [];
    const mirror = recordingPopupMirror(told);
    const el = document.createElement("div");
    mirror.show("ws-a", el, el);
    mirror.place("ws-a");
    mirror.hide("ws-a");
    expect(told).toEqual(["show:ws-a", "place:ws-a", "hide:ws-a"]);
  });

  it("records each shown panel and the line it stands beside", () => {
    const shown: HTMLElement[] = [];
    const panel = document.createElement("div");
    const line = document.createElement("div");
    recordingPopupMirror([], shown).show("ws-a", panel, line);
    expect(shown).toEqual([panel, line]);
  });

  it("says the pointer is never on a copy", () => {
    expect(recordingPopupMirror([]).pointerOnCopy()).toBe(false);
  });
});
