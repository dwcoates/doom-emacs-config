// @vitest-environment jsdom
// The popup channel stand-ins. See test/host/popup-channel.ts.
import { describe, expect, it } from "vitest";

import { fakePopupChannel, quietPopupChannel } from "./popup-channel.js";

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

describe("the suites that hold a popup channel", () => {
  it.each(["sidebar-popups.test.ts", "sidebar-page.test.ts", "start.test.ts"])(
    "%s takes its channel from the shared stand-ins, never its own",
    async (file) => {
      const fs = await import("node:fs");
      const path = await import("node:path");
      const source = fs.readFileSync(path.join(__dirname, file), "utf8");
      expect([source.includes("./popup-channel.js"), /postMessage:\s*\(/.test(source)]).toEqual([true, false]);
    },
  );
});
