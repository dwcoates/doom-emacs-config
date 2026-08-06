import { describe, expect, it } from "vitest";

import { addressLabel, pageAddress, scopedStreamUrl, type PageAddress } from "../src/address.js";

/** A page's query parameters, spelled the way a browser hands them over. */
function params(query: string): URLSearchParams {
  return new URLSearchParams(query);
}

describe("pageAddress", () => {
  it.each([
    // [name, query, expected address]
    ["a plain workspace path", "workspace=%2FUsers%2Fdev%2Fproj", { kind: "workspace", workspace: "/Users/dev/proj" }],
    [
      "a workspace path with spaces",
      "workspace=%2FUsers%2Fdev%2FMy%20Projects%2Fagent%20repl",
      { kind: "workspace", workspace: "/Users/dev/My Projects/agent repl" },
    ],
    [
      "a non-ASCII workspace path",
      "workspace=%2FUsers%2Fdev%2Fprosjekt%2F%C3%A6%C3%B8%C3%A5%2F%E6%97%A5%E6%9C%AC%E8%AA%9E",
      { kind: "workspace", workspace: "/Users/dev/prosjekt/æøå/日本語" },
    ],
    [
      "a workspace path alongside unrelated parameters",
      "composer=0&workspace=%2Fw&parent_ws=main",
      { kind: "workspace", workspace: "/w" },
    ],
    ["a session id", "session=s_9", { kind: "session", sessionId: "s_9" }],
    ["nothing at all", "", { kind: "unaddressed" }],
    ["an empty workspace value", "workspace=", { kind: "unaddressed" }],
    ["an empty session value", "session=", { kind: "unaddressed" }],
  ])("reads %s", (_name, query, want) => {
    // Act.
    const got = pageAddress(params(query));

    // Assert.
    expect(got).toEqual(want);
  });

  it.each([
    ["a relative path", "workspace=proj%2Fsub"],
    ["a bare name", "workspace=proj"],
    ["a tilde path, which no daemon expands", "workspace=~%2Fproj"],
  ])("refuses %s rather than handing it to the daemon", (_name, query) => {
    // Act / Assert — the refusal names the offending value, so the malformed
    // URL is diagnosable from the message alone.
    expect(() => pageAddress(params(query))).toThrow(/not an absolute directory path/);
  });
});

describe("scopedStreamUrl", () => {
  it.each([
    // [name, address, expected URL]
    [
      "a workspace address",
      { kind: "workspace", workspace: "/Users/dev/proj" } as PageAddress,
      "ws://h:1/workspace-stream?workspace=%2FUsers%2Fdev%2Fproj",
    ],
    [
      "a workspace path needing encoding",
      { kind: "workspace", workspace: "/Users/dev/My Projects/æøå" } as PageAddress,
      "ws://h:1/workspace-stream?workspace=%2FUsers%2Fdev%2FMy%20Projects%2F%C3%A6%C3%B8%C3%A5",
    ],
    ["a session address", { kind: "session", sessionId: "s_9" } as PageAddress, "ws://h:1/sessions/s_9/stream"],
  ])("serves %s", (_name, address, want) => {
    // Act.
    const got = scopedStreamUrl("ws://h:1", address);

    // Assert.
    expect(got).toBe(want);
  });

  it("round-trips a workspace path through the URL it builds", () => {
    // Arrange — the daemon decodes the query with a standard parser, so the
    // encoding this end emits must survive one.
    const workspace = "/Users/dev/a&b=c?d#e/My Projects/日本語";

    // Act.
    const url = new URL(scopedStreamUrl("ws://h:1", { kind: "workspace", workspace }));

    // Assert.
    expect(url.searchParams.get("workspace")).toBe(workspace);
  });

  it("refuses to guess a socket for an unaddressed page", () => {
    // Assert — attaching to a conversation nobody named is worse than failing.
    expect(() => scopedStreamUrl("ws://h:1", { kind: "unaddressed" })).toThrow(/no scoped stream/);
  });
});

describe("addressLabel", () => {
  it.each([
    [{ kind: "workspace", workspace: "/w" } as PageAddress, "workspace /w"],
    [{ kind: "session", sessionId: "s_9" } as PageAddress, "session s_9"],
    [{ kind: "unaddressed" } as PageAddress, "unaddressed"],
  ])("names %o", (address, want) => {
    expect(addressLabel(address)).toBe(want);
  });
});
