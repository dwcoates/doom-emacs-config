import { describe, expect, it } from "vitest";
import { parseUnsupportedCommand, requestSupportWorkspace } from "../src/unsupported.js";

describe("parseUnsupportedCommand", () => {
  it("names the command in the headless-builtin refusal", () => {
    expect(parseUnsupportedCommand("/status isn't available in this environment.")).toBe("status");
  });

  it("names the command in the interactive-panel refusal", () => {
    const text =
      "/model opens an interactive panel and isn't available in this environment. Run it from the Claude Code terminal instead.";
    expect(parseUnsupportedCommand(text)).toBe("model");
  });

  it("names a namespaced command", () => {
    expect(parseUnsupportedCommand("/plugin:cmd isn't available in this environment.")).toBe(
      "plugin:cmd",
    );
  });

  it("ignores the policy-disabled refusal, which no workspace could fix", () => {
    expect(parseUnsupportedCommand("/status isn't available in this session.")).toBeNull();
  });

  it("ignores an ordinary answer that merely quotes a refusal", () => {
    const text = "I ran it and got: /status isn't available in this environment.";
    expect(parseUnsupportedCommand(text)).toBeNull();
  });

  it("tolerates surrounding whitespace", () => {
    expect(parseUnsupportedCommand("  /status isn't available in this environment.  ")).toBe(
      "status",
    );
  });

  it("returns null for undefined result text", () => {
    expect(parseUnsupportedCommand(undefined)).toBeNull();
  });

  it("returns null for empty result text", () => {
    expect(parseUnsupportedCommand("")).toBeNull();
  });

  it("returns null for an unrelated final answer", () => {
    expect(parseUnsupportedCommand("Done: the tests pass.")).toBeNull();
  });
});

describe("requestSupportWorkspace", () => {
  it("posts the command to the session's add-support route", async () => {
    // Arrange.
    let seenUrl = "";
    let seenBody = "";
    const fetchFn = (async (url: string, init: RequestInit) => {
      seenUrl = url;
      seenBody = String(init.body);
      return new Response(JSON.stringify({ workspace: "add-support-status" }), { status: 202 });
    }) as unknown as typeof fetch;

    // Act.
    await requestSupportWorkspace("http://d", "s_1", "status", fetchFn);

    // Assert.
    expect(seenUrl).toBe("http://d/sessions/s_1/add-support");
    expect(JSON.parse(seenBody)).toEqual({ command: "status" });
  });

  it("resolves to the workspace name the daemon reports", async () => {
    // Arrange.
    const fetchFn = (async () =>
      new Response(JSON.stringify({ workspace: "add-support-status" }), {
        status: 202,
      })) as unknown as typeof fetch;

    // Act.
    const got = await requestSupportWorkspace("http://d", "s_1", "status", fetchFn);

    // Assert.
    expect(got).toBe("add-support-status");
  });

  it("rejects on a non-2xx rather than reporting a request that never landed", async () => {
    // Arrange.
    const fetchFn = (async () =>
      new Response("no such session", { status: 404 })) as unknown as typeof fetch;

    // Act + Assert.
    await expect(requestSupportWorkspace("http://d", "s_1", "status", fetchFn)).rejects.toThrow(
      /404/,
    );
  });

  it("escapes a session id into the path", async () => {
    // Arrange.
    let seenUrl = "";
    const fetchFn = (async (url: string) => {
      seenUrl = url;
      return new Response(JSON.stringify({ workspace: "w" }), { status: 202 });
    }) as unknown as typeof fetch;

    // Act.
    await requestSupportWorkspace("http://d", "s/1", "status", fetchFn);

    // Assert.
    expect(seenUrl).toBe("http://d/sessions/s%2F1/add-support");
  });
});
