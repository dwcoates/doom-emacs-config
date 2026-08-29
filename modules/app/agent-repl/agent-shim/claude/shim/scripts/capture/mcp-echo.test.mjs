/**
 * Tests for the capture harness's MCP echo server.
 *
 * Driven BOTH in-process (every handler arm) and over a REAL stdio pipe: the
 * whole point of this file is that it is a working server the vendor can
 * connect to, and only a spawn proves the line framing and the executable
 * entrypoint work.
 */
import { spawn } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  MAX_SLEEP_MS,
  PROTOCOL_VERSION,
  SERVER_INFO,
  TOOLS,
  boundedSleepMs,
  fail,
  handleRequest,
  ok,
  sleep,
  textContent,
} from "./mcp-echo.mjs";

const HERE = path.dirname(fileURLToPath(import.meta.url));
const SERVER = path.join(HERE, "mcp-echo.mjs");

/**
 * Drive the real server over stdio, writing each request as a line and
 * collecting every response line.
 */
function overStdio(requests) {
  return new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [SERVER], { stdio: ["pipe", "pipe", "pipe"] });
    let out = "";
    child.stdout.on("data", (chunk) => {
      out += chunk;
    });
    child.on("error", reject);
    child.on("close", () => {
      resolve(
        out
          .split("\n")
          .filter((line) => line.trim() !== "")
          .map((line) => JSON.parse(line)),
      );
    });
    for (const request of requests) child.stdin.write(`${JSON.stringify(request)}\n`);
    child.stdin.end();
  });
}

describe("over a real stdio pipe", () => {
  it("completes the initialize handshake", async () => {
    const [response] = await overStdio([{ jsonrpc: "2.0", id: 1, method: "initialize", params: {} }]);
    expect(response.result.serverInfo).toEqual(SERVER_INFO);
  });

  it("advertises the protocol version", async () => {
    const [response] = await overStdio([{ jsonrpc: "2.0", id: 1, method: "initialize", params: {} }]);
    expect(response.result.protocolVersion).toBe(PROTOCOL_VERSION);
  });

  it("lists both tools", async () => {
    const [response] = await overStdio([{ jsonrpc: "2.0", id: 2, method: "tools/list" }]);
    expect(response.result.tools.map((t) => t.name)).toEqual(["echo", "slow"]);
  });

  it("echoes a call's text back", async () => {
    const [response] = await overStdio([
      { jsonrpc: "2.0", id: 3, method: "tools/call", params: { name: "echo", arguments: { text: "unmodeled probe" } } },
    ]);
    expect(response.result.content[0].text).toBe("unmodeled probe");
  });

  it("answers several requests in order on one connection", async () => {
    const responses = await overStdio([
      { jsonrpc: "2.0", id: 1, method: "initialize", params: {} },
      { jsonrpc: "2.0", id: 2, method: "tools/list" },
      { jsonrpc: "2.0", id: 3, method: "tools/call", params: { name: "echo", arguments: { text: "x" } } },
    ]);
    expect(responses.map((r) => r.id)).toEqual([1, 2, 3]);
  });

  it("answers a malformed line with a parse error instead of dying", async () => {
    const child = spawn(process.execPath, [SERVER], { stdio: ["pipe", "pipe", "pipe"] });
    const responses = await new Promise((resolve) => {
      let out = "";
      child.stdout.on("data", (c) => {
        out += c;
      });
      child.on("close", () =>
        resolve(out.split("\n").filter((l) => l.trim() !== "").map((l) => JSON.parse(l))),
      );
      child.stdin.write("{ not json\n");
      child.stdin.write(`${JSON.stringify({ jsonrpc: "2.0", id: 9, method: "ping" })}\n`);
      child.stdin.end();
    });
    expect(responses[0].error.code).toBe(-32700);
  });

  it("keeps serving after a malformed line — a dead server would be a DIFFERENT scenario's golden", async () => {
    const child = spawn(process.execPath, [SERVER], { stdio: ["pipe", "pipe", "pipe"] });
    const responses = await new Promise((resolve) => {
      let out = "";
      child.stdout.on("data", (c) => {
        out += c;
      });
      child.on("close", () =>
        resolve(out.split("\n").filter((l) => l.trim() !== "").map((l) => JSON.parse(l))),
      );
      child.stdin.write("{ not json\n");
      child.stdin.write(`${JSON.stringify({ jsonrpc: "2.0", id: 9, method: "ping" })}\n`);
      child.stdin.end();
    });
    expect(responses[1].id).toBe(9);
  });
});

describe("handleRequest", () => {
  it("answers tools/list with both declared tools", async () => {
    expect((await handleRequest({ id: 1, method: "tools/list" })).result.tools).toBe(TOOLS);
  });

  it("returns null for a notification, which JSON-RPC forbids answering", async () => {
    expect(await handleRequest({ method: "notifications/initialized" })).toBeNull();
  });

  it("returns null for an initialize sent as a notification", async () => {
    expect(await handleRequest({ method: "initialize" })).toBeNull();
  });

  it("rejects an unknown method", async () => {
    expect((await handleRequest({ id: 1, method: "nope" })).error.code).toBe(-32601);
  });

  it("rejects an unknown tool", async () => {
    const response = await handleRequest({ id: 1, method: "tools/call", params: { name: "nope" } });
    expect(response.error.code).toBe(-32602);
  });

  it("rejects an echo with no text argument", async () => {
    const response = await handleRequest({
      id: 1,
      method: "tools/call",
      params: { name: "echo", arguments: {} },
    });
    expect(response.error.message).toMatch(/requires a string/);
  });

  it("echoes an empty string, which is a valid input", async () => {
    const response = await handleRequest({
      id: 1,
      method: "tools/call",
      params: { name: "echo", arguments: { text: "" } },
    });
    expect(response.result.content[0].text).toBe("");
  });

  it("answers ping", async () => {
    expect((await handleRequest({ id: 1, method: "ping" })).result).toEqual({});
  });

  it("runs the slow tool and reports how long it slept", async () => {
    const response = await handleRequest({
      id: 1,
      method: "tools/call",
      params: { name: "slow", arguments: { ms: 1 } },
    });
    expect(response.result.content[0].text).toBe("slept 1ms");
  });

  it("reports a sleep it actually performed", async () => {
    const response = await handleRequest({
      id: 1,
      method: "tools/call",
      params: { name: "slow", arguments: { ms: 2 } },
    });
    expect(response.result.content[0].text).toBe("slept 2ms");
  });
});

describe("envelopes", () => {
  it("builds a success envelope", () => {
    expect(ok(1, { a: 2 })).toEqual({ jsonrpc: "2.0", id: 1, result: { a: 2 } });
  });

  it("builds an error envelope", () => {
    expect(fail(1, -1, "why")).toEqual({ jsonrpc: "2.0", id: 1, error: { code: -1, message: "why" } });
  });

  it("builds an MCP text content block", () => {
    expect(textContent("hi")).toEqual({ content: [{ type: "text", text: "hi" }], isError: false });
  });

  it("treats a negative sleep as zero", () => {
    expect(boundedSleepMs(-5)).toBe(0);
  });

  it("CAPS a huge sleep so a malformed request cannot hang the capture", () => {
    expect(boundedSleepMs(999_999_999)).toBe(MAX_SLEEP_MS);
  });

  it("treats a non-numeric sleep as zero", () => {
    expect(boundedSleepMs("soon")).toBe(0);
  });

  it("passes an in-range sleep through unchanged", () => {
    expect(boundedSleepMs(250)).toBe(250);
  });

  it("actually resolves", async () => {
    await expect(sleep(1)).resolves.toBeUndefined();
  });
});

describe("the tool declarations", () => {
  it("requires text on echo", () => {
    expect(TOOLS[0].inputSchema.required).toEqual(["text"]);
  });

  it("requires ms on slow", () => {
    expect(TOOLS[1].inputSchema.required).toEqual(["ms"]);
  });

  it("names the server capture-probe, matching the corpus", () => {
    expect(SERVER_INFO.name).toBe("capture-probe");
  });
});
