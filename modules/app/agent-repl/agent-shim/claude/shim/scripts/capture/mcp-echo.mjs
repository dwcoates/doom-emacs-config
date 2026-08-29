#!/usr/bin/env node
/**
 * A MINIMAL, REAL MCP SERVER over stdio — the capture harness's unmodeled tool.
 *
 * WHY IT EXISTS: `mcp-unmodeled-tool` and `mcp-server-healths` used to ship a
 * COMMENT-ONLY stub for this file and tell the operator to "supply a working
 * stdio MCP server at capture time". So the two scenarios that exist to capture
 * `AgentUnmodeled` — the one arm that can ONLY be produced by a tool whose
 * schema the shim genuinely cannot know — could not run, and the one MCP health
 * the harness could control (a connected server) was unreachable.
 *
 * The shim's fold has a branch for exactly this case, and nothing else in the
 * corpus reaches it: every other tool is a modeled built-in. Leaving it to the
 * operator meant that branch's golden was never going to be captured.
 *
 * NODE BUILT-INS ONLY, deliberately: adding `@modelcontextprotocol/sdk` to the
 * shim's dependency tree so a capture script can echo a string would put a
 * package in production's lockfile for a test fixture's benefit. The protocol
 * here is line-delimited JSON-RPC 2.0 over stdin/stdout, which is all stdio MCP
 * transport is.
 *
 * TOOLS:
 *   echo  — returns its input, so a tool call and its result are both trivially
 *           checkable in the golden.
 *   slow  — sleeps, so the vendor emits its per-call PROGRESS heartbeat. That
 *           beat is the only evidence of a wedged call (THE WEDGE RULING), and
 *           an unmodeled tool is the one place the harness can produce one on
 *           demand.
 */

import { createInterface } from "node:readline";

/** The protocol version this server speaks. */
export const PROTOCOL_VERSION = "2024-11-05";

/** The server's advertised identity. */
export const SERVER_INFO = { name: "capture-probe", version: "1.0.0" };

/** The tools this server offers, in the MCP `tools/list` shape. */
export const TOOLS = [
  {
    name: "echo",
    description:
      "Echo the given text straight back. Used by the agent-repl capture harness " +
      "as a tool whose schema the shim cannot know.",
    inputSchema: {
      type: "object",
      properties: {
        text: { type: "string", description: "The text to echo back." },
      },
      required: ["text"],
    },
  },
  {
    name: "slow",
    description:
      "Sleep for a number of milliseconds, then report. Used by the capture " +
      "harness to provoke the vendor's per-call progress heartbeat.",
    inputSchema: {
      type: "object",
      properties: {
        ms: { type: "number", description: "Milliseconds to sleep (capped at 60000)." },
      },
      required: ["ms"],
    },
  },
];

/** A JSON-RPC success envelope. */
export function ok(id, result) {
  return { jsonrpc: "2.0", id, result };
}

/** A JSON-RPC error envelope. */
export function fail(id, code, message) {
  return { jsonrpc: "2.0", id, error: { code, message } };
}

/** The MCP content block a tool result carries. */
export function textContent(text) {
  return { content: [{ type: "text", text }], isError: false };
}

/** The longest a `slow` call may sleep. */
export const MAX_SLEEP_MS = 60_000;

/**
 * Clamp a requested sleep into range.
 *
 * Separated from {@link sleep} so the BOUND can be tested without a test that
 * actually sleeps for a minute — the timeout is the behavior under test, not
 * something to wait out.
 */
export function boundedSleepMs(ms) {
  return Math.max(0, Math.min(Number(ms) || 0, MAX_SLEEP_MS));
}

/** Sleep, capped so a malformed request cannot hang the capture. */
export function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, boundedSleepMs(ms)));
}

/**
 * Handle one decoded request, returning the response envelope — or `null` for a
 * NOTIFICATION (a request with no `id`), which JSON-RPC forbids answering.
 *
 * Pure but for `slow`'s timer, so the unit test drives every method directly as
 * well as over a real pipe.
 */
export async function handleRequest(request) {
  const { id, method, params } = request ?? {};
  const isNotification = id === undefined || id === null;

  switch (method) {
    case "initialize":
      return isNotification
        ? null
        : ok(id, {
            protocolVersion: PROTOCOL_VERSION,
            capabilities: { tools: {} },
            serverInfo: SERVER_INFO,
          });
    case "notifications/initialized":
    case "initialized":
      return null;
    case "tools/list":
      return isNotification ? null : ok(id, { tools: TOOLS });
    case "tools/call": {
      if (isNotification) return null;
      const name = params?.name;
      const args = params?.arguments ?? {};
      if (name === "echo") {
        if (typeof args.text !== "string") {
          return fail(id, -32602, "echo requires a string `text` argument");
        }
        return ok(id, textContent(args.text));
      }
      if (name === "slow") {
        await sleep(args.ms);
        return ok(id, textContent(`slept ${boundedSleepMs(args.ms)}ms`));
      }
      return fail(id, -32602, `unknown tool ${JSON.stringify(name)}`);
    }
    case "ping":
      return isNotification ? null : ok(id, {});
    default:
      return isNotification ? null : fail(id, -32601, `unknown method ${JSON.stringify(method)}`);
  }
}

/**
 * Serve line-delimited JSON-RPC on a stream pair.
 *
 * A line that does not parse is answered with a parse error rather than
 * crashing the server: a dead MCP server would show up in the capture as a
 * `failed` health, which is a DIFFERENT scenario's golden and would quietly
 * corrupt this one.
 */
export function serve(input = process.stdin, output = process.stdout) {
  const lines = createInterface({ input, crlfDelay: Infinity });
  const write = (message) => output.write(`${JSON.stringify(message)}\n`);
  lines.on("line", async (line) => {
    if (line.trim() === "") return;
    let request;
    try {
      request = JSON.parse(line);
    } catch {
      write(fail(null, -32700, "parse error"));
      return;
    }
    try {
      const response = await handleRequest(request);
      if (response !== null) write(response);
    } catch (err) {
      write(fail(request?.id ?? null, -32603, String(err)));
    }
  });
  return lines;
}

// Only serve when executed directly, so the unit test can import the handlers.
if (process.argv[1] !== undefined && process.argv[1].endsWith("mcp-echo.mjs")) {
  serve();
}
