/**
 * test/engine/api-reachability.test.ts — where the probe goes, and what counts
 * as reachable.
 *
 * NO NETWORK. The probe is driven through an injected transport; Node's own
 * transport is exercised against an in-process LOOPBACK server only, which
 * never leaves the machine.
 */
import * as http from "node:http";
import type { AddressInfo } from "node:net";
import { mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import {
  DEFAULT_API_BASE_URL,
  NODE_TRANSPORT,
  PROBE_TIMEOUT_MS,
  createFakeReachabilityProbe,
  createReachabilityProbe,
  noProxyExempts,
  recordReachabilityTarget,
  resolveReachabilityTarget,
  type ProbeTransport,
  type ReachabilityTarget,
} from "../../src/engine/api-reachability.js";
import { NETWORK_RESUME_PROBE_INTERVAL_MS } from "../../src/engine/network-resume.js";
import { logRecordsSince, logSinkMark } from "../log-records.js";

const servers: http.Server[] = [];

afterEach(async () => {
  await Promise.all(servers.splice(0).map((server) => new Promise((resolve) => server.close(resolve))));
});

/** A loopback server; answers with the handler, or leaves CONNECT to `onConnect`. */
async function loopback(
  handler: http.RequestListener,
  onConnect?: (request: http.IncomingMessage, socket: import("node:stream").Duplex) => void,
): Promise<number> {
  const server = http.createServer(handler);
  if (onConnect !== undefined) server.on("connect", onConnect);
  servers.push(server);
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  return (server.address() as AddressInfo).port;
}

/** A transport that records its calls and answers as told. */
function scriptedTransport(answer: { head?: number | Error; connect?: number | Error }): ProbeTransport & {
  calls: string[];
} {
  const calls: string[] = [];
  const settle = (value: number | Error | undefined): Promise<number> =>
    value instanceof Error ? Promise.reject(value) : Promise.resolve(value ?? 200);
  return {
    calls,
    head: (url) => {
      calls.push(`HEAD ${url.href}`);
      return settle(answer.head);
    },
    connect: (proxy, host, port) => {
      calls.push(`CONNECT ${proxy.host} ${host}:${String(port)}`);
      return settle(answer.connect);
    },
  };
}

describe("resolveReachabilityTarget", () => {
  it("probes the public API when nothing configures another", () => {
    // Act
    const target = resolveReachabilityTarget({});

    // Assert
    expect(target).toMatchObject({ kind: "direct" });
    expect(target.kind === "direct" && target.url.origin).toBe(DEFAULT_API_BASE_URL);
  });

  it("probes ANTHROPIC_BASE_URL when it is set", () => {
    // Act
    const target = resolveReachabilityTarget({ ANTHROPIC_BASE_URL: "https://gateway.example.com/v1" });

    // Assert
    expect(target.kind === "direct" && target.url.href).toBe("https://gateway.example.com/v1");
  });

  it("a base URL that does not parse is unresolvable, never replaced by the default", () => {
    // Act
    const target = resolveReachabilityTarget({ ANTHROPIC_BASE_URL: "not a url" });

    // Assert
    expect(target.kind).toBe("unresolvable");
  });

  it("a base URL that is not http(s) is unresolvable", () => {
    // Act
    const target = resolveReachabilityTarget({ ANTHROPIC_BASE_URL: "ftp://api.example.com" });

    // Assert
    expect(target.kind).toBe("unresolvable");
  });

  it("goes through HTTPS_PROXY for an https API", () => {
    // Act
    const target = resolveReachabilityTarget({ HTTPS_PROXY: "http://proxy.local:3128" });

    // Assert
    expect(target.kind === "proxied" && target.proxy.host).toBe("proxy.local:3128");
  });

  it("reads the lowercase https_proxy too", () => {
    // Act
    const target = resolveReachabilityTarget({ https_proxy: "http://proxy.local:3128" });

    // Assert
    expect(target.kind).toBe("proxied");
  });

  it("goes through HTTP_PROXY for an http API", () => {
    // Act
    const target = resolveReachabilityTarget({
      ANTHROPIC_BASE_URL: "http://gateway.internal",
      HTTP_PROXY: "http://proxy.local:3128",
    });

    // Assert
    expect(target.kind).toBe("proxied");
  });

  it("ignores HTTP_PROXY for an https API", () => {
    // Act
    const target = resolveReachabilityTarget({ HTTP_PROXY: "http://proxy.local:3128" });

    // Assert
    expect(target.kind).toBe("direct");
  });

  it("NO_PROXY naming the API host goes direct", () => {
    // Act
    const target = resolveReachabilityTarget({ HTTPS_PROXY: "http://proxy.local:3128", NO_PROXY: "anthropic.com" });

    // Assert
    expect(target.kind).toBe("direct");
  });

  it("a proxy that does not parse is unresolvable", () => {
    // Act
    const target = resolveReachabilityTarget({ HTTPS_PROXY: "::nonsense::" });

    // Assert
    expect(target.kind).toBe("unresolvable");
  });
});

describe("recordReachabilityTarget", () => {
  it("records an unresolvable target at ERROR with its reason", () => {
    // Arrange
    const mark = logSinkMark();

    // Act
    recordReachabilityTarget({ kind: "unresolvable", detail: "bad url" });

    // Assert
    expect(logRecordsSince(mark)).toMatchObject([{ level: "error", context: { detail: "bad url" } }]);
  });

  it("records a direct target at DEBUG naming the API origin", () => {
    // Arrange
    const mark = logSinkMark();

    // Act
    recordReachabilityTarget({ kind: "direct", url: new URL("https://api.anthropic.com") });

    // Assert
    expect(logRecordsSince(mark)).toMatchObject([
      { level: "debug", context: { api_origin: "https://api.anthropic.com" } },
    ]);
  });

  it("records a proxied target at DEBUG naming the proxy", () => {
    // Arrange
    const mark = logSinkMark();

    // Act
    recordReachabilityTarget({
      kind: "proxied",
      url: new URL("https://api.anthropic.com"),
      proxy: new URL("http://proxy.local:3128"),
    });

    // Assert
    expect(logRecordsSince(mark)).toMatchObject([{ level: "debug", context: { proxy: "proxy.local:3128" } }]);
  });
});

describe("noProxyExempts", () => {
  const CASES: readonly [string, string | undefined, string, boolean][] = [
    ["unset", undefined, "api.anthropic.com", false],
    ["a wildcard", "*", "api.anthropic.com", true],
    ["the host itself", "api.anthropic.com", "api.anthropic.com", true],
    ["a parent domain", "anthropic.com", "api.anthropic.com", true],
    ["a dotted suffix", ".anthropic.com", "api.anthropic.com", true],
    ["an entry with a port", "api.anthropic.com:443", "api.anthropic.com", true],
    ["an unrelated host", "internal.example, localhost", "api.anthropic.com", false],
    ["a lookalike suffix", "thropic.com", "api.anthropic.com", false],
  ];
  it.each(CASES)("%s", (_name, noProxy, host, exempt) => {
    // Act / Assert
    expect(noProxyExempts(noProxy, host)).toBe(exempt);
  });
});

describe("createReachabilityProbe", () => {
  const direct: ReachabilityTarget = { kind: "direct", url: new URL("https://api.anthropic.com") };
  const proxied: ReachabilityTarget = {
    kind: "proxied",
    url: new URL("https://api.anthropic.com"),
    proxy: new URL("http://proxy.local:3128"),
  };

  it("any HTTP answer from the API is reachable, a 404 included", async () => {
    // Arrange
    const probe = createReachabilityProbe(direct, scriptedTransport({ head: 404 }));

    // Act
    const answer = await probe();

    // Assert
    expect(answer.reachable).toBe(true);
  });

  it("probes with HEAD, which the API does not bill", async () => {
    // Arrange
    const transport = scriptedTransport({ head: 405 });

    // Act
    await createReachabilityProbe(direct, transport)();

    // Assert
    expect(transport.calls).toEqual(["HEAD https://api.anthropic.com/"]);
  });

  it("no answer from the API is unreachable, naming why", async () => {
    // Arrange
    const failure = Object.assign(new Error("getaddrinfo ENOTFOUND api.anthropic.com"), { code: "ENOTFOUND" });
    const probe = createReachabilityProbe(direct, scriptedTransport({ head: failure }));

    // Act
    const answer = await probe();

    // Assert
    expect(answer).toMatchObject({ reachable: false });
    expect(answer.detail).toContain("ENOTFOUND");
  });

  it("an error whose message omits its code states the code", async () => {
    // Arrange
    const failure = Object.assign(new Error("connect failed"), { code: "ECONNREFUSED" });
    const probe = createReachabilityProbe(direct, scriptedTransport({ head: failure }));

    // Act
    const answer = await probe();

    // Assert
    expect(answer.detail).toContain("ECONNREFUSED: connect failed");
  });

  it("a non-Error rejection is still an unreachable answer", async () => {
    // Arrange
    const transport: ProbeTransport = {
      head: () => Promise.reject("a string"),
      connect: () => Promise.reject("a string"),
    };

    // Act
    const answer = await createReachabilityProbe(direct, transport)();

    // Assert
    expect(answer).toMatchObject({ reachable: false });
  });

  it("behind a proxy, CONNECTs to the API's host and port", async () => {
    // Arrange
    const transport = scriptedTransport({ connect: 200 });

    // Act
    await createReachabilityProbe(proxied, transport)();

    // Assert
    expect(transport.calls).toEqual(["CONNECT proxy.local:3128 api.anthropic.com:443"]);
  });

  it("a proxy that tunnels to the API is reachable", async () => {
    // Act
    const answer = await createReachabilityProbe(proxied, scriptedTransport({ connect: 200 }))();

    // Assert
    expect(answer.reachable).toBe(true);
  });

  it("a proxy answering 502 could not reach the API", async () => {
    // Act
    const answer = await createReachabilityProbe(proxied, scriptedTransport({ connect: 502 }))();

    // Assert
    expect(answer.reachable).toBe(false);
  });

  it("a proxy that cannot be reached is unreachable", async () => {
    // Act
    const answer = await createReachabilityProbe(proxied, scriptedTransport({ connect: new Error("ECONNREFUSED") }))();

    // Assert
    expect(answer).toMatchObject({ reachable: false });
  });

  it("an unresolvable target answers unreachable with its reason, touching nothing", async () => {
    // Arrange
    const transport = scriptedTransport({});

    // Act
    const answer = await createReachabilityProbe({ kind: "unresolvable", detail: "bad url" }, transport)();

    // Assert
    expect(answer).toEqual({ reachable: false, detail: "bad url" });
    expect(transport.calls).toEqual([]);
  });

  it("one probe's bound is under the loop's beat", () => {
    // Assert
    expect(PROBE_TIMEOUT_MS).toBeLessThan(NETWORK_RESUME_PROBE_INTERVAL_MS);
  });
});

describe("NODE_TRANSPORT, against a loopback server", () => {
  it("HEAD resolves with the status the server answered", async () => {
    // Arrange
    const port = await loopback((request, response) => {
      response.statusCode = request.method === "HEAD" ? 404 : 500;
      response.end();
    });

    // Act
    const status = await NODE_TRANSPORT.head(new URL(`http://127.0.0.1:${String(port)}/`), 1_000);

    // Assert
    expect(status).toBe(404);
  });

  it("HEAD rejects when nothing listens", async () => {
    // Arrange: take a port, then close it.
    const port = await loopback((_request, response) => response.end());
    await new Promise((resolve) => servers.pop()?.close(resolve));

    // Act / Assert
    await expect(NODE_TRANSPORT.head(new URL(`http://127.0.0.1:${String(port)}/`), 1_000)).rejects.toThrow();
  });

  it("HEAD rejects when the server never answers within the bound", async () => {
    // Arrange: a server that holds every request open.
    const port = await loopback(() => undefined);

    // Act / Assert
    await expect(NODE_TRANSPORT.head(new URL(`http://127.0.0.1:${String(port)}/`), 50)).rejects.toThrow(/no answer/);
  });

  it("CONNECT resolves with the proxy's status and sends its credentials", async () => {
    // Arrange
    let seen: { url?: string; auth?: string } = {};
    const port = await loopback(
      (_request, response) => response.end(),
      (request, socket) => {
        seen = { url: request.url, auth: request.headers["proxy-authorization"] };
        socket.end("HTTP/1.1 200 Connection established\r\n\r\n");
      },
    );

    // Act
    const status = await NODE_TRANSPORT.connect(
      new URL(`http://user:pa%20ss@127.0.0.1:${String(port)}`),
      "api.anthropic.com",
      443,
      1_000,
    );

    // Assert
    expect(status).toBe(200);
    expect(seen).toEqual({
      url: "api.anthropic.com:443",
      auth: `Basic ${Buffer.from("user:pa ss").toString("base64")}`,
    });
  });

  it("CONNECT rejects when the proxy never answers within the bound", async () => {
    // Arrange: a proxy that holds every tunnel request open.
    const held: import("node:stream").Duplex[] = [];
    const port = await loopback(
      (_request, response) => response.end(),
      (_request, socket) => {
        held.push(socket);
      },
    );

    // Act
    const connecting = NODE_TRANSPORT.connect(new URL(`http://127.0.0.1:${String(port)}`), "api.anthropic.com", 443, 50);

    // Assert
    await expect(connecting).rejects.toThrow(/no answer/);
    for (const socket of held) socket.destroy();
  });
});

describe("createFakeReachabilityProbe", () => {
  it("with no gate is always reachable", async () => {
    // Act
    const answer = await createFakeReachabilityProbe(undefined)();

    // Assert
    expect(answer.reachable).toBe(true);
  });

  it("with a gate is unreachable while the gate is absent", async () => {
    // Arrange
    const gate = path.join(mkdtempSync(path.join(os.tmpdir(), "shim-gate-")), "reachable");

    // Act
    const answer = await createFakeReachabilityProbe(gate)();

    // Assert
    expect(answer.reachable).toBe(false);
  });

  it("with a gate is reachable once the gate exists", async () => {
    // Arrange
    const gate = path.join(mkdtempSync(path.join(os.tmpdir(), "shim-gate-")), "reachable");
    writeFileSync(gate, "");

    // Act
    const answer = await createFakeReachabilityProbe(gate)();

    // Assert
    expect(answer.reachable).toBe(true);
  });
});
