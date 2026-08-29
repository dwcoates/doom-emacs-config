import { describe, expect, it, vi } from "vitest";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { createDaemonTransport } from "../../src/rpc/transport.js";

describe("createDaemonTransport", () => {
  it("builds a transport with the two methods a Connect client calls", () => {
    // ARRANGE / ACT
    const transport = createDaemonTransport("http://localhost:8080");
    // ASSERT
    expect(typeof transport.unary).toBe("function");
    expect(typeof transport.stream).toBe("function");
  });

  it("builds a transport without an injected fetch, as production does", () => {
    expect(() => createDaemonTransport("http://localhost:8080")).not.toThrow();
  });

  it("accepts an injected fetch, which is how a node-hosted suite drives it", () => {
    const fetchFn = vi.fn() as unknown as typeof globalThis.fetch;
    expect(() => createDaemonTransport("http://localhost:8080", { fetch: fetchFn })).not.toThrow();
  });

  it("uses the injected fetch rather than the page's for a real call", async () => {
    // ARRANGE
    const fetchFn = vi.fn(async () => new Response(new Uint8Array(), { status: 500 }));
    const client = createAgentReplClient(
      createDaemonTransport("http://localhost:8080", {
        fetch: fetchFn as unknown as typeof globalThis.fetch,
      }),
    );
    // ACT: the daemon is not there, so the call fails -- but only after the
    // transport reached for the fetch it was handed.
    await expect(client.daemonHealth({})).rejects.toBeDefined();
    // ASSERT
    expect(fetchFn).toHaveBeenCalledOnce();
  });

  it("posts binary rather than JSON, so the library owns the framing", async () => {
    // ARRANGE
    const seen: Array<string | undefined> = [];
    const fetchFn = vi.fn(async (_url: unknown, init?: RequestInit) => {
      seen.push(new Headers(init?.headers).get("content-type") ?? undefined);
      return new Response(new Uint8Array(), { status: 500 });
    });
    const client = createAgentReplClient(
      createDaemonTransport("http://localhost:8080", {
        fetch: fetchFn as unknown as typeof globalThis.fetch,
      }),
    );
    // ACT
    await expect(client.daemonHealth({})).rejects.toBeDefined();
    // ASSERT
    expect(seen[0]).toBe("application/proto");
  });
});
