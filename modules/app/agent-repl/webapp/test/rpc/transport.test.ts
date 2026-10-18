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

  it.each([
    { name: "carries no signal of its own", own: undefined },
    { name: "carries a signal of its own", own: new AbortController().signal },
  ])("hands every request a signal the page's end aborts, when the call $name", async ({ own }) => {
    // Arrange
    const page = new AbortController();
    let signal: AbortSignal | null | undefined;
    const fetchFn = vi.fn(async (_url: unknown, init?: RequestInit) => {
      signal = init?.signal;
      return new Response(new Uint8Array(), { status: 500 });
    });
    const client = createAgentReplClient(
      createDaemonTransport("http://localhost:8080", { fetch: fetchFn as unknown as typeof globalThis.fetch, signal: page.signal }),
    );
    await expect(client.daemonHealth({}, own === undefined ? {} : { signal: own })).rejects.toBeDefined();
    // Act
    page.abort();
    // Assert
    expect(signal?.aborted).toBe(true);
  });

  it.each([
    { name: "carries no signal of its own", own: undefined },
    { name: "carries a signal of its own", own: new AbortController().signal },
  ])("aborts a call in flight at the page's end without AbortSignal.any, when the call $name", async ({ own }) => {
    // Arrange: an engine without AbortSignal.any (WebKitGTK 2.40), and a fetch
    // that answers only when its signal aborts.
    const any = Object.getOwnPropertyDescriptor(AbortSignal, "any");
    Reflect.deleteProperty(AbortSignal, "any");
    try {
      const page = new AbortController();
      let sent: AbortSignal | null | undefined;
      const fetchFn = vi.fn(
        (_url: unknown, init?: RequestInit) =>
          new Promise<Response>((_resolve, reject) => {
            sent = init?.signal;
            init?.signal?.addEventListener("abort", () => reject(new DOMException("aborted", "AbortError")));
          }),
      );
      const client = createAgentReplClient(
        createDaemonTransport("http://localhost:8080", { fetch: fetchFn as unknown as typeof globalThis.fetch, signal: page.signal }),
      );
      const call = client.daemonHealth({}, own === undefined ? {} : { signal: own });
      await Promise.resolve();
      // Act
      page.abort();
      // Assert: the request reached fetch, and the page's end aborted it.
      await expect(call).rejects.toBeDefined();
      expect([fetchFn.mock.calls.length, sent?.aborted]).toEqual([1, true]);
    } finally {
      if (any !== undefined) Object.defineProperty(AbortSignal, "any", any);
    }
  });

  it("leaves no listener on the page's signal once a call completes", async () => {
    // Arrange: every listener the page's signal holds, by identity.
    const page = new AbortController();
    const listening = new Set<unknown>();
    const add = page.signal.addEventListener.bind(page.signal);
    const remove = page.signal.removeEventListener.bind(page.signal);
    page.signal.addEventListener = ((...args: Parameters<AbortSignal["addEventListener"]>) => {
      listening.add(args[1]);
      add(...args);
    }) as AbortSignal["addEventListener"];
    page.signal.removeEventListener = ((...args: Parameters<AbortSignal["removeEventListener"]>) => {
      listening.delete(args[1]);
      remove(...args);
    }) as AbortSignal["removeEventListener"];
    const answer = vi.fn(async () => new Response(new Uint8Array(), { status: 500 }));
    const client = createAgentReplClient(
      createDaemonTransport("http://localhost:8080", { fetch: answer as unknown as typeof globalThis.fetch, signal: page.signal }),
    );
    // Act
    await expect(client.daemonHealth({}, { signal: new AbortController().signal })).rejects.toBeDefined();
    // Assert
    expect(listening.size).toBe(0);
  });
});
