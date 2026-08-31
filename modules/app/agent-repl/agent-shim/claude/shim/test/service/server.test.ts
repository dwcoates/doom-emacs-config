/**
 * The listener: the stale/live judgement, the preface sniff, and both dialects
 * reaching the SAME handlers over one unix socket.
 */
import { createClient, Code, ConnectError } from "@connectrpc/connect";
import { createConnectTransport } from "@connectrpc/connect-node";
import { create } from "@bufbuild/protobuf";
import { connect as netConnect } from "node:net";
import { request as httpRequest } from "node:http";
import { connect as http2Connect } from "node:http2";
import { mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import { NotImplementedEngine } from "../../src/engine/engine.js";
import { shimv1 } from "../../src/proto.js";
import { shimRoutes } from "../../src/service/routes.js";
import {
  HTTP2_PREFACE,
  flushStreamHead,
  isStreamingContentType,
  probeSocket,
  serve,
  sniffProtocol,
  type ShimServer,
} from "../../src/service/server.js";

const started: ShimServer[] = [];

afterEach(async () => {
  for (const server of started.splice(0)) await server.close();
});

function socketPath(): string {
  return path.join(mkdtempSync(path.join(os.tmpdir(), "shim-server-")), "shim.sock");
}

async function start(sock: string): Promise<ShimServer> {
  const server = await serve(sock, shimRoutes(new NotImplementedEngine()));
  started.push(server);
  return server;
}

/**
 * A Connect client over the unix socket.
 *
 * THE TWO DIALECTS ARE DIALED DIFFERENTLY, and this is the one place that
 * knowledge lives. `http.request` takes a `socketPath` option, so HTTP/1.1 over
 * a unix socket is a plain option. `http2.connect` has NO `socketPath` — it
 * takes a `createConnection` factory instead — so an h2c client that passes
 * `socketPath` silently dials the authority over TCP and fails Unavailable.
 */
function client(sock: string, httpVersion: "1.1" | "2") {
  const nodeOptions =
    httpVersion === "1.1"
      ? { socketPath: sock }
      : { createConnection: (): ReturnType<typeof netConnect> => netConnect({ path: sock }) };
  return createClient(
    shimv1.Shim,
    createConnectTransport({ httpVersion, baseUrl: "http://shim", nodeOptions }),
  );
}

describe("sniffProtocol", () => {
  it("recognizes the HTTP/2 preface once enough of it has arrived", () => {
    // Arrange, Act.
    const verdict = sniffProtocol(Buffer.from(HTTP2_PREFACE, "latin1"));

    // Assert.
    expect(verdict).toBe("h2");
  });

  it("recognizes an HTTP/1.1 request line immediately", () => {
    // Arrange, Act.
    const verdict = sniffProtocol(Buffer.from("POST /shim.v1.Shim/Hibernate HTTP/1.1\r\n", "latin1"));

    // Assert.
    expect(verdict).toBe("h1");
  });

  it("WAITS rather than guessing on an incomplete preface prefix", () => {
    // Arrange, Act.
    const verdict = sniffProtocol(Buffer.from("PRI * HT", "latin1"));

    // Assert.
    expect(verdict).toBe("need-more");
  });

  it("waits on an empty first chunk instead of calling it HTTP/1.1", () => {
    // Arrange, Act.
    const verdict = sniffProtocol(Buffer.alloc(0));

    // Assert.
    expect(verdict).toBe("need-more");
  });
});

describe("probeSocket", () => {
  it("reports a path with nothing at it as free", async () => {
    // Arrange.
    const sock = socketPath();

    // Act.
    const verdict = await probeSocket(sock);

    // Assert.
    expect(verdict).toBe("free");
  });

  it("reports a leftover socket FILE with no process behind it as stale", async () => {
    // Arrange.
    const sock = socketPath();
    writeFileSync(sock, "");

    // Act.
    const verdict = await probeSocket(sock);

    // Assert.
    expect(verdict).toBe("stale");
  });

  it("reports a socket somebody is listening on as live", async () => {
    // Arrange.
    const sock = socketPath();
    await start(sock);

    // Act.
    const verdict = await probeSocket(sock);

    // Assert.
    expect(verdict).toBe("live");
  });
});

describe("serve", () => {
  it("binds over a stale socket file left by a dead predecessor", async () => {
    // Arrange.
    const sock = socketPath();
    writeFileSync(sock, "");

    // Act.
    const server = await start(sock);

    // Assert.
    expect(server.socketPath).toBe(sock);
  });

  it("REFUSES to bind over a live listener rather than orphaning it", async () => {
    // Arrange.
    const sock = socketPath();
    await start(sock);

    // Act, Assert.
    await expect(serve(sock, shimRoutes(new NotImplementedEngine()))).rejects.toThrow(
      /already has a live listener/,
    );
  });

  it("serves the router over HTTP/1.1 on the unix socket", async () => {
    // Arrange.
    const sock = socketPath();
    await start(sock);

    // Act.
    const rejection = await client(sock, "1.1")
      .hibernate(create(shimv1.HibernateRequestSchema, {}))
      .then(() => null, (err: unknown) => ConnectError.from(err));

    // Assert.
    expect(rejection?.code).toBe(Code.Unimplemented);
  });

  it("serves the SAME router over h2c on the same socket", async () => {
    // Arrange.
    const sock = socketPath();
    await start(sock);

    // Act.
    const rejection = await client(sock, "2")
      .hibernate(create(shimv1.HibernateRequestSchema, {}))
      .then(() => null, (err: unknown) => ConnectError.from(err));

    // Assert.
    expect(rejection?.code).toBe(Code.Unimplemented);
  });

  it("removes its socket file on close so a successor sees a free path", async () => {
    // Arrange.
    const sock = socketPath();
    const server = await start(sock);
    started.length = 0;

    // Act.
    await server.close();

    // Assert.
    await expect(probeSocket(sock)).resolves.toBe("free");
  });
});

/**
 * The response head goes out ON ACCEPT for a standing stream.
 *
 * WHAT THIS GUARDS: that a healthy stream which has not spoken yet is
 * distinguishable, at the client, from a refused one. connect-node writes the
 * head lazily — for a stream that has pushed nothing it fires only at the end —
 * and a Go client surfaces a server-stream refusal at its first Receive. Without
 * the early head the daemon's bring-up blocks on a WatchSession that is
 * perfectly fine and merely quiet.
 */
describe("standing streams", () => {
  /** An engine whose WatchSession accepts and then says nothing. */
  function silentEngine(): NotImplementedEngine {
    const engine = new NotImplementedEngine();
    engine.watchSession = (): AsyncIterable<shimv1.WatchSessionResponse> => ({
      [Symbol.asyncIterator]: (): AsyncIterator<shimv1.WatchSessionResponse> => ({
        next: () => new Promise<IteratorResult<shimv1.WatchSessionResponse>>(() => {}),
      }),
    });
    return engine;
  }

  async function startSilent(sock: string): Promise<ShimServer> {
    const server = await serve(sock, shimRoutes(silentEngine()));
    started.push(server);
    return server;
  }

  /** One Connect streaming envelope carrying an empty message. */
  const EMPTY_ENVELOPE = Buffer.from([0, 0, 0, 0, 0]);
  const STREAM_CONTENT_TYPE = "application/connect+proto";
  const WATCH_SESSION_PATH = "/shim.v1.Shim/WatchSession";

  it("sends HTTP/1.1 response headers before the first frame", async () => {
    const sock = socketPath();
    await startSilent(sock);

    const status = await new Promise<number>((resolve, reject) => {
      const request = httpRequest(
        {
          socketPath: sock,
          path: WATCH_SESSION_PATH,
          method: "POST",
          headers: { "content-type": STREAM_CONTENT_TYPE, "connect-protocol-version": "1" },
        },
        (response) => resolve(response.statusCode ?? 0),
      );
      request.on("error", reject);
      request.write(EMPTY_ENVELOPE);
    });

    expect(status).toBe(200);
  });

  it("echoes the request's streaming content type on that early head", async () => {
    const sock = socketPath();
    await startSilent(sock);

    const contentType = await new Promise<string | undefined>((resolve, reject) => {
      const request = httpRequest(
        {
          socketPath: sock,
          path: WATCH_SESSION_PATH,
          method: "POST",
          headers: { "content-type": STREAM_CONTENT_TYPE, "connect-protocol-version": "1" },
        },
        (response) => resolve(response.headers["content-type"]),
      );
      request.on("error", reject);
      request.write(EMPTY_ENVELOPE);
    });

    expect(contentType).toBe(STREAM_CONTENT_TYPE);
  });

  it("sends h2c response headers before the first frame", async () => {
    const sock = socketPath();
    await startSilent(sock);

    const session = http2Connect("http://shim", {
      createConnection: (): ReturnType<typeof netConnect> => netConnect({ path: sock }),
    });
    try {
      const status = await new Promise<number>((resolve, reject) => {
        const stream = session.request({
          ":method": "POST",
          ":path": WATCH_SESSION_PATH,
          "content-type": STREAM_CONTENT_TYPE,
          "connect-protocol-version": "1",
        });
        stream.on("response", (headers) => resolve(Number(headers[":status"])));
        stream.on("error", reject);
        stream.write(EMPTY_ENVELOPE);
      });

      expect(status).toBe(200);
    } finally {
      session.destroy();
    }
  });

  it("still surfaces an Unimplemented streaming verb's Connect error after the early head", async () => {
    // The head is not the refusal's carrier: the Connect protocol puts a
    // stream's error in its end-of-stream frame, which is why committing to 200
    // on accept costs nothing.
    const sock = socketPath();
    await start(sock);

    const failure = await (async (): Promise<unknown> => {
      try {
        for await (const _ of client(sock, "1.1").watchWorkflow(
          create(shimv1.WatchWorkflowRequestSchema, {
            watch: create(shimv1.WorkflowWatchTokenSchema, { value: "w" }),
          }),
        )) {
          return "yielded";
        }
        return "ended";
      } catch (err) {
        return ConnectError.from(err).code;
      }
    })();

    expect(failure).toBe(Code.Unimplemented);
  });

  it("surfaces it over h2c too", async () => {
    const sock = socketPath();
    await start(sock);

    const failure = await (async (): Promise<unknown> => {
      try {
        for await (const _ of client(sock, "2").watchWorkflow(
          create(shimv1.WatchWorkflowRequestSchema, {
            watch: create(shimv1.WorkflowWatchTokenSchema, { value: "w" }),
          }),
        )) {
          return "yielded";
        }
        return "ended";
      } catch (err) {
        return ConnectError.from(err).code;
      }
    })();

    expect(failure).toBe(Code.Unimplemented);
  });
});

describe("the streaming content-type judgement", () => {
  it("recognizes the Connect streaming framing", () => {
    expect(isStreamingContentType("application/connect+proto")).toBe(true);
  });

  it("ignores parameters after the media type", () => {
    expect(isStreamingContentType("application/connect+json; charset=utf-8")).toBe(true);
  });

  it("does NOT claim a unary Connect request", () => {
    // A unary call has a real status to report; a committed 200 would lose it.
    expect(isStreamingContentType("application/proto")).toBe(false);
  });

  it("does not claim a request with no content type", () => {
    expect(isStreamingContentType(undefined)).toBe(false);
  });
});

describe("flushing a stream's head", () => {
  function response(): { headersSent: boolean; head?: [number, Record<string, string>]; flushed: number; writeHead: (status: number, headers: Record<string, string>) => unknown; flushHeaders: () => void } {
    const state = {
      headersSent: false,
      head: undefined as [number, Record<string, string>] | undefined,
      flushed: 0,
      writeHead(status: number, headers: Record<string, string>): unknown {
        state.head = [status, headers];
        return state;
      },
      flushHeaders(): void {
        state.flushed++;
      },
    };
    return state;
  }

  it("writes a 200 with the request's own content type", () => {
    const res = response();

    flushStreamHead({ headers: { "content-type": "application/connect+proto" } }, res);

    expect(res.head).toEqual([200, { "content-type": "application/connect+proto" }]);
  });

  it("flushes them", () => {
    const res = response();

    flushStreamHead({ headers: { "content-type": "application/connect+proto" } }, res);

    expect(res.flushed).toBe(1);
  });

  it("ABSORBS the adapter's later writeHead, which would otherwise throw", () => {
    const res = response();
    flushStreamHead({ headers: { "content-type": "application/connect+proto" } }, res);

    res.writeHead(500, {});

    expect(res.head).toEqual([200, { "content-type": "application/connect+proto" }]);
  });

  it("leaves a unary request alone", () => {
    const res = response();

    expect(flushStreamHead({ headers: { "content-type": "application/proto" } }, res)).toBe(false);
  });

  it("does nothing when the head has already gone out", () => {
    const res = { ...response(), headersSent: true };

    expect(flushStreamHead({ headers: { "content-type": "application/connect+proto" } }, res)).toBe(
      false,
    );
  });
});
