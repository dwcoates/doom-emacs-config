/**
 * The listener: the stale/live judgement, the preface sniff, and both dialects
 * reaching the SAME handlers over one unix socket.
 */
import { createClient, Code, ConnectError } from "@connectrpc/connect";
import { createConnectTransport } from "@connectrpc/connect-node";
import { create } from "@bufbuild/protobuf";
import { connect as netConnect } from "node:net";
import { mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { afterEach, describe, expect, it } from "vitest";
import { NotImplementedEngine } from "../../src/engine/engine.js";
import { shimv1 } from "../../src/proto.js";
import { shimRoutes } from "../../src/service/routes.js";
import { HTTP2_PREFACE, probeSocket, serve, sniffProtocol, type ShimServer } from "../../src/service/server.js";

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
