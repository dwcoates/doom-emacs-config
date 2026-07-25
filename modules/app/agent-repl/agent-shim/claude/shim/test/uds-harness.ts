/**
 * Test-only helpers for the UDS transport suites: ephemeral socket paths and
 * a framed "peer" that speaks the same Any-over-length-prefix convention the
 * production code does (so a test can play the daemon or the store).
 *
 * NB: not a *.test.ts file, so vitest does not collect it as a suite.
 */
import net from "node:net";
import os from "node:os";
import fs from "node:fs";
import path from "node:path";
import { randomUUID } from "node:crypto";
import type { DescMessage, MessageShape } from "@bufbuild/protobuf";
import {
  FrameDecoder,
  decodeEnvelope,
  encodeMessage,
  unpackAs,
} from "../src/uds/framing.js";
import type { Any } from "../src/uds/framing.js";

/** A unique socket path under a fresh temp dir (short, to dodge the ~104-char UDS limit). */
export function tmpSocketPath(): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), "uds-"));
  return path.join(dir, `s-${randomUUID().slice(0, 8)}.sock`);
}

/**
 * Default budget for the poll helpers below. Deliberately a WALL-CLOCK budget
 * rather than an event-loop-turn count: a turn count is load-sensitive, since
 * a spinning `setImmediate` loop burns its whole budget in single-digit
 * milliseconds when the machine is busy, long before a pending connect() or
 * socket write has been delivered. Under a parallel vitest run that made
 * genuinely-correct code fail. Kept under vitest's 5s default testTimeout so
 * the labeled error below wins the race and names the missing condition.
 */
const POLL_BUDGET_MS = 4000;

/**
 * Poll `pred` until it holds or the budget expires, yielding to the event loop
 * between checks so pending I/O can land. Returns as soon as the condition is
 * met -- the budget is only a failure deadline, never a synchronization delay.
 * The first turns yield via setImmediate (cheap, sub-millisecond) and later
 * ones via setTimeout so a doomed wait cannot hot-spin a core for seconds and
 * starve the sibling workers it shares the machine with.
 */
async function pollFor(pred: () => boolean, describe: () => string, budgetMs: number): Promise<void> {
  const deadline = Date.now() + budgetMs;
  for (let turn = 0; ; turn++) {
    if (pred()) return;
    if (Date.now() >= deadline) throw new Error(describe());
    await (turn < 200
      ? new Promise<void>((resolve) => setImmediate(resolve))
      : new Promise<void>((resolve) => setTimeout(resolve, 1)));
  }
}

/** Await a predicate without wall-clock sleeps (advances the event loop). */
export async function until(
  pred: () => boolean,
  label = "condition",
  budgetMs = POLL_BUDGET_MS,
): Promise<void> {
  await pollFor(pred, () => `until(): ${label} never became true within ${budgetMs}ms`, budgetMs);
}

/**
 * Wraps a connected socket as a framed message peer: decodes inbound Any
 * frames into a queue and sends outbound messages with the production
 * encoder. `next(schema)` resolves the next inbound message of that type.
 */
export class FramedPeer {
  private readonly decoder = new FrameDecoder();
  readonly inbox: Any[] = [];
  closed = false;
  closeErr: Error | null = null;

  constructor(readonly socket: net.Socket) {
    socket.on("data", (chunk: Buffer) => {
      for (const frame of this.decoder.push(chunk)) {
        this.inbox.push(decodeEnvelope(frame));
      }
    });
    socket.on("error", (err: Error) => {
      this.closeErr = err;
    });
    socket.on("close", () => {
      this.closed = true;
    });
  }

  send<Desc extends DescMessage>(schema: Desc, message: MessageShape<Desc>): void {
    this.socket.write(encodeMessage(schema, message));
  }

  /** Resolve the next inbound message decodable as `schema` (consumes it). */
  async next<Desc extends DescMessage>(
    schema: Desc,
    label?: string,
    budgetMs = POLL_BUDGET_MS,
  ): Promise<MessageShape<Desc>> {
    const indexOfMatch = () => this.inbox.findIndex((a) => unpackAs(a, schema) !== undefined);
    await pollFor(
      () => indexOfMatch() >= 0,
      () => `FramedPeer.next(): no ${label ?? schema.typeName} arrived within ${budgetMs}ms`,
      budgetMs,
    );
    const [any] = this.inbox.splice(indexOfMatch(), 1);
    return unpackAs(any!, schema)!;
  }

  count<Desc extends DescMessage>(schema: Desc): number {
    return this.inbox.filter((a) => unpackAs(a, schema) !== undefined).length;
  }

  destroy(): void {
    this.socket.destroy();
  }
}

/** Connect a FramedPeer to `socketPath`, resolving once connected. */
export function connectPeer(socketPath: string): Promise<FramedPeer> {
  return new Promise((resolve, reject) => {
    const socket = net.connect(socketPath);
    socket.once("error", reject);
    socket.once("connect", () => {
      socket.removeListener("error", reject);
      resolve(new FramedPeer(socket));
    });
  });
}

/**
 * A one-shot UDS server that yields the two ends of one accepted connection.
 * Used to exercise MessageConn/FramedPeer round-trips.
 */
export function socketPair(): Promise<{ server: net.Server; a: net.Socket; b: net.Socket; path: string; close: () => void }> {
  const socketPath = tmpSocketPath();
  return new Promise((resolve, reject) => {
    const server = net.createServer((accepted) => {
      resolve({
        server,
        a: accepted,
        b: client,
        path: socketPath,
        close: () => {
          accepted.destroy();
          client.destroy();
          server.close();
        },
      });
    });
    server.once("error", reject);
    let client: net.Socket;
    server.listen(socketPath, () => {
      client = net.connect(socketPath);
    });
  });
}

/**
 * A fake DAEMON: listen on socketPath and hand back each shim that dials in.
 *
 * The shim dials the daemon now (design-shim-transport-inversion.md), so tests
 * must be the listener. Start this BEFORE `server.connect()`, which is the
 * ordering production has — the daemon listens long before it spawns a shim.
 *
 * `next()` resolves with successive connections, so a test can watch the shim
 * RECONNECT after a drop rather than dialling back in itself.
 */
export function acceptShim(socketPath: string): { next: () => Promise<FramedPeer>; close: () => void } {
  const server = net.createServer();
  const queued: FramedPeer[] = [];
  const waiting: Array<(p: FramedPeer) => void> = [];
  server.on("connection", (socket) => {
    const peer = new FramedPeer(socket);
    const resolve = waiting.shift();
    if (resolve) resolve(peer);
    else queued.push(peer);
  });
  server.listen(socketPath);
  return {
    next: () =>
      new Promise<FramedPeer>((resolve) => {
        const ready = queued.shift();
        if (ready) resolve(ready);
        else waiting.push(resolve);
      }),
    close: () => server.close(),
  };
}
