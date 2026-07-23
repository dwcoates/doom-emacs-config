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

/** Await a predicate without wall-clock sleeps (advances the event loop). */
export async function until(pred: () => boolean, label = "condition"): Promise<void> {
  for (let i = 0; i < 2000; i++) {
    if (pred()) return;
    await new Promise<void>((resolve) => setImmediate(resolve));
  }
  throw new Error(`until(): ${label} never became true`);
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
  async next<Desc extends DescMessage>(schema: Desc, label?: string): Promise<MessageShape<Desc>> {
    for (let i = 0; i < 2000; i++) {
      const idx = this.inbox.findIndex((a) => unpackAs(a, schema) !== undefined);
      if (idx >= 0) {
        const [any] = this.inbox.splice(idx, 1);
        return unpackAs(any!, schema)!;
      }
      await new Promise<void>((resolve) => setImmediate(resolve));
    }
    throw new Error(`FramedPeer.next(): no ${label ?? schema.typeName} arrived`);
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
