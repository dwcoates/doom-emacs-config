import { afterEach, describe, expect, test } from "vitest";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import { freshSession, openStream, startTurnRequest, watchAgentRequest } from "../integration-support/client.js";

afterEach(cleanupShims);

describe("diag", () => {
  test("away-summary", async () => {
    const t0 = Date.now();
    const mark = (s: string): void => console.log(`[${Date.now() - t0}ms] ${s}`);
    const shim = await spawnShim({ logPipe: true });
    mark("spawned");
    await shim.clients.h1.startSession(freshSession());
    mark("session started");
    const stream = openStream((o) => shim.clients.h1.watchAgent(watchAgentRequest(), o));
    await stream.next();
    mark("stream opened");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!residue" }));
    mark("turn started");
    await new Promise((r) => setTimeout(r, 5000));
    mark("slept");
    console.log("UNSERVED:", JSON.stringify(shim.store?.unserved() ?? []));
    console.log("STDERR:", shim.stderr().slice(-3000));
    stream.close();
    expect(true).toBe(true);
  }, 120000);
});
