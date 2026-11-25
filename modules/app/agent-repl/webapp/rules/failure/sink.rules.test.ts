/**
 * THE FAILURE SINK'S OUTAGE QUESTION IS THE ONE EVERY TRANSPORT FAILURE ASKS
 * (`src/failure/sink.ts`): no transport reads `suppressing` by hand.
 */
import { describe, expect, it } from "vitest";

describe("outageAnnounced", () => {
  it("is the one question every transport failure asks", async () => {
    // ARRANGE: a hand-rolled check would read `suppressing` itself.
    const { readFile } = await import("node:fs/promises");
    const sources = await Promise.all(["src/rpc/streams.ts", "src/rpc/unary.ts"].map((p) => readFile(p, "utf8")));
    // ACT, ASSERT
    expect(sources.map((src) => [src.includes("outageAnnounced(ctx.failures)"), src.includes(".suppressing")])).toEqual([
      [true, false],
      [true, false],
    ]);
  });
});
