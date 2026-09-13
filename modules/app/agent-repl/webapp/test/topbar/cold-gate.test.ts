// @vitest-environment jsdom
//
// The cold-gate cell: the literal words, the context figure, and the age it
// ticks from the wire's instant.
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { TopbarColdGateSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { drawTopbarColdGate } from "../../src/topbar/cold-gate.js";
import { NOW, appContext, fakeTicker, topbarContext } from "./fixtures.js";

describe("drawTopbarColdGate", () => {
  it("draws the literal words with the context figure and the age", () => {
    const { tc } = topbarContext();
    const cell = drawTopbarColdGate(
      create(TopbarColdGateSchema, { contextTokens: 142_300n, sinceMs: BigInt(NOW - 90_000) }),
      tc,
    );
    expect(cell.textContent).toBe("cold context 142.3k 1m 30s");
  });

  it("ticks the age forward on the shared clock", () => {
    const ticker = fakeTicker();
    const { tc } = topbarContext(appContext({}, undefined, ticker));
    const cell = drawTopbarColdGate(
      create(TopbarColdGateSchema, { contextTokens: 1_000n, sinceMs: BigInt(NOW) }),
      tc,
    );

    ticker.set(NOW + 5_000);

    expect(cell.textContent).toBe("cold context 1k 5s");
  });

  it("refuses a negative context count as malformed", () => {
    const { tc } = topbarContext();
    expect(() =>
      drawTopbarColdGate(
        create(TopbarColdGateSchema, { contextTokens: -1n, sinceMs: BigInt(NOW) }),
        tc,
      ),
    ).toThrow(MalformedView);
  });
});
