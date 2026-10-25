/**
 * One pulled vendor fact, and the health of reading it.
 *
 * WHAT THIS GUARDS: the contract every pulled read shares — an answer is
 * handed on and lifts its component's fault, a refusal is a fault on its own
 * component naming the call and never a rejection, and no query reads
 * nothing. The last case pins that the session's probes go through this one
 * helper rather than each spelling the contract again.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import { probeVendorFact, type ProbeHealth } from "../../src/engine/vendor-probe.js";
import type { QueryLike } from "../../src/sdk/types.js";
import { logRecordsSince, logSinkMark } from "../log-records.js";

/** A health table that records what it was told. */
function recordingHealth(): ProbeHealth & {
  resolved: string[];
  faults: conversationv1.SessionFault[];
} {
  const resolved: string[] = [];
  const faults: conversationv1.SessionFault[] = [];
  return {
    resolved,
    faults,
    resolveComponent: (component: string) => {
      resolved.push(component);
      return true;
    },
    fault: (fault: conversationv1.SessionFault) => {
      faults.push(fault);
    },
  };
}

const ACTIVE = {} as QueryLike;

/** The session closed no query. */
const NEVER_CLOSED = (): boolean => false;

describe("probeVendorFact", () => {
  it("hands the answer on and lifts the component's fault", async () => {
    // Arrange.
    const health = recordingHealth();
    const taken: string[] = [];

    // Act.
    await probeVendorFact(health, ACTIVE, {
      component: "vendor-thing",
      call: "readThing",
      read: async () => "the answer",
      take: (answer) => taken.push(answer),
    }, NEVER_CLOSED);

    // Assert.
    expect(taken).toEqual(["the answer"]);
    expect(health.resolved).toEqual(["vendor-thing"]);
    expect(health.faults).toEqual([]);
  });

  it("states a refused read as a vendorQueryFailed fault naming the call", async () => {
    // Arrange.
    const health = recordingHealth();
    const taken: string[] = [];

    // Act.
    await probeVendorFact(health, ACTIVE, {
      component: "vendor-thing",
      call: "readThing",
      read: async (): Promise<string> => {
        throw new Error("the vendor is busy");
      },
      take: (answer) => taken.push(answer),
    }, NEVER_CLOSED);

    // Assert.
    expect(taken).toEqual([]);
    expect(health.resolved).toEqual([]);
    expect(health.faults.map((fault) => [fault.component, fault.kind.case, fault.detail])).toEqual([
      ["vendor-thing", "vendorQueryFailed", "readThing failed: the vendor is busy"],
    ]);
  });

  it("carries a bare-string rejection into the fault whole", async () => {
    // Arrange.
    const health = recordingHealth();

    // Act.
    await probeVendorFact(health, ACTIVE, {
      component: "vendor-thing",
      call: "readThing",
      read: async (): Promise<string> => {
        throw "the endpoint went away";
      },
      take: () => undefined,
    }, NEVER_CLOSED);

    // Assert.
    expect(health.faults.map((fault) => fault.detail)).toEqual(["readThing failed: the endpoint went away"]);
  });

  it("states an answer that cannot be handed on as the read failing", async () => {
    // Arrange.
    const health = recordingHealth();

    // Act.
    await probeVendorFact(health, ACTIVE, {
      component: "vendor-thing",
      call: "readThing",
      read: async () => "malformed",
      take: () => {
        throw new Error("could not convert");
      },
    }, NEVER_CLOSED);

    // Assert.
    expect(health.resolved).toEqual([]);
    expect(health.faults.map((fault) => fault.detail)).toEqual(["readThing failed: could not convert"]);
  });

  it("reads nothing and states nothing with no query", async () => {
    // Arrange.
    const health = recordingHealth();
    let reads = 0;

    // Act.
    await probeVendorFact(health, undefined, {
      component: "vendor-thing",
      call: "readThing",
      read: async () => {
        reads += 1;
        return "never";
      },
      take: () => undefined,
    }, NEVER_CLOSED);

    // Assert.
    expect(reads).toBe(0);
    expect(health.resolved).toEqual([]);
    expect(health.faults).toEqual([]);
  });

  it("raises no fault for a read cut short by the session closing its own query", async () => {
    // Arrange.
    const health = recordingHealth();
    const before = logSinkMark();

    // Act.
    await probeVendorFact(
      health,
      ACTIVE,
      {
        component: "vendor-thing",
        call: "readThing",
        read: async (): Promise<string> => {
          throw new Error("Query closed before response received");
        },
        take: () => undefined,
      },
      (query) => query === ACTIVE,
    );

    // Assert.
    expect(health.faults).toEqual([]);
    expect(health.resolved).toEqual([]);
    expect(
      logRecordsSince(before)
        .filter((record) => String(record.message).includes("cut short by the session closing its own query"))
        .map((record) => [record.level, record.context.call, record.context.cause]),
    ).toEqual([["info", "readThing", "Query closed before response received"]]);
  });

  it("still faults a read whose query the session did not close", async () => {
    // Arrange.
    const health = recordingHealth();
    const other = {} as QueryLike;

    // Act.
    await probeVendorFact(
      health,
      ACTIVE,
      {
        component: "vendor-thing",
        call: "readThing",
        read: async (): Promise<string> => {
          throw new Error("Query closed before response received");
        },
        take: () => undefined,
      },
      (query) => query === other,
    );

    // Assert.
    expect(health.faults.map((fault) => fault.detail)).toEqual([
      "readThing failed: Query closed before response received",
    ]);
  });

  it("is told of every close of the session's main query", () => {
    // A close that skips `closeQuery` leaves a read it cuts short reading as a vendor fault.
    const source = readFileSync(path.join(import.meta.dirname, "../../src/engine/session.ts"), "utf8");
    expect(source).not.toMatch(/\b(previous|orphan|active)\??\.close\(\)/);
    expect(source.match(/closeQuery\((previous|orphan|active)\)/g)?.length).toBe(3);
  });

  it("is the one read every pulled-fact probe of the session goes through", () => {
    // A probe that spells the contract itself is a second copy free to drift.
    const source = readFileSync(path.join(import.meta.dirname, "../../src/engine/session.ts"), "utf8");
    for (const probe of ["probeContextUsage", "pushModelCatalog", "pushMcpServerStatus", "pushCommandCatalog"]) {
      expect(source).toMatch(new RegExp(`async function ${probe}\\(\\): Promise<void> \\{\\n\\s+await probeVendorFact\\(`));
    }
  });
});
