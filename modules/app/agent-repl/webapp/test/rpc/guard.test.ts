import { describe, expect, it } from "vitest";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import type { FailureSink } from "../../src/failure/sink.js";
import { guardMalformed } from "../../src/rpc/guard.js";
import { MalformedView } from "../../src/rpc/malformed.js";

/** A sink that keeps what was filed, so a test can read the arm and its facts. */
function recordingSink(): FailureSink & { reports: FailureKind[] } {
  const reports: FailureKind[] = [];
  return { reports, report: (kind) => reports.push(kind), retract: () => undefined };
}

describe("guardMalformed", () => {
  it("answers false for work that resolves", async () => {
    const ctx = { failures: recordingSink() };
    await expect(guardMalformed(ctx, "op", Promise.resolve("done"))).resolves.toBe(false);
  });

  it("files nothing for work that resolves", async () => {
    const sink = recordingSink();
    await guardMalformed({ failures: sink }, "op", Promise.resolve());
    expect(sink.reports).toHaveLength(0);
  });

  it("answers true for work that refused as malformed", async () => {
    const ctx = { failures: recordingSink() };
    const work = Promise.reject(new MalformedView("A.b", "the oneof sets no arm"));
    await expect(guardMalformed(ctx, "op", work)).resolves.toBe(true);
  });

  it("files a frame_undecodable card for a malformed answer", async () => {
    const sink = recordingSink();
    const work = Promise.reject(new MalformedView("A.b", "the oneof sets no arm"));
    await guardMalformed({ failures: sink }, "op", work);
    expect(sink.reports[0]?.kind.case).toBe("frameUndecodable");
  });

  it("carries the refusal's detail as the card's cause", async () => {
    const sink = recordingSink();
    const work = Promise.reject(new MalformedView("A.b", "the oneof sets no arm"));
    await guardMalformed({ failures: sink }, "op", work);
    const kind = sink.reports[0]?.kind;
    expect(kind?.case === "frameUndecodable" ? kind.value.cause : undefined).toBe(
      "the oneof sets no arm",
    );
  });

  it("carries the refusal's path as the card's frame head", async () => {
    const sink = recordingSink();
    const work = Promise.reject(new MalformedView("A.b", "the oneof sets no arm"));
    await guardMalformed({ failures: sink }, "op", work);
    const kind = sink.reports[0]?.kind;
    expect(kind?.case === "frameUndecodable" ? kind.value.frameHead : undefined).toBe("A.b");
  });

  it("lets an error that is not a malformed view travel on", async () => {
    const ctx = { failures: recordingSink() };
    const work = Promise.reject(new Error("no daemon"));
    await expect(guardMalformed(ctx, "op", work)).rejects.toThrow("no daemon");
  });

  it("files nothing for an error that is not a malformed view", async () => {
    const sink = recordingSink();
    const work = Promise.reject(new Error("no daemon"));
    await expect(guardMalformed({ failures: sink }, "op", work)).rejects.toThrow();
    expect(sink.reports).toHaveLength(0);
  });
});
