import { describe, expect, it } from "vitest";
import { guardMalformed, guardMalformedNow, type GuardContext } from "../../src/rpc/guard.js";
import { captureLogRecords } from "../log-capture.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { recordingFailures } from "../failure-sink.js";

/** Each guard, run over work that resolves with nothing or throws ERR. */
const guards: Array<[string, (ctx: GuardContext, err: unknown) => Promise<boolean>]> = [
  ["guardMalformed", (ctx, err) => guardMalformed(ctx, "op", err === undefined ? Promise.resolve() : Promise.reject(err))],
  [
    "guardMalformedNow",
    (ctx, err) =>
      Promise.resolve().then(() =>
        guardMalformedNow(ctx, "op", () => {
          if (err !== undefined) throw err;
        }),
      ),
  ],
];

const malformed = (): MalformedView => new MalformedView("A.b", "the oneof sets no arm");

describe.each(guards)("%s", (_name, guard) => {
  it("answers false for work that completes", async () => {
    await expect(guard({ failures: recordingFailures() }, undefined)).resolves.toBe(false);
  });

  it("files nothing for work that completes", async () => {
    const sink = recordingFailures();
    await guard({ failures: sink }, undefined);
    expect(sink.reports).toHaveLength(0);
  });

  it("answers true for work that refused as malformed", async () => {
    await expect(guard({ failures: recordingFailures() }, malformed())).resolves.toBe(true);
  });

  it("files a frame_undecodable card for a malformed answer", async () => {
    const sink = recordingFailures();
    await guard({ failures: sink }, malformed());
    expect(sink.reports[0]?.kind.case).toBe("frameUndecodable");
  });

  it("carries the refusal's detail as the card's cause", async () => {
    const sink = recordingFailures();
    await guard({ failures: sink }, malformed());
    const kind = sink.reports[0]?.kind;
    expect(kind?.case === "frameUndecodable" ? kind.value.cause : undefined).toBe("the oneof sets no arm");
  });

  it("carries the refusal's path as the card's frame head", async () => {
    const sink = recordingFailures();
    await guard({ failures: sink }, malformed());
    const kind = sink.reports[0]?.kind;
    expect(kind?.case === "frameUndecodable" ? kind.value.frameHead : undefined).toBe("A.b");
  });

  it("logs the refusal at error under the owning layer's operation", async () => {
    const capture = captureLogRecords("debug");
    await guard({ failures: recordingFailures() }, malformed());
    capture.logger.flush();
    await Promise.resolve();
    const record = capture.sent.find((r) => r.operation === "op-undecodable");
    expect([record?.level.case, record?.context]).toEqual([
      "error",
      expect.objectContaining({ path: "A.b", cause: "the oneof sets no arm" }),
    ]);
  });

  it("lets an error that is not a malformed view travel on", async () => {
    await expect(guard({ failures: recordingFailures() }, new Error("no daemon"))).rejects.toThrow("no daemon");
  });

  it("files nothing for an error that is not a malformed view", async () => {
    const sink = recordingFailures();
    await expect(guard({ failures: sink }, new Error("no daemon"))).rejects.toThrow();
    expect(sink.reports).toHaveLength(0);
  });
});
