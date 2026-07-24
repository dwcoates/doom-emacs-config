import { beforeEach, describe, expect, it } from "vitest";
import { fromBinary, toBinary } from "@bufbuild/protobuf";
import {
  __resetExtrasSeen,
  MissingFieldError,
  Reader,
  RAW_CAP_BYTES,
  unparsedEvent,
} from "../src/proto/extras.js";
import { UnparsedEventSchema } from "../src/uds/proto.js";

beforeEach(() => __resetExtrasSeen());

// ---------------------------------------------------------------------------
// Reader: unknown-field capture.
// ---------------------------------------------------------------------------

describe("Reader unknown-field capture", () => {
  it("captures an unconsumed top-level field into extras", () => {
    const r = new Reader({ known: "a", surprise: 7 });
    r.str("known");
    const out = r.finish("demo");
    expect(out.extras).toEqual({ surprise: 7 });
  });

  it("loud-logs a newly-seen unknown field once", () => {
    const r = new Reader({ surprise: 1 });
    expect(r.finish("demo").logged).toEqual(["demo.surprise"]);
  });

  it("does NOT re-log a field path already seen this process", () => {
    new Reader({ surprise: 1 }).finish("demo");
    const second = new Reader({ surprise: 2 }).finish("demo");
    expect(second.logged).toEqual([]);
    expect(second.extras).toEqual({ surprise: 2 }); // still captured, never dropped
  });

  it("consumed fields never reach extras", () => {
    const r = new Reader({ a: 1, b: 2 });
    r.num("a");
    r.num("b");
    expect(r.finish("demo").extras).toBeUndefined();
  });

  it("carry() preserves a recognized field into extras WITHOUT logging", () => {
    const r = new Reader({ ttft_ms: 865 });
    r.carry("ttft_ms");
    const out = r.finish("stream_event");
    expect(out.extras).toEqual({ ttft_ms: 865 });
    expect(out.logged).toEqual([]);
  });

  it("ignore() drops a structural key from both extras and logs", () => {
    const r = new Reader({ type: "x" });
    r.ignore("type");
    const out = r.finish("demo");
    expect(out.extras).toBeUndefined();
    expect(out.logged).toEqual([]);
  });

  it("reads camelCase or snake_case aliases interchangeably", () => {
    const r = new Reader({ sessionId: "cc" });
    expect(r.str("session_id", "sessionId")).toBe("cc");
    expect(r.finish("demo").extras).toBeUndefined();
  });

  it("coerces a wrong-typed value to the getter's zero (no throw)", () => {
    const r = new Reader({ n: "not-a-number" });
    expect(r.num("n")).toBe(0);
    expect(r.big("n")).toBe(0n);
  });
});

// ---------------------------------------------------------------------------
// unparsedEvent: missing-expected hard-error path.
// ---------------------------------------------------------------------------

describe("unparsedEvent", () => {
  it("wraps the raw bytes, error, and producer", () => {
    const evt = unparsedEvent("{\"broken\":true}", "boom", { sessionId: "s1" });
    expect(evt.payload.case).toBe("unparsed");
    if (evt.payload.case !== "unparsed") throw new Error("case");
    expect(evt.payload.value.error).toBe("boom");
    expect(evt.payload.value.producer).toBe("claude-shim");
    expect(evt.sessionId).toBe("s1");
    expect(new TextDecoder().decode(evt.payload.value.raw)).toBe("{\"broken\":true}");
  });

  it("caps raw bytes at 64 KiB", () => {
    const huge = "x".repeat(RAW_CAP_BYTES + 5000);
    const evt = unparsedEvent(huge, "too big");
    if (evt.payload.case !== "unparsed") throw new Error("case");
    expect(evt.payload.value.raw.length).toBe(RAW_CAP_BYTES);
  });

  it("round-trips through protobuf binary", () => {
    const evt = unparsedEvent("raw", "err");
    if (evt.payload.case !== "unparsed") throw new Error("case");
    const decoded = fromBinary(UnparsedEventSchema, toBinary(UnparsedEventSchema, evt.payload.value));
    expect(decoded.error).toBe("err");
  });

  it("MissingFieldError is an Error subclass with the given message", () => {
    const e = new MissingFieldError("nope");
    expect(e).toBeInstanceOf(Error);
    expect(e.message).toBe("nope");
  });
});
