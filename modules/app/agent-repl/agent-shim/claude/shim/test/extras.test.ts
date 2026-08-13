import { beforeEach, describe, expect, it } from "vitest";
import { fromBinary, toBinary } from "@bufbuild/protobuf";
import {
  __resetExtrasSeen,
  MissingFieldError,
  PRODUCER,
  Reader,
  RAW_CAP_BYTES,
  STREAM_SOURCE,
  logUnknownDiscriminator,
  unknownEntry,
  unknownFieldsEntry,
  unparsedEntry,
  vendorSpecificEntry,
} from "../src/proto/extras.js";
import { EntrySchema } from "../src/uds/proto.js";

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

  it("consumeAll() leaves nothing for the leftover set to collect", () => {
    const r = new Reader({ a: 1, b: 2 });
    r.consumeAll();
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

  it("rejects a present-but-blank optional id rather than reading it as absent", () => {
    const r = new Reader({ request_id: "" });
    expect(() => r.optionalNonBlankStr("request_id")).toThrow(MissingFieldError);
  });
});

// ---------------------------------------------------------------------------
// logUnknownDiscriminator: once per distinct family.
// ---------------------------------------------------------------------------

describe("logUnknownDiscriminator", () => {
  it("reports the first sighting of a discriminator as newly logged", () => {
    expect(logUnknownDiscriminator("brand_new", "")).toBe(true);
  });

  it("does not re-report a discriminator already logged this process", () => {
    logUnknownDiscriminator("brand_new", "");
    expect(logUnknownDiscriminator("brand_new", "")).toBe(false);
  });

  it("keys a nested subtype separately from a top-level family of the same name", () => {
    logUnknownDiscriminator("thing", "");
    expect(logUnknownDiscriminator("thing", "system")).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// The unconverted arms: every one is internal-only, by shape.
// ---------------------------------------------------------------------------

describe("vendorSpecificEntry", () => {
  it("carries the vendor's own name for the record", () => {
    const entry = vendorSpecificEntry("system:status", { status: "ok" });
    expect(entry.internal?.unconverted.case).toBe("vendorSpecific");
    if (entry.internal?.unconverted.case !== "vendorSpecific") throw new Error("case");
    expect(entry.internal.unconverted.value.kind).toBe("system:status");
  });

  it("preserves the record whole and verbatim", () => {
    const entry = vendorSpecificEntry("system:status", { status: "ok", nested: { n: 1 } });
    if (entry.internal?.unconverted.case !== "vendorSpecific") throw new Error("case");
    expect(entry.internal.unconverted.value.raw).toEqual({ status: "ok", nested: { n: 1 } });
  });

  it("has NO external half, so it has no path to the daemon", () => {
    expect(vendorSpecificEntry("system:status", {}).external).toBeUndefined();
  });

  it("records the stream plane, because this shim observes no file", () => {
    expect(vendorSpecificEntry("system:status", {}).internal?.plane?.plane.case).toBe("stream");
  });

  it("leaves write_id empty for the store client to mint once", () => {
    expect(vendorSpecificEntry("system:status", {}).internal?.writeId).toBe("");
  });
});

describe("unknownFieldsEntry", () => {
  it("names the family whose unknown fields it carries", () => {
    const entry = unknownFieldsEntry("result", { surprise: 1 });
    if (entry.internal?.unconverted.case !== "vendorSpecific") throw new Error("case");
    expect(entry.internal.unconverted.value.kind).toBe("result.unknown-fields");
  });

  it("carries the leftover fields verbatim", () => {
    const entry = unknownFieldsEntry("result", { surprise: 1 });
    if (entry.internal?.unconverted.case !== "vendorSpecific") throw new Error("case");
    expect(entry.internal.unconverted.value.raw).toEqual({ surprise: 1 });
  });
});

describe("unknownEntry", () => {
  it("carries the discriminator no arm matched", () => {
    const entry = unknownEntry("brand_new", "type", { type: "brand_new" });
    if (entry.internal?.unconverted.case !== "unknown") throw new Error("case");
    expect(entry.internal.unconverted.value.discriminator).toBe("brand_new");
  });

  it("names WHERE the discriminator was read from", () => {
    const entry = unknownEntry("brand_new", "subtype", { subtype: "brand_new" });
    if (entry.internal?.unconverted.case !== "unknown") throw new Error("case");
    expect(entry.internal.unconverted.value.discriminatorField).toBe("subtype");
  });

  it("has NO external half", () => {
    expect(unknownEntry("brand_new", "type", {}).external).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// unparsedEntry: the read-failure arm.
// ---------------------------------------------------------------------------

describe("unparsedEntry", () => {
  it("wraps the raw text and the parse error", () => {
    const entry = unparsedEntry("{\"broken\":true}", "boom", { sessionId: "s1", log: false });
    if (entry.internal?.unconverted.case !== "unparsed") throw new Error("case");
    expect(entry.internal.unconverted.value.parseError).toBe("boom");
    expect(entry.internal.unconverted.value.raw).toBe("{\"broken\":true}");
  });

  it("defaults its source to the SDK stream this shim reads", () => {
    const entry = unparsedEntry("{bad", "boom", { log: false });
    if (entry.internal?.unconverted.case !== "unparsed") throw new Error("case");
    expect(entry.internal.unconverted.value.source).toBe(STREAM_SOURCE);
  });

  it("caps raw at 64 KiB", () => {
    const huge = "x".repeat(RAW_CAP_BYTES + 5000);
    const entry = unparsedEntry(huge, "too big", { log: false });
    if (entry.internal?.unconverted.case !== "unparsed") throw new Error("case");
    expect(entry.internal.unconverted.value.raw.length).toBe(RAW_CAP_BYTES);
  });

  it("never splits a multi-byte code point when it caps", () => {
    // Arrange: a 3-byte character straddling the cap boundary.
    const head = "x".repeat(RAW_CAP_BYTES - 1);
    const raw = `${head}${"中".repeat(10)}`;

    // Act.
    const entry = unparsedEntry(raw, "too big", { log: false });

    // Assert: the partial sequence is dropped rather than replaced by U+FFFD.
    if (entry.internal?.unconverted.case !== "unparsed") throw new Error("case");
    expect(entry.internal.unconverted.value.raw).toBe(head);
  });

  it("has NO external half, so an unreadable record cannot reach a page", () => {
    expect(unparsedEntry("{bad", "boom", { log: false }).external).toBeUndefined();
  });

  it("round-trips through protobuf binary", () => {
    const entry = unparsedEntry("raw", "err", { log: false });
    const decoded = fromBinary(EntrySchema, toBinary(EntrySchema, entry));
    if (decoded.internal?.unconverted.case !== "unparsed") throw new Error("case");
    expect(decoded.internal.unconverted.value.parseError).toBe("err");
  });
});

describe("MissingFieldError", () => {
  it("is an Error subclass carrying the given message", () => {
    const e = new MissingFieldError("nope");
    expect(e).toBeInstanceOf(Error);
    expect(e.message).toBe("nope");
  });
});

describe("PRODUCER", () => {
  it("identifies this shim on every record it produces", () => {
    expect(PRODUCER).toBe("claude-shim");
  });
});
