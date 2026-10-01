import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { openingInstantMs, placeRecordEntries, recordTimestampMs, sdkRecordTimestampMs } from "../../src/convert/place.js";
import { conversationv1 } from "../../src/proto.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import type { PersistEntry } from "../../src/store/persistence.js";

const AT = "2026-09-30T12:00:00.000Z";
const AT_MS = Date.parse(AT);

function record(timestamp?: unknown): SdkMessage {
  return { type: "assistant", uuid: "u-1", ...(timestamp === undefined ? {} : { timestamp }) } as unknown as SdkMessage;
}

function frameEntry(frame: conversationv1.AgentFrame): PersistEntry {
  return {
    agentId: create(conversationv1.AgentIdSchema, { value: "main" }),
    upsertKey: "k",
    source: { vendorUuid: "u-1", discriminator: "d" },
    keepalive: false,
    turn: undefined,
    item: { kind: "frame", frame },
  };
}

const startedAt = (ms: number): conversationv1.AgentActivityStartedAt =>
  create(conversationv1.AgentActivityStartedAtSchema, { atMs: BigInt(ms) });

describe("recordTimestampMs", () => {
  it.each([
    ["a parsable timestamp", AT, AT_MS],
    ["no timestamp", undefined, undefined],
    ["an unparsable timestamp", "not a time", undefined],
    ["an empty timestamp", "", undefined],
    ["a non-string timestamp", 42, undefined],
  ])("reads %s", (_name, timestamp, want) => {
    expect(recordTimestampMs(record(timestamp))).toBe(want);
  });
});

describe("openingInstantMs", () => {
  it("answers the earliest start instant the value states", () => {
    expect(openingInstantMs({ a: startedAt(5_000), b: [startedAt(3_000), { c: startedAt(4_000) }] })).toBe(3_000);
  });

  it("answers 0 for a value that states no start", () => {
    expect(openingInstantMs({ a: { b: "c" } })).toBe(0);
  });

  it("never walks a residue's verbatim record", () => {
    expect(openingInstantMs({ $typeName: "google.protobuf.Struct", hidden: startedAt(1_000) })).toBe(0);
  });
});

describe("placeRecordEntries", () => {
  it("places each entry at the record's timestamp, ranked by its index", () => {
    const entries = [frameEntry(create(conversationv1.AgentFrameSchema, {})), frameEntry(create(conversationv1.AgentFrameSchema, {}))];

    const placed = placeRecordEntries(record(AT), entries);

    expect(placed.map((entry) => entry.recordPlace)).toEqual([
      { atMs: AT_MS, ordinal: 0 },
      { atMs: AT_MS, ordinal: 1 },
    ]);
  });

  it("places an entry that states its unit's start at that start", () => {
    const entry = frameEntry(create(conversationv1.AgentFrameSchema, {}));
    const withStart = { ...entry, item: { kind: "frame" as const, frame: entry.item.kind === "frame" ? entry.item.frame : create(conversationv1.AgentFrameSchema, {}) } };
    (withStart.item.frame as unknown as { started: unknown }).started = startedAt(AT_MS - 500);

    const placed = placeRecordEntries(record(AT), [withStart]);

    expect(placed[0]?.recordPlace).toEqual({ atMs: AT_MS - 500, ordinal: 0 });
  });

  it("answers the very array it was given for a record with no timestamp", () => {
    const entries = [frameEntry(create(conversationv1.AgentFrameSchema, {}))];

    expect(placeRecordEntries(record(), entries)).toBe(entries);
  });

  it("answers the very array it was given when there is nothing to place", () => {
    const entries: PersistEntry[] = [];

    expect(placeRecordEntries(record(AT), entries)).toBe(entries);
  });
});

describe("sdkRecordTimestampMs", () => {
  it.each([
    ["a parsable timestamp", AT, AT_MS],
    ["the epoch itself, which a settle instant still reads", "1970-01-01T00:00:00.000Z", 0],
    ["no timestamp", undefined, undefined],
    ["an unparsable timestamp", "not a time", undefined],
  ])("reads %s", (_name, timestamp, want) => {
    expect(sdkRecordTimestampMs(record(timestamp))).toBe(want);
  });

  it("is the one read of an SDK record's timestamp in the converters", async () => {
    // A converter reading `timestamp` off a record by hand would drift from
    // the place and the settle instant this helper gives both.
    const { readdirSync, readFileSync } = await import("node:fs");
    const dir = new URL("../../src/convert/", import.meta.url);
    const handRolled = readdirSync(dir)
      .filter((name) => name.endsWith(".ts") && name !== "place.ts")
      .filter((name) => readFileSync(new URL(name, dir), "utf8").includes("readonly timestamp?: unknown"));
    expect(handRolled).toEqual([]);
  });
});
