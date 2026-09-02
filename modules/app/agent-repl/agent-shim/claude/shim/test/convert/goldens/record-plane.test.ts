/**
 * The RECORD PLANE over a real capture: which row each frame replaces, and
 * whether re-sending the same capture mints the same write ids.
 *
 * The conversion goldens assert what the fold SAYS. This file asserts the two
 * facts the store depends on and nothing else can check as cheaply: every frame
 * of one unit carries ONE upsert key, and a write id is a pure function of the
 * producer, the vendor's coordinates and the frame's arm — so a shim that
 * re-sends a batch after an outage duplicates nothing.
 */
import { describe, expect, it } from "vitest";
import { producerId } from "../../../src/store/keys.js";
import { entryWriteId } from "../../../src/store/writer.js";
import type { PersistEntry } from "../../../src/store/persistence.js";
import { activityOf, foldScenario } from "./harness.js";

/** One scenario, folded twice, is the whole subject here. */
const SCENARIO = "read-whole-head-range";
const PRODUCER = producerId("f89f093c-8390-4e9e-8d76-2a92519c7cb9");

/** Every write id a run would mint, in order. */
function writeIds(): string[] {
  return foldScenario(SCENARIO).entries.map((entry) => entryWriteId(PRODUCER, entry));
}

describe("upsert keys over a real capture", () => {
  it("names every row it writes", () => {
    for (const entry of foldScenario(SCENARIO).entries) {
      expect(entry.upsertKey).not.toBe("");
    }
  });

  it("keys an activity row by its unit and nothing else", () => {
    for (const entry of foldScenario(SCENARIO).entries) {
      if (activityOf(entry) === undefined) continue;
      expect(entry.upsertKey).toMatch(/^activity:/);
    }
  });

  it("gives one unit's start and terminal the SAME key", () => {
    const run = foldScenario(SCENARIO);
    const started = run.entries
      .filter((entry) => entry.source.discriminator === "activity.read.start")
      .map((entry) => entry.upsertKey)
      .sort();
    const settled = run.entries
      .filter((entry) => entry.source.discriminator === "activity.read.success")
      .map((entry) => entry.upsertKey)
      .sort();
    expect(settled).toEqual(started);
  });

  it("gives two different units different keys", () => {
    const keys = foldScenario(SCENARIO)
      .entries.filter((entry) => entry.source.discriminator === "activity.read.start")
      .map((entry) => entry.upsertKey);
    expect(new Set(keys).size).toBe(keys.length);
  });

  it("files a session fact under a session key, never an activity one", () => {
    for (const entry of foldScenario(SCENARIO).entries) {
      if (entry.item.kind !== "session_update") continue;
      expect(entry.upsertKey).toMatch(/^session:/);
    }
  });
});

describe("write ids over a real capture", () => {
  it("mints the same id for the same fold, twice", () => {
    expect(writeIds()).toEqual(writeIds());
  });

  it("mints a distinct id for every row of the capture", () => {
    const ids = writeIds();
    expect(new Set(ids).size).toBe(ids.length);
  });

  it("separates two arms derived from ONE vendor record", () => {
    const [entry] = foldScenario(SCENARIO).entries;
    expect(entry).toBeDefined();
    const one = { ...(entry as PersistEntry) };
    const other = {
      ...one,
      source: { ...one.source, discriminator: `${one.source.discriminator}.other` },
    };
    expect(entryWriteId(PRODUCER, one)).not.toBe(entryWriteId(PRODUCER, other));
  });

  it("separates two producers writing the same record", () => {
    const [entry] = foldScenario(SCENARIO).entries;
    expect(entryWriteId(PRODUCER, entry as PersistEntry)).not.toBe(
      entryWriteId(producerId("another-vendor-session"), entry as PersistEntry),
    );
  });
});
