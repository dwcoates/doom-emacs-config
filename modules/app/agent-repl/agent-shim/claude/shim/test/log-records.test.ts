/**
 * The one decoder of the canonical logger's records out of the mocked sink.
 *
 * WHAT THIS GUARDS: that a record the REAL logger wrote comes back whole, that
 * a window reads only what was written inside it, and that no suite grows its
 * own copy of the decode again — copies drift, and a drifted copy asserts on a
 * record shape the logger no longer writes.
 */
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { bindLog } from "../src/log.js";
import { containing } from "./expect-shapes.js";
import { logRecordsDuring, logRecordsSince, logSinkMark } from "./log-records.js";

const TEST_DIR = fileURLToPath(new URL(".", import.meta.url));

/** The hand-rolled decode this module replaces, split so this file does not carry it. */
const HAND_ROLLED_DECODE = ["subarray(offset,", "offset + length)"].join(" ");

/** Every `.ts` file under `test/`, relative to it, with `/` separators. */
function testSources(): string[] {
  return readdirSync(TEST_DIR, { recursive: true, encoding: "utf8" })
    .filter((file) => file.endsWith(".ts"))
    .map((file) => file.split(path.sep).join("/"));
}

/** Whether `file` (relative to `test/`) carries the hand-rolled decode. */
function carriesDecode(file: string): boolean {
  return readFileSync(path.join(TEST_DIR, file), "utf8").includes(HAND_ROLLED_DECODE);
}

/**
 * Files allowed to carry the decode, each with the reason it is not a copy.
 * Paths are relative to `test/`.
 */
const EXEMPT: ReadonlyMap<string, string> = new Map([
  ["log-records.ts", "it is the one shared decoder"],
  ["log.test.ts", "it asserts the sink's raw short-write byte slices, which are not whole records"],
]);

describe("logRecordsSince", () => {
  it.each([
    { field: "level", want: "warn" },
    { field: "message", want: "a record to decode" },
    { field: "operation", want: "shim.test.log-records" },
    { field: "context", want: containing({ probe: "value" }) },
  ])("decodes the $field of a record the real logger wrote", ({ field, want }) => {
    // Arrange.
    const before = logSinkMark();

    // Act.
    bindLog({ operation: "shim.test.log-records" }).warn({ probe: "value" }, "a record to decode");

    // Assert.
    expect(logRecordsSince(before).map((record) => record[field])).toEqual([want]);
  });

  it("reads nothing when nothing was written since the mark", () => {
    // Arrange.
    bindLog({ operation: "shim.test.log-records" }).debug({}, "written before the mark");

    // Act.
    const records = logRecordsSince(logSinkMark());

    // Assert.
    expect(records).toEqual([]);
  });
});

describe("logRecordsDuring", () => {
  it("returns only the records written while act ran", () => {
    // Arrange.
    const log = bindLog({ operation: "shim.test.log-records" });
    log.debug({}, "outside the window");

    // Act.
    const records = logRecordsDuring(() => log.debug({}, "inside the window"));

    // Assert.
    expect(records.map((record) => record.message)).toEqual(["inside the window"]);
  });
});

describe("the shared decoder is the only decoder", () => {
  it("finds the hand-rolled decode in no test file but the exempt ones", () => {
    // Arrange.
    const files = testSources();

    // Act.
    const copies = files.filter((file) => !EXEMPT.has(file) && carriesDecode(file));

    // Assert.
    expect(copies).toEqual([]);
  });

  it("sees the shared decoder's own decode, so the scan cannot pass vacuously", () => {
    // Arrange.
    const files = testSources();

    // Act.
    const found = files.filter((file) => file === "log-records.ts" && carriesDecode(file));

    // Assert.
    expect(found).toEqual(["log-records.ts"]);
  });
});
