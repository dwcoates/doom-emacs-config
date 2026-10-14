import { describe, expect, it } from "vitest";
import { readFileSync, readdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { FooterStatusSchema } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { RosterRowSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { InterruptErrorSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { oneofArms } from "../../src/rpc/oneof.js";
import { oneofArms as suiteOneofArms } from "../arms.js";
import { FOOTER_STATUS_CASES } from "../../src/footer/tones.js";
import { ROSTER_STATUS_CASES } from "../../src/sidebar/tones.js";
import { INTERRUPT_ERROR_ARMS } from "../../src/interrupt-error.js";
import { ROSTER_STATUS_ARMS } from "../integration/fixtures.js";
import { captureLogRecords } from "../log-capture.js";

describe("oneofArms", () => {
  it("answers the oneof's arms in the generated spelling, in declaration order", () => {
    // Arrange
    const declared = RosterRowSchema.oneofs.find((o) => o.name === "status")?.fields.map((f) => f.localName);

    // Act
    const arms = oneofArms(RosterRowSchema, "status");

    // Assert
    expect(arms).toEqual(declared);
  });

  it("spells an arm lowerCamel, as a received message carries it", () => {
    // Act
    const arms = oneofArms(RosterRowSchema, "status");

    // Assert
    expect([arms.includes("idleAsync"), arms.includes("idle_async")]).toEqual([true, false]);
  });

  it("refuses a oneof the schema does not declare", () => {
    // Act / Assert
    expect(() => oneofArms(RosterRowSchema, "teleport")).toThrow("frontend.v1.RosterRow has no oneof named teleport");
  });

  it("logs the missing oneof as an error with the schema and the name", async () => {
    // Arrange
    const capture = captureLogRecords("debug");

    // Act
    expect(() => oneofArms(RosterRowSchema, "teleport")).toThrow();
    capture.logger.flush();
    await Promise.resolve();

    // Assert
    const record = capture.sent.find((r) => r.operation === "rpc.oneof.missing");
    expect(record?.level.case).toBe("error");
    expect(record?.context).toMatchObject({ schema: "frontend.v1.RosterRow", oneof: "teleport" });
  });
});

// EVERY CALL SITE READS ITS ARMS THROUGH THE ONE READER: a site that typed a
// list out by hand, or read the schema its own way, fails here.
describe("the arm lists that read off a schema", () => {
  it.each([
    ["the footer's status arms", FOOTER_STATUS_CASES, FooterStatusSchema, "status"],
    ["the sidebar's status arms", ROSTER_STATUS_CASES, RosterRowSchema, "status"],
    ["the interrupt refusal's arms", INTERRUPT_ERROR_ARMS, InterruptErrorSchema, "kind"],
  ] as const)("%s are the reader's", (_name, list, schema, oneof) => {
    expect(list).toEqual(oneofArms(schema, oneof));
  });

  it("gives the suites the source's own reader", () => {
    expect(suiteOneofArms).toBe(oneofArms);
  });

  it("gives the integration fixtures the sidebar's own list", () => {
    expect(ROSTER_STATUS_ARMS).toBe(ROSTER_STATUS_CASES);
  });
});

describe("the source reads a oneof's arms in one place", () => {
  it("finds no oneof lookup outside the reader", () => {
    // Arrange: every source file under src, read synchronously. Awaiting each
    // of some two hundred reads in turn queued every one behind the busy gate
    // run's thread pool and outran the suite's timeout.
    const src = join(dirname(fileURLToPath(import.meta.url)), "../../src");
    const files = readdirSync(src, { recursive: true, encoding: "utf8" }).filter((f) => f.endsWith(".ts"));

    // Act
    const lookups = files.filter((f) => readFileSync(join(src, f), "utf8").includes(".oneofs.find("));

    // Assert: only the reader's own module.
    expect(lookups).toEqual(["rpc/oneof.ts"]);
  });
});
