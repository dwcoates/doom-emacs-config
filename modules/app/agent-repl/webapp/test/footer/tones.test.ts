// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import renderColors from "../../../proto/vocab/render-colors.json";
import {
  FooterAllowanceSchema,
  FooterStatusSchema,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { FOOTER_STATUS_ARMS, protoArmName } from "../../src/vocab.js";
import {
  FOOTER_ALLOWANCE_STATUS_CASES,
  FOOTER_STATUS_CASES,
  STATUS_ARM_CLASS,
  activityDatumClass,
  allowanceStatusClass,
  statusArmClass,
  type ActivityDatum,
} from "../../src/footer/tones.js";

/** Every `FooterStatus.status` arm, read off the generated schema. */
const SCHEMA_ARMS: readonly string[] = (
  FooterStatusSchema.oneofs.find((oneof) => oneof.name === "status")?.fields ?? []
).map((field) => field.localName);

describe("FOOTER_STATUS_CASES: the arm set is the schema's", () => {
  it("names every arm the proto declares", () => {
    expect([...FOOTER_STATUS_CASES].sort()).toEqual([...SCHEMA_ARMS].sort());
  });

  it("names the ten arms the contract carries", () => {
    expect([...FOOTER_STATUS_CASES].sort()).toEqual(
      [
        "background",
        "blocked",
        "closing",
        "disconnected",
        "idle",
        "interrupted",
        "loading",
        "merging",
        "thinking",
        "waiting",
      ].sort(),
    );
  });
});

describe("STATUS_ARM_CLASS: asserted row for row against render-colors.json", () => {
  it("has a row for every arm the proto declares", () => {
    expect(Object.keys(STATUS_ARM_CLASS).sort()).toEqual([...SCHEMA_ARMS].sort());
  });

  it("has an arm for every key the vocabulary file declares", () => {
    expect([...FOOTER_STATUS_ARMS].sort()).toEqual(
      SCHEMA_ARMS.map((arm) => protoArmName(arm)).sort(),
    );
  });

  const rows: ReadonlyArray<[string, string]> = Object.entries(
    renderColors.footer_status as Record<string, string>,
  );
  it.each(rows)("paints %s with the file's %s", (armName, color) => {
    const generated = SCHEMA_ARMS.find((arm) => protoArmName(arm) === armName);
    expect(generated).toBeDefined();
    expect(STATUS_ARM_CLASS[generated as string]).toBe(color);
  });
});

describe("statusArmClass", () => {
  it.each([
    ["thinking", "tone-red"],
    ["waiting", "tone-green"],
    ["idle", "tone-green"],
    ["interrupted", "tone-green"],
    // The footer speaks about ONE session, so a merge holding it is purple —
    // where a merging ROSTER row spends no color and reports itself by glyph.
    ["merging", "tone-purple"],
    ["background", "tone-yellow"],
    ["blocked", "tone-purple"],
    ["disconnected", "tone-blue"],
    ["closing", "tone-blue"],
    ["loading", "tone-red"],
  ])("gives %s the class %s", (arm, expected) => {
    expect(statusArmClass(arm)).toBe(expected);
  });

  it("refuses an arm the shared vocabulary has no color for", () => {
    expect(() => statusArmClass("hibernated")).toThrow(MalformedView);
  });
});

describe("activityDatumClass", () => {
  it.each<[ActivityDatum, string]>([
    // A sha NAMES something, so it takes the identity blue.
    ["sha", "tone-blue"],
    // Everything else is a figure the reader watches move.
    ["attempt", "tone-yellow"],
    ["count", "tone-yellow"],
    ["position", "tone-yellow"],
    ["percent", "tone-yellow"],
  ])("gives %s the class %s", (datum, expected) => {
    expect(activityDatumClass(datum)).toBe(expected);
  });
});

/** Every `FooterAllowance.status` arm, read off the generated schema. */
const ALLOWANCE_SCHEMA_ARMS: readonly string[] = (
  FooterAllowanceSchema.oneofs.find((oneof) => oneof.name === "status")?.fields ?? []
).map((field) => field.localName);

describe("FOOTER_ALLOWANCE_STATUS_CASES: the arm set is the schema's", () => {
  it("names every arm the proto declares", () => {
    expect([...FOOTER_ALLOWANCE_STATUS_CASES].sort()).toEqual([...ALLOWANCE_SCHEMA_ARMS].sort());
  });

  it("names the three arms the retired status string was replaced by", () => {
    expect([...FOOTER_ALLOWANCE_STATUS_CASES].sort()).toEqual(
      ["allowed", "allowedWarning", "rejected"].sort(),
    );
  });
});

describe("allowanceStatusClass", () => {
  it.each(ALLOWANCE_SCHEMA_ARMS)("gives %s a colour", (arm) => {
    expect(allowanceStatusClass(arm)).toMatch(/^tone-/);
  });

  it.each<[string, string]>([
    // The traffic-light reading the arms already are: headroom, the vendor's
    // own warning, and a call that would be refused.
    ["allowed", "tone-green"],
    ["allowedWarning", "tone-yellow"],
    ["rejected", "tone-red"],
  ])("gives %s the class %s", (arm, expected) => {
    expect(allowanceStatusClass(arm)).toBe(expected);
  });

  it("refuses an allowance arm this table has no colour for", () => {
    expect(() => allowanceStatusClass("throttled")).toThrow(MalformedView);
  });

  it("has no colour the schema does not declare an arm for", () => {
    for (const arm of ["allowed", "allowedWarning", "rejected"]) {
      expect(ALLOWANCE_SCHEMA_ARMS).toContain(arm);
    }
  });
});
