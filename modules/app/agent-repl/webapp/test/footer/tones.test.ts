// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import renderColors from "../../../proto/vocab/render-colors.json";
import {
  FooterAllowanceSchema,
  FooterStatusSchema,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { FOOTER_ALLOWANCE_ARMS, FOOTER_STATUS_ARMS, protoArmName } from "../../src/vocab.js";
import {
  ALLOWANCE_ARM_CLASS,
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

  it("names the fifteen arms the contract carries", () => {
    expect([...FOOTER_STATUS_CASES].sort()).toEqual(
      [
        "background",
        "blocked",
        "closing",
        "degraded",
        "disconnected",
        "idle",
        "interrupted",
        "loading",
        "mergeConflict",
        "mergeFailed",
        "merged",
        "merging",
        "working",
        "turnFailed",
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
    ["working", "tone-red"],
    ["waiting", "tone-green"],
    ["idle", "tone-green"],
    ["interrupted", "tone-green"],
    // The footer speaks about ONE session, so a merge holding it is purple —
    // where a merging ROSTER row spends no color and reports itself by glyph.
    ["merging", "tone-purple"],
    // A STOPPED merge is not `merging` (owner ruling, 2026-09-28): a conflict
    // awaiting the user is green, a failure is blue, and a landed merge is
    // green (owner ruling, 2026-09-28).
    ["mergeConflict", "tone-green"],
    ["mergeFailed", "tone-turquoise"],
    ["turnFailed", "tone-turquoise"],
    ["degraded", "tone-turquoise"],
    ["merged", "tone-green"],
    ["background", "tone-yellow"],
    // blocked is blue, like disconnected and closing: a blocked session cannot
    // proceed until something outside it changes, which renders the agent
    // unusable — the same claim, and the same color, as the roster's
    // vendor_blocked.
    ["blocked", "tone-blue"],
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

describe("ALLOWANCE_ARM_CLASS: asserted row for row against render-colors.json", () => {
  it("has a row for every arm the proto declares", () => {
    expect(Object.keys(ALLOWANCE_ARM_CLASS).sort()).toEqual([...ALLOWANCE_SCHEMA_ARMS].sort());
  });

  it("leaves no key in the file without an arm in the proto", () => {
    expect([...FOOTER_ALLOWANCE_ARMS].sort()).toEqual(
      ALLOWANCE_SCHEMA_ARMS.map(protoArmName).sort(),
    );
  });

  it("takes each arm's colour from the shared file rather than a local copy", () => {
    for (const arm of ALLOWANCE_SCHEMA_ARMS) {
      expect(ALLOWANCE_ARM_CLASS[arm]).toBe(
        (renderColors.footer_allowance as Record<string, string>)[protoArmName(arm)],
      );
    }
  });
});

describe("allowanceStatusClass", () => {
  it.each(ALLOWANCE_SCHEMA_ARMS)("gives %s a colour", (arm) => {
    expect(allowanceStatusClass(arm)).toMatch(/^tone-/);
  });

  it.each<[string, string]>([
    // render-colors.json#footer_allowance: the ordinary state, the newsworthy
    // rung, and the vendor refusing.
    ["allowed", "tone-green"],
    ["allowedWarning", "tone-yellow"],
    ["rejected", "tone-red"],
  ])("gives %s the class %s", (arm, expected) => {
    expect(allowanceStatusClass(arm)).toBe(expected);
  });

  it("refuses an arm the shared vocabulary has no colour for", () => {
    expect(() => allowanceStatusClass("throttled")).toThrow(MalformedView);
  });
});
