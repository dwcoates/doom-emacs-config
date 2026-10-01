// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import renderColors from "../../../proto/vocab/render-colors.json";
import { FooterStatusSchema } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { FOOTER_STATUS_ARMS, protoArmName } from "../../src/vocab.js";
import {
  FOOTER_STATUS_CASES,
  STATUS_ARM_CLASS,
  activityDatumClass,
  footerPercentColor,
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

  it("names the fourteen arms the contract carries", () => {
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
    // A STOPPED merge is not `merging`: a failure is turquoise (something
    // went wrong, the workspace is usable) and a landed merge is green.
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
    // So does the subagent label a transient carries.
    ["agent", "tone-blue"],
    // Everything else is a figure the reader watches move.
    ["attempt", "tone-yellow"],
    ["count", "tone-yellow"],
    ["position", "tone-yellow"],
  ])("gives %s the class %s", (datum, expected) => {
    expect(activityDatumClass(datum)).toBe(expected);
  });
});

describe("footerPercentColor", () => {
  // The hue is the first field of the `hsl(H S% L%)` string.
  const hueOf = (color: string): number => Number(/^hsl\((\d+)\s/.exec(color)?.[1]);

  it.each([
    ["flat green well below 40", 10, 140],
    ["still green at 40", 40, 140],
    ["halfway between green and yellow at 55", 55, 100],
    ["yellow at 70", 70, 60],
    ["halfway between yellow and orange at 80", 80, 45],
    ["nearly orange at 89", 89, 32],
    ["red at 90", 90, 0],
    ["red at 100", 100, 0],
  ])("is %s", (_name, percent, hue) => {
    expect(hueOf(footerPercentColor(percent))).toBe(hue);
  });
});
