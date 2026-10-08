// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import renderColors from "../../../proto/vocab/render-colors.json";
import { RosterRowSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { toneClass, rosterStatusColor, protoArmName } from "../../src/vocab.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  GLYPH_CHARS,
  ROSTER_ARM_CLASS,
  ROSTER_STATUS_CASES,
  armBreathes,
  armSpins,
  glyphName,
  rosterArmMark,
  type RosterStatusCase,
} from "../../src/sidebar/tones.js";

/** The proto's own arms, in the generated spelling a received row carries. */
const SCHEMA_ARMS = RosterRowSchema.oneofs[0].fields.map((field) => field.localName);

const ROSTER_STATUS = renderColors.roster_status as Record<string, string>;
const MERGE_GLYPHS = renderColors.merge_glyphs as Record<string, string>;

describe("the rail's arm table against the schema", () => {
  it.each(SCHEMA_ARMS)("names the schema's %s arm", (arm) => {
    expect(ROSTER_STATUS_CASES).toContain(arm);
  });

  it.each([...ROSTER_STATUS_CASES])("draws only arms the schema declares: %s", (arm) => {
    expect(SCHEMA_ARMS).toContain(arm);
  });
});

describe("the rail's arm table against the shared vocabulary", () => {
  it.each([...ROSTER_STATUS_CASES])("paints %s the color the file assigns", (arm) => {
    expect(ROSTER_ARM_CLASS[arm]).toBe(toneClass(rosterStatusColor(arm)));
  });

  it.each(Object.keys(ROSTER_STATUS))("has a row for the file's %s key", (key) => {
    const arms = ROSTER_STATUS_CASES.map((arm) => protoArmName(arm));
    expect(arms).toContain(key);
  });
});

describe("the status mark", () => {
  it.each(Object.keys(MERGE_GLYPHS))("gives the %s arm its shared glyph name", (key) => {
    const arm = ROSTER_STATUS_CASES.find((c) => protoArmName(c) === key) as RosterStatusCase;
    expect(glyphName(arm)).toBe(MERGE_GLYPHS[key]);
  });

  it.each([...ROSTER_STATUS_CASES])("draws a glyph character for %s", (arm) => {
    const mark = rosterArmMark(arm);
    expect(mark.char).toBe(GLYPH_CHARS[mark.glyph]);
  });

  it("draws the lifecycle arms as the plain dot", () => {
    expect(rosterArmMark("ready")).toEqual({ toneClass: "tone-green", glyph: "dot", char: "" });
  });

  it("draws a failed turn end as a turquoise dot", () => {
    expect(rosterArmMark("turnFailed")).toEqual({ toneClass: "tone-turquoise", glyph: "dot", char: "" });
  });

  it("draws a failed merge as a turquoise cross", () => {
    expect(rosterArmMark("mergeFailed")).toEqual({ toneClass: "tone-turquoise", glyph: "failed", char: "✕" });
  });

  it("draws a degraded view as a turquoise dot", () => {
    expect(rosterArmMark("degraded")).toEqual({ toneClass: "tone-turquoise", glyph: "dot", char: "" });
  });

  it("draws a merge in progress as a purple recycle mark", () => {
    expect(rosterArmMark("merging")).toEqual({ toneClass: "tone-purple", glyph: "recycle", char: "⟳" });
  });

  it("draws a landed merge as a green check", () => {
    expect(rosterArmMark("merged")).toEqual({ toneClass: "tone-green", glyph: "check", char: "✓" });
  });

  it("draws a question mark for a perspective-less workspace", () => {
    expect(rosterArmMark("inactive")).toEqual({
      toneClass: "tone-none",
      glyph: "inactive",
      char: "?",
    });
  });

  it("draws nothing at all for a workspace with no session", () => {
    expect(rosterArmMark("none")).toEqual({ toneClass: "tone-none", glyph: "none", char: "" });
  });

  it("refuses an arm this build has no mark for", () => {
    expect(() => rosterArmMark("teleporting")).toThrow(MalformedView);
  });
});

describe("the rail's two animations", () => {
  it.each(["submitting", "thinking", "clearing", "compacting", "permission", "waiting", "idleAsync"] as const)(
    "breathes on %s",
    (arm) => {
      expect(armBreathes(arm)).toBe(true);
    },
  );

  it.each(["ready", "done", "interrupted", "turnFailed", "dead", "merged", "none", "inactive", "closing", "daemonImpaired"] as const)(
    "is still on %s",
    (arm) => {
      expect(armBreathes(arm)).toBe(false);
    },
  );

  it.each(["merging"] as const)("spins on %s", (arm) => {
    expect(armSpins(arm)).toBe(true);
  });

  it.each(["mergeQueued", "mergeFailed", "merged"] as const)(
    "does not spin on %s",
    (arm) => {
      expect(armSpins(arm)).toBe(false);
    },
  );
});
