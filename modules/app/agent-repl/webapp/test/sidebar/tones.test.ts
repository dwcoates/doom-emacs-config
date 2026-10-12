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
  rosterRowMark,
  type RosterStatusCase,
} from "../../src/sidebar/tones.js";

/** The proto's own arms, in the generated spelling a received row carries. */
const SCHEMA_ARMS = RosterRowSchema.oneofs[0].fields.map((field) => field.localName);

const ROSTER_STATUS = renderColors.roster_status as Record<string, string>;
const MERGE_GLYPHS = renderColors.merge_glyphs as Record<string, string>;
const GATE_GLYPHS = renderColors.gate_glyphs as Record<string, string>;

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
  it.each(Object.entries({ ...MERGE_GLYPHS, ...GATE_GLYPHS }))("gives the %s arm its shared glyph name %s", (key, name) => {
    const arm = ROSTER_STATUS_CASES.find((c) => protoArmName(c) === key) as RosterStatusCase;
    expect(glyphName(arm)).toBe(name);
  });

  it.each([...ROSTER_STATUS_CASES])("draws a glyph character for %s", (arm) => {
    const mark = rosterArmMark(arm);
    expect(mark.char).toBe(GLYPH_CHARS[mark.glyph]);
  });

  it("draws the lifecycle arms as the plain dot", () => {
    expect(rosterArmMark("ready")).toEqual({ toneClass: "tone-green", glyph: "dot", char: "" });
  });

  // A GATE IS READY FOR YOU (owner ruling, 2026-10-08): the green of a
  // finished turn, with the gate's emoji in place of the dot (owner request,
  // 2026-10-11).
  it("draws a permission gate as a green exclamation emoji", () => {
    expect(rosterArmMark("permission")).toEqual({ toneClass: "tone-green", glyph: "exclaim", char: "❗" });
  });

  it("draws a question gate as a green question emoji", () => {
    expect(rosterArmMark("question")).toEqual({ toneClass: "tone-green", glyph: "ask", char: "❓" });
  });

  it("draws a waiting session as the plain green dot, since it is no gate", () => {
    expect(rosterArmMark("waiting")).toEqual({ toneClass: "tone-green", glyph: "dot", char: "" });
  });

  it("draws a failed turn end as a turquoise dot", () => {
    expect(rosterArmMark("turnFailed")).toEqual({ toneClass: "tone-turquoise", glyph: "dot", char: "" });
  });

  it("draws a failed merge as a turquoise grey cross", () => {
    expect(rosterArmMark("mergeFailed")).toEqual({
      toneClass: "tone-turquoise",
      glyph: "failed",
      char: "\u2716\uFE0F",
    });
  });

  it("draws a degraded view as a turquoise dot", () => {
    expect(rosterArmMark("degraded")).toEqual({ toneClass: "tone-turquoise", glyph: "dot", char: "" });
  });

  it("draws a merge in progress as a purple recycle mark", () => {
    expect(rosterArmMark("merging")).toEqual({ toneClass: "tone-purple", glyph: "recycle", char: "⟳" });
  });

  it("draws no character for a landed merge, under the vocabulary's check glyph", () => {
    expect(rosterArmMark("merged")).toEqual({ toneClass: "tone-green", glyph: "check", char: "" });
  });

  it("draws a question mark for a perspective-less workspace", () => {
    expect(rosterArmMark("inactive")).toEqual({
      toneClass: "tone-none",
      glyph: "inactive",
      char: "?",
    });
  });

  it("draws a grey dot for a workspace still opening", () => {
    expect(rosterArmMark("opening")).toEqual({ toneClass: "tone-none", glyph: "dot", char: "" });
  });

  it("draws nothing at all for a workspace with no session", () => {
    expect(rosterArmMark("none")).toEqual({ toneClass: "tone-none", glyph: "none", char: "" });
  });

  it("refuses an arm this build has no mark for", () => {
    expect(() => rosterArmMark("teleporting")).toThrow(MalformedView);
  });
});

describe("the rail's two animations", () => {
  it.each(["submitting", "thinking", "clearing", "compacting", "permission", "question", "waiting", "idleAsync"] as const)(
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

  it.each(["mergeQueued", "mergeWaiting", "mergeFailed", "merged"] as const)(
    "does not spin on %s",
    (arm) => {
      expect(armSpins(arm)).toBe(false);
    },
  );
});

describe("a row's mark with its one-shot outcome", () => {
  it("draws a failed one-shot's failure mark on the arm's own tone", () => {
    expect(rosterRowMark("done", true)).toEqual({
      toneClass: "tone-green",
      glyph: "failed",
      char: GLYPH_CHARS.failed,
    });
  });

  it.each(["permission", "question"])("lets an open %s gate outrank a failed one-shot", (arm) => {
    expect(rosterRowMark(arm, true)).toEqual(rosterArmMark(arm));
  });

  it("draws the arm's own mark when the one-shot has not failed", () => {
    expect(rosterRowMark("thinking", false)).toEqual(rosterArmMark("thinking"));
  });

  it("refuses an arm the rail cannot draw even for a failed one-shot", () => {
    expect(() => rosterRowMark("notAnArm", true)).toThrow(MalformedView);
  });
});
