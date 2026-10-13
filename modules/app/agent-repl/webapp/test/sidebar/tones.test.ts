// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import renderColors from "../../../proto/vocab/render-colors.json";
import { RosterRowSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { toneClass, rosterStatusColor, protoArmName } from "../../src/vocab.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { oneofArms } from "../../src/rpc/oneof.js";
import {
  GLYPH_CHARS,
  ROSTER_ARM_CLASS,
  ROSTER_STATUS_CASES,
  armMotion,
  glyphName,
  rosterArmMark,
  rosterRowMark,
  type RosterStatusCase,
} from "../../src/sidebar/tones.js";


const ROSTER_STATUS = renderColors.roster_status as Record<string, string>;
const MERGE_GLYPHS = renderColors.merge_glyphs as Record<string, string>;
const GATE_GLYPHS = renderColors.gate_glyphs as Record<string, string>;
const GLYPH_INKS = renderColors.glyph_inks as Record<string, string>;
/** The arms a one-shot's glyph replaces the mark on, in the generated spelling. */
const ONE_SHOT_ARMS: string[] = ROSTER_STATUS_CASES.filter((arm) =>
  (renderColors.one_shot_glyph_arms as string[]).includes(protoArmName(arm)),
);

describe("the rail's arm table against the schema", () => {
  it("reads its arms off the schema through the one oneof reader", () => {
    expect(ROSTER_STATUS_CASES).toEqual(oneofArms(RosterRowSchema, "status"));
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

  it.each(Object.entries(GLYPH_INKS))("inks the %s arm's mark %s, apart from its tone", (key, ink) => {
    const arm = ROSTER_STATUS_CASES.find((c) => protoArmName(c) === key) as RosterStatusCase;
    expect(rosterArmMark(arm).ink).toBe(ink);
  });

  it("draws the lifecycle arms as the plain still dot", () => {
    expect(rosterArmMark("ready")).toEqual({ toneClass: "tone-green", glyph: "dot", char: "", ink: undefined, motion: "still" });
  });

  // A GATE IS READY FOR YOU (owner ruling, 2026-10-08): the green of a
  // finished turn, with the gate's emoji in place of the dot (owner requests,
  // 2026-10-11 and 2026-10-13).
  it("draws a permission gate as a green exclamation emoji", () => {
    expect(rosterArmMark("permission")).toEqual({ toneClass: "tone-green", glyph: "exclaim", char: "❗", ink: undefined, motion: "breathes" });
  });

  it("draws a question gate as a green question emoji", () => {
    expect(rosterArmMark("question")).toEqual({ toneClass: "tone-green", glyph: "ask", char: "❓", ink: undefined, motion: "breathes" });
  });

  it("draws a cold gate as a green stop sign", () => {
    expect(rosterArmMark("coldGate")).toEqual({ toneClass: "tone-green", glyph: "stop", char: "🛑", ink: undefined, motion: "breathes" });
  });

  it("draws an interrupt landing as the plain green dot, since it is no gate", () => {
    expect(rosterArmMark("waiting")).toEqual({ toneClass: "tone-green", glyph: "dot", char: "", ink: undefined, motion: "breathes" });
  });

  it("draws a compaction as a red workspace with an orange dot", () => {
    expect(rosterArmMark("compacting")).toEqual({ toneClass: "tone-red", glyph: "dot", char: "", ink: "orange", motion: "breathes" });
  });

  it("draws a failed turn end as a turquoise dot", () => {
    expect(rosterArmMark("turnFailed")).toEqual({ toneClass: "tone-turquoise", glyph: "dot", char: "", ink: undefined, motion: "still" });
  });

  it("draws a degraded view as a turquoise dot", () => {
    expect(rosterArmMark("degraded")).toEqual({ toneClass: "tone-turquoise", glyph: "dot", char: "", ink: undefined, motion: "still" });
  });

  // ONE MERGE GLYPH (owner ruling, 2026-10-13): its ink and motion say which arm.
  it("draws a queued merge as the still purple merge glyph", () => {
    expect(rosterArmMark("mergeQueued")).toEqual({ toneClass: "tone-purple", glyph: "merge", char: "", ink: undefined, motion: "still" });
  });

  it("draws a running merge as the turning purple merge glyph", () => {
    expect(rosterArmMark("merging")).toEqual({ toneClass: "tone-purple", glyph: "merge", char: "", ink: undefined, motion: "spins" });
  });

  it("draws a failed merge as a green workspace with a red merge glyph", () => {
    expect(rosterArmMark("mergeFailed")).toEqual({ toneClass: "tone-green", glyph: "merge", char: "", ink: "red", motion: "still" });
  });

  it("draws a landed merge as the green merge glyph", () => {
    expect(rosterArmMark("merged")).toEqual({ toneClass: "tone-green", glyph: "merge", char: "", ink: undefined, motion: "still" });
  });

  it("draws an incidentally inactive workspace as the grey dot", () => {
    expect(rosterArmMark("inactive")).toEqual({ toneClass: "tone-none", glyph: "dot", char: "", ink: undefined, motion: "still" });
  });

  it("draws a booting session as the same grey dot", () => {
    expect(rosterArmMark("init")).toEqual(rosterArmMark("inactive"));
  });

  it("draws nothing at all for a workspace with no session", () => {
    expect(rosterArmMark("none")).toEqual({ toneClass: "tone-none", glyph: "none", char: "", ink: undefined, motion: "still" });
  });

  it("refuses an arm this build has no mark for", () => {
    expect(() => rosterArmMark("teleporting")).toThrow(MalformedView);
  });
});

describe("the rail's two animations", () => {
  it.each(["submitting", "thinking", "clearing", "compacting", "permission", "question", "coldGate", "waiting", "idleAsync"] as const)(
    "breathes on %s",
    (arm) => {
      expect(armMotion(arm)).toBe("breathes");
    },
  );

  it.each(["ready", "done", "interrupted", "turnFailed", "dead", "init", "none", "inactive", "closing", "daemonImpaired", "mergeQueued", "mergeFailed", "merged"] as const)(
    "is still on %s",
    (arm) => {
      expect(armMotion(arm)).toBe("still");
    },
  );

  it("spins on merging", () => {
    expect(armMotion("merging")).toBe("spins");
  });
});

describe("a row's mark with its one-shot mark", () => {
  it.each(ONE_SHOT_ARMS)("draws a still target in place of the %s mark, on the arm's own tone", (arm) => {
    expect(rosterRowMark(arm, true)).toEqual({
      toneClass: rosterArmMark(arm).toneClass,
      glyph: "target",
      char: "🎯",
      ink: undefined,
      motion: "still",
    });
  });

  it.each(
    ROSTER_STATUS_CASES.filter((arm) => !ONE_SHOT_ARMS.includes(arm)),
  )("draws the %s arm's own mark for a one-shot", (arm) => {
    expect(rosterRowMark(arm, true)).toEqual(rosterArmMark(arm));
  });

  it("draws the arm's own mark for a row that is no one-shot", () => {
    expect(rosterRowMark("thinking", false)).toEqual(rosterArmMark("thinking"));
  });

  it("refuses an arm the rail cannot draw even for a one-shot", () => {
    expect(() => rosterRowMark("notAnArm", true)).toThrow(MalformedView);
  });
});
