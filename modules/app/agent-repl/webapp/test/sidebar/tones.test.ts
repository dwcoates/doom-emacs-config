// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import renderColors from "../../../proto/vocab/render-colors.json";
import { RosterRowSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { toneClass, rosterStatusColor, protoArmName } from "../../src/vocab.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { oneofArms } from "../../src/rpc/oneof.js";
import {
  GLYPH_IMAGES,
  ROSTER_ARM_CLASS,
  ROSTER_STATUS_CASES,
  armMotion,
  glyphName,
  rosterArmMark,
  rosterRowMark,
  NO_MARKERS,
  type RosterStatusCase,
  type RowMarkers,
} from "../../src/sidebar/tones.js";
import { readGlyphPicture } from "../glyph-pictures";


const ROSTER_STATUS = renderColors.roster_status as Record<string, string>;
const MERGE_GLYPHS = renderColors.merge_glyphs as Record<string, string>;
const GATE_GLYPHS = renderColors.gate_glyphs as Record<string, string>;
const GLYPH_INKS = renderColors.glyph_inks as Record<string, string>;
/** A one-shot row carrying no other marker. */
const ONE_SHOT: RowMarkers = { oneShot: true, asyncMerging: false };
/** The arms a one-shot's glyph replaces the mark on, in the generated spelling. */
const ONE_SHOT_ARMS: string[] = ROSTER_STATUS_CASES.filter((arm) =>
  renderColors.one_shot_glyph_arms.includes(protoArmName(arm)),
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

  it.each([...ROSTER_STATUS_CASES])("draws a glyph picture for %s", (arm) => {
    const mark = rosterArmMark(arm);
    expect(mark.image).toBe(GLYPH_IMAGES[mark.glyph]);
  });

  // ONE PICTURE PER NAME FOR BOTH SURFACES: every glyph the rail draws as a
  // picture is the file the Emacs tab bar draws, byte for byte.
  it.each(Object.entries(GLYPH_IMAGES).filter(([, image]) => image !== ""))(
    "draws the %s glyph from the shared images/glyphs file",
    (name, image) => {
      expect(image).toBe(readGlyphPicture(name));
    },
  );

  it("draws the merge picture in currentColor, so the tone or the ink colors it", () => {
    expect(GLYPH_IMAGES.merge).toContain('fill="currentColor"');
  });

  it.each([...Object.values(MERGE_GLYPHS), ...Object.values(GATE_GLYPHS), "target"])(
    "has a picture for the shared glyph name %s",
    (name) => {
      expect(GLYPH_IMAGES[name]).toMatch(/^<svg /);
    },
  );

  it.each(Object.entries(GLYPH_INKS))("inks the %s arm's mark %s, apart from its tone", (key, ink) => {
    const arm = ROSTER_STATUS_CASES.find((c) => protoArmName(c) === key) as RosterStatusCase;
    expect(rosterArmMark(arm).ink).toBe(ink);
  });

  it("draws the lifecycle arms as the plain still dot", () => {
    expect(rosterArmMark("ready")).toEqual({ toneClass: "tone-green", glyph: "dot", image: "", ink: undefined, motion: "still" });
  });

  // A GATE IS READY FOR YOU (owner ruling, 2026-10-08): the green of a
  // finished turn, with the gate's emoji in place of the dot (owner requests,
  // 2026-10-11 and 2026-10-13).
  it("draws a permission gate as a still green exclamation emoji", () => {
    expect(rosterArmMark("permission")).toEqual({ toneClass: "tone-green", glyph: "exclaim", image: GLYPH_IMAGES.exclaim, ink: undefined, motion: "still" });
  });

  it("draws a question gate as a still green question emoji", () => {
    expect(rosterArmMark("question")).toEqual({ toneClass: "tone-green", glyph: "ask", image: GLYPH_IMAGES.ask, ink: undefined, motion: "still" });
  });

  it("draws a cold gate as a still green snowflake", () => {
    expect(rosterArmMark("coldGate")).toEqual({ toneClass: "tone-green", glyph: "snowflake", image: GLYPH_IMAGES.snowflake, ink: undefined, motion: "still" });
  });

  it("draws an interrupt landing as the still green dot, since it is no gate", () => {
    expect(rosterArmMark("waiting")).toEqual({ toneClass: "tone-green", glyph: "dot", image: "", ink: undefined, motion: "still" });
  });

  it("draws a compaction as a red workspace with an orange dot", () => {
    expect(rosterArmMark("compacting")).toEqual({ toneClass: "tone-red", glyph: "dot", image: "", ink: "orange", motion: "breathes" });
  });

  it("draws a failed turn end as a turquoise dot", () => {
    expect(rosterArmMark("turnFailed")).toEqual({ toneClass: "tone-turquoise", glyph: "dot", image: "", ink: undefined, motion: "still" });
  });

  it("draws a degraded view as a turquoise dot", () => {
    expect(rosterArmMark("degraded")).toEqual({ toneClass: "tone-turquoise", glyph: "dot", image: "", ink: undefined, motion: "still" });
  });

  // ONE MERGE GLYPH (owner ruling, 2026-10-13): its ink and motion say which arm.
  it("draws a queued merge as the still purple merge glyph", () => {
    expect(rosterArmMark("mergeQueued")).toEqual({ toneClass: "tone-purple", glyph: "merge", image: GLYPH_IMAGES.merge, ink: undefined, motion: "still" });
  });

  it("draws a running merge as the breathing purple merge glyph", () => {
    expect(rosterArmMark("merging")).toEqual({ toneClass: "tone-purple", glyph: "merge", image: GLYPH_IMAGES.merge, ink: undefined, motion: "breathes" });
  });

  it("draws a failed merge as a green workspace with a red merge glyph", () => {
    expect(rosterArmMark("mergeFailed")).toEqual({ toneClass: "tone-green", glyph: "merge", image: GLYPH_IMAGES.merge, ink: "red", motion: "still" });
  });

  it("draws a landed merge as the green merge glyph", () => {
    expect(rosterArmMark("merged")).toEqual({ toneClass: "tone-green", glyph: "merge", image: GLYPH_IMAGES.merge, ink: undefined, motion: "still" });
  });

  it("draws an incidentally inactive workspace as the grey dot", () => {
    expect(rosterArmMark("inactive")).toEqual({ toneClass: "tone-none", glyph: "dot", image: "", ink: undefined, motion: "still" });
  });

  it("draws a booting session as the same grey dot, breathing", () => {
    expect(rosterArmMark("init")).toEqual({ ...rosterArmMark("inactive"), motion: "breathes" });
  });

  it("draws nothing at all for a workspace with no session", () => {
    expect(rosterArmMark("none")).toEqual({ toneClass: "tone-none", glyph: "none", image: "", ink: undefined, motion: "still" });
  });

  it("refuses an arm this build has no mark for", () => {
    expect(() => rosterArmMark("teleporting")).toThrow(MalformedView);
  });
});

// ONE ANIMATION, ONE CLAIM (owner ruling, 2026-10-13): a breath is something
// in progress; a gate or an interrupt landing waits on the user and is still.
describe("the rail's one animation", () => {
  it.each(["submitting", "thinking", "clearing", "compacting", "idleAsync", "init", "merging"] as const)(
    "breathes on %s",
    (arm) => {
      expect(armMotion(arm)).toBe("breathes");
    },
  );

  it.each(["permission", "question", "coldGate", "waiting", "ready", "done", "interrupted", "turnFailed", "dead", "none", "inactive", "closing", "daemonImpaired", "mergeQueued", "mergeFailed", "merged"] as const)(
    "is still on %s",
    (arm) => {
      expect(armMotion(arm)).toBe("still");
    },
  );
});

describe("a row's mark with its one-shot mark", () => {
  it.each(ONE_SHOT_ARMS)("draws a breathing target in place of the %s mark, on the arm's own tone", (arm) => {
    expect(rosterRowMark(arm, ONE_SHOT)).toEqual({
      toneClass: rosterArmMark(arm).toneClass,
      glyph: "target",
      image: GLYPH_IMAGES.target,
      ink: undefined,
      motion: "breathes",
    });
  });

  it.each(
    ROSTER_STATUS_CASES.filter((arm) => !ONE_SHOT_ARMS.includes(arm)),
  )("draws the %s arm's own mark for a one-shot", (arm) => {
    expect(rosterRowMark(arm, ONE_SHOT)).toEqual(rosterArmMark(arm));
  });

  it("draws the arm's own mark for a row that is no one-shot", () => {
    expect(rosterRowMark("thinking", NO_MARKERS)).toEqual(rosterArmMark("thinking"));
  });

  it("refuses an arm the rail cannot draw even for a one-shot", () => {
    expect(() => rosterRowMark("notAnArm", ONE_SHOT)).toThrow(MalformedView);
  });
});

// AN ASYNC MERGE OF THE WORKSPACE'S OWN BRANCH (owner request, 2026-10-13):
// the plain dot gives way to the shared vocabulary's async-merge glyph, ink
// and motion, on the arm's own tone; every other mark outranks it.
describe("a row's mark with its async-merging marker", () => {
  const ASYNC_MERGING: RowMarkers = { oneShot: false, asyncMerging: true };
  const PLAIN_DOT_ARMS = ROSTER_STATUS_CASES.filter(
    (arm) => rosterArmMark(arm).glyph === "dot" && rosterArmMark(arm).ink === undefined,
  );
  const OTHER_MARK_ARMS = ROSTER_STATUS_CASES.filter((arm) => !PLAIN_DOT_ARMS.includes(arm));

  it.each(PLAIN_DOT_ARMS)("draws the purple merge glyph, breathing, over the plain %s dot on the arm's own tone", (arm) => {
    expect(rosterRowMark(arm, ASYNC_MERGING)).toEqual({
      ...rosterArmMark(arm),
      glyph: "merge",
      image: GLYPH_IMAGES.merge,
      ink: "purple",
      motion: "breathes",
    });
  });

  it("draws the same picture as a running merge", () => {
    expect(rosterRowMark("ready", ASYNC_MERGING).image).toBe(rosterArmMark("merging").image);
  });

  it.each(OTHER_MARK_ARMS)("keeps the %s arm's own mark over the async merge", (arm) => {
    expect(rosterRowMark(arm, ASYNC_MERGING)).toEqual(rosterArmMark(arm));
  });

  it("keeps the compaction's orange dot over the async merge", () => {
    expect(rosterRowMark("compacting", ASYNC_MERGING).ink).toBe("orange");
  });

  it.each(ONE_SHOT_ARMS)("keeps a working one-shot's target over the async merge on %s", (arm) => {
    expect(rosterRowMark(arm, { oneShot: true, asyncMerging: true })).toEqual(rosterRowMark(arm, ONE_SHOT));
  });

  it("covers the idle and working dots among the plain ones", () => {
    expect(["ready", "done", "idleAsync", "thinking"].every((arm) => PLAIN_DOT_ARMS.includes(arm as never))).toBe(true);
  });

  it("refuses an arm the rail cannot draw even under an async merge", () => {
    expect(() => rosterRowMark("notAnArm", ASYNC_MERGING)).toThrow(MalformedView);
  });
});
