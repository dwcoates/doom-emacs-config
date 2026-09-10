/**
 * Tests for shared worlds and multi-turn scenarios.
 *
 * These replace three `manual_setup` notes that asked the operator to
 * hand-arrange state the harness could have arranged itself.
 */
import { describe, expect, it } from "vitest";

import {
  WORLD_PREFIX,
  isPromptDriven,
  planWorlds,
  promptTurnsOf,
  resumeCaptureOf,
  resumeTurnIndexes,
  worldOf,
} from "./worlds.mjs";

const isolated = (name) => ({ name });
const inWorld = (name, world) => ({ name, config_root: `${WORLD_PREFIX}${world}` });

describe("worldOf", () => {
  it("returns null for a scenario with no config_root", () => {
    expect(worldOf(isolated("a"))).toBeNull();
  });

  it("returns null for an explicit null", () => {
    expect(worldOf({ name: "a", config_root: null })).toBeNull();
  });

  it("reads the world name out of the prefix", () => {
    expect(worldOf(inWorld("a", "conversation-history"))).toBe("conversation-history");
  });

  it("THROWS on a config_root missing the prefix, rather than silently isolating it", () => {
    expect(() => worldOf({ name: "a", config_root: "/some/path" })).toThrow(/must be/);
  });

  it("throws on an empty world name", () => {
    expect(() => worldOf({ name: "a", config_root: WORLD_PREFIX })).toThrow(/empty world/);
  });

  it("names the offending scenario in the error", () => {
    expect(() => worldOf({ name: "the-scenario", config_root: 7 })).toThrow(/the-scenario/);
  });
});

describe("planWorlds", () => {
  it("gives an isolated scenario a group of its own", () => {
    const groups = planWorlds([isolated("a")]);
    expect(groups).toEqual([{ world: null, scenarios: [isolated("a")] }]);
  });

  it("puts scenarios of one world into one group", () => {
    const groups = planWorlds([inWorld("a", "w"), inWorld("b", "w")]);
    expect(groups).toHaveLength(1);
    expect(groups[0].scenarios.map((s) => s.name)).toEqual(["a", "b"]);
  });

  it("keeps corpus order inside a world", () => {
    const groups = planWorlds([inWorld("first", "w"), isolated("x"), inWorld("second", "w")]);
    expect(groups[0].scenarios.map((s) => s.name)).toEqual(["first", "second"]);
  });

  it("places a world's group where its FIRST member sits", () => {
    const groups = planWorlds([isolated("x"), inWorld("a", "w"), isolated("y"), inWorld("b", "w")]);
    expect(groups.map((g) => g.world)).toEqual([null, "w", null]);
  });

  it("keeps two different worlds apart", () => {
    const groups = planWorlds([inWorld("a", "one"), inWorld("b", "two")]);
    expect(groups.map((g) => g.world)).toEqual(["one", "two"]);
  });

  it("returns nothing for an empty corpus", () => {
    expect(planWorlds([])).toEqual([]);
  });
});

describe("promptTurnsOf", () => {
  it("reads the single-prompt spelling, so the one-shot corpus needs no edit", () => {
    expect(promptTurnsOf({ prompt: "hello" })).toEqual([{ text: "hello", resume: false }]);
  });

  it("reads an array of plain strings as consecutive turns", () => {
    expect(promptTurnsOf({ prompts: ["one", "two"] })).toEqual([
      { text: "one", resume: false },
      { text: "two", resume: false },
    ]);
  });

  it("reads a resume turn", () => {
    expect(promptTurnsOf({ prompts: [{ text: "again", resume: true }] })).toEqual([
      { text: "again", resume: true },
    ]);
  });

  it("defaults resume to false on an object turn", () => {
    expect(promptTurnsOf({ prompts: [{ text: "x" }] })[0].resume).toBe(false);
  });

  it("returns no turns for a MANUAL scenario", () => {
    expect(promptTurnsOf({ manual: "do it by hand" })).toEqual([]);
  });

  it("prefers prompts over a stray prompt field", () => {
    expect(promptTurnsOf({ prompt: "ignored", prompts: ["used"] })).toEqual([
      { text: "used", resume: false },
    ]);
  });

  it("throws on an empty string turn", () => {
    expect(() => promptTurnsOf({ name: "a", prompts: [""] })).toThrow(/prompt 0 is empty/);
  });

  it("throws on a turn that is neither a string nor a { text } object", () => {
    expect(() => promptTurnsOf({ name: "a", prompts: [{ resume: true }] })).toThrow(/must be/);
  });

  it("names the offending index", () => {
    expect(() => promptTurnsOf({ name: "a", prompts: ["ok", 5] })).toThrow(/prompt 1/);
  });
});

describe("isPromptDriven", () => {
  it("is true for a single-prompt scenario", () => {
    expect(isPromptDriven({ prompt: "x" })).toBe(true);
  });

  it("is true for a multi-turn scenario", () => {
    expect(isPromptDriven({ prompts: ["a", "b"] })).toBe(true);
  });

  it("is false for a MANUAL scenario", () => {
    expect(isPromptDriven({ manual: "by hand" })).toBe(false);
  });

  it("is false for an empty prompt", () => {
    expect(isPromptDriven({ prompt: "" })).toBe(false);
  });
});

describe("resumeTurnIndexes", () => {
  it("finds no resume in an ordinary scenario", () => {
    expect(resumeTurnIndexes({ prompts: ["a", "b"] })).toEqual([]);
  });

  it("reports the index of a resume turn", () => {
    expect(resumeTurnIndexes({ prompts: ["a", { text: "b", resume: true }] })).toEqual([1]);
  });

  it("reports several resumes", () => {
    expect(
      resumeTurnIndexes({
        prompts: [{ text: "a", resume: true }, "b", { text: "c", resume: true }],
      }),
    ).toEqual([0, 2]);
  });
});

describe("resumeCaptureOf", () => {
  it("is null for an ordinary scenario", () => {
    expect(resumeCaptureOf({ name: "s", prompt: "hi" })).toBe(null);
  });

  it("names the capture whose session is resumed", () => {
    expect(resumeCaptureOf({ name: "cold", resume_capture: "prose-streamed" })).toBe(
      "prose-streamed",
    );
  });

  it("rejects an empty capture name", () => {
    expect(() => resumeCaptureOf({ name: "cold", resume_capture: "" })).toThrow(
      /non-empty capture directory name/,
    );
  });

  it("rejects a non-string capture name", () => {
    expect(() => resumeCaptureOf({ name: "cold", resume_capture: 7 })).toThrow(
      /non-empty capture directory name/,
    );
  });

  it("refuses to pair a resumed session with a shared world", () => {
    // A resumed session brings the ORIGINAL capture's cwd with it, which is the
    // whole reason the resume resolves; a world supplies a cwd of its own, so
    // the two cannot both hold.
    expect(() =>
      resumeCaptureOf({
        name: "cold",
        resume_capture: "prose-streamed",
        config_root: `${WORLD_PREFIX}conversation-history`,
      }),
    ).toThrow(/cannot share one/);
  });
});
