/**
 * The vendor's `supportedCommands()` answer, as the session's command catalog.
 *
 * WHAT THIS GUARDS: that the composer is offered exactly what the runtime
 * answers to, in the runtime's order and words. The failure modes being
 * excluded are a nameless entry completing to a bare `/`, a repeated name
 * offered twice, and an empty description or argument hint stated as if the
 * vendor had said something.
 */
import { describe, expect, it } from "vitest";
import { commandCatalogOf, commandCatalogUpdate, sameSkillFiles, skillFilesOf } from "../../src/convert/command-catalog.js";
import type { ContextUsageLike, SlashCommandLike } from "../../src/sdk/types.js";
import { logRecordsSince, logSinkMark } from "../log-records.js";

/** No skill files known: the catalog before the vendor's context reading. */
const NO_FILES: ReadonlyMap<string, string> = new Map();

function command(name: string, extra: Partial<SlashCommandLike> = {}): SlashCommandLike {
  return { name, description: "", argumentHint: "", ...extra };
}

describe("the command catalog", () => {
  it("keeps every command in the vendor's order", () => {
    const catalog = commandCatalogOf([command("merge-queue"), command("compact"), command("plugin:skill")], NO_FILES);

    expect(catalog.commands.map((entry) => entry.name)).toEqual(["merge-queue", "compact", "plugin:skill"]);
  });

  it("states the vendor's description and argument hint", () => {
    const catalog = commandCatalogOf([
      command("compact", { description: "Compact the conversation", argumentHint: "[instructions]" }),
    ], NO_FILES);

    expect(catalog.commands[0]?.description).toBe("Compact the conversation");
    expect(catalog.commands[0]?.argumentHint).toBe("[instructions]");
  });

  it("leaves a blank description and argument hint unset", () => {
    const catalog = commandCatalogOf([command("status", { description: "  ", argumentHint: "" })], NO_FILES);

    expect(catalog.commands[0]?.description).toBeUndefined();
    expect(catalog.commands[0]?.argumentHint).toBeUndefined();
  });

  it("carries the vendor's aliases, dropping blank ones", () => {
    const catalog = commandCatalogOf([command("usage", { aliases: ["cost", " ", "stats"] })], NO_FILES);

    expect(catalog.commands[0]?.aliases).toEqual(["cost", "stats"]);
  });

  it("drops a nameless row and says so at warn", () => {
    const mark = logSinkMark();

    const catalog = commandCatalogOf([command("  ", { description: "nobody" }), command("clear")], NO_FILES);

    expect(catalog.commands.map((entry) => entry.name)).toEqual(["clear"]);
    expect(
      logRecordsSince(mark).some(
        (record) => record.level === "warn" && record.operation === "shim.convert.command_catalog",
      ),
    ).toBe(true);
  });

  it("offers a repeated name once, keeping the first row", () => {
    const catalog = commandCatalogOf([
      command("review", { description: "the project's" }),
      command("review", { description: "the user's" }),
    ], NO_FILES);

    expect(catalog.commands.map((entry) => entry.description)).toEqual(["the project's"]);
  });

  it("states an empty catalog for a runtime that offers nothing", () => {
    expect(commandCatalogOf([], NO_FILES).commands).toEqual([]);
  });
});

describe("the catalog's skill files", () => {
  /** A context reading whose skills are SKILLS. */
  function contextWith(skills: Array<{ name: string; path?: string }>): ContextUsageLike {
    return {
      skills: {
        totalSkills: skills.length,
        includedSkills: skills.length,
        tokens: 0,
        skillFrontmatter: skills.map((skill) => ({ source: "userSettings", tokens: 1, ...skill })),
      },
    } as unknown as ContextUsageLike;
  }

  it("reads each skill's file by the name it answers to", () => {
    const files = skillFilesOf(
      contextWith([
        { name: "ceac-resource-usage", path: "/w/.claude/skills/ceac-resource-usage/SKILL.md" },
        { name: "plugin:skill", path: "/p/skills/skill/SKILL.md" },
      ]),
    );

    expect([...files]).toEqual([
      ["ceac-resource-usage", "/w/.claude/skills/ceac-resource-usage/SKILL.md"],
      ["plugin:skill", "/p/skills/skill/SKILL.md"],
    ]);
  });

  it("names no file for a skill the vendor gave no path", () => {
    expect(skillFilesOf(contextWith([{ name: "dataviz" }, { name: "blank", path: " " }])).size).toBe(0);
  });

  it("keeps the first file of a name listed twice", () => {
    const files = skillFilesOf(contextWith([
      { name: "review", path: "/project/SKILL.md" },
      { name: "review", path: "/user/SKILL.md" },
    ]));

    expect(files.get("review")).toBe("/project/SKILL.md");
  });

  it.each([
    ["the same files", [["a", "/a"]], [["a", "/a"]], true],
    ["no files on either side", [], [], true],
    ["a file moved", [["a", "/a"]], [["a", "/b"]], false],
    ["a skill added", [["a", "/a"]], [["a", "/a"], ["b", "/b"]], false],
    ["a skill renamed", [["a", "/a"]], [["b", "/a"]], false],
  ] as const)("compares skill files: %s", (_name, a, b, same) => {
    expect(sameSkillFiles(new Map(a), new Map(b))).toBe(same);
  });

  it("names no file when the reading states no skills", () => {
    expect(skillFilesOf({} as unknown as ContextUsageLike).size).toBe(0);
  });

  it("states a command's file on its entry, and none on a command without one", () => {
    const files = new Map([["ceac-resource-usage", "/w/SKILL.md"]]);

    const catalog = commandCatalogOf([command("ceac-resource-usage"), command("compact")], files);

    expect(catalog.commands.map((entry) => entry.skillFile)).toEqual(["/w/SKILL.md", undefined]);
  });
});

describe("the catalog's session update", () => {
  it("rides the command_catalog arm", () => {
    const catalog = commandCatalogOf([command("clear")], NO_FILES);

    const update = commandCatalogUpdate(catalog);

    expect(update.update.case).toBe("commandCatalog");
    expect(update.update.value).toBe(catalog);
  });
});
