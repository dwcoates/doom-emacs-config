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
import { commandCatalogOf, commandCatalogUpdate } from "../../src/convert/command-catalog.js";
import type { SlashCommandLike } from "../../src/sdk/types.js";
import { logRecordsSince, logSinkMark } from "../log-records.js";

function command(name: string, extra: Partial<SlashCommandLike> = {}): SlashCommandLike {
  return { name, description: "", argumentHint: "", ...extra };
}

describe("the command catalog", () => {
  it("keeps every command in the vendor's order", () => {
    const catalog = commandCatalogOf([command("merge-queue"), command("compact"), command("plugin:skill")]);

    expect(catalog.commands.map((entry) => entry.name)).toEqual(["merge-queue", "compact", "plugin:skill"]);
  });

  it("states the vendor's description and argument hint", () => {
    const catalog = commandCatalogOf([
      command("compact", { description: "Compact the conversation", argumentHint: "[instructions]" }),
    ]);

    expect(catalog.commands[0]?.description).toBe("Compact the conversation");
    expect(catalog.commands[0]?.argumentHint).toBe("[instructions]");
  });

  it("leaves a blank description and argument hint unset", () => {
    const catalog = commandCatalogOf([command("status", { description: "  ", argumentHint: "" })]);

    expect(catalog.commands[0]?.description).toBeUndefined();
    expect(catalog.commands[0]?.argumentHint).toBeUndefined();
  });

  it("carries the vendor's aliases, dropping blank ones", () => {
    const catalog = commandCatalogOf([command("usage", { aliases: ["cost", " ", "stats"] })]);

    expect(catalog.commands[0]?.aliases).toEqual(["cost", "stats"]);
  });

  it("drops a nameless row and says so at warn", () => {
    const mark = logSinkMark();

    const catalog = commandCatalogOf([command("  ", { description: "nobody" }), command("clear")]);

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
    ]);

    expect(catalog.commands.map((entry) => entry.description)).toEqual(["the project's"]);
  });

  it("states an empty catalog for a runtime that offers nothing", () => {
    expect(commandCatalogOf([]).commands).toEqual([]);
  });
});

describe("the catalog's session update", () => {
  it("rides the command_catalog arm", () => {
    const catalog = commandCatalogOf([command("clear")]);

    const update = commandCatalogUpdate(catalog);

    expect(update.update.case).toBe("commandCatalog");
    expect(update.update.value).toBe(catalog);
  });
});
