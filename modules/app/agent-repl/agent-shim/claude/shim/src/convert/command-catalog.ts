/**
 * convert/command-catalog.ts — the vendor's `supportedCommands()` answer, as
 * the session's command catalog.
 *
 * THE RUNTIME IS THE AUTHORITY. The list is whatever the live CLI answers to
 * as `/<name>`: its built-ins and every skill, custom command, plugin command
 * and MCP prompt its settings sources loaded. Nothing here adds, filters by
 * kind, or re-derives one from disk; the only entries dropped are the ones the
 * wire contract cannot carry (a nameless row, a repeated name).
 *
 * A converter does no I/O: `engine/session.ts` owns the read and the push.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { ContextUsageLike, SlashCommandLike } from "../sdk/types.js";

const LOGGER = bindLog("shim.convert.command-catalog", { component: "shim-convert-command-catalog", operation: "shim.convert.command_catalog" });

/** A string the vendor stated, or absence when it stated nothing. */
function stated(text: string | undefined): string | undefined {
  if (text === undefined) return undefined;
  const trimmed = text.trim();
  return trimmed === "" ? undefined : trimmed;
}

/**
 * The `SKILL.md` of every skill the vendor loaded, by the name it answers to.
 *
 * THE VENDOR'S CONTEXT READING IS WHERE IT SAYS WHICH FILE A SKILL IS. Its
 * command rows name no file, and its `skills.skillFrontmatter` rows name each
 * loaded skill under the same name the command rows use (a plugin's as
 * `plugin:skill`), with the path of the file it resolved that name to. A row
 * with no path (a built-in skill) names no file.
 */
export type SkillFiles = ReadonlyMap<string, string>;

/** The skill files CONTEXT states; none when it states no skills. */
export function skillFilesOf(context: ContextUsageLike): SkillFiles {
  const files = new Map<string, string>();
  for (const skill of context.skills?.skillFrontmatter ?? []) {
    const file = stated(skill.path);
    if (file !== undefined && !files.has(skill.name)) files.set(skill.name, file);
  }
  return files;
}

/** Whether A and B name the same file for every skill. */
export function sameSkillFiles(a: SkillFiles, b: SkillFiles): boolean {
  if (a.size !== b.size) return false;
  for (const [name, file] of a) if (b.get(name) !== file) return false;
  return true;
}

/** One catalog entry from one vendor command row. */
function catalogEntry(command: SlashCommandLike, skillFiles: SkillFiles): conversationv1.SessionCommandCatalogEntry {
  const description = stated(command.description);
  const argumentHint = stated(command.argumentHint);
  const skillFile = skillFiles.get(command.name);
  return create(conversationv1.SessionCommandCatalogEntrySchema, {
    name: command.name,
    ...(description === undefined ? {} : { description }),
    ...(argumentHint === undefined ? {} : { argumentHint }),
    ...(skillFile === undefined ? {} : { skillFile }),
    aliases: (command.aliases ?? []).filter((alias) => alias.trim() !== ""),
  });
}

/**
 * The catalog the vendor's command rows state, in the vendor's order, each
 * skill with the file SKILL_FILES names for it.
 *
 * A ROW THAT NAMES NO COMMAND IS NOT A COMMAND: `SessionCommandCatalogEntry.name`
 * is never empty, and nothing can complete to a bare `/`. A REPEATED NAME keeps
 * its first row, because names are unique within one statement and the vendor
 * lists the one it resolves first.
 */
export function commandCatalogOf(
  commands: readonly SlashCommandLike[],
  skillFiles: SkillFiles,
): conversationv1.SessionCommandCatalog {
  const seen = new Set<string>();
  const entries: conversationv1.SessionCommandCatalogEntry[] = [];
  for (const command of commands) {
    if (command.name.trim() === "") {
      // A NAMELESS ROW cannot ride the wire, whose entry name is never empty.
      // warn: a defect because the vendor stated a command row with no name.
      LOGGER.warn(
        { description: command.description, argument_hint: command.argumentHint },
        "the vendor listed a slash command with no name; it is not offered",
      );
      continue;
    }
    if (seen.has(command.name)) {
      LOGGER.debug({ name: command.name }, "the vendor listed a slash command name twice; the first row is offered");
      continue;
    }
    seen.add(command.name);
    entries.push(catalogEntry(command, skillFiles));
  }
  return create(conversationv1.SessionCommandCatalogSchema, { commands: entries });
}

/** The session update that states CATALOG. */
export function commandCatalogUpdate(catalog: conversationv1.SessionCommandCatalog): conversationv1.SessionUpdate {
  return create(conversationv1.SessionUpdateSchema, {
    update: { case: "commandCatalog", value: catalog },
  });
}
