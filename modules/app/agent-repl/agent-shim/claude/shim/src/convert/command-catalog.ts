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
import type { SlashCommandLike } from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-convert-command-catalog", operation: "shim.convert.command_catalog" });

/** A string the vendor stated, or absence when it stated nothing. */
function stated(text: string | undefined): string | undefined {
  if (text === undefined) return undefined;
  const trimmed = text.trim();
  return trimmed === "" ? undefined : trimmed;
}

/** One catalog entry from one vendor command row. */
function catalogEntry(command: SlashCommandLike): conversationv1.SessionCommandCatalogEntry {
  const description = stated(command.description);
  const argumentHint = stated(command.argumentHint);
  return create(conversationv1.SessionCommandCatalogEntrySchema, {
    name: command.name,
    ...(description === undefined ? {} : { description }),
    ...(argumentHint === undefined ? {} : { argumentHint }),
    aliases: (command.aliases ?? []).filter((alias) => alias.trim() !== ""),
  });
}

/**
 * The catalog the vendor's command rows state, in the vendor's order.
 *
 * A ROW THAT NAMES NO COMMAND IS NOT A COMMAND: `SessionCommandCatalogEntry.name`
 * is never empty, and nothing can complete to a bare `/`. A REPEATED NAME keeps
 * its first row, because names are unique within one statement and the vendor
 * lists the one it resolves first.
 */
export function commandCatalogOf(commands: readonly SlashCommandLike[]): conversationv1.SessionCommandCatalog {
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
    entries.push(catalogEntry(command));
  }
  return create(conversationv1.SessionCommandCatalogSchema, { commands: entries });
}

/** The session update that states CATALOG. */
export function commandCatalogUpdate(catalog: conversationv1.SessionCommandCatalog): conversationv1.SessionUpdate {
  return create(conversationv1.SessionUpdateSchema, {
    update: { case: "commandCatalog", value: catalog },
  });
}
