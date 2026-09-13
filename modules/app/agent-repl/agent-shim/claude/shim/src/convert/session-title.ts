/**
 * convert/session-title.ts — the vendor's `ai-title` line, decoded.
 *
 * THE VENDOR SUMMARIZES ITS OWN CONVERSATION. Somewhere after the first
 * exchange it appends an UNCHAINED transcript line of its own:
 *
 *     {"type":"ai-title","aiTitle":"Add SPC j keybinding support","sessionId":…}
 *
 * It carries no uuid and no parentUuid, it does not advance the record chain,
 * and it is restated as the conversation moves on — the LAST one is the title.
 * That sentence is the best answer a strip can give to "which conversation is
 * this", which is why the topbar draws it in place of the workspace name
 * (owner ruling, 2026-09-13).
 *
 * IT IS A LINE SHAPE, NOT AN SDK MESSAGE. The vendor states the title only on
 * the FILE plane; nothing on the stream announces it. So this module decodes
 * the line and mints the update, and `engine/title.ts` owns the reading of the
 * bytes — a converter does no I/O.
 *
 * THE SIDECAR IS UNAFFECTED. It classifies `ai-title` as a WITHHELD kind
 * (shim-sidecar internal/convert/convert.go) — CLI bookkeeping that must never
 * become a feed row — and that stays true: this is a session FACT pushed on
 * WatchSession, never a row, and it is never written to the store.
 */
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../proto.js";

/** The transcript line type the vendor writes its summary as. */
export const AI_TITLE_LINE_TYPE = "ai-title";

/**
 * The title one raw transcript line states, or absence.
 *
 * A LINE THAT DOES NOT PARSE IS ABSENCE, not a throw: the transcript is
 * appended to by a live process, so the last line of any read can be a partial
 * write, and one torn byte must not cost a turn its title.
 */
export function aiTitleOf(raw: string): string | undefined {
  const line = raw.trim();
  if (line === "") return undefined;
  let record: { type?: unknown; aiTitle?: unknown };
  try {
    record = JSON.parse(line) as { type?: unknown; aiTitle?: unknown };
  } catch {
    return undefined;
  }
  if (record.type !== AI_TITLE_LINE_TYPE) return undefined;
  if (typeof record.aiTitle !== "string") return undefined;
  const title = record.aiTitle.trim();
  // AN EMPTY TITLE IS NOT A TITLE. `SessionTitle.text` is documented as never
  // empty, and a blank strip title is strictly worse than the workspace name
  // it would replace.
  return title === "" ? undefined : title;
}

/** The LAST title stated across a chunk of complete transcript lines. */
export function lastAiTitle(chunk: string): string | undefined {
  let found: string | undefined;
  for (const raw of chunk.split("\n")) {
    const title = aiTitleOf(raw);
    if (title !== undefined) found = title;
  }
  return found;
}

/** The session fact, as the daemon receives it. */
export function sessionTitleUpdate(text: string): conversationv1.SessionUpdate {
  return create(conversationv1.SessionUpdateSchema, {
    update: {
      case: "title",
      value: create(conversationv1.SessionTitleSchema, { text }),
    },
  });
}
