/**
 * Projections of the user's own turns: the words the person actually typed,
 * with the host's injected spans stripped.
 *
 * Where a conversation's history STOPS informing the agent is not decided
 * here — a clear and a compaction are typed events now
 * (`clear-compact.ts`), not
 * prompt text this end recognizes by its spelling.
 */
import { stripMetaSpans } from "./meta.js";
import { ContentBlock } from "./protocol.js";
import type { UserTurnItem } from "./store.js";

/**
 * Prompt text of a content-block list, non-text blocks standing in as
 * `[kind]`. The host's injected spans (the metaprompt read-directive, the
 * workspace-generation preamble and wrap-up gate) are marked at their
 * injection site and dropped here. Shared by user turns and queued
 * messages (§2.13), so a parked message reads as the user's own words
 * exactly as a live turn does.
 */
export function blocksToText(content: ContentBlock[]): string {
  return stripMetaSpans(
    content
      .map((b) => (b.type === "text" ? String((b as { text: string }).text) : `[${b.type}]`))
      .join("\n"),
  );
}

/** A user turn's prompt text, delegating to `blocksToText` on its content. */
export function userTurnText(item: UserTurnItem): string {
  return blocksToText(item.content);
}
