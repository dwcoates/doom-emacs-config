/**
 * Projections of the user's own turns: the words the person actually
 * typed, whether those words were a `/clear`, and where the last `/clear`
 * left the conversation.
 *
 * Shared rather than render-local: the boundary the feed draws as a red
 * divider is the same boundary the topbar's subagent roster counts from,
 * so both read it from one place instead of each deciding for itself what
 * a `/clear` looks like.
 */
import { stripMetaSpans } from "./meta.js";
import { ContentBlock } from "./protocol.js";
import { ConversationItem, UserTurnItem } from "./store.js";

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

/**
 * A user turn's prompt text, delegating to `blocksToText` on its content —
 * both for the bubble and for `isClearTurn`.
 */
export function userTurnText(item: UserTurnItem): string {
  return blocksToText(item.content);
}

/**
 * Whether a user turn is the `/clear` command — the prompt that drops the
 * CLI's context and re-inits the session. Its bubble opens the feed after a
 * clear, carrying the context boundary rule beneath it, above the
 * re-initialized conversation (`system: init`, then a contextless reply).
 */
export function isClearTurn(item: UserTurnItem): boolean {
  return userTurnText(item).trim() === "/clear";
}

/**
 * Index of the last `/clear` turn in ITEMS, or -1 when the session never
 * cleared.
 *
 * The `/clear` turn is the cut rather than the `system: init` frame it
 * provokes, even though the store treats that init as the clear: a
 * RESUMED session emits an init of its own AFTER the transcript replay
 * (`SeedFromTranscript` runs before `Run`), and cutting there would throw
 * away a conversation the resumed context does still carry. The typed
 * `/clear` survives replay — the daemon collapses the transcript's
 * command envelope back to it precisely so a replayed session draws the
 * same boundary it drew live.
 */
function lastClearIndex(items: readonly ConversationItem[]): number {
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (item.kind === "user-turn" && isClearTurn(item)) return i;
  }
  return -1;
}

/**
 * The items the session's CURRENT context carries: everything after the
 * last `/clear`, or all of them when the session never cleared.
 *
 * Any projection that speaks for the CONTEXT (what the agent still knows)
 * cuts here; the `/clear` turn itself is chrome, not context, so it falls
 * outside the cut.
 */
export function itemsSinceClear(
  items: readonly ConversationItem[],
): readonly ConversationItem[] {
  const i = lastClearIndex(items);
  return i === -1 ? items : items.slice(i + 1);
}

/**
 * The items the feed still DRAWS: the last `/clear` turn and everything
 * after it, or all items when the session never cleared.
 *
 * A `/clear` clears the SCREEN as well as the context — the discarded
 * turns leave the feed entirely rather than lingering behind the divider —
 * so the feed opens on the `/clear` bubble, the boundary rule beneath it,
 * and the re-initialized conversation below. One item wider than
 * `itemsSinceClear`: the `/clear` turn stays visible as the record of the
 * cut, even though the context it names no longer carries it.
 */
export function itemsFromLastClear(
  items: readonly ConversationItem[],
): readonly ConversationItem[] {
  const i = lastClearIndex(items);
  return i === -1 ? items : items.slice(i);
}
