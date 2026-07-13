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
import { ConversationItem, UserTurnItem } from "./store.js";

/**
 * A user turn's prompt text, non-text blocks standing in as `[kind]`.
 *
 * The host's injected spans (the metaprompt read-directive, the
 * workspace-generation preamble and wrap-up gate) are marked at their
 * injection site and dropped here: a user turn reads as the user's own
 * words, both in the bubble and to `isClearTurn`.
 */
export function userTurnText(item: UserTurnItem): string {
  return stripMetaSpans(
    item.content
      .map((b) => (b.type === "text" ? String((b as { text: string }).text) : `[${b.type}]`))
      .join("\n"),
  );
}

/**
 * Whether a user turn is the `/clear` command — the prompt that drops the
 * CLI's context and re-inits the session. Its bubble carries the context
 * boundary rule beneath it, splitting the discarded conversation above from
 * the re-initialized one (`system: init`, then a contextless reply) below.
 */
export function isClearTurn(item: UserTurnItem): boolean {
  return userTurnText(item).trim() === "/clear";
}

/**
 * The items the session's CURRENT context carries: everything after the
 * last `/clear`, or all of them when the session never cleared.
 *
 * The feed keeps the discarded turns on screen behind the divider, so any
 * projection that speaks for the CONTEXT (what the agent still knows)
 * rather than the SCROLLBACK (what the user can still scroll to) has to
 * cut them here.
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
export function itemsSinceClear(
  items: readonly ConversationItem[],
): readonly ConversationItem[] {
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (item.kind === "user-turn" && isClearTurn(item)) return items.slice(i + 1);
  }
  return items;
}
