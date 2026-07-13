/**
 * Projections of the user's own turns: the words the person actually
 * typed, and whether those words were a `/clear`.
 *
 * Shared rather than render-local: the boundary the feed draws as a red
 * divider is the same boundary other views of the conversation reason
 * about, so all of them read it from one place instead of each deciding
 * for itself what a `/clear` looks like.
 */
import { stripMetaSpans } from "./meta.js";
import { UserTurnItem } from "./store.js";

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
