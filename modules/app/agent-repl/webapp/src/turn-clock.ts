/**
 * turn-clock — the counted-turn clock the topbar counters age against.
 *
 * A counter's retention window is measured in "turns", where a turn is a
 * user prompt that actually drives the agent. A bare built-in slash
 * command (`/clear`, `/model`, …) is chrome, not a prompt, so it does not
 * advance the clock. A skill invocation (`/some-skill`) IS a prompt and
 * does advance it.
 *
 * KNOWN LIMITATION: the webapp has no skill registry, so "is this slash a
 * skill?" is answered by a denylist of the built-in commands below rather
 * than by an authoritative signal. A built-in not in the list advances the
 * clock (harmless over-count); a skill is never in the list, so a skill is
 * never wrongly dropped from the clock. A future host/daemon signal should
 * replace the denylist.
 */
import { stripMetaSpans } from "./meta.js";
import { ConversationItem, UserTurnItem } from "./store.js";

/**
 * Built-in slash commands that are NOT skills. A user turn that is exactly
 * one of these (bare, no prose) is chrome and does not advance the clock.
 */
export const NON_SKILL_SLASH_COMMANDS: ReadonlySet<string> = new Set([
  "clear",
  "compact",
  "config",
  "cost",
  "help",
  "model",
  "login",
  "logout",
  "status",
  "doctor",
  "exit",
  "quit",
  "permissions",
  "vim",
  "release-notes",
  "bug",
  "mcp",
  "memory",
  "resume",
  "terminal-setup",
  "agents",
  "add-dir",
  "hooks",
  "export",
  "upgrade",
  "privacy-settings",
]);

/** A user turn's own words: text blocks joined, host-injected spans removed. */
function userTurnPlainText(item: UserTurnItem): string {
  return stripMetaSpans(
    item.content
      .map((b) => (b.type === "text" ? String((b as { text: string }).text) : ""))
      .join("\n"),
  ).trim();
}

/**
 * Whether a user turn advances the clock: every turn except one whose text
 * is a bare non-skill slash command. A slash command carrying trailing
 * prose (`/model give me opus`) is treated as a real prompt.
 */
export function isCountedTurn(item: UserTurnItem): boolean {
  const text = userTurnPlainText(item);
  if (!text.startsWith("/")) return true;
  if (/\s/.test(text)) return true;
  return !NON_SKILL_SLASH_COMMANDS.has(text.slice(1));
}

/**
 * The counted-turn ordinal through `end` (inclusive), i.e. how many
 * counted user turns have appeared in `items[0..end]`. With `end` omitted
 * it is the session's current turn.
 */
export function countedTurns(
  items: readonly ConversationItem[],
  end: number = items.length - 1,
): number {
  let n = 0;
  const last = Math.min(end, items.length - 1);
  for (let i = 0; i <= last; i++) {
    const item = items[i];
    if (item.kind === "user-turn" && isCountedTurn(item)) n++;
  }
  return n;
}
