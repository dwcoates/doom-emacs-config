/**
 * convert/effort.ts — the vendor's effort-level spelling onto the one canonical
 * vocabulary (`conversation.v1.AgentEffortLevel`).
 *
 * THE ONE MAPPING. A review's report, a model catalog's accepted levels and
 * every other place the vendor names an effort level read it through here, so
 * a level the vendor adds lands in one switch rather than drifting between
 * near-duplicate tables.
 */
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { AppliedSettingsLike, EffortLevelLike } from "../sdk/types.js";

const LOGGER = bindLog({
  component: "shim-convert-effort",
  operation: "shim.convert.effort",
});

/** The vendor's effort level, in the one canonical vocabulary. */
export function effortLevelOf(level: string | undefined): conversationv1.AgentEffortLevel {
  switch (level) {
    case "low":
      return conversationv1.AgentEffortLevel.LOW;
    case "medium":
      return conversationv1.AgentEffortLevel.MEDIUM;
    case "high":
      return conversationv1.AgentEffortLevel.HIGH;
    case "xhigh":
      return conversationv1.AgentEffortLevel.XHIGH;
    case "max":
      return conversationv1.AgentEffortLevel.MAX;
    case undefined:
      return conversationv1.AgentEffortLevel.UNSPECIFIED;
    default:
      LOGGER.debug(
        { effort: level },
        "the vendor named an effort level this vocabulary has no value for; the level is left unspecified",
      );
      return conversationv1.AgentEffortLevel.UNSPECIFIED;
  }
}

/**
 * The canonical level in the vendor's spelling — the inverse of
 * {@link effortLevelOf}, for the one direction the shim ASKS the vendor for a
 * level. UNSPECIFIED is never asked for: every request carrying one was
 * refused at validation, so reaching here with it is a defect.
 */
export function vendorEffortLevel(level: conversationv1.AgentEffortLevel): EffortLevelLike {
  switch (level) {
    case conversationv1.AgentEffortLevel.LOW:
      return "low";
    case conversationv1.AgentEffortLevel.MEDIUM:
      return "medium";
    case conversationv1.AgentEffortLevel.HIGH:
      return "high";
    case conversationv1.AgentEffortLevel.XHIGH:
      return "xhigh";
    case conversationv1.AgentEffortLevel.MAX:
      return "max";
    default:
      throw new Error(`shim effort: AgentEffortLevel ${level} has no vendor spelling; validation admits only named levels`);
  }
}

/**
 * The level the vendor states its next request sends, read off its settings
 * answer, or `undefined` when it states none (`applied.effort: null`). An
 * answer missing the field, or naming a level this vocabulary has no value
 * for, is a vendor change and throws: a guessed level is never stated.
 */
export function appliedEffortOf(settings: AppliedSettingsLike): conversationv1.AgentEffortLevel | undefined {
  const applied = (settings as { applied?: { effort?: unknown } }).applied;
  if (applied === undefined || !("effort" in applied)) {
    throw new Error("the vendor's settings answer carries no applied.effort");
  }
  const effort = applied.effort;
  if (effort === null) return undefined;
  const level = typeof effort === "string" ? effortLevelOf(effort) : conversationv1.AgentEffortLevel.UNSPECIFIED;
  if (level === conversationv1.AgentEffortLevel.UNSPECIFIED) {
    throw new Error(`the vendor's applied.effort is ${JSON.stringify(effort)}, which names no known level`);
  }
  return level;
}
