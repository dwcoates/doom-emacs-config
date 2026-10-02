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
