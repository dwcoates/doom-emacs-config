/** Model-value normalization shared by shim launch, control, and stream data. */

import { syntheticModelLiteral } from "../../../../proto/ts/schema-literals.js";

/**
 * Claude's marker for "not running a real, nameable model".
 *
 * READ OFF THE SCHEMA, NEVER SPELLED HERE. This was one of six independent
 * declarations of the same literal across Go, this shim, and three inline
 * webapp comparisons, kept aligned only by review — so a vendor that renamed
 * the marker would have been adopted in some and not the others, and the ones
 * left behind would have begun treating the placeholder as a selectable model.
 * The MODEL_MARKER_SYNTHETIC enum value's option is now the one definition.
 *
 * Resolved once at module load: the value is fixed when the stubs are
 * generated, so a schema that disagrees with them is a broken build rather
 * than a runtime condition.
 */
export const SYNTHETIC_MODEL = syntheticModelLiteral();

/**
 * Return the canonical model representation carried inside the shim.
 *
 * Empty is the canonical "no model override" value. Claude's synthetic marker
 * means exactly the same thing and must never reach SDK options or outgoing
 * authoritative model fields as though it named a model.
 */
export function normalizeModel(model: string): string {
  return model.trim() === SYNTHETIC_MODEL ? "" : model;
}

/** Convert an optional launch model into an SDK-option value, or absence. */
export function normalizeOptionalModel(model: string | undefined): string | undefined {
  if (model === undefined) return undefined;
  const normalized = normalizeModel(model);
  return normalized === "" ? undefined : normalized;
}
