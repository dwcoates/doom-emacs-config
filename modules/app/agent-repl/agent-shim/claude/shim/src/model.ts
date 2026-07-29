/** Model-value normalization shared by shim launch, control, and stream data. */

export const SYNTHETIC_MODEL = "<synthetic>";

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
