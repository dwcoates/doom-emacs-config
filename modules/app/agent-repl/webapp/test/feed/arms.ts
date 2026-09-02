/**
 * arms — one oneof's arm names, read off the generated schema.
 *
 * NOT A SUITE. It exists so a suite can enumerate a message's arms FROM THE
 * CONTRACT rather than from a list a reviewer would have to keep in step: an
 * arm a later landing adds then fails the suite loudly instead of silently
 * going undrawn.
 */

/** The proto field names of one oneof, as generated arm case names. */
export function armsOf(
  oneofs: readonly { name: string; fields: readonly { name: string }[] }[],
  name: string,
): string[] {
  const oneof = oneofs.find((o) => o.name === name);
  if (oneof === undefined) throw new Error(`no oneof named ${name}`);
  return oneof.fields.map((f) => f.name.replace(/_([a-z])/g, (_m, c: string) => c.toUpperCase()));
}
