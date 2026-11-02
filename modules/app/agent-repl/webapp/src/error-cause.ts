/**
 * The cause of a thrown value, as a record or a failure carries it: an
 * Error's name and message, and any other value as its string.
 */
export function errorCause(err: unknown): string {
  return err instanceof Error ? `${err.name}: ${err.message}` : String(err);
}
