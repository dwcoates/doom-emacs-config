/**
 * optional-equal — whether two optional protobuf messages say the same thing.
 *
 * An optional message field is unset (undefined) or a message. Two values
 * agree when both are unset, or both are set and equal by the schema's own
 * equality (`@bufbuild/protobuf` `equals`); a value set on one side only
 * never agrees. Every comparison of an optional message field goes through
 * here, so none forgets the unset case.
 */
import { equals, type DescMessage, type MessageShape } from "@bufbuild/protobuf";

/** Whether A and B, each a SCHEMA message or unset, are both unset or equal. */
export function sameOptional<D extends DescMessage>(schema: D, a: MessageShape<D> | undefined, b: MessageShape<D> | undefined): boolean {
  if (a === undefined || b === undefined) return a === b;
  return equals(schema, a, b);
}
