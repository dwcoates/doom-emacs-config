import type { DescMessage } from "@bufbuild/protobuf";

/**
 * Every arm of one oneof, read off the GENERATED SCHEMA rather than a list
 * written out by hand.
 *
 * A refusal suite's job is to prove the call site says something for every arm
 * the contract can send. Enumerating from the schema is what makes an arm added
 * to the proto fail the existing tests instead of quietly going undrawn.
 */
export function oneofArms(schema: DescMessage, oneof: string): readonly string[] {
  const found = schema.oneofs.find((o) => o.name === oneof);
  if (found === undefined) throw new Error(`${schema.typeName} has no oneof named ${oneof}`);
  return found.fields.map((field) => field.localName);
}
