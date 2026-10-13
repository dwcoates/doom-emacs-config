/**
 * oneof — every arm of one oneof, read off a GENERATED SCHEMA.
 *
 * THE ONE READER. A component that must say something for every arm the
 * contract can send (a colour table, a refusal sentence, a status mark) takes
 * the arm list from here rather than typing it out, so an arm added to the
 * proto reaches every such table by itself, and the table's own row-for-row
 * check against the shared vocabulary is what then fails loudly.
 *
 * A schema with no such oneof is a programming error, never an empty list: an
 * empty list would quietly draw nothing for every arm.
 */
import type { DescMessage } from "@bufbuild/protobuf";
import { log } from "../log.js";

/** The generated (lowerCamel) names of SCHEMA's ONEOF arms, in declaration order. */
export function oneofArms(schema: DescMessage, oneof: string): readonly string[] {
  const found = schema.oneofs.find((o) => o.name === oneof);
  if (found === undefined) {
    log.error("a generated schema has no oneof by the name its reader asked for", {
      operation: "rpc.oneof.missing",
      context: { schema: schema.typeName, oneof },
    });
    throw new Error(`${schema.typeName} has no oneof named ${oneof}`);
  }
  return found.fields.map((field) => field.localName);
}
