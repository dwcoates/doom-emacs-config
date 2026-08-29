/**
 * strict — the four refusals every renderer reaches for, and the deep
 * unknown-field check every received message passes through first.
 *
 * TYPED ARMS, NO FALLBACKS. The contract is a tree of closed oneofs and
 * non-optional message fields; a view that leaves one of them unset says
 * something this build cannot draw. Rather than defaulting (which renders a
 * different, plausible-looking thing and hides the producer's bug), every such
 * reading goes through a helper here that throws `MalformedView` with the
 * field path.
 *
 * WHY UNKNOWN FIELDS ARE A REFUSAL. protobuf-es preserves fields it has no
 * descriptor for instead of discarding them, so a daemon built against a NEWER
 * schema reaches an older webapp bundle as a view whose extra facts are
 * invisible to the renderer — a screen that is silently wrong, which is the
 * exact failure `stale_bundle` and `frame_undecodable` exist to make loud. The
 * check is DEEP because the new field is far more likely to sit on a nested
 * component (a footer's activity arm, a feed row's card) than on the response
 * envelope.
 */
import type { DescField, DescMessage, Message } from "@bufbuild/protobuf";
import { reflect, type ReflectMessage } from "@bufbuild/protobuf/reflect";
import { MalformedView } from "./malformed.js";

/**
 * Refuse MSG when it, or anything reachable from it, carries a field this
 * build has no descriptor for.
 *
 * The walk covers singular message fields, repeated message fields, message
 * map values, and oneof arms — a oneof member is an ordinary field in the
 * reflection model and is `isSet` only when it is the selected case, so
 * iterating the field list covers oneofs without a second pass.
 *
 * `check` is disabled on the reflection: the message came off the wire through
 * the generated schema, so its field values are already well-typed, and the
 * per-field validation would be a second full walk for nothing.
 */
export function assertNoUnknownFields(schema: DescMessage, msg: Message): void {
  walk(reflect(schema, msg as never, false), schema.typeName);
}

function walk(node: ReflectMessage, path: string): void {
  const unknown = node.getUnknown();
  if (unknown !== undefined && unknown.length > 0) {
    const numbers = unknown.map((field) => field.no).join(", ");
    throw new MalformedView(
      path,
      `carries ${unknown.length} field number(s) this build has no descriptor for (${numbers}); the daemon is newer than this bundle`,
    );
  }
  for (const field of node.fields) {
    if (!node.isSet(field)) continue;
    walkField(node, field, `${path}.${field.name}`);
  }
}

function walkField(node: ReflectMessage, field: DescField, path: string): void {
  switch (field.fieldKind) {
    case "message":
      walk(node.get(field) as ReflectMessage, path);
      return;
    case "list": {
      if (field.listKind !== "message") return;
      let index = 0;
      for (const item of node.get(field) as Iterable<ReflectMessage>) {
        walk(item, `${path}[${index}]`);
        index += 1;
      }
      return;
    }
    case "map": {
      if (field.mapKind !== "message") return;
      const map = node.get(field) as { entries(): Iterable<[unknown, ReflectMessage]> };
      for (const [key, value] of map.entries()) walk(value, `${path}[${String(key)}]`);
      return;
    }
    case "scalar":
    case "enum":
      return;
  }
}

/**
 * The value of a message-typed field the contract declares non-optional.
 *
 * A `T | undefined` in the generated code is protobuf's message presence, not
 * an "absent is fine" signal: the proto comment says the producer always sets
 * it, so an absent one is the producer's bug and drawing around it would hide
 * that. `optional` fields are read directly instead — absent means draw
 * nothing, and no helper is involved.
 */
export function requireMessage<T>(value: T | undefined, path: string): T {
  if (value === undefined || value === null) {
    throw new MalformedView(path, "a non-optional message field is unset");
  }
  return value;
}

/**
 * The selected arm of a oneof the contract declares always-set.
 *
 * The generated oneof is `{ case: undefined; value?: undefined } | {case: "x";
 * value: X} | …`, so narrowing to `case: string` here is what lets the caller
 * switch exhaustively with `unreachableArm` in the default.
 */
export function requireCase<T extends { case?: string | undefined }>(
  oneof: T,
  path: string,
): T & { case: string } {
  if (oneof === undefined || oneof === null || oneof.case === undefined) {
    throw new MalformedView(path, "a oneof sets no arm");
  }
  return oneof as T & { case: string };
}

/**
 * The `default:` of an exhaustive arm switch.
 *
 * The `never` parameter is the compile-time half: adding an arm to a proto
 * breaks the build at every switch that does not handle it. The throw is the
 * run-time half, for an arm a NEWER daemon set that this bundle's descriptors
 * do not name.
 */
export function unreachableArm(path: string, arm: never | string): never {
  throw new MalformedView(path, `arm '${String(arm)}' is not one this build can draw`);
}

/**
 * An `int64` instant as the milliseconds number the clock and the formatters
 * take.
 *
 * The wire ships instants as `bigint`, and `Number` silently rounds past
 * 2^53 — a rounded instant is a clock that is wrong by an unbounded amount
 * with nothing to show for it, so an out-of-range value is refused instead.
 */
export function msOf(v: bigint, path: string): number {
  if (v > BigInt(Number.MAX_SAFE_INTEGER) || v < -BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new MalformedView(path, `instant ${v.toString()} does not fit a JS safe integer`);
  }
  return Number(v);
}
