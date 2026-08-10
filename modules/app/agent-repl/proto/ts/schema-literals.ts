/**
 * The TypeScript half of reading the schema-carried literals back off the
 * generated descriptors.
 *
 * WHY THIS MODULE EXISTS. The values here are not traffic — no frame carries
 * them — but every process needs the same answer to them: the daemon to
 * recognize a submitted prompt as a session command, the frontends to spell
 * and label one, and everything downstream of the shim to tell the CLI's
 * `<synthetic>` marker apart from a real model id. They used to be restated
 * once per runtime (a Go table, a shim constant, the webapp's SESSION_COMMANDS
 * list and SESSION_COMMAND_LABELS table, and three inline comparisons), kept
 * aligned by review alone — the arrangement where one corrected spelling
 * leaves the rest stale and each surface is separately, quietly wrong.
 *
 * The `.proto` files now carry them as enum-value options and this module
 * reads them back. It contains NO copy of any literal: every string returned
 * comes out of the descriptor protoc-gen-es emitted, so a schema edit reaches
 * TypeScript with no TypeScript edit.
 *
 * WHY IT LIVES BESIDE `gen/`, NOT IN A PACKAGE. Both `webapp/src` and
 * `agent-shim/claude/shim/src` must read the same answers, and they already
 * share exactly one thing this way: the committed stubs under `proto/gen/ts`,
 * imported relatively by both. A copy in either package would recreate the
 * divergence the options were added to end.
 *
 * A MISSING OPTION THROWS RATHER THAN DEGRADING. These are build-time facts,
 * fixed when the stubs were generated: an absent option means the schema and
 * the bundle disagree, which no run-time branch can repair. Returning an empty
 * literal instead would hand a recognizer a string that matches nothing and a
 * picker a model nothing can spawn — the exact silent wrong answers this move
 * removes. Go's `internal/protocmd` fails the same way for the same reason.
 */

import { getOption, hasOption } from "@bufbuild/protobuf";
import {
  ModelMarker,
  ModelMarkerSchema,
  model_marker_literal,
} from "../gen/ts/agentshim/core/v1/core_pb.js";
import {
  SessionCommand,
  SessionCommandSchema,
  session_command_spec,
} from "../gen/ts/agentshim/frontend/v1/slash-menu_pb.js";

/** One session command's schema facts. */
export interface SessionCommandSpec {
  /**
   * The command as the user TYPES it, leading slash included, and equally the
   * form a reader is shown — one value so a corrected spelling cannot reach
   * the recognizer while a chip still renders the old one.
   */
  readonly literal: string;
  /**
   * Whether text following the name is an ARGUMENT rather than prose.
   *
   * FALSE IS THE SAFE SIDE: a command taking no argument is recognized only as
   * an ENTIRE prompt, so "/status of the build" stays a prompt and keeps its
   * bubble. Marking a command that takes none as taking some is the one way
   * the table can swallow something a user genuinely wrote.
   */
  readonly takesArgs: boolean;
}

/**
 * Every session command the schema names, keyed by its enum value.
 *
 * `SESSION_COMMAND_UNSPECIFIED` is absent, matching the schema: it names no
 * command, so it has no literal and nothing can match it. A caller iterating
 * this map iterates exactly the real commands, with no sentinel to skip.
 */
export function sessionCommandSpecs(): ReadonlyMap<SessionCommand, SessionCommandSpec> {
  const specs = new Map<SessionCommand, SessionCommandSpec>();
  for (const value of SessionCommandSchema.values) {
    if (value.number === SessionCommand.UNSPECIFIED) {
      continue;
    }
    if (!hasOption(value, session_command_spec)) {
      throw new Error(
        `schema-literals: ${value.name} carries no session_command_spec option; ` +
          "the schema and these generated stubs disagree",
      );
    }
    const spec = getOption(value, session_command_spec);
    if (spec.literal === "") {
      throw new Error(
        `schema-literals: ${value.name} session_command_spec carries an empty literal`,
      );
    }
    specs.set(value.number as SessionCommand, {
      literal: spec.literal,
      takesArgs: spec.takesArgs,
    });
  }
  return specs;
}

/**
 * The exact string the CLI reports when it is not running a real nameable
 * model.
 *
 * It is a MARKER, never an id: nothing can be spawned under it and no picker
 * may offer it, so every site that commits or displays a reported model must
 * compare against this value first.
 */
export function syntheticModelLiteral(): string {
  const value = ModelMarkerSchema.values.find((v) => v.number === ModelMarker.SYNTHETIC);
  if (value === undefined) {
    throw new Error("schema-literals: MODEL_MARKER_SYNTHETIC is absent from its own enum descriptor");
  }
  if (!hasOption(value, model_marker_literal)) {
    throw new Error(
      "schema-literals: MODEL_MARKER_SYNTHETIC carries no model_marker_literal option; " +
        "the schema and these generated stubs disagree",
    );
  }
  const literal = getOption(value, model_marker_literal);
  if (literal === "") {
    throw new Error("schema-literals: MODEL_MARKER_SYNTHETIC model_marker_literal option is empty");
  }
  return literal;
}

/**
 * A model value that HAS ALREADY BEEN NORMALIZED, carried as a type rather
 * than as a convention.
 *
 * THE RUNG THIS CLIMBS. A shared normalizer achieves agreement rather than
 * enforcement: every call site is expected to remember to call it, a new one
 * compiles perfectly well without doing so, and three sites in this codebase
 * had already drifted into hand-inlined `trim() === "<synthetic>"` comparisons
 * instead. A branded value inverts that — a consumer added later inherits the
 * rule without knowing it exists, and handing an arbitrary string to something
 * expecting a checked selection is a COMPILE ERROR rather than a lapse.
 *
 * The brand is a phantom property that exists only in the type system, so
 * there is no runtime cost and no wrapper to unwrap: a `SelectedModel` IS its
 * string wherever a plain string is wanted. What it is not is assignable FROM
 * one — `selectedModel()` is the only way in.
 */
export type SelectedModel = string & { readonly __selectedModel: unique symbol };

/**
 * THE constructor, and the only way to obtain a `SelectedModel`.
 *
 * It REFUSES rather than normalizing to absence, which is the difference
 * between this and Go's `registry.Model`: every TypeScript call site is a
 * point where a real selection was promised — a `SetModel` receipt, a picker
 * option, a `SessionView` snapshot — so an empty or placeholder value there is
 * a protocol violation by the producer, not an honest "pin nothing". The
 * daemon's side has an absent-model case to represent; the frontend's does
 * not, and a picker rendering the marker as a selectable option was the
 * concrete failure.
 *
 * @param where names the field being decoded, so the throw says which
 *   producer violated the contract rather than only that one did.
 */
export function selectedModel(raw: string, where: string): SelectedModel {
  const trimmed = raw.trim();
  if (trimmed === "" || trimmed === syntheticModelLiteral()) {
    throw new Error(
      `schema-literals: ${where} is absent, empty, or the ${syntheticModelLiteral()} marker, none of which names a model`,
    );
  }
  return raw as SelectedModel;
}
