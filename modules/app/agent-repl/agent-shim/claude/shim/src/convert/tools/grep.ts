/**
 * convert/tools/grep.ts — the agent searched file CONTENTS for a pattern.
 *
 * # The query comes from the CALL, the answer from the RESULT
 *
 * `AgentGrepQuery` rides every frame of the unit — announcement and terminal
 * alike — because each frame of an upserted unit has to stand alone. None of it
 * is in the vendor's output, so all of it is read off the call's input.
 *
 * # Three answers, not three views of one
 *
 * The vendor's `mode` says which shape came back, and each shape carries only
 * what applies to it: a count knows no filenames, a filenames answer knows no
 * lines. When the vendor states no mode at all the INPUT'S OWN DEFAULT applies
 * — `output_mode` defaults to `files_with_matches` — rather than a guess.
 *
 * # The omitted figures are SUBTRACTED HERE, ONCE
 *
 * The vendor declares totals (`totalLines`, `totalFiles`); the proto carries
 * the OMITTED figure, because that is what a reader is shown. The shim does
 * that subtraction once so no consumer re-derives it, and clamps at zero so a
 * total that trails the returned count never becomes a negative "42 fewer".
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { arr, asRecord, bool, failureOf, settle, str, uint } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-tools", operation: "shim.convert.tools.grep" });

/** The vendor's own default output mode, applied when the result states none. */
const DEFAULT_MODE = "files_with_matches";

/** What was searched for, as the caller wrote it. */
function grepQuery(call: PendingCall, pattern: string): conversationv1.AgentGrepQuery {
  return create(conversationv1.AgentGrepQuerySchema, {
    pattern,
    // UNSET rather than "": the caller named no root and the performer chose
    // one, so a consumer draws no scope instead of inventing a default.
    path: str(call.input, "path"),
    glob: str(call.input, "glob"),
    fileType: str(call.input, "type"),
    caseInsensitive: bool(call.input, "-i") ?? false,
    multiline: bool(call.input, "multiline") ?? false,
  });
}

/** The pattern, which no query can be stated without. */
function requestedPattern(call: PendingCall): string | undefined {
  return str(call.input, "pattern");
}

/** How many were left out, never negative. */
function omitted(total: number | undefined, returned: number): number | undefined {
  if (total === undefined || total <= returned) return undefined;
  return total - returned;
}

/** The matching lines themselves. */
function contentMatches(
  call: PendingCall,
  record: Record<string, unknown>,
): conversationv1.AgentGrepSuccess["matches"] | undefined {
  const content = str(record, "content");
  const numLines = uint(record, "numLines");
  if (content === undefined || numLines === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a content-mode search stated no content or no line count; no success frame is produced",
    );
    return undefined;
  }
  const left = omitted(uint(record, "totalLines"), numLines);
  return {
    case: "content",
    value: create(conversationv1.AgentGrepContentSchema, {
      content,
      extent:
        left === undefined
          ? {
              case: "all",
              value: create(conversationv1.AgentGrepContentAllSchema, { linesReturned: numLines }),
            }
          : {
              case: "partial",
              value: create(conversationv1.AgentGrepContentPartialSchema, {
                linesReturned: numLines,
                linesOmitted: left,
              }),
            },
    }),
  };
}

/** Only the files that contained a match. */
function fileMatches(
  call: PendingCall,
  record: Record<string, unknown>,
): conversationv1.AgentGrepSuccess["matches"] | undefined {
  const numFiles = uint(record, "numFiles");
  if (numFiles === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a filenames-mode search stated no file count; no success frame is produced",
    );
    return undefined;
  }
  const paths = (arr(record, "filenames") ?? []).filter(
    (name): name is string => typeof name === "string",
  );
  const left = omitted(uint(record, "totalFiles"), numFiles);
  return {
    case: "files",
    value: create(conversationv1.AgentGrepFilesSchema, {
      paths,
      extent:
        left === undefined
          ? {
              case: "all",
              value: create(conversationv1.AgentGrepFilesAllSchema, { filesReturned: numFiles }),
            }
          : {
              case: "partial",
              value: create(conversationv1.AgentGrepFilesPartialSchema, {
                filesReturned: numFiles,
                filesOmitted: left,
              }),
            },
    }),
  };
}

/** Only how many matches there were. */
function countMatches(
  call: PendingCall,
  record: Record<string, unknown>,
): conversationv1.AgentGrepSuccess["matches"] | undefined {
  const matches = uint(record, "numMatches");
  if (matches === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a count-mode search stated no match count; no success frame is produced",
    );
    return undefined;
  }
  return {
    case: "count",
    value: create(conversationv1.AgentGrepCountSchema, { matches }),
  };
}

/** The search's `success` arm, or `undefined` when no shape can be stated. */
function grepSuccess(
  call: PendingCall,
  outcome: ToolOutcome,
  query: conversationv1.AgentGrepQuery,
): conversationv1.AgentGrepSuccess | undefined {
  const record = asRecord(outcome.structured);
  if (record === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a search settled with no typed output; no success frame is produced",
    );
    return undefined;
  }
  const mode = str(record, "mode") ?? DEFAULT_MODE;
  let matches: conversationv1.AgentGrepSuccess["matches"] | undefined;
  switch (mode) {
    case "content":
      matches = contentMatches(call, record);
      break;
    case "files_with_matches":
      matches = fileMatches(call, record);
      break;
    case "count":
      matches = countMatches(call, record);
      break;
    default:
      LOGGER.debug(
        { tool_use_id: call.toolUseId, mode },
        "a search stated an output mode this contract has no shape for; no success frame is produced",
      );
      return undefined;
  }
  if (matches === undefined) return undefined;
  return create(conversationv1.AgentGrepSuccessSchema, {
    query,
    matches,
    settledAt: settle(outcome),
  });
}

/** The agent searching file contents. */
export const grepConverter: ToolConverter = {
  kind: "grep",
  carriesProgress: true,

  start(call) {
    const pattern = requestedPattern(call);
    if (pattern === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a search was announced with no pattern; no start frame is produced",
      );
      return undefined;
    }
    return {
      case: "grep",
      value: create(conversationv1.AgentGrepSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentGrepStartSchema, {
            query: grepQuery(call, pattern),
            startedAt: startedAt(call.startedAtMs),
          }),
        },
      }),
    };
  },

  settle(call, outcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a search failed");
      // THE SETTLED FRAME STANDS ALONE: it restates what the call named, since
      // the start it upserts over is gone once it lands. A call that named
      // nothing has no start either, so it gets no failure frame, exactly as
      // it got no announcement.
      const pattern = requestedPattern(call);
      if (pattern === undefined) {
        LOGGER.debug(
          { tool_use_id: call.toolUseId },
          "a search failed with no pattern to restate; no failure frame is produced",
        );
        return undefined;
      }
      return {
        case: "grep",
        value: create(conversationv1.AgentGrepSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentGrepFailureSchema, {
              error: failureOf(outcome),
              query: grepQuery(call, pattern),
            }),
          },
        }),
      };
    }
    const pattern = requestedPattern(call);
    if (pattern === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a search settled with no pattern to restate; no success frame is produced",
      );
      return undefined;
    }
    const success = grepSuccess(call, outcome, grepQuery(call, pattern));
    if (success === undefined) return undefined;
    return {
      case: "grep",
      value: create(conversationv1.AgentGrepSchema, {
        result: { case: "success", value: success },
      }),
    };
  },

  progress(beat) {
    return {
      case: "grep",
      value: create(conversationv1.AgentGrepSchema, {
        result: { case: "progress", value: beat },
      }),
    };
  },
};
