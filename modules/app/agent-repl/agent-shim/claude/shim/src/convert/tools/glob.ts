/**
 * convert/tools/glob.ts — the agent matched file PATHS against a pattern.
 *
 * # Two different claims about what was left out
 *
 * `truncated` says the list stopped short. `totalMatches` minus the returned
 * count is the omitted figure — but `countIsComplete === false` means the
 * underlying search capped its OWN counting, so that figure is a FLOOR. The
 * proto keeps the two apart as arms precisely so no consumer draws a floor as a
 * total: "42 more" and "at least 42 more" are different claims.
 *
 * A CLI old enough to persist neither `totalMatches` nor `countIsComplete`
 * leaves the shim knowing only that the list stopped short. `at least 0 more`
 * is the honest reading of that — trivially true and never overstating — so
 * that is what is recorded, with a log line saying the figure was unavailable.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { arr, asRecord, bool, failureOf, settle, str, uint } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-tools", operation: "shim.convert.tools.glob" });

/** What was matched against, as the caller wrote it. */
function globQuery(call: PendingCall, pattern: string): conversationv1.AgentGlobQuery {
  return create(conversationv1.AgentGlobQuerySchema, {
    pattern,
    // UNSET when the caller named no root: the performer chose one and this
    // producer does not know which.
    path: str(call.input, "path"),
  });
}

/** The pattern, which no query can be stated without. */
function requestedPattern(call: PendingCall): string | undefined {
  return str(call.input, "pattern");
}

/** How much was left out, and whether that figure can be trusted as exact. */
function omittedArm(
  call: PendingCall,
  record: Record<string, unknown>,
  numFiles: number,
): conversationv1.AgentGlobPartial["omitted"] {
  const total = uint(record, "totalMatches");
  if (total === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a truncated match stated no total; only a floor of zero omitted can be claimed",
    );
    return {
      case: "atLeast",
      value: create(conversationv1.AgentGlobOmittedAtLeastSchema, { filesOmittedAtLeast: 0 }),
    };
  }
  const left = total <= numFiles ? 0 : total - numFiles;
  if (bool(record, "countIsComplete") === false) {
    return {
      case: "atLeast",
      value: create(conversationv1.AgentGlobOmittedAtLeastSchema, { filesOmittedAtLeast: left }),
    };
  }
  return {
    case: "exact",
    value: create(conversationv1.AgentGlobOmittedExactSchema, { filesOmitted: left }),
  };
}

/** The walk's `success` arm, or `undefined` when the count is unstatable. */
function globSuccess(
  call: PendingCall,
  outcome: ToolOutcome,
  query: conversationv1.AgentGlobQuery,
): conversationv1.AgentGlobSuccess | undefined {
  const record = asRecord(outcome.structured);
  if (record === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a match settled with no typed output; no success frame is produced",
    );
    return undefined;
  }
  const numFiles = uint(record, "numFiles");
  if (numFiles === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a match stated no returned-file count; no success frame is produced",
    );
    return undefined;
  }
  const paths = (arr(record, "filenames") ?? []).filter(
    (name): name is string => typeof name === "string",
  );
  const extent: conversationv1.AgentGlobSuccess["extent"] =
    bool(record, "truncated") === true
      ? {
          case: "partial",
          value: create(conversationv1.AgentGlobPartialSchema, {
            filesReturned: numFiles,
            omitted: omittedArm(call, record, numFiles),
          }),
        }
      : {
          case: "all",
          value: create(conversationv1.AgentGlobAllSchema, { filesReturned: numFiles }),
        };
  return create(conversationv1.AgentGlobSuccessSchema, {
    query,
    paths,
    extent,
    settledAt: settle(call, outcome),
  });
}

/** The agent matching file paths. */
export const globConverter: ToolConverter = {
  kind: "glob",
  carriesProgress: true,

  start(call) {
    const pattern = requestedPattern(call);
    if (pattern === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a match was announced with no pattern; no start frame is produced",
      );
      return undefined;
    }
    return {
      case: "glob",
      value: create(conversationv1.AgentGlobSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentGlobStartSchema, {
            query: globQuery(call, pattern),
            startedAt: startedAt(call.startedAtMs),
          }),
        },
      }),
    };
  },

  settle(call, outcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a match failed");
      // THE SETTLED FRAME STANDS ALONE: it restates what the call named, since
      // the start it upserts over is gone once it lands. A call that named
      // nothing has no start either, so it gets no failure frame, exactly as
      // it got no announcement.
      const pattern = requestedPattern(call);
      if (pattern === undefined) {
        LOGGER.debug(
          { tool_use_id: call.toolUseId },
          "a match failed with no pattern to restate; no failure frame is produced",
        );
        return undefined;
      }
      return {
        case: "glob",
        value: create(conversationv1.AgentGlobSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentGlobFailureSchema, {
              error: failureOf(call, outcome),
              query: globQuery(call, pattern),
            }),
          },
        }),
      };
    }
    const pattern = requestedPattern(call);
    if (pattern === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a match settled with no pattern to restate; no success frame is produced",
      );
      return undefined;
    }
    const success = globSuccess(call, outcome, globQuery(call, pattern));
    if (success === undefined) return undefined;
    return {
      case: "glob",
      value: create(conversationv1.AgentGlobSchema, {
        result: { case: "success", value: success },
      }),
    };
  },

  progress(beat) {
    return {
      case: "glob",
      value: create(conversationv1.AgentGlobSchema, {
        result: { case: "progress", value: beat },
      }),
    };
  },
};
