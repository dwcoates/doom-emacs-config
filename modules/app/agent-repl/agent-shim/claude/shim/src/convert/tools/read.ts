/**
 * convert/tools/read.ts — the agent read a file.
 *
 * # Where the extent comes from
 *
 * The vendor states WHAT CAME BACK (`content`, `startLine`, `numLines`,
 * `totalLines`, `truncatedByTokenCap`); the CALLER states WHAT WAS ASKED FOR
 * (`offset`, `limit`). The proto's extent arm is a fact about both, so this
 * converter reads both:
 *
 *   - `truncatedByTokenCap` — the vendor auto-paginated a whole-file read, so
 *     the answer is a HEAD cut at a token budget and its last line may be
 *     incomplete.
 *   - an `offset` in the input — the caller asked for a MIDDLE SLICE, and the
 *     answer is a RANGE that states where it begins.
 *   - a `limit` alone — the read began at line 1 and stopped at a line budget:
 *     a HEAD cut on a line boundary.
 *   - neither, and nothing left over — the file came back WHOLE.
 *
 * # The extents that do not exist this wave
 *
 * `AgentReadSuccess` retired tags 4-8: the image, pdf, notebook, split-to-
 * directory and unchanged extents are deferred. A read of any of those has NO
 * arm that could describe HOW MUCH came back — but the read still HAPPENED and
 * still SETTLED, and a card that never settles is a lie about a live call. So
 * such a read settles as a success whose `extent` oneof is UNSET: the path and
 * the settle instant are stated, the extent is not. That is the contract's own
 * spelling of "returned nothing to draw" — the daemon's read resolver already
 * answers an unset extent with no output form (resolve/feed/toolcall.go
 * readForm: "A read that came back with no extent has nothing to draw; the
 * card still says it returned"), which applyReturnedForm renders as
 * `FeedToolCallNoOutput`. Nothing is invented: no image bytes are dressed as
 * text, and the absent arm remains absent.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { asRecord, bool, failureOf, obj, settle, str, uint } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-tools", operation: "shim.convert.tools.read" });

/** The path a read acted on, as the tool resolved it. */
function readPath(path: string): conversationv1.ReadPath {
  return create(conversationv1.ReadPathSchema, { path });
}

/** The file the CALLER named, which is the only path the start arm can know. */
function requestedPath(call: PendingCall): string | undefined {
  return str(call.input, "file_path");
}

/** The read's `start` arm. */
function readStart(call: PendingCall, path: string): conversationv1.AgentReadStart {
  return create(conversationv1.AgentReadStartSchema, {
    path: readPath(path),
    startedAt: startedAt(call.startedAtMs),
  });
}

/** The whole-file extent. */
function wholeExtent(contents: string): conversationv1.AgentReadSuccess["extent"] {
  return {
    case: "whole",
    value: create(conversationv1.AgentReadWholeSchema, { contents }),
  };
}

/** The leading-portion extent, with WHAT CUT IT stated. */
function headExtent(
  contents: string,
  totalLines: number,
  cut: "line_cap" | "token_cap",
): conversationv1.AgentReadSuccess["extent"] {
  return {
    case: "head",
    value: create(conversationv1.AgentReadHeadSchema, {
      contents,
      totalLines,
      cut:
        cut === "token_cap"
          ? { case: "tokenCap", value: create(conversationv1.AgentReadCutAtTokenCapSchema, {}) }
          : { case: "lineCap", value: create(conversationv1.AgentReadCutAtLineCapSchema, {}) },
    }),
  };
}

/** The middle-slice extent. */
function rangeExtent(
  contents: string,
  firstLine: number,
  lineCount: number,
  totalLines: number,
): conversationv1.AgentReadSuccess["extent"] {
  return {
    case: "range",
    value: create(conversationv1.AgentReadRangeSchema, {
      contents,
      firstLine,
      lineCount,
      totalLines,
    }),
  };
}

/**
 * Which extent a text read is, from what was asked and what came back.
 *
 * Answers `undefined` when the vendor withheld a figure the chosen arm cannot
 * be stated without — a head with no total would draw "showing 200 of 0".
 */
function textExtent(
  call: PendingCall,
  file: Record<string, unknown>,
): conversationv1.AgentReadSuccess["extent"] | undefined {
  const contents = str(file, "content");
  if (contents === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a text read carried no content; no success frame is produced",
    );
    return undefined;
  }
  const totalLines = uint(file, "totalLines");
  const startLine = uint(file, "startLine");
  const numLines = uint(file, "numLines");
  const askedOffset = uint(call.input, "offset");
  const askedLimit = uint(call.input, "limit");

  if (bool(file, "truncatedByTokenCap") === true && (startLine === undefined || startLine <= 1)) {
    if (totalLines === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a token-capped read stated no total line count; no success frame is produced",
      );
      return undefined;
    }
    return headExtent(contents, totalLines, "token_cap");
  }

  if (askedOffset !== undefined) {
    if (startLine === undefined || numLines === undefined || totalLines === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId, asked_offset: askedOffset },
        "an offset read stated no slice bounds; no success frame is produced",
      );
      return undefined;
    }
    return rangeExtent(contents, startLine, numLines, totalLines);
  }

  const startedAtFirstLine = startLine === undefined || startLine <= 1;
  const stoppedShort =
    totalLines !== undefined && numLines !== undefined && numLines < totalLines;
  if (askedLimit !== undefined && startedAtFirstLine && stoppedShort) {
    return headExtent(contents, totalLines, "line_cap");
  }

  if (stoppedShort) {
    // NOT A WHOLE FILE, whatever the caller asked: fewer lines came back than
    // the file holds, and calling that `whole` would claim there is nothing
    // more to fetch.
    if (!startedAtFirstLine) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a short read began past line 1 with no offset asked; recorded as a range",
      );
      return rangeExtent(contents, startLine, numLines, totalLines);
    }
    return headExtent(contents, totalLines, "line_cap");
  }

  return wholeExtent(contents);
}

/** The read's `success` arm, or `undefined` when no arm can describe it. */
function readSuccess(
  call: PendingCall,
  outcome: ToolOutcome,
): conversationv1.AgentReadSuccess | undefined {
  const record = asRecord(outcome.structured);
  if (record === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a read settled with no typed output; no success frame is produced",
    );
    return undefined;
  }
  const type = str(record, "type");
  const file = obj(record, "file");
  if (type !== "text") {
    // The image, pdf, notebook, parts and file_unchanged extents are RETIRED
    // tags on AgentReadSuccess this wave. No arm can say HOW MUCH came back, so
    // the extent stays unset — but the read settled, and the card must too.
    const nonTextPath = str(file ?? {}, "filePath") ?? requestedPath(call);
    if (nonTextPath === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId, read_type: type ?? "unstated" },
        "a non-text read settled with no path at all; no success frame is produced",
      );
      return undefined;
    }
    LOGGER.debug(
      { tool_use_id: call.toolUseId, read_type: type ?? "unstated" },
      "this read's extent has no arm in the contract this wave; it settles with no extent stated",
    );
    return create(conversationv1.AgentReadSuccessSchema, {
      path: readPath(nonTextPath),
      settledAt: settle(outcome),
    });
  }
  if (file === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a text read carried no file object; no success frame is produced",
    );
    return undefined;
  }
  const path = str(file, "filePath") ?? requestedPath(call);
  if (path === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a read settled with no path at all; no success frame is produced",
    );
    return undefined;
  }
  const extent = textExtent(call, file);
  if (extent === undefined) return undefined;
  return create(conversationv1.AgentReadSuccessSchema, {
    path: readPath(path),
    extent,
    settledAt: settle(outcome),
  });
}

/** The agent reading a file. */
export const readConverter: ToolConverter = {
  kind: "read",
  carriesProgress: true,

  start(call) {
    const path = requestedPath(call);
    if (path === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a read was announced with no file path; no start frame is produced",
      );
      return undefined;
    }
    return {
      case: "read",
      value: create(conversationv1.AgentReadSchema, {
        result: { case: "start", value: readStart(call, path) },
      }),
    };
  },

  settle(call, outcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a read failed");
      // THE SETTLED FRAME STANDS ALONE: it restates what the call named, since
      // the start it upserts over is gone once it lands. A call that named
      // nothing has no start either, so it gets no failure frame, exactly as
      // it got no announcement.
      const path = requestedPath(call);
      if (path === undefined) {
        LOGGER.debug(
          { tool_use_id: call.toolUseId },
          "a read failed with no file path to restate; no failure frame is produced",
        );
        return undefined;
      }
      return {
        case: "read",
        value: create(conversationv1.AgentReadSchema, {
          result: {
            case: "failure",
            value: create(conversationv1.AgentReadFailureSchema, {
              error: failureOf(outcome),
              path: readPath(path),
            }),
          },
        }),
      };
    }
    const success = readSuccess(call, outcome);
    if (success === undefined) return undefined;
    return {
      case: "read",
      value: create(conversationv1.AgentReadSchema, {
        result: { case: "success", value: success },
      }),
    };
  },

  progress(beat) {
    return {
      case: "read",
      value: create(conversationv1.AgentReadSchema, {
        result: { case: "progress", value: beat },
      }),
    };
  },
};
