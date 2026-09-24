/**
 * convert/tools/web-search.ts — a query, and the heterogeneous answer.
 *
 * # The engine's array holds TWO kinds of thing
 *
 * `WebSearchOutput.results` is `({ tool_use_id, content: {title,url}[] } | string)[]`
 * — a hit group, or a bare narration line the model wrote between groups. The
 * corpus's one observed search holds exactly one of each, in that order. Both
 * kinds land in ONE repeated field, in the engine's order, because the order is
 * the answer's shape: a note that floated to the end would be narrating links
 * it was never about.
 *
 * A hit group is FLATTENED — one link per entry of its `content` array, in
 * order — because the group is the vendor's batching of one server-side call,
 * not a fact about the results.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { arr, asRecord, failureOf, num, str, strOr, uint } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-web-search",
  operation: "shim.convert.tools.web_search",
});

/** What was searched for, from the CALL — carried on every frame. */
function queryOf(call: PendingCall): conversationv1.AgentWebSearchQuery {
  const terms = str(call.input, "query");
  if (terms === undefined) {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a web search call names no query; the unit's query cannot restate one",
    );
  }
  return create(conversationv1.AgentWebSearchQuerySchema, { terms: terms ?? "" });
}

/** One found page. */
function link(title: string, url: string): conversationv1.AgentWebSearchResult {
  return create(conversationv1.AgentWebSearchResultSchema, {
    entry: {
      case: "link",
      value: create(conversationv1.AgentWebSearchLinkSchema, { title, url }),
    },
  });
}

/** One narration line the engine interleaved with the links. */
function note(text: string): conversationv1.AgentWebSearchResult {
  return create(conversationv1.AgentWebSearchResultSchema, {
    entry: { case: "note", value: create(conversationv1.AgentWebSearchNoteSchema, { text }) },
  });
}

/** The engine's answer, flattened into one ordered list of two kinds. */
function resultsOf(
  output: Record<string, unknown>,
  toolUseId: string,
): conversationv1.AgentWebSearchResult[] {
  const entries = arr(output, "results");
  if (entries === undefined) {
    LOGGER.debug(
      { tool_use_id: toolUseId },
      "a settled web search stated no results array; the answer is recorded as empty",
    );
    return [];
  }
  const results: conversationv1.AgentWebSearchResult[] = [];
  for (const entry of entries) {
    if (typeof entry === "string") {
      results.push(note(entry));
      continue;
    }
    const group = asRecord(entry);
    const hits = arr(group, "content");
    if (hits === undefined) {
      LOGGER.debug(
        { tool_use_id: toolUseId },
        "a web search result entry was neither a narration line nor a hit group; it is dropped",
      );
      continue;
    }
    for (const hit of hits) {
      const record = asRecord(hit);
      const url = str(record, "url");
      if (url === undefined) {
        // A hit with no url is not a page anyone can open; a link row built
        // around an empty href would be a dead row drawn as a live one.
        LOGGER.debug(
          { tool_use_id: toolUseId },
          "a web search hit named no url; it is dropped rather than drawn as a dead link",
        );
        continue;
      }
      results.push(link(strOr(record, "title"), url));
    }
  }
  return results;
}

/** The one wrapper every frame of this unit shares. */
function item(
  result: conversationv1.AgentWebSearch["result"],
): conversationv1.AgentActivity["item"] {
  return { case: "webSearch", value: create(conversationv1.AgentWebSearchSchema, { result }) };
}

export const webSearchConverter: ToolConverter = {
  kind: "web_search",
  // AgentWebSearch declares an AgentToolCallProgress arm.
  carriesProgress: true,

  start(call) {
    LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a web search was issued");
    return item({
      case: "start",
      value: create(conversationv1.AgentWebSearchStartSchema, {
        query: queryOf(call),
        startedAtMs: BigInt(Math.trunc(call.startedAtMs)),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "the web search never ran");
      return item({
        case: "failure",
        value: create(conversationv1.AgentWebSearchFailureSchema, {
          query: queryOf(call),
          failure: failureOf(call, outcome),
        }),
      });
    }
    const output = asRecord(outcome.structured);
    if (output === undefined) {
      // An empty success arm is the vendor's own "found nothing", so building
      // one from a missing output would state a fact the vendor never did.
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a settled web search carried no typed output; an empty answer would be a claim the vendor never made",
      );
      return undefined;
    }
    const results = resultsOf(output, call.toolUseId);
    LOGGER.logVerbose(
      { tool_use_id: call.toolUseId, results: results.length },
      "the web search was answered",
    );
    return item({
      case: "success",
      value: create(conversationv1.AgentWebSearchSuccessSchema, {
        query: queryOf(call),
        results,
        searchCount: uint(output, "searchCount") ?? 0,
        durationSeconds: num(output, "durationSeconds") ?? 0,
      }),
    });
  },

  progress(beat) {
    return item({ case: "progress", value: beat });
  },
};
