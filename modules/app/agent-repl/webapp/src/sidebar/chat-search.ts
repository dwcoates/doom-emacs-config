/**
 * chat-search — the rail search's content half while the Chat app is shown.
 *
 * The rail's search (search.ts) narrows every row by its name. With Chat
 * shown, a thread whose QUESTIONS OR ANSWERS say the typed words is kept too,
 * and its row draws the words around the best match, the matched words
 * marked: the daemon answers which (SearchChats, over zettel's search).
 *
 * ONE ANSWER PER QUERY, AND ONLY THE NEWEST QUERY'S. A query is asked once
 * the typing pauses (`CHAT_SEARCH_PAUSE_MS`), and an answer that arrives for
 * a query the bar no longer holds is dropped, so a slow answer can never draw
 * matches for words the user has since changed.
 */
import { SearchChatsResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_search_chats_pb.js";
import type { Snippet } from "../../../proto/gen/ts/zettel/v1/endpoint_search_threads_pb.js";
import { childrenMatching, queryAll } from "../dom.js";
import { log } from "../log.js";
import type { DaemonContext } from "../rpc/context.js";
import { noteRefusedArm } from "../rpc/refusal.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import type { BlinkTimers } from "./attention.js";
import { SEARCH_MISS_CLASS } from "./search.js";

/** How long the typing must pause before the query is asked. */
export const CHAT_SEARCH_PAUSE_MS = 250;

/** The class of the snippet a content match draws under its row. */
export const CHAT_SNIPPET_CLASS = "sb-thread-snippet";

/** The content half of the rail's search. */
export interface ChatSearch {
  /**
   * The rail's query is QUERY, and the Chat app is shown when CHAT: a new
   * query is asked once the typing pauses; a blank one, or Workspaces shown,
   * asks nothing and draws no match.
   */
  changed(query: string, chat: boolean): void;
  /** Draw the current query's matches onto ROOT's thread rows. */
  apply(root: ParentNode): void;
  /** Cancel a pending ask; an answer still in flight is dropped. */
  dispose(): void;
}

/** What a chat search draws onto, and is told through. */
export interface ChatSearchDeps {
  ctx: DaemonContext;
  timers: BlinkTimers;
  /** The rail's drawn body, which matches are applied to when they arrive. */
  root(): ParentNode;
  /** Told once a new answer's matches are drawn, so the rail can refit. */
  applied(): void;
}

/** Create the rail's chat search. */
export function createChatSearch(deps: ChatSearchDeps): ChatSearch {
  let query = "";
  let matches = new Map<string, Snippet>();
  let pending: number | null = null;
  let disposed = false;

  const cancel = (): void => {
    if (pending !== null) deps.timers.clearTimeout(pending);
    pending = null;
  };

  const ask = (words: string): void => {
    log.debug("searching the chats for the rail's query", { operation: "sidebar.chat-search", context: { query: words } });
    callUnary(deps.ctx, "SearchChats", (client) => client.searchChats({ query: words }), SearchChatsResponseSchema).then(
      (response) => {
        if (disposed || words !== query) {
          log.debug("dropped a chat search answer for a query the bar no longer holds", {
            operation: "sidebar.chat-search.stale",
            context: { answered: words, current: query },
          });
          return;
        }
        const result = requireCase(response.result, "SearchChatsResponse.result");
        if (noteRefusedArm("SearchChats", "sidebar.chat-search.refused", result) !== null) return;
        if (result.case !== "success") {
          const other: { case: string } = result;
          unreachableArm("SearchChatsResponse.result", other.case);
        }
        matches = new Map(
          result.value.matches.map((m, i) => [
            requireMessage(m.thread, `SearchChatsSuccess.matches[${i}].thread`).id,
            requireMessage(m.snippet, `SearchChatsSuccess.matches[${i}].snippet`),
          ]),
        );
        apply(deps.root());
        deps.applied();
      },
      () => undefined, // the transport failure is callUnary's to record
    );
  };

  const apply = (root: ParentNode): void => {
    for (const section of queryAll<HTMLElement>(root, "[data-thread-section]")) {
      for (const rows of childrenMatching<HTMLElement>(section, ".rows")) {
        for (const ws of childrenMatching<HTMLElement>(rows, "[data-roster-row]")) {
          for (const old of childrenMatching(ws, `.${CHAT_SNIPPET_CLASS}`)) old.remove();
          const snippet = matches.get(ws.getAttribute("data-roster-row") ?? "");
          if (snippet === undefined) continue;
          ws.classList.remove(SEARCH_MISS_CLASS);
          ws.insertBefore(drawSnippet(snippet), ws.querySelector(":scope > .sb-menu"));
        }
      }
    }
  };

  return {
    changed(next, chat) {
      const words = chat ? next.trim() : "";
      if (words === query) return;
      cancel();
      query = words;
      matches = new Map();
      if (words === "") return;
      pending = deps.timers.setTimeout(() => {
        pending = null;
        ask(words);
      }, CHAT_SEARCH_PAUSE_MS);
    },
    apply,
    dispose() {
      disposed = true;
      cancel();
    },
  };
}

/** A snippet's parts, the matched ones marked. */
function drawSnippet(snippet: Snippet): HTMLElement {
  const line = document.createElement("div");
  line.className = CHAT_SNIPPET_CLASS;
  snippet.parts.forEach((part, i) => {
    const run = requireCase(part.part, `Snippet.parts[${i}].part`);
    switch (run.case) {
      case "plain":
        line.append(run.value);
        return;
      case "hit": {
        const mark = document.createElement("mark");
        mark.textContent = run.value;
        line.append(mark);
        return;
      }
      default: {
        const other: { case: string } = run;
        unreachableArm(`Snippet.parts[${i}].part`, other.case);
      }
    }
  });
  return line;
}
