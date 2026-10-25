/**
 * THE CHAT PAGE'S BODY: a zettel chat thread's chat view, mounted where an
 * agent workspace's page mounts its feed (AdoptWebWorkspace names which).
 *
 * It watches the thread's view (WatchChat, on the page's one stream), draws it
 * with React (chat-view.tsx), and hands every gesture to the daemon:
 *   - a sibling switch (arrows, or a horizontal trackpad swipe) is
 *     SwitchChatSibling;
 *   - words selected in an answer are SelectChatText, and a click that selects
 *     nothing lets the held highlight go; the next question typed in Emacs
 *     asks about them;
 *   - an answer bubble coming FULLY onto a shown screen is a sighting, batched
 *     into ReportChatSightings, which is what the least-seen branch is picked
 *     by.
 * There is no input box: Emacs's composer is the input.
 */
import { createRoot, type Root } from "react-dom/client";
import { flushSync } from "react-dom";
import { WatchChatResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_chat_pb.js";
import { SwitchChatSiblingResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_switch_chat_sibling_pb.js";
import { SelectChatTextResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_chat_text_pb.js";
import { ReportChatSightingsResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_report_chat_sightings_pb.js";
import type { ChatView } from "../../../proto/gen/ts/frontend/v1/chat_pb.js";
import { log } from "../log.js";
import { landChatAtEnd } from "../scroll.js";
import type { AppContext } from "../rpc/context.js";
import { noteRefusedArm } from "../rpc/refusal.js";
import { requireCase, requireMessage } from "../rpc/strict.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import { callUnary } from "../rpc/unary.js";
import { ChatStream, type ChatActions } from "./chat-view.js";
import { markConversationDrawn } from "../feed/conversation-drawn.js";

/** How long sightings gather before they are reported together. */
export const SIGHTING_BATCH_MS = 1000;

/** What the chat needs beyond the app context. */
export interface ChatDeps {
  /** The scroll box the chat draws in, kept at its end while the end is shown. */
  scroll: HTMLElement;
  /** Whether the page is on screen: an answer on a hidden page is not seen. */
  shown(): boolean;
  /** The observer that reports an answer fully on screen (a seam for tests). */
  observe?: (onSeen: (element: Element) => void) => AnswerObserver;
}

/** The slice of IntersectionObserver the chat uses. */
export interface AnswerObserver {
  observe(element: Element): void;
  unobserve(element: Element): void;
  disconnect(): void;
}

/** The mounted chat. */
export interface ChatHandle {
  /** Stop watching and unmount. */
  dispose(): void;
}

/** The production observer: an answer counts once it is wholly in view. */
function intersectionObserver(onSeen: (element: Element) => void): AnswerObserver {
  return new IntersectionObserver(
    (entries) => {
      for (const entry of entries) if (entry.isIntersecting && entry.intersectionRatio >= 1) onSeen(entry.target);
    },
    { threshold: 1 },
  );
}

/** A refused gesture, recorded at INFO: the next view is the answer. */
function refused(rpc: string, result: { case?: string | undefined; value?: unknown }): void {
  noteRefusedArm(rpc, "chat.gesture-refused", result);
}

/** Mount the chat into HOST. */
export function mountChat(host: HTMLElement, ctx: AppContext, deps: ChatDeps): ChatHandle {
  log.info("mounting the chat", { operation: "chat.mount" });
  const root: Root = createRoot(host);
  const thread = ctx.workspace;

  // ---- sightings ----
  const nodeOf = new Map<Element, string>();
  const elementOf = new Map<string, Element>();
  let seen: string[] = [];
  let flushTimer = 0;
  const flush = (): void => {
    flushTimer = 0;
    const nodes = seen;
    seen = [];
    if (nodes.length === 0) return;
    callUnary(
      ctx,
      "ReportChatSightings",
      (client) => client.reportChatSightings({ thread, nodes: nodes.map((id) => ({ id })) }),
      ReportChatSightingsResponseSchema,
    ).then(
      (response) => refused("ReportChatSightings", requireCase(response.result, "ReportChatSightingsResponse.result")),
      () => undefined, // the transport failure is callUnary's to record
    );
  };
  const observer = (deps.observe ?? intersectionObserver)((element) => {
    const node = nodeOf.get(element);
    if (node === undefined || !deps.shown()) return;
    seen.push(node);
    if (flushTimer === 0) flushTimer = window.setTimeout(flush, SIGHTING_BATCH_MS);
  });

  const actions: ChatActions = {
    switchSibling(node, way) {
      log.info("switching a chat sibling", { operation: "chat.switch-sibling", context: { node, way } });
      callUnary(
        ctx,
        "SwitchChatSibling",
        (client) =>
          client.switchChatSibling({
            thread,
            shown: { id: node },
            direction: { way: way === "next" ? { case: "next", value: {} } : { case: "previous", value: {} } },
          }),
        SwitchChatSiblingResponseSchema,
      ).then(
        (response) => refused("SwitchChatSibling", requireCase(response.result, "SwitchChatSiblingResponse.result")),
        () => undefined,
      );
    },
    select(node, words, context) {
      log.info("holding a chat highlight", { operation: "chat.select", context: { node, words: words.length } });
      callUnary(
        ctx,
        "SelectChatText",
        (client) =>
          client.selectChatText({
            thread,
            selection: {
              state: {
                case: "highlighted",
                value: { node: { id: node }, quote: { text: words, questionContext: context === "" ? undefined : { text: context } } },
              },
            },
          }),
        SelectChatTextResponseSchema,
      ).then(
        (response) => refused("SelectChatText", requireCase(response.result, "SelectChatTextResponse.result")),
        () => undefined,
      );
    },
    clearSelection() {
      log.info("letting the chat highlight go", { operation: "chat.clear-selection" });
      callUnary(
        ctx,
        "SelectChatText",
        (client) => client.selectChatText({ thread, selection: { state: { case: "cleared", value: {} } } }),
        SelectChatTextResponseSchema,
      ).then(
        (response) => refused("SelectChatText", requireCase(response.result, "SelectChatTextResponse.result")),
        () => undefined,
      );
    },
    observeAnswer(node, element) {
      const before = elementOf.get(node);
      if (before !== undefined) {
        observer.unobserve(before);
        nodeOf.delete(before);
        elementOf.delete(node);
      }
      if (element === null) return;
      elementOf.set(node, element);
      nodeOf.set(element, node);
      observer.observe(element);
    },
  };

  const draw = (view: ChatView): void => {
    // THE END STAYS IN VIEW while it was in view, so an answer arriving is
    // followed; a reader scrolled up is left where they are.
    const scroll = deps.scroll;
    const atEnd = scroll.scrollHeight - scroll.scrollTop - scroll.clientHeight < 24;
    flushSync(() => root.render(<ChatStream view={view} actions={actions} />));
    if (atEnd) landChatAtEnd(scroll, "chatAnswerFollowed");
    // THE THREAD'S FIRST VIEW IS ITS CONVERSATION DRAWN, as the feed's opening
    // page is the feed's: the daemon opens the thread's tab (OpenTabs) only
    // once its page says so (page-drawn-report.ts), and the frame follows the
    // thread only once its tab is open.
    markConversationDrawn("page");
  };

  const stream: StreamHandle = watchStream(ctx, {
    name: "WatchChat",
    schema: WatchChatResponseSchema,
    open: (_client, signal) => ctx.streams.watch("chat", { thread }, signal),
    onPush: (response) => draw(requireMessage(response.view, "WatchChatResponse.view")),
  });

  return {
    dispose() {
      log.debug("disposing the chat", { operation: "chat.dispose" });
      stream.cancel();
      observer.disconnect();
      window.clearTimeout(flushTimer);
      root.unmount();
    },
  };
}
