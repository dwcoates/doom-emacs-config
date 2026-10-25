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
import type { ChatReveal, ChatView } from "../../../proto/gen/ts/frontend/v1/chat_pb.js";
import { log } from "../log.js";
import { CHAT_LEEWAY_FRACTION, chatEndTop, landChatAtEnd, revealChatExchange } from "../scroll.js";
import { installChatOverscroll } from "./overscroll.js";
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

/**
 * The question VIEW has in flight, as one string standing for its quote and
 * its text, or null when none is.
 */
export function pendingQuestionOf(view: ChatView): string | null {
  if (view.state.case !== "conversation" || view.state.value.pending === undefined) return null;
  const question = requireMessage(view.state.value.pending.question, "ChatPending.question");
  return JSON.stringify([question.quote?.text ?? null, requireMessage(question.text, "ChatQuestion.text").text]);
}

/** The node ids of VIEW's shown path, oldest first; none for an empty thread. */
export function shownPathOf(view: ChatView): string[] {
  if (view.state.case !== "conversation") return [];
  return view.state.value.exchanges.map((ex, i) => requireMessage(ex.node, `ChatConversation.exchanges[${i}].node`).id);
}

/**
 * Whether NEXT's shown path swaps an exchange PREV showed for another (a
 * sibling switch) rather than only adding to PREV's: the two differ at an
 * index both hold.
 */
export function pathSwapped(prev: readonly string[], next: readonly string[]): boolean {
  const n = Math.min(prev.length, next.length);
  for (let i = 0; i < n; i++) if (prev[i] !== next[i]) return true;
  return false;
}

/**
 * The drawn exchange of REVEAL's node. The daemon names only a node on the
 * shown path, which the view just drawn holds, so a missing one is a defect.
 */
function revealedExchange(host: HTMLElement, reveal: ChatReveal): HTMLElement {
  const node = requireMessage(reveal.node, "ChatReveal.node").id;
  const exchange = [...host.querySelectorAll<HTMLElement>("[data-chat-node]")].find(
    (el) => el.getAttribute("data-chat-node") === node,
  );
  if (exchange === undefined) {
    log.error("the chat was asked to reveal a node it did not draw", { operation: "chat.reveal-missing", context: { node } });
    throw new Error(`the chat drew no exchange for revealed node ${node}`);
  }
  return exchange;
}

/** Mount the chat into HOST. */
export function mountChat(host: HTMLElement, ctx: AppContext, deps: ChatDeps): ChatHandle {
  log.info("mounting the chat", { operation: "chat.mount" });
  const root: Root = createRoot(host);
  const thread = ctx.workspace;
  // THE LEEWAY IS THE CHAT'S OWN BOTTOM PADDING, in `cqh` of the scroll box
  // (#feed-scroll's size container), so the box can never scroll further
  // than a quarter page past the last answer (scroll.ts CHAT_LEEWAY_FRACTION).
  host.style.paddingBottom = `${CHAT_LEEWAY_FRACTION * 100}cqh`;
  const releaseOverscroll = installChatOverscroll(deps.scroll, host);

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

  // The pending question of the last view drawn: undefined before the first
  // view, null while none is in flight.
  let lastAsked: string | null | undefined = undefined;
  // The shown path of the last view drawn.
  let lastPath: string[] = [];
  // The reveal sequence of the last view drawn: undefined before the first
  // view, whose reveal (an ask made before this page) is only recorded.
  let lastReveal: bigint | undefined = undefined;
  const draw = (view: ChatView): void => {
    // THE END STAYS IN VIEW while it was in view, so an answer arriving is
    // followed; a reader scrolled up is left where they are. A QUESTION JUST
    // ASKED lands the chat at its end wherever the reader was (owner request,
    // 2026-10-25), as a sent prompt parks the agent feed at its tail. The
    // first view is the thread's opening, not a question asked.
    const scroll = deps.scroll;
    const atEnd = scroll.scrollTop >= chatEndTop(scroll) - 24;
    const asked = pendingQuestionOf(view);
    const sent = lastAsked !== undefined && asked !== null && asked !== lastAsked;
    lastAsked = asked;
    // A SIBLING SWITCH MOVES NOTHING (owner request, 2026-10-25): the view
    // that swaps an exchange for its sibling keeps the scroll where it was,
    // never following the end. Only a shorter branch can move it, and only
    // as far as its own end plus the leeway: the chat never stands past that.
    const path = shownPathOf(view);
    const switched = !sent && pathSwapped(lastPath, path);
    lastPath = path;
    const top = scroll.scrollTop;
    // A NODE THE USER ASKED TO SEE (the sidebar's outline) is brought into
    // view, whatever else the view changed.
    const reveal = view.state.case === "conversation" ? view.state.value.reveal : undefined;
    const sequence = reveal?.sequence ?? 0n;
    const revealed = lastReveal !== undefined && sequence > lastReveal ? reveal : undefined;
    lastReveal = sequence;
    flushSync(() => root.render(<ChatStream view={view} actions={actions} />));
    if (sent) landChatAtEnd(scroll, "promptSent");
    else if (revealed !== undefined) revealChatExchange(scroll, revealedExchange(host, revealed));
    else if (switched) scroll.scrollTop = top;
    else if (atEnd) landChatAtEnd(scroll, "chatAnswerFollowed");
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
      releaseOverscroll();
      host.style.paddingBottom = "";
      root.unmount();
    },
  };
}
