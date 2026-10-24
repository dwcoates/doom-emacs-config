/**
 * THE CHAT VIEW: one zettel thread drawn as a chat stream (frontend.v1.ChatView).
 *
 * THE DAEMON RESOLVES EVERYTHING and this draws it verbatim: which branch is
 * shown below each node, the sibling labels, which words are marked. The view
 * holds no state of its own beyond a swipe in progress; every gesture is
 * handed to ChatActions, which tells the daemon, and the next view is drawn.
 *
 * Layout, as the design mock (docs/knowledge-graph/chat-mock.html) draws it:
 * questions are right-aligned user bubbles, a question with siblings is a stack
 * of cards with a "2 of 3" control under it, answers are full-width cards, and
 * the question in flight is drawn last with its answer arriving.
 */
import { useLayoutEffect, useRef, type ReactElement, type WheelEvent } from "react";
import type {
  ChatAnswer,
  ChatExchange,
  ChatPending,
  ChatQuestion,
  ChatView,
} from "../../../proto/gen/ts/frontend/v1/chat_pb.js";
import { log } from "../log.js";
import { renderMarkdown } from "../markdown.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { contextOf, markWords } from "./marks.js";

/** What the person can do to the view; each is the daemon's to act on. */
export interface ChatActions {
  /** Show the sibling before or after the shown node. */
  switchSibling(node: string, way: "previous" | "next"): void;
  /** Hold WORDS of NODE's answer highlighted, with the text around them. */
  select(node: string, words: string, context: string): void;
  /** Let the held highlight go. */
  clearSelection(): void;
  /** NODE's answer bubble is ELEMENT (null when it is no longer drawn). */
  observeAnswer(node: string, element: HTMLElement | null): void;
}

/** The mark classes, shared with the stylesheet. */
export const QUOTE_MARK_CLASS = "chat-quote-mark";
export const HIGHLIGHT_CLASS = "chat-highlight";

/** How far a horizontal trackpad swipe travels before it switches a sibling. */
export const SWIPE_DISTANCE = 80;

/** The whole view. */
export function ChatStream({ view, actions }: { view: ChatView; actions: ChatActions }): ReactElement {
  const state = requireCase(view.state, "ChatView.state");
  switch (state.case) {
    case "empty":
      return (
        <div className="chat-col">
          <div className="chat-empty" data-chat-empty="">
            {requireMessage(state.value.hint, "ChatViewEmpty.hint").text}
          </div>
        </div>
      );
    case "conversation": {
      const conversation = state.value;
      const held = conversation.exchanges.some((ex) => ex.answer?.highlight !== undefined);
      return (
        <div
          className="chat-col"
          onMouseUp={() => {
            // A CLICK THAT SELECTS NOTHING LETS A HELD HIGHLIGHT GO; a
            // selection inside an answer is taken by that answer's own handler.
            const selection = window.getSelection();
            if (held && (selection === null || selection.isCollapsed)) actions.clearSelection();
          }}
        >
          {conversation.exchanges.map((exchange) => (
            <Exchange key={requireMessage(exchange.node, "ChatExchange.node").id} exchange={exchange} actions={actions} />
          ))}
          {conversation.pending !== undefined && <Pending pending={conversation.pending} />}
        </div>
      );
    }
    default: {
      const other: { case: string } = state;
      return unreachableArm("ChatView.state", other.case);
    }
  }
}

/** One node on the shown path. */
function Exchange({ exchange, actions }: { exchange: ChatExchange; actions: ChatActions }): ReactElement {
  const node = requireMessage(exchange.node, "ChatExchange.node").id;
  const swipe = useRef({ travelled: 0, spent: false, idle: 0 });
  const siblings = exchange.siblings;
  const onWheel = (event: WheelEvent<HTMLDivElement>): void => {
    if (siblings === undefined || Math.abs(event.deltaX) <= Math.abs(event.deltaY)) return;
    const s = swipe.current;
    window.clearTimeout(s.idle);
    // ONE SWIPE IS ONE SWITCH: the gesture is spent until the wheel rests.
    s.idle = window.setTimeout(() => {
      s.travelled = 0;
      s.spent = false;
    }, 250);
    if (s.spent) return;
    s.travelled += event.deltaX;
    if (Math.abs(s.travelled) >= SWIPE_DISTANCE) {
      s.spent = true;
      actions.switchSibling(node, s.travelled > 0 ? "next" : "previous");
    }
  };
  return (
    <div className="chat-exchange" data-chat-node={node}>
      <div className="chat-q-wrap" onWheel={onWheel}>
        <Question question={requireMessage(exchange.question, "ChatExchange.question")} stacked={siblings !== undefined} />
        {siblings !== undefined && (
          <div className="chat-siblings" data-chat-siblings="">
            <button type="button" aria-label="previous sibling" onClick={() => actions.switchSibling(node, "previous")}>
              ‹
            </button>
            <span>{requireMessage(siblings.position, "ChatSiblings.position").label}</span>
            <button type="button" aria-label="next sibling" onClick={() => actions.switchSibling(node, "next")}>
              ›
            </button>
          </div>
        )}
      </div>
      <Answer node={node} answer={requireMessage(exchange.answer, "ChatExchange.answer")} actions={actions} />
    </div>
  );
}

/** A question bubble. */
function Question({ question, stacked }: { question: ChatQuestion; stacked: boolean }): ReactElement {
  return (
    <div className={stacked ? "chat-q chat-q-stacked" : "chat-q"} data-chat-question="">
      {question.quote !== undefined && <span className="chat-q-quote">{question.quote.text}</span>}
      {requireMessage(question.text, "ChatQuestion.text").text}
    </div>
  );
}

/** An answer bubble: its markdown, the words its children quoted, and the highlight. */
function Answer({ node, answer, actions }: { node: string; answer: ChatAnswer; actions: ChatActions }): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  const markdown = requireMessage(answer.markdown, "ChatAnswer.markdown").text;
  // THE MARKS AS ONE KEY, so a new array holding the same words is not a change.
  const marksKey = answer.quoteMarks.map((m) => m.text).join("\u0000");
  const highlight = answer.highlight?.text;
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    // REDRAWN WHOLE, so a mark that moved is never left beside its successor.
    el.innerHTML = renderMarkdown(markdown);
    const missing = markWords(el, marksKey === "" ? [] : marksKey.split("\u0000"), QUOTE_MARK_CLASS);
    if (highlight !== undefined) missing.push(...markWords(el, [highlight], HIGHLIGHT_CLASS));
    if (missing.length > 0) {
      log.debug("words the daemon marked do not occur in the drawn answer", {
        operation: "chat.marks-missing",
        context: { node, missing: missing.length },
      });
    }
  }, [markdown, marksKey, highlight, node]);
  useLayoutEffect(() => {
    actions.observeAnswer(node, ref.current);
    return () => actions.observeAnswer(node, null);
  }, [actions, node]);
  return (
    <div
      ref={ref}
      className="chat-a md"
      data-chat-answer={node}
      onMouseUp={(event) => {
        const selection = window.getSelection();
        const el = ref.current;
        if (selection === null || selection.isCollapsed || el === null || selection.anchorNode === null) return;
        const words = selection.toString().trim();
        if (words === "" || !el.contains(selection.anchorNode)) return;
        // TAKEN HERE, so the stream's own handler does not read it as a clear.
        event.stopPropagation();
        actions.select(node, words, contextOf(el, selection.anchorNode));
      }}
    />
  );
}

/** The question in flight and its answer so far. */
function Pending({ pending }: { pending: ChatPending }): ReactElement {
  const answer = requireCase(requireMessage(pending.answer, "ChatPending.answer").progress, "ChatPendingAnswer.progress");
  let body: ReactElement;
  switch (answer.case) {
    case "waiting":
      body = (
        <div className="chat-a chat-a-waiting" data-chat-waiting="">
          <span className="chat-dot" />
          <span className="chat-dot" />
          <span className="chat-dot" />
        </div>
      );
      break;
    case "streaming":
      body = <Streaming markdown={requireMessage(answer.value.markdown, "ChatPendingAnswerStreaming.markdown").text} />;
      break;
    default: {
      const other: { case: string } = answer;
      return unreachableArm("ChatPendingAnswer.progress", other.case);
    }
  }
  return (
    <div className="chat-exchange chat-pending" data-chat-pending="">
      <div className="chat-q-wrap">
        <Question question={requireMessage(pending.question, "ChatPending.question")} stacked={false} />
      </div>
      {body}
    </div>
  );
}

/** An answer still arriving. */
function Streaming({ markdown }: { markdown: string }): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    if (ref.current !== null) ref.current.innerHTML = renderMarkdown(markdown);
  }, [markdown]);
  return <div ref={ref} className="chat-a md chat-a-streaming" data-chat-streaming="" />;
}
