// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  CONVERSATION_DRAWN_ATTRIBUTE,
  markConversationDrawn,
  type ConversationDrawnEnv,
} from "../../src/feed/conversation-drawn.js";

/** A document whose `hidden` is HIDDEN and whose frames run only when pumped. */
function env(hidden: boolean): { env: ConversationDrawnEnv; pump: () => void } {
  const queued: Array<() => void> = [];
  Object.defineProperty(document, "hidden", { configurable: true, get: () => hidden });
  return {
    env: { doc: document, frame: (callback) => queued.push(callback) },
    pump: () => {
      const now = queued.splice(0);
      for (const callback of now) callback();
    },
  };
}

afterEach(() => {
  document.documentElement.removeAttribute(CONVERSATION_DRAWN_ATTRIBUTE);
});

describe("markConversationDrawn", () => {
  it("marks a shown page only after the frame that paints it", () => {
    // Arrange
    const { env: e, pump } = env(false);

    // Act
    markConversationDrawn("page", e);
    pump();
    const afterOne = document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE);
    pump();

    // Assert
    expect([afterOne, document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE)]).toEqual([null, "page"]);
  });

  it("marks a hidden page at once, since nothing paints it", () => {
    // Arrange
    const { env: e } = env(true);

    // Act
    markConversationDrawn("page", e);

    // Assert
    expect(document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE)).toBe("page");
  });

  it("marks a refused open as refused", () => {
    // Arrange
    const { env: e } = env(true);

    // Act
    markConversationDrawn("refused", e);

    // Assert
    expect(document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE)).toBe("refused");
  });

  it("keeps the first mark", () => {
    // Arrange
    const { env: e } = env(true);
    markConversationDrawn("page", e);

    // Act
    markConversationDrawn("refused", e);

    // Assert
    expect(document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE)).toBe("page");
  });
});
