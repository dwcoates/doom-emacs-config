// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { pagePresence } from "../../src/presence.js";
import {
  CONVERSATION_DRAWN_ATTRIBUTE,
  conversationDrawnEnv,
  markConversationDrawn,
  type ConversationDrawnEnv,
} from "../../src/feed/conversation-drawn.js";

/** A page that is SEEN or not, and whose frames run only when pumped. */
function env(seen: boolean): { env: ConversationDrawnEnv; pump: () => void } {
  const queued: Array<() => void> = [];
  return {
    env: { doc: document, frame: (callback) => queued.push(callback), seen: () => seen },
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
    const { env: e, pump } = env(true);

    // Act
    markConversationDrawn("page", e);
    pump();
    const afterOne = document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE);
    pump();

    // Assert
    expect([afterOne, document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE)]).toEqual([null, "page"]);
  });

  it("marks a page not seen at once, since nothing paints it", () => {
    // Arrange
    const { env: e } = env(false);

    // Act
    markConversationDrawn("page", e);

    // Assert
    expect(document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE)).toBe("page");
  });

  it("marks a refused open as refused", () => {
    // Arrange
    const { env: e } = env(false);

    // Act
    markConversationDrawn("refused", e);

    // Assert
    expect(document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE)).toBe("refused");
  });

  it("keeps the first mark", () => {
    // Arrange
    const { env: e } = env(false);
    markConversationDrawn("page", e);

    // Act
    markConversationDrawn("refused", e);

    // Assert
    expect(document.documentElement.getAttribute(CONVERSATION_DRAWN_ATTRIBUTE)).toBe("page");
  });

  it("reads seen from the page's presence, which a host can hold unshown", () => {
    // Arrange
    pagePresence().setShown(false);

    // Act
    const seen = conversationDrawnEnv().seen();

    // Assert
    pagePresence().setShown(true);
    expect(seen).toBe(false);
  });
});
