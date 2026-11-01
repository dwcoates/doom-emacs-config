// @vitest-environment jsdom
// The page's earliest errors, kept by index.html's inline hook where Emacs
// reads them (lisp/webhost.el `agent-repl--webhost-read-boot`).
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { pageErrorHookScript } from "./shell-html.js";

declare global {
  interface Window {
    __agentReplPageErrors?: string[];
  }
}

/** Every listener the hook added, so each case removes what it installed. */
let added: Array<[string, EventListenerOrEventListenerObject, boolean | AddEventListenerOptions | undefined]> = [];
const realAdd = window.addEventListener.bind(window);

beforeEach(() => {
  added = [];
  window.addEventListener = ((type: string, listener: EventListenerOrEventListenerObject, options?: boolean | AddEventListenerOptions) => {
    added.push([type, listener, options]);
    realAdd(type, listener, options);
  }) as typeof window.addEventListener;
  // Run as the page runs it: a classic script in the window's global scope.
  window.eval(pageErrorHookScript());
  window.addEventListener = realAdd;
});

afterEach(() => {
  for (const [type, listener, options] of added) window.removeEventListener(type, listener, options);
  delete window.__agentReplPageErrors;
});

describe("index.html's page error hook", () => {
  it("starts the page with no errors kept", () => {
    // Assert
    expect(window.__agentReplPageErrors).toEqual([]);
  });

  it("keeps an error a script threw, naming where", () => {
    // Act
    window.dispatchEvent(
      new ErrorEvent("error", {
        message: "ReferenceError: Cannot access uninitialized variable.",
        filename: "http://h/assets/app-1.js",
        lineno: 1,
        colno: 15005,
      }),
    );

    // Assert
    expect(window.__agentReplPageErrors).toEqual([
      "error: ReferenceError: Cannot access uninitialized variable. at http://h/assets/app-1.js:1:15005",
    ]);
  });

  it("keeps a script that failed to load, naming its source", () => {
    // Arrange
    const script = document.createElement("script");
    script.setAttribute("src", "http://h/assets/app-2.js");
    document.head.append(script);

    // Act
    script.dispatchEvent(new Event("error"));
    script.remove();

    // Assert
    expect(window.__agentReplPageErrors).toEqual(["failed to load: http://h/assets/app-2.js"]);
  });

  it("keeps a rejection no one handled, with its stack", () => {
    // Arrange
    const reason = new TypeError("Importing a module script failed.");
    const event = new Event("unhandledrejection") as Event & { reason?: unknown };
    event.reason = reason;

    // Act
    window.dispatchEvent(event);

    // Assert
    expect(window.__agentReplPageErrors).toEqual([`rejection: ${reason.stack}`]);
  });

  it("keeps only the first twenty, which are the boot's", () => {
    // Act
    for (let i = 0; i < 25; i++) window.dispatchEvent(new ErrorEvent("error", { message: `e${i}`, filename: "f", lineno: 1, colno: 1 }));

    // Assert
    expect(window.__agentReplPageErrors?.map((text) => text.split(" ")[1])).toEqual(
      Array.from({ length: 20 }, (_, i) => `e${i}`),
    );
  });
});
