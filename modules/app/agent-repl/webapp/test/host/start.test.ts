// @vitest-environment jsdom
// The host page's start: the workspace shell cleared, the root marked, the
// host and its focus answer handed to Emacs. See src/host/main.ts.
import { afterEach, describe, expect, it } from "vitest";

import { HOST_ROOT_CLASS, startHost } from "../../src/host/main.js";
import type { Host } from "../../src/host/host.js";

let started: Host | null = null;

afterEach(() => {
  started?.dispose();
  started = null;
  document.documentElement.classList.remove(HOST_ROOT_CLASS);
  delete window.agentReplHost;
  delete window.xwHasFocus;
  document.body.replaceChildren();
});

describe("startHost", () => {
  it("clears the workspace page's shell markup", () => {
    // Arrange
    document.body.innerHTML = '<nav id="ws-sidebar"></nav><div id="main-col"></div>';

    // Act
    started = startHost(document, window);

    // Assert
    expect(document.body.children).toHaveLength(0);
  });

  it("marks the root as the host", () => {
    // Act
    started = startHost(document, window);

    // Assert
    expect(document.documentElement.classList.contains(HOST_ROOT_CLASS)).toBe(true);
  });

  it("hands the host to Emacs", () => {
    // Act
    started = startHost(document, window);

    // Assert
    expect(window.agentReplHost).toBe(started);
  });

  it("answers Emacs's focus question for the host", () => {
    // Arrange
    started = startHost(document, window);
    const field = document.createElement("textarea");
    document.body.append(field);

    // Act
    field.focus();

    // Assert
    expect(window.xwHasFocus?.()).toBe(true);
  });

  it("puts its focus answer back once the page has loaded", () => {
    // Arrange
    started = startHost(document, window);
    window.xwHasFocus = () => false;
    const field = document.createElement("textarea");
    document.body.append(field);
    field.focus();

    // Act
    window.dispatchEvent(new Event("load"));

    // Assert
    expect(window.xwHasFocus()).toBe(true);
  });
});
