// @vitest-environment jsdom
// The host page's start: the workspace shell cleared, the root marked, the
// host and its focus answer handed to Emacs. See src/host/main.ts.
import { afterEach, describe, expect, it } from "vitest";

import { HOST_ROOT_CLASS, WEBHOST_FRAMES_CLASS, startHost } from "../../src/host/main.js";
import { WEBHOST_SIDEBAR_ID } from "../../src/host/sidebar.js";
import type { Host } from "../../src/host/host.js";

let started: Host | null = null;

/** The sidebar a suite not about it starts: none, and no daemon dialed. */
const noSidebar = (): void => undefined;

afterEach(() => {
  started?.dispose();
  started = null;
  document.documentElement.classList.remove(HOST_ROOT_CLASS);
  delete window.agentReplHost;
  delete window.xwHasFocus;
  delete window.__agentReplHostPending;
  document.body.replaceChildren();
  document.head.replaceChildren();
});

describe("startHost", () => {
  it("clears the workspace page's shell markup", () => {
    // Arrange
    document.body.innerHTML = '<nav id="ws-sidebar"></nav><div id="main-col"></div>';

    // Act
    started = startHost(document, window, { startSidebar: noSidebar });

    // Assert
    expect(document.getElementById("main-col")).toBeNull();
  });

  it("installs the status colors' values before the sidebar draws", () => {
    // Act
    started = startHost(document, window, { startSidebar: noSidebar });

    // Assert
    expect(document.head.querySelector('style[data-source="render-colors.json#color_values"]')).not.toBeNull();
  });

  it("lays out the stack of workspace pages and the sidebar over it", () => {
    // Act
    started = startHost(document, window, { startSidebar: noSidebar });

    // Assert
    expect([...document.body.children].map((e) => e.className || e.id)).toEqual([
      WEBHOST_FRAMES_CLASS,
      WEBHOST_SIDEBAR_ID,
    ]);
  });

  it("holds the workspace pages in the stack", () => {
    // Arrange
    started = startHost(document, window, { startSidebar: noSidebar });

    // Act
    started.ensure("a", "http://ws-a.localhost:1/");

    // Assert
    expect(document.querySelector(`.${WEBHOST_FRAMES_CLASS} iframe`)).not.toBeNull();
  });

  it("starts the sidebar in its hidden nav, once the queued calls have run", () => {
    // Arrange
    const order: string[] = [];
    window.__agentReplHostPending = [() => order.push("queued call")];
    let nav: HTMLElement | null = null;

    // Act
    started = startHost(document, window, {
      startSidebar: (_doc, _win, _host, element) => {
        order.push("sidebar");
        nav = element;
      },
    });

    // Assert
    expect([order, nav === null ? null : [(nav as HTMLElement).id, (nav as HTMLElement).hidden]]).toEqual([
      ["queued call", "sidebar"],
      [WEBHOST_SIDEBAR_ID, true],
    ]);
  });

  it("marks the root as the host", () => {
    // Act
    started = startHost(document, window, { startSidebar: noSidebar });

    // Assert
    expect(document.documentElement.classList.contains(HOST_ROOT_CLASS)).toBe(true);
  });

  it("hands the host to Emacs", () => {
    // Act
    started = startHost(document, window, { startSidebar: noSidebar });

    // Assert
    expect(window.agentReplHost).toBe(started);
  });

  it("answers Emacs's focus question for the host", () => {
    // Arrange
    started = startHost(document, window, { startSidebar: noSidebar });
    const field = document.createElement("textarea");
    document.body.append(field);

    // Act
    field.focus();

    // Assert
    expect(window.xwHasFocus?.()).toBe(true);
  });

  it("puts its focus answer back once the page has loaded", () => {
    // Arrange
    started = startHost(document, window, { startSidebar: noSidebar });
    window.xwHasFocus = () => false;
    const field = document.createElement("textarea");
    document.body.append(field);
    field.focus();

    // Act
    window.dispatchEvent(new Event("load"));

    // Assert
    expect(window.xwHasFocus()).toBe(true);
  });

  it("runs the calls Emacs queued before it started, in order", () => {
    // Arrange
    const order: string[] = [];
    window.__agentReplHostPending = [
      (host) => order.push(host.ensure("a", "http://ws-a.localhost:1/")),
      (host) => order.push(host.show("a")),
    ];

    // Act
    started = startHost(document, window, { startSidebar: noSidebar });

    // Assert
    expect(order.map((answer) => (JSON.parse(answer) as { outcome: string }).outcome)).toEqual(["created", "shown"]);
  });

  it("leaves no queue behind once it has run it", () => {
    // Arrange
    window.__agentReplHostPending = [];

    // Act
    started = startHost(document, window, { startSidebar: noSidebar });

    // Assert
    expect(window.__agentReplHostPending).toBeUndefined();
  });
});
