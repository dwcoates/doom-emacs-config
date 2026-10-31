// @vitest-environment jsdom
// The host page's start: the workspace shell cleared, the root marked, the
// host and its focus answer handed to Emacs. See src/host/main.ts.
import { afterEach, describe, expect, it } from "vitest";
import { resetLoggingForTests, setLogger } from "../../src/log.js";
import { captureLogRecords } from "../log-capture.js";

import { HOST_ROOT_CLASS, WEBHOST_FRAMES_CLASS, startHost } from "../../src/host/main.js";
import { WEBHOST_GATE_ID } from "../../src/host/gate.js";
import { WEBHOST_LOAD_ID } from "../../src/host/load-button.js";
import type { Host } from "../../src/host/host.js";
import type { PopupLayerEnv } from "../../src/host/sidebar-popups.js";
import { quietPopupChannel } from "./popup-channel.js";

let started: Host | null = null;

/** The services a suite not about them start: none, and no daemon dialed. */
const noServices = (): void => undefined;

/** The popup layer a suite not about it installs: none, and no channel opened. */
const noPopups = (): void => undefined;

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
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });

    // Assert
    expect(document.getElementById("main-col")).toBeNull();
  });

  it("installs the status colors' values before the gate draws", () => {
    // Act
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });

    // Assert
    expect(document.head.querySelector('style[data-source="render-colors.json#color_values"]')).not.toBeNull();
  });

  it("lays out the stack of workspace pages, the load button over it, and the gate below them", () => {
    // Act
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });

    // Assert
    expect([...document.body.children].map((e) => e.className || e.id)).toEqual([
      WEBHOST_FRAMES_CLASS,
      WEBHOST_LOAD_ID,
      WEBHOST_GATE_ID,
    ]);
  });

  it("hands the services' start the load button's element to draw in", () => {
    // Arrange
    let handed: HTMLElement | undefined;

    // Act
    started = startHost(document, window, {
      startServices: (_doc, _win, _host, deps) => {
        handed = deps.load;
      },
      installPopups: noPopups,
    });

    // Assert
    expect(handed?.id).toBe(WEBHOST_LOAD_ID);
  });

  it("hands the services' start the gate element to draw in", () => {
    // Arrange
    let handed: HTMLElement | undefined;

    // Act
    started = startHost(document, window, {
      startServices: (_doc, _win, _host, deps) => {
        handed = deps.gate;
      },
      installPopups: noPopups,
    });

    // Assert
    expect(handed?.id).toBe(WEBHOST_GATE_ID);
  });

  it("holds the workspace pages in the stack", () => {
    // Arrange
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });

    // Act
    started.ensure("a", "http://ws-a.localhost:1/");

    // Assert
    expect(document.querySelector(`.${WEBHOST_FRAMES_CLASS} iframe`)).not.toBeNull();
  });

  it("starts the services before the queued calls run", () => {
    // Arrange
    const order: string[] = [];
    window.__agentReplHostPending = [() => order.push("queued call")];

    // Act
    started = startHost(document, window, {
      startServices: () => {
        order.push("services");
      },
      installPopups: () => {
        order.push("popups");
      },
    });

    // Assert
    expect(order).toEqual(["services", "popups", "queued call"]);
  });

  it("runs a queued call that logs with the logger the services installed", () => {
    // Arrange
    const capture = captureLogRecords("debug");
    resetLoggingForTests();
    window.__agentReplHostPending = [(host) => host.focusMoved()];

    // Act
    started = startHost(document, window, {
      startServices: () => {
        setLogger(capture.logger);
      },
      installPopups: noPopups,
    });

    // Assert
    capture.logger.flush();
    expect(capture.sent.some((r) => r.operation === "page.focus-left")).toBe(true);
  });

  it("runs every call queued after one that logs", () => {
    // Arrange
    const capture = captureLogRecords("debug");
    resetLoggingForTests();
    window.__agentReplHostPending = [
      (host) => host.focusMoved(),
      (host) => host.ensure("a", "http://ws-a.localhost:1/"),
    ];

    // Act
    started = startHost(document, window, {
      startServices: () => {
        setLogger(capture.logger);
      },
      installPopups: noPopups,
    });

    // Assert
    expect(document.querySelectorAll(`.${WEBHOST_FRAMES_CLASS} iframe`).length).toBe(1);
  });

  it("draws no sidebar of its own", () => {
    // Act
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });

    // Assert
    expect(document.getElementById("ws-sidebar")).toBeNull();
  });

  it("marks the root as the host", () => {
    // Act
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });

    // Assert
    expect(document.documentElement.classList.contains(HOST_ROOT_CLASS)).toBe(true);
  });

  it("hands the host to Emacs", () => {
    // Act
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });

    // Assert
    expect(window.agentReplHost).toBe(started);
  });

  it("answers Emacs's focus question for the host", () => {
    // Arrange
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });
    const field = document.createElement("textarea");
    document.body.append(field);

    // Act
    field.focus();

    // Assert
    expect(window.xwHasFocus?.()).toBe(true);
  });

  it("puts its focus answer back once the page has loaded", () => {
    // Arrange
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });
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
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });

    // Assert
    expect(order.map((answer) => (JSON.parse(answer) as { outcome: string }).outcome)).toEqual(["created", "shown"]);
  });

  it("leaves no queue behind once it has run it", () => {
    // Arrange
    window.__agentReplHostPending = [];

    // Act
    started = startHost(document, window, { startServices: noServices, installPopups: noPopups });

    // Assert
    expect(window.__agentReplHostPending).toBeUndefined();
  });

  it("installs the sidebar's popup layer on the channel it opens, placed by the host's sidebar offset", () => {
    // Arrange
    const channel = quietPopupChannel();
    let env: PopupLayerEnv | null = null;

    // Act
    started = startHost(document, window, {
      startServices: noServices,
      popupChannel: () => channel,
      installPopups: (e) => {
        env = e;
      },
    });
    const offsets: number[] = [];
    (env as PopupLayerEnv | null)?.onOffset?.((pixels) => offsets.push(pixels));
    started.setSidebarOffset(7);

    // Assert
    expect([(env as PopupLayerEnv | null)?.channel, offsets]).toEqual([channel, [0, 7]]);
  });
});
