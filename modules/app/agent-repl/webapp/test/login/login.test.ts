// @vitest-environment jsdom
import { createControl } from "../../src/control.js";
import { beforeEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  CloseLoginErrorSchema,
  CloseLoginResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_login_pb";
import {
  OpenLoginErrorSchema,
  OpenLoginResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_login_pb";
import {
  SendLoginInputErrorSchema,
  SendLoginInputResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_send_login_input_pb";
import {
  LoginTerminalOutputSchema,
  type LoginTerminalOutput,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_login_terminal_pb";
import type { LoginLink } from "../../src/login/link.js";
import {
  closeLogin,
  handleFrame,
  mountLoginOverlay,
  openLogin,
  pump,
  report,
} from "../../src/login/login.js";
import type { LoginTerminalView } from "../../src/login/terminal.js";
import type { AppContext } from "../../src/rpc/context.js";
import { oneofArms } from "../arms.js";
import { RecordingSink, appContext } from "../topbar/fixtures.js";

/** A terminal that records what was written and hands back its keystrokes. */
function fakeTerminal(): LoginTerminalView & {
  written: number[][];
  type(data: Uint8Array): void;
  disposed: boolean;
} {
  let onData: (data: Uint8Array) => void = () => undefined;
  const term = {
    written: [] as number[][],
    disposed: false,
    write(data: Uint8Array): void {
      term.written.push(Array.from(data));
    },
    onData(fn: (data: Uint8Array) => void): void {
      onData = fn;
    },
    fit: () => ({ rows: 40, cols: 200 }),
    focus: () => undefined,
    dispose(): void {
      term.disposed = true;
    },
    type(data: Uint8Array): void {
      onData(data);
    },
  };
  return term;
}

const opened = (configDir: string) =>
  create(OpenLoginResponseSchema, { result: { case: "success", value: { configDir } } });

const bytes = (data: number[]): LoginTerminalOutput =>
  create(LoginTerminalOutputSchema, {
    output: { case: "bytes", value: { data: new Uint8Array(data) } },
  });

const closedFrame = (): LoginTerminalOutput =>
  create(LoginTerminalOutputSchema, { output: { case: "closed", value: {} } });

/** A link whose stream a test drives frame by frame. */
function scriptedLink(frames: LoginTerminalOutput[], sends: Partial<LoginLink> = {}): LoginLink & {
  sent: Array<{ kind: string; value: unknown }>;
} {
  const sent: Array<{ kind: string; value: unknown }> = [];
  const ok = () => create(SendLoginInputResponseSchema, { result: { case: "success", value: {} } });
  return {
    sent,
    attach: sends.attach ??
      (async function* () {
        for (const frame of frames) yield frame;
        // A login that is still running does not end its own stream; only the
        // `closed` frame or the overlay's abort does.
        await new Promise<never>(() => undefined);
      }),
    sendKeystrokes:
      sends.sendKeystrokes ??
      (async (data) => {
        sent.push({ kind: "keystrokes", value: Array.from(data) });
        return ok();
      }),
    sendResize:
      sends.sendResize ??
      (async (rows, cols) => {
        sent.push({ kind: "resize", value: { rows, cols } });
        return ok();
      }),
  };
}

let host: HTMLElement;

beforeEach(() => {
  host = document.createElement("div");
  host.setAttribute("data-component", "login-overlay");
  document.body.replaceChildren(host);
});

/** Flush the overlay's async open. */
const flush = async (): Promise<void> => {
  for (let i = 0; i < 10; i += 1) await Promise.resolve();
  await new Promise((resolve) => setTimeout(resolve, 0));
};

describe("mountLoginOverlay", () => {
  it("ships hidden, so a healthy page costs nothing", () => {
    mountLoginOverlay(host, appContext(), { terminalFactory: async () => fakeTerminal() });
    expect(host.hidden).toBe(true);
  });

  it("raises the overlay on a successful open", async () => {
    const overlay = mountLoginOverlay(
      host,
      appContext({ openLogin: () => opened("/root") }),
      { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
    );
    overlay.open();
    await flush();
    expect(host.hidden).toBe(false);
    overlay.dispose();
  });

  it("names the account root the terminal logs in", async () => {
    const overlay = mountLoginOverlay(
      host,
      appContext({ openLogin: () => opened("/Users/x/.claude-work") }),
      { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
    );
    overlay.open();
    await flush();
    expect(host.querySelector(".login-account")?.textContent).toBe("/Users/x/.claude-work");
    overlay.dispose();
  });

  it("writes every bytes frame to the terminal", async () => {
    const terminal = fakeTerminal();
    const overlay = mountLoginOverlay(host, appContext({ openLogin: () => opened("/root") }), {
      terminalFactory: async () => terminal,
      link: scriptedLink([bytes([1, 2]), bytes([3])]),
    });
    overlay.open();
    await flush();
    expect(terminal.written).toEqual([[1, 2], [3]]);
    overlay.dispose();
  });

  it("reports the viewer's geometry once the terminal is up", async () => {
    const link = scriptedLink([]);
    const overlay = mountLoginOverlay(host, appContext({ openLogin: () => opened("/root") }), {
      terminalFactory: async () => fakeTerminal(),
      link,
    });
    overlay.open();
    await flush();
    expect(link.sent).toContainEqual({ kind: "resize", value: { rows: 40, cols: 200 } });
    overlay.dispose();
  });

  it("forwards the reader's keystrokes as bytes", async () => {
    const terminal = fakeTerminal();
    const link = scriptedLink([]);
    const overlay = mountLoginOverlay(host, appContext({ openLogin: () => opened("/root") }), {
      terminalFactory: async () => terminal,
      link,
    });
    overlay.open();
    await flush();
    terminal.type(new Uint8Array([13]));
    await flush();
    expect(link.sent).toContainEqual({ kind: "keystrokes", value: [13] });
    overlay.dispose();
  });

  it("closes the overlay on the closed frame, the login child having exited", async () => {
    const overlay = mountLoginOverlay(host, appContext({ openLogin: () => opened("/root") }), {
      terminalFactory: async () => fakeTerminal(),
      link: scriptedLink([bytes([1]), closedFrame()]),
    });
    overlay.open();
    await flush();
    expect(host.hidden).toBe(true);
    overlay.dispose();
  });

  it("joins rather than building a second terminal on a second open", async () => {
    let terminals = 0;
    const overlay = mountLoginOverlay(host, appContext({ openLogin: () => opened("/root") }), {
      terminalFactory: async () => {
        terminals += 1;
        return fakeTerminal();
      },
      link: scriptedLink([]),
    });
    overlay.open();
    await flush();
    overlay.open();
    await flush();
    expect(terminals).toBe(1);
    overlay.dispose();
  });

  it("does NOT close on Escape, because the TUI needs the key", async () => {
    const overlay = mountLoginOverlay(host, appContext({ openLogin: () => opened("/root") }), {
      terminalFactory: async () => fakeTerminal(),
      link: scriptedLink([]),
    });
    overlay.open();
    await flush();
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    expect(host.hidden).toBe(false);
    overlay.dispose();
  });

  it("calls CloseLogin from the close button", async () => {
    let closed = 0;
    const overlay = mountLoginOverlay(
      host,
      appContext({
        openLogin: () => opened("/root"),
        closeLogin: () => {
          closed += 1;
          return create(CloseLoginResponseSchema, { result: { case: "success", value: {} } });
        },
      }),
      { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
    );
    overlay.open();
    await flush();
    host.querySelector("[data-login-close]")!.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    await flush();
    expect([closed, host.hidden]).toEqual([1, true]);
    overlay.dispose();
  });

  it("reports a stream that failed as a control-plane failure", async () => {
    const sink = new RecordingSink();
    const overlay = mountLoginOverlay(
      host,
      appContext({ openLogin: () => opened("/root") }, sink),
      {
        terminalFactory: async () => fakeTerminal(),
        link: scriptedLink([], {
          attach: async function* () {
            throw new Error("the pipe broke");
          },
        }),
      },
    );
    overlay.open();
    await flush();
    expect(sink.reported.map((k) => k.kind.case)).toContain("controlPlaneFailed");
    overlay.dispose();
  });

  it("reports a stream that ended without a closed frame", async () => {
    const sink = new RecordingSink();
    const overlay = mountLoginOverlay(
      host,
      appContext({ openLogin: () => opened("/root") }, sink),
      {
        terminalFactory: async () => fakeTerminal(),
        link: scriptedLink([], {
          attach: async function* () {
            yield bytes([1]);
          },
        }),
      },
    );
    overlay.open();
    await flush();
    expect(sink.reported.map((k) => k.kind.case)).toContain("controlPlaneFailed");
    overlay.dispose();
  });

  it("disposes the terminal it built", async () => {
    const terminal = fakeTerminal();
    const overlay = mountLoginOverlay(host, appContext({ openLogin: () => opened("/root") }), {
      terminalFactory: async () => terminal,
      link: scriptedLink([]),
    });
    overlay.open();
    await flush();
    overlay.dispose();
    expect(terminal.disposed).toBe(true);
  });
});

describe("the refusals", () => {
  const refusal = (): string | null =>
    host.querySelector(".refusal")?.getAttribute("data-arm") ?? null;

  const openCauses: Readonly<Record<string, unknown>> = {
    unknownWorkspace: {},
    workspaceRefMismatch: { registryDir: "/elsewhere" },
    transferringAway: { address: "127.0.0.1:9" },
    notYetAdopted: {},
    spawnFailed: { detail: "no pty" },
  };

  for (const arm of oneofArms(OpenLoginErrorSchema, "cause")) {
    it(`states the OpenLogin ${arm} refusal in the header`, async () => {
      const overlay = mountLoginOverlay(
        host,
        appContext({
          openLogin: () =>
            create(OpenLoginResponseSchema, {
              result: { case: "error", value: { cause: { case: arm, value: openCauses[arm] } as never } },
            }),
        }),
        { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
      );
      overlay.open();
      await flush();
      expect(refusal()).toBe(arm);
      overlay.dispose();
    });
  }

  it("states the refusal at the control that opened the login", async () => {
    // ARRANGE: every click's refusal renders AT the clicked control, and the
    // control here belongs to another component (the topbar's account chip),
    // so it is handed in rather than guessed at.
    const control = createControl();
    document.body.append(control);
    const overlay = mountLoginOverlay(
      host,
      appContext({
        openLogin: () =>
          create(OpenLoginResponseSchema, {
            result: { case: "error", value: { cause: { case: "unknownWorkspace", value: {} } } },
          }),
      }),
      { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
    );
    // ACT
    overlay.open(control);
    await flush();
    // ASSERT
    expect(control.querySelector(".refusal")?.getAttribute("data-arm")).toBe("unknownWorkspace");
    overlay.dispose();
    control.remove();
  });

  it("states it exactly once, at the control rather than also in the header", async () => {
    // ARRANGE
    const control = createControl();
    document.body.append(control);
    const overlay = mountLoginOverlay(
      host,
      appContext({
        openLogin: () =>
          create(OpenLoginResponseSchema, {
            result: { case: "error", value: { cause: { case: "unknownWorkspace", value: {} } } },
          }),
      }),
      { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
    );
    // ACT
    overlay.open(control);
    await flush();
    // ASSERT: one click, one refusal.
    expect(refusal()).toBeNull();
    overlay.dispose();
    control.remove();
  });

  it("leaves the overlay hidden when the open was refused", async () => {
    const overlay = mountLoginOverlay(
      host,
      appContext({
        openLogin: () =>
          create(OpenLoginResponseSchema, {
            result: { case: "error", value: { cause: { case: "notYetAdopted", value: {} } } },
          }),
      }),
      { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
    );
    overlay.open();
    await flush();
    expect(host.hidden).toBe(true);
    overlay.dispose();
  });

  it("refuses an OpenLogin error naming no cause", async () => {
    const overlay = mountLoginOverlay(
      host,
      appContext({
        openLogin: () =>
          create(OpenLoginResponseSchema, {
            result: { case: "error", value: create(OpenLoginErrorSchema, {}) },
          }),
      }),
      { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
    );
    overlay.open();
    await flush();
    // An error with no cause set is a frame this build cannot read, not a
    // refusal with no words: it is reported through the failure sink, and no
    // sentence is invented at the control (src/rpc/refuse.ts).
    expect(refusal()).toBeNull();
    overlay.dispose();
  });

  const inputCauses: Readonly<Record<string, unknown>> = {
    unknownWorkspace: {},
    workspaceRefMismatch: { registryDir: "/elsewhere" },
    transferringAway: { address: "127.0.0.1:9" },
    notYetAdopted: {},
    noLoginOpen: {},
  };

  for (const arm of oneofArms(SendLoginInputErrorSchema, "cause")) {
    it(`states the SendLoginInput ${arm} refusal in the header`, async () => {
      const terminal = fakeTerminal();
      const overlay = mountLoginOverlay(
        host,
        appContext({ openLogin: () => opened("/root") }),
        {
          terminalFactory: async () => terminal,
          link: scriptedLink([], {
            sendKeystrokes: async () =>
              create(SendLoginInputResponseSchema, {
                result: {
                  case: "error",
                  value: { cause: { case: arm, value: inputCauses[arm] } as never },
                },
              }),
            sendResize: async () =>
              create(SendLoginInputResponseSchema, { result: { case: "success", value: {} } }),
          }),
        },
      );
      overlay.open();
      await flush();
      terminal.type(new Uint8Array([13]));
      await flush();
      expect(refusal()).toBe(arm);
      overlay.dispose();
    });
  }

  const closeCauses: Readonly<Record<string, unknown>> = {
    unknownWorkspace: {},
    workspaceRefMismatch: { registryDir: "/elsewhere" },
    transferringAway: { address: "127.0.0.1:9" },
    notYetAdopted: {},
  };

  for (const arm of oneofArms(CloseLoginErrorSchema, "cause")) {
    it(`states the CloseLogin ${arm} refusal and still closes the overlay`, async () => {
      const overlay = mountLoginOverlay(
        host,
        appContext({
          openLogin: () => opened("/root"),
          closeLogin: () =>
            create(CloseLoginResponseSchema, {
              result: { case: "error", value: { cause: { case: arm, value: closeCauses[arm] } as never } },
            }),
        }),
        { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
      );
      overlay.open();
      await flush();
      host
        .querySelector("[data-login-close]")!
        .dispatchEvent(new MouseEvent("click", { bubbles: true }));
      await flush();
      expect([refusal(), host.hidden]).toEqual([arm, true]);
      overlay.dispose();
    });
  }
});

describe("report: one SendLoginInput, awaited", () => {
  /** A header standing on its own — `report` only ever draws into one. */
  function header(): HTMLElement {
    const el = document.createElement("div");
    host.replaceChildren(el);
    return el;
  }

  it("states a transport failure at the header when the send never answered", async () => {
    // ARRANGE
    const el = header();
    // ACT
    await report(appContext(), el, Promise.reject(new Error("socket closed")));
    // ASSERT
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });

  it("files a result arm this build cannot read through the failure sink", async () => {
    // ARRANGE: a newer daemon's third arm is a frame this build cannot read,
    // not a refusal — the overlay says so through the sink.
    const sink = new RecordingSink();
    const el = header();
    // ACT
    await report(appContext({}, sink), el, Promise.resolve({ result: { case: "deferred" } }));
    // ASSERT
    expect(sink.reported.map((k) => k.kind.case)).toEqual(["frameUndecodable"]);
  });

  it("draws no refusal at the header for a result arm it cannot read", async () => {
    // ARRANGE
    const el = header();
    // ACT
    await report(appContext({}, new RecordingSink()), el, Promise.resolve({
      result: { case: "deferred" },
    }));
    // ASSERT
    expect(el.querySelector(".refusal")).toBeNull();
  });

  it("files a response naming no result arm through the failure sink", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const el = header();
    // ACT
    await report(appContext({}, sink), el, Promise.resolve({ result: {} }));
    // ASSERT
    expect(sink.reported.map((k) => k.kind.case)).toEqual(["frameUndecodable"]);
  });

  it("rethrows an error that is not a view this build could not read", async () => {
    // ARRANGE: only a MalformedView is the refusal path's to interpret;
    // anything else must travel up rather than being swallowed at the header.
    const el = header();
    const answered = {
      get result(): never {
        throw new Error("reading the answer threw");
      },
    };
    // ACT / ASSERT
    await expect(report(appContext(), el, Promise.resolve(answered))).rejects.toThrow(
      "reading the answer threw",
    );
  });
});

describe("openLogin: the paths that are not an answer", () => {
  const header = (): HTMLElement => {
    const el = document.createElement("div");
    host.replaceChildren(el);
    return el;
  };

  /** A context whose client is scripted directly, past the wire schema. */
  const withClient = (client: unknown, sink = new RecordingSink()): AppContext => ({
    ...appContext({}, sink),
    client: client as AppContext["client"],
  });

  /** A well-formed response whose result names an arm this build has not. */
  const openArm = (arm: string) => {
    const response = create(OpenLoginResponseSchema, {});
    (response as { result: unknown }).result = { case: arm, value: {} };
    return response;
  };

  it("states a transport refusal at the header when OpenLogin never answered", async () => {
    // ARRANGE
    const el = header();
    const ctx = appContext({
      openLogin: () => {
        throw new ConnectError("socket closed", Code.Unavailable);
      },
    });
    // ACT
    const configDir = await openLogin(ctx, el);
    // ASSERT
    expect([configDir, el.querySelector(".refusal")?.getAttribute("data-arm")]).toEqual([
      null,
      "transport",
    ]);
  });

  it("leaves the overlay hidden when the open failed at the transport", async () => {
    // ARRANGE
    const overlay = mountLoginOverlay(
      host,
      appContext({
        openLogin: () => {
          throw new ConnectError("socket closed", Code.Unavailable);
        },
      }),
      { terminalFactory: async () => fakeTerminal(), link: scriptedLink([]) },
    );
    // ACT
    overlay.open();
    await flush();
    // ASSERT
    expect(host.hidden).toBe(true);
    overlay.dispose();
  });

  it("files a result arm this build cannot read through the failure sink", async () => {
    // ARRANGE: a newer daemon's third arm on OpenLoginResponse.result.
    const sink = new RecordingSink();
    const el = header();
    const ctx = withClient({ openLogin: async () => openArm("deferred") }, sink);
    // ACT
    const configDir = await openLogin(ctx, el);
    // ASSERT
    expect([configDir, sink.reported.map((k) => k.kind.case)]).toEqual([null, ["frameUndecodable"]]);
  });

  it("draws no refusal at the header for a result arm it cannot read", async () => {
    // ARRANGE
    const el = header();
    const ctx = withClient({ openLogin: async () => openArm("deferred") });
    // ACT
    await openLogin(ctx, el);
    // ASSERT
    expect(el.querySelector(".refusal")).toBeNull();
  });
});

describe("closeLogin: the overlay comes down either way", () => {
  const header = (): HTMLElement => {
    const el = document.createElement("div");
    host.replaceChildren(el);
    return el;
  };

  const withClient = (client: unknown, sink = new RecordingSink()): AppContext => ({
    ...appContext({}, sink),
    client: client as AppContext["client"],
  });

  /** A well-formed response whose result names an arm this build has not. */
  const closeArm = (arm: string) => {
    const response = create(CloseLoginResponseSchema, {});
    (response as { result: unknown }).result = { case: arm, value: {} };
    return response;
  };

  it("states a transport refusal at the header when CloseLogin never answered", async () => {
    // ARRANGE
    const el = header();
    const ctx = appContext({
      closeLogin: () => {
        throw new ConnectError("socket closed", Code.Unavailable);
      },
    });
    // ACT
    await closeLogin(ctx, el, () => undefined);
    // ASSERT
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });

  it("still hides the overlay when CloseLogin never answered", async () => {
    // ARRANGE: leaving a terminal up over an unreachable daemon helps nobody.
    const el = header();
    let hidden = 0;
    const ctx = appContext({
      closeLogin: () => {
        throw new ConnectError("socket closed", Code.Unavailable);
      },
    });
    // ACT
    await closeLogin(ctx, el, () => {
      hidden += 1;
    });
    // ASSERT
    expect(hidden).toBe(1);
  });

  it("files a result arm this build cannot read through the failure sink", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const el = header();
    const ctx = withClient(
      { closeLogin: async () => closeArm("deferred") },
      sink,
    );
    // ACT
    await closeLogin(ctx, el, () => undefined);
    // ASSERT
    expect(sink.reported.map((k) => k.kind.case)).toEqual(["frameUndecodable"]);
  });

  it("still hides the overlay for a result arm it cannot read", async () => {
    // ARRANGE
    const el = header();
    let hidden = 0;
    const ctx = withClient({ closeLogin: async () => closeArm("deferred") });
    // ACT
    await closeLogin(ctx, el, () => {
      hidden += 1;
    });
    // ASSERT
    expect(hidden).toBe(1);
  });
});

describe("pump: the stream's own conclusions", () => {
  it("reports a non-Error thrown by the stream by its string form", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const terminal = fakeTerminal();
    const controller = new AbortController();
    const link = scriptedLink([], {
      attach: async function* () {
        throw "the pipe broke";
      },
    });
    // ACT
    await pump(appContext({}, sink), link, controller, terminal, () => undefined);
    // ASSERT
    const kind = sink.reported[0]?.kind;
    expect(kind?.case === "controlPlaneFailed" ? kind.value.cause : null).toBe("the pipe broke");
  });

  it("stays silent when the stream throws because the overlay aborted it", async () => {
    // ARRANGE: an abort is the overlay closing, not the link failing.
    const sink = new RecordingSink();
    const controller = new AbortController();
    controller.abort();
    const link = scriptedLink([], {
      attach: async function* () {
        throw new Error("aborted");
      },
    });
    // ACT
    await pump(appContext({}, sink), link, controller, fakeTerminal(), () => undefined);
    // ASSERT
    expect(sink.reported).toEqual([]);
  });

  it("does not hide again when the stream throws after the overlay aborted it", async () => {
    // ARRANGE
    let hidden = 0;
    const controller = new AbortController();
    controller.abort();
    const link = scriptedLink([], {
      attach: async function* () {
        throw new Error("aborted");
      },
    });
    // ACT
    await pump(appContext({}, new RecordingSink()), link, controller, fakeTerminal(), () => {
      hidden += 1;
    });
    // ASSERT
    expect(hidden).toBe(0);
  });

  it("stays silent when the stream ENDS because the overlay aborted it", async () => {
    // ARRANGE: no closed frame, but the abort explains the ending.
    const sink = new RecordingSink();
    const controller = new AbortController();
    controller.abort();
    const link = scriptedLink([], {
      attach: async function* () {
        yield bytes([1]);
      },
    });
    // ACT
    await pump(appContext({}, sink), link, controller, fakeTerminal(), () => undefined);
    // ASSERT
    expect(sink.reported).toEqual([]);
  });

  it("does not hide when the stream ends after the overlay aborted it", async () => {
    // ARRANGE
    let hidden = 0;
    const controller = new AbortController();
    controller.abort();
    const link = scriptedLink([], {
      attach: async function* () {
        yield bytes([1]);
      },
    });
    // ACT
    await pump(appContext({}, new RecordingSink()), link, controller, fakeTerminal(), () => {
      hidden += 1;
    });
    // ASSERT
    expect(hidden).toBe(0);
  });
});

describe("handleFrame", () => {
  it("refuses an output arm this build cannot draw", () => {
    // ARRANGE: a newer daemon's third arm on LoginTerminalOutput.output.
    const frame = { output: { case: "resized", value: {} } } as unknown as LoginTerminalOutput;
    // ACT / ASSERT
    expect(() => handleFrame(frame, fakeTerminal())).toThrow(
      "arm 'resized' is not one this build can draw",
    );
  });
});

describe("the overlay's own defaults", () => {
  it("mounts its hidden panel with no deps supplied at all", () => {
    // ARRANGE / ACT: no terminalFactory and no link, so both defaults are
    // taken; neither is reached until the overlay is opened.
    const overlay = mountLoginOverlay(host, appContext());
    // ASSERT
    expect([host.hidden, host.querySelector(".login-panel") !== null]).toEqual([true, true]);
    overlay.dispose();
  });
});
