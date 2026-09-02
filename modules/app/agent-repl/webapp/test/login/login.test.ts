// @vitest-environment jsdom
import { beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
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
import { mountLoginOverlay } from "../../src/login/login.js";
import type { LoginTerminalView } from "../../src/login/terminal.js";
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
          // eslint-disable-next-line require-yield
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
    expect(refusal()).toBe("malformed");
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
