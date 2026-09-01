// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  LoginTerminalOutputSchema,
  type LoginTerminalOutput,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_login_terminal_pb";
import {
  SendLoginInputRequestSchema,
  SendLoginInputResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_send_login_input_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { connectLoginLink } from "../../src/login/link.js";
import { appContext } from "../topbar/fixtures.js";

const bytes = (data: number[]): LoginTerminalOutput =>
  create(LoginTerminalOutputSchema, {
    output: { case: "bytes", value: { data: new Uint8Array(data) } },
  });

const closed = (): LoginTerminalOutput =>
  create(LoginTerminalOutputSchema, { output: { case: "closed", value: {} } });

const ok = () => create(SendLoginInputResponseSchema, { result: { case: "success", value: {} } });

/** Collect every frame ATTACH yields. */
async function drain(frames: LoginTerminalOutput[]): Promise<string[]> {
  const ctx = appContext({
    watchLoginTerminal: async function* () {
      for (const frame of frames) yield frame;
    },
  });
  const seen: string[] = [];
  for await (const frame of connectLoginLink(ctx).attach(new AbortController().signal)) {
    seen.push(frame.output.case ?? "unset");
  }
  return seen;
}

describe("connectLoginLink: attach", () => {
  it("yields the pty's bytes in order", async () => {
    expect(await drain([bytes([1]), bytes([2])])).toEqual(["bytes", "bytes"]);
  });

  it("ends cleanly on the closed frame, which is a legitimate conclusion", async () => {
    expect(await drain([bytes([1]), closed()])).toEqual(["bytes", "closed"]);
  });

  it("refuses a frame carrying a field this build has no descriptor for", async () => {
    const frame = bytes([1]);
    frame.$unknown = [{ no: 999, wireType: 0, data: new Uint8Array([1]) }];
    await expect(drain([frame])).rejects.toThrow(MalformedView);
  });
});

describe("connectLoginLink: the input direction", () => {
  it("sends keystrokes on the keystrokes arm", async () => {
    // ARRANGE
    let arm = "";
    const ctx = appContext({
      sendLoginInput: (req: ReturnType<typeof create<typeof SendLoginInputRequestSchema>>) => {
        arm = req.input.case ?? "unset";
        return ok();
      },
    });
    // ACT
    await connectLoginLink(ctx).sendKeystrokes(new Uint8Array([13]));
    // ASSERT
    expect(arm).toBe("keystrokes");
  });

  it("carries the keystroke bytes verbatim", async () => {
    let data: Uint8Array = new Uint8Array();
    const ctx = appContext({
      sendLoginInput: (req) => {
        if (req.input.case === "keystrokes") data = new Uint8Array(req.input.value.data);
        return ok();
      },
    });
    await connectLoginLink(ctx).sendKeystrokes(new Uint8Array([13, 10]));
    expect(Array.from(data)).toEqual([13, 10]);
  });

  it("sends geometry on the resize arm", async () => {
    let rows = 0;
    let cols = 0;
    const ctx = appContext({
      sendLoginInput: (req) => {
        if (req.input.case === "resize") {
          rows = req.input.value.rows;
          cols = req.input.value.cols;
        }
        return ok();
      },
    });
    await connectLoginLink(ctx).sendResize(40, 200);
    expect([rows, cols]).toEqual([40, 200]);
  });

  it("refuses a response whose result oneof is unset", async () => {
    const ctx = appContext({
      sendLoginInput: () => create(SendLoginInputResponseSchema, {}),
    });
    await expect(connectLoginLink(ctx).sendKeystrokes(new Uint8Array([1]))).rejects.toThrow(
      MalformedView,
    );
  });
});
