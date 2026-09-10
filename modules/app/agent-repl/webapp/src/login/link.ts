/**
 * The login pty's two directions.
 *
 * A SERVER STREAM OUT, UNARY CALLS IN (ruled 2026-08-29): a WKWebView on a
 * cleartext loopback origin cannot speak a bidirectional Connect stream, so the
 * bytes come down `WatchLoginTerminal` and the keystrokes and resizes go back
 * as `SendLoginInput`. That asymmetry is the transport's, not the protocol's.
 *
 * THIS STREAM IS NOT A STANDING ONE, and that is why it does not go through
 * `watchStream`. A component stream never ends and any ending is a fault; this
 * one CONCLUDES, with a `closed` frame, when the login child exits — that is
 * the whole point of the frame. Run through the standing helper it would be
 * reported as a transport failure and reopened, respawning a login the user
 * just finished.
 *
 * EVERY FRAME IS STILL STRICT-CHECKED. The helper's reconnect is what is
 * inapplicable here, not its refusal of a frame carrying fields this build
 * cannot read.
 */
import { LoginTerminalOutputSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_login_terminal_pb";
import type { LoginTerminalOutput } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_login_terminal_pb";
import {
  SendLoginInputResponseSchema,
  type SendLoginInputResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_send_login_input_pb";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { assertNoUnknownFields } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";

export interface LoginLink {
  /** The pty's output, until the `closed` frame. SIGNAL ends it early. */
  attach(signal: AbortSignal): AsyncIterable<LoginTerminalOutput>;
  /** Keystrokes, forwarded to the pty verbatim. */
  sendKeystrokes(data: Uint8Array): Promise<SendLoginInputResponse>;
  /** The viewer's terminal geometry, which the OAuth URL's wrapping depends on. */
  sendResize(rows: number, cols: number): Promise<SendLoginInputResponse>;
}

/** Bind the link for this page's workspace. */
export function connectLoginLink(ctx: AppContext): LoginLink {
  return {
    attach(signal: AbortSignal): AsyncIterable<LoginTerminalOutput> {
      log("debug", "attaching to the login terminal", { operation: "login.attach" });
      return strictFrames(
        ctx.streams.watch("loginTerminal", { workspace: ctx.workspace }, signal),
      );
    },

    sendKeystrokes(data: Uint8Array): Promise<SendLoginInputResponse> {
      return callUnary(
        ctx,
        "SendLoginInput",
        (client) =>
          client.sendLoginInput({
            workspace: ctx.workspace,
            input: { case: "keystrokes", value: { data } },
          }),
        SendLoginInputResponseSchema,
      );
    },

    sendResize(rows: number, cols: number): Promise<SendLoginInputResponse> {
      log("debug", `reporting the login terminal geometry ${rows}x${cols}`, {
        operation: "login.resize",
        context: { rows, cols },
      });
      return callUnary(
        ctx,
        "SendLoginInput",
        (client) =>
          client.sendLoginInput({
            workspace: ctx.workspace,
            input: { case: "resize", value: { rows, cols } },
          }),
        SendLoginInputResponseSchema,
      );
    },
  };
}

/**
 * Pass frames through, refusing any that carries a field this build has no
 * descriptor for.
 *
 * A newer daemon's extra fact on a login frame is a loud refusal rather than a
 * terminal that silently misses part of what it was told.
 */
async function* strictFrames(
  source: AsyncIterable<LoginTerminalOutput>,
): AsyncIterable<LoginTerminalOutput> {
  for await (const frame of source) {
    assertNoUnknownFields(LoginTerminalOutputSchema, frame);
    yield frame;
  }
}
