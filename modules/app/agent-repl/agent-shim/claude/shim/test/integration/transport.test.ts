/**
 * test/integration/transport.test.ts — the wire, below the session.
 *
 * The subject here is the shim's TRANSPORT contract: that both HTTP dialects
 * reach one socket, that validation refuses an illegal request before the
 * engine sees it, that the kicked workflow verbs answer Unimplemented in both
 * shapes, that a standing stream's acceptance is observable before its first
 * frame, and that a client closing a stream ends nothing on the shim.
 *
 * Nothing here asserts anything about a conversation: a transport failure and a
 * session failure are different diagnoses, and mixing them into one suite makes
 * every failure ambiguous.
 */
import { create } from "@bufbuild/protobuf";
import { Code } from "@connectrpc/connect";
import { afterEach, describe, expect, test } from "vitest";
import { shimv1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import {
  connectCode,
  freshSession,
  openStream,
  readHistoryFirst,
  startTurnRequest,
  streamOpenCode,
  watchAgentRequest,
  workId,
} from "../integration-support/client.js";
import { sessionStarted, sessionUpdate } from "../integration-support/expect.js";
import { rawBody, rawStreamOpenH1, rawStreamOpenH2 } from "../integration-support/raw.js";

afterEach(cleanupShims);

describe("both dialects over one socket", () => {
  test("Connect over HTTP/1.1 reaches the service", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.startSession(freshSession());

    expect(sessionStarted(response).vendorSessionId).not.toBe("");
  });

  test("Connect over h2c reaches the SAME service on the SAME socket", async () => {
    // One socket, both HTTP versions: the listener sniffs the HTTP/2 preface
    // (`allowHTTP1` is a TLS-only option and does nothing on a cleartext h2
    // server, and a unix socket has no ALPN to negotiate with).
    const shim = await spawnShim();

    const response = await shim.clients.h2.startSession(freshSession());

    expect(sessionStarted(response).vendorSessionId).not.toBe("");
  });

  test("a stream works over h2c, where socketPath would have been ignored", async () => {
    const shim = await spawnShim();
    await shim.clients.h2.startSession(freshSession());

    const watch = openStream((options) =>
      shim.clients.h2.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    const opening = await watch.next();

    expect(sessionUpdate(opening).update.case).toBe("diagnostics");
    watch.close();
  });
});

describe("validation refuses before the engine", () => {
  test("an unset request oneof is InvalidArgument", async () => {
    // There is no legal response to an illegal request: answering "success:
    // false" would teach the caller its message was understood.
    const shim = await spawnShim();

    const code = await connectCode(
      shim.clients.h1.startSession(create(shimv1.StartSessionRequestSchema, {})),
    );

    expect(code).toBe(Code.InvalidArgument);
  });

  test("an unset ReadHistory position is InvalidArgument", async () => {
    const shim = await spawnShim();

    const code = await connectCode(
      shim.clients.h1.readHistory(
        create(shimv1.ReadHistoryRequestSchema, { pageSize: 10 }),
      ),
    );

    expect(code).toBe(Code.InvalidArgument);
  });

  test("an unset UpdateAgent input is InvalidArgument", async () => {
    const shim = await spawnShim();

    const code = await connectCode(
      shim.clients.h1.updateAgent(create(shimv1.UpdateAgentRequestSchema, {})),
    );

    expect(code).toBe(Code.InvalidArgument);
  });
});

describe("page_size 0 is REFUSED (presence, never sentinels)", () => {
  test("StartTurn with page_size 0 is InvalidArgument", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const code = await connectCode(
      shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "hello", pageSize: 0 })),
    );

    expect(code).toBe(Code.InvalidArgument);
  });

  test("WatchAgent with page_size 0 is InvalidArgument", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const stream = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest({ pageSize: 0 }), options),
    );

    expect(await streamOpenCode(stream)).toBe(Code.InvalidArgument);
  });

  test("ReadHistory with page_size 0 is InvalidArgument", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const code = await connectCode(shim.clients.h1.readHistory(readHistoryFirst({ pageSize: 0 })));

    expect(code).toBe(Code.InvalidArgument);
  });
});

describe("the workflow trio (WORKFLOW IS KICKED)", () => {
  test("GetWorkflow answers Unimplemented", async () => {
    // Unimplemented and not an empty success: a caller receiving an empty
    // workflow cannot tell "no workflows" from "this shim does not do
    // workflows", and would draw the difference as an absence.
    const shim = await spawnShim();

    const code = await connectCode(
      shim.clients.h1.getWorkflow(
        create(shimv1.GetWorkflowRequestSchema, { work: workId("w1") }),
      ),
    );

    expect(code).toBe(Code.Unimplemented);
  });

  test("StopWorkflow answers Unimplemented", async () => {
    const shim = await spawnShim();

    const code = await connectCode(
      shim.clients.h1.stopWorkflow(
        create(shimv1.StopWorkflowRequestSchema, { work: workId("w1") }),
      ),
    );

    expect(code).toBe(Code.Unimplemented);
  });

  test("WatchWorkflow answers Unimplemented at the stream open", async () => {
    const shim = await spawnShim();

    const stream = openStream((options) =>
      shim.clients.h1.watchWorkflow(
        create(shimv1.WatchWorkflowRequestSchema, {
          watch: create(shimv1.WorkflowWatchTokenSchema, { value: "tok" }),
        }),
        options,
      ),
    );

    expect(await streamOpenCode(stream)).toBe(Code.Unimplemented);
  });
});

describe("acceptance is observable before the first frame", () => {
  // THE STANDING-STREAM TRANSPORT RULE: the shim flushes response headers the
  // moment it accepts a watch, so connect-go's "a server-stream refusal
  // surfaces only at the first Receive" does not make bring-up ambiguous. The
  // raw dial is the only place the head and the first body byte are separately
  // observable — a Connect client hands back an iterable and hides the head.
  test("WatchSession's head arrives before any body byte over HTTP/1.1", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const observed = await rawStreamOpenH1(
      shim.dirs.listen,
      "/shim.v1.Shim/WatchSession",
      rawBody(
        shimv1.WatchSessionRequestSchema,
        create(shimv1.WatchSessionRequestSchema, {}),
      ),
    );

    expect(observed.status).toBe(200);
    expect(observed.contentType).toContain("connect");
    expect(observed.bodyBeforeHeaders).toBe(false);
  });

  test("WatchSession's response headers precede the first data frame over h2c", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const observed = await rawStreamOpenH2(
      shim.dirs.listen,
      "/shim.v1.Shim/WatchSession",
      rawBody(
        shimv1.WatchSessionRequestSchema,
        create(shimv1.WatchSessionRequestSchema, {}),
      ),
    );

    expect(observed.status).toBe(200);
    expect(observed.bodyBeforeHeaders).toBe(false);
    expect(observed.firstByteAt).not.toBeNull();
  });

  test("WatchAgent's head arrives before its opening page", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const observed = await rawStreamOpenH1(
      shim.dirs.listen,
      "/shim.v1.Shim/WatchAgent",
      rawBody(shimv1.WatchAgentRequestSchema, watchAgentRequest()),
    );

    expect(observed.status).toBe(200);
    expect(observed.bodyBeforeHeaders).toBe(false);
  });

  test("WatchBash's head arrives before its start frame", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const observed = await rawStreamOpenH1(
      shim.dirs.listen,
      "/shim.v1.Shim/WatchBash",
      rawBody(
        shimv1.WatchBashRequestSchema,
        create(shimv1.WatchBashRequestSchema, { work: workId("nobody") }),
      ),
    );

    // Even a REFUSED open flushes its head: the refusal rides the stream's own
    // end-of-stream frame, so the head cannot be waiting on the verdict.
    expect(observed.status).toBe(200);
    expect(observed.bodyBeforeHeaders).toBe(false);
  });
});

describe("a stream closed by the client ends nothing", () => {
  test("cancelling WatchSession leaves the session serving", async () => {
    // ATTACH ONLY: closing a watch ends nothing, which is what makes a
    // restarted daemon able to reattach without having killed anything.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const watch = openStream((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await watch.next();

    watch.close();

    // The session is still there: a second StartSession reports it as ALREADY
    // started rather than starting a new one, and a fresh watch still opens.
    const second = await shim.clients.h1.startSession(freshSession());
    expect(second.result.case).toBe("failure");
    const reopened = openStream((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    expect(sessionUpdate(await reopened.next()).update.case).toBe("diagnostics");
    expect(started.vendorSessionId).not.toBe("");
    reopened.close();
  });

  test("cancelling WatchAgent mid-turn leaves the turn running", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    watch.close();

    // The turn is still open: a second StartTurn is refused for that reason.
    const second = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "another" }),
    );
    expect(second.result.case).toBe("failure");
  });
});
