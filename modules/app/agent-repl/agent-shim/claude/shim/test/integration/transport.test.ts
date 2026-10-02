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
  openStream, openSessionUpdates,
  readHistoryFirst,
  startTurnRequest,
  streamOpenCode,
  watchAgentRequest,
  workId,
} from "../integration-support/client.js";
import { sessionStarted, sessionUpdate, watchAgentPage } from "../integration-support/expect.js";
import { rawBody, rawHeadH1, rawStreamOpenH2 } from "../integration-support/raw.js";

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

  test("a healthy stream accept reports NO response-encoding loss", async () => {
    // The early head goes out before the adapter negotiates a response
    // encoding, so the adapter's `Connect-Content-Encoding: gzip` -- its answer
    // to the client's accept list -- could never reach the client, and the
    // guard fired on EVERY healthy WatchSession and WatchAgent accept. An error
    // on the happy path is an error that teaches a reader to ignore errors.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const watch = openStream((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await watch.next();
    watch.close();

    expect(
      shim.log.records().filter((record) => record.message.includes("response encoding")),
    ).toEqual([]);
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

  test("an UNSPECIFIED effort level is InvalidArgument", async () => {
    const shim = await spawnShim();

    const code = await connectCode(
      shim.clients.h1.setSessionEffort(create(shimv1.SetSessionEffortRequestSchema, {})),
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
  //
  // WHICH HALF EACH TRANSPORT WITNESSES. Over h2c the head is its own HEADERS
  // frame, so the ORDERING claim is directly observable and is asserted below.
  // Over HTTP/1.1 the head and the first frame share one byte stream and can
  // arrive in a single read, so ordering is not an observable there at all —
  // the h1 tests instead assert what the rule actually needs from h1: the
  // response head ALONE decides acceptance, arriving complete and parseable
  // with no frame consulted.
  test("WatchSession's acceptance is decidable from the HTTP/1.1 head alone", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    // Resolving at all is half the claim: the helper never reads a body byte.
    const head = await rawHeadH1(
      shim.dirs.listen,
      "/shim.v1.Shim/WatchSession",
      rawBody(
        shimv1.WatchSessionRequestSchema,
        create(shimv1.WatchSessionRequestSchema, {}),
      ),
    );

    expect(head.status).toBe(200);
    expect(head.contentType).toContain("connect");
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

  test("WatchAgent's acceptance is decidable from the HTTP/1.1 head alone", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const head = await rawHeadH1(
      shim.dirs.listen,
      "/shim.v1.Shim/WatchAgent",
      rawBody(shimv1.WatchAgentRequestSchema, watchAgentRequest()),
    );

    expect(head.status).toBe(200);
    expect(head.contentType).toContain("connect");
  });

  test("WatchBash's acceptance is decidable from the HTTP/1.1 head alone", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const head = await rawHeadH1(
      shim.dirs.listen,
      "/shim.v1.Shim/WatchBash",
      rawBody(
        shimv1.WatchBashRequestSchema,
        create(shimv1.WatchBashRequestSchema, { work: workId("nobody") }),
      ),
    );

    // Even a REFUSED open answers from its head: the refusal rides the stream's
    // own end-of-stream frame, so the head is a 200 that says "accepted as a
    // stream" without the verdict being in it.
    expect(head.status).toBe(200);
    expect(head.contentType).toContain("connect");
  });
});

describe("a stream closed by the client ends nothing", () => {
  test("cancelling WatchSession leaves the session serving", async () => {
    // ATTACH ONLY: closing a watch ends nothing, which is what makes a
    // restarted daemon able to reattach without having killed anything.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const watch = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await watch.next();

    watch.close();

    // The session is still there: a second StartSession reports it as ALREADY
    // started rather than starting a new one, and a fresh watch still opens.
    const second = await shim.clients.h1.startSession(freshSession());
    expect(second.result.case).toBe("failure");
    const reopened = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    expect(sessionUpdate(await reopened.next()).update.case).toBe("diagnostics");
    expect(started.vendorSessionId).not.toBe("");
    reopened.close();
  });

  test("cancelling WatchAgent CANCELS the shim's own tail on the store", async () => {
    // ATTACH ONLY CUTS BOTH WAYS. A daemon that stops reading an agent must not
    // leave the shim holding a `WatchAgentSession` open against the store: a
    // subscription per abandoned watch leaks, and the store has no way to tell
    // a reader that will never return from one that is merely slow. Nothing on
    // the client says whether the tail was cancelled, so the fake store's
    // open-tail ledger is the only place the fact is observable.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    // Drive a turn so a row has actually travelled THROUGH the tail: an open
    // that had not yet reached the store would make the ledger empty for a
    // reason that has nothing to do with cancellation.
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await watch.until((frame) => frame.frame.case === "entry");
    // THE TAIL'S OPEN IS AN EVENT, not a level. A row reaching this client says
    // the shim pulled from its tail; it does not say the store's own generator
    // has begun on the other side of the socket, so reading `openTails()` here
    // raced the store and read an empty ledger under load.
    const token = await (shim.store?.tailOpened() ??
      Promise.reject(new Error("this shim has no store")));
    expect(shim.store?.openTails()).toEqual([token]);

    watch.close();

    // The cancellation crosses a real socket, so the client returning and the
    // server's generator unwinding are two instants; this awaits the second.
    await shim.store?.tailClosed(token);
    expect(shim.store?.openTails()).toEqual([]);
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

describe("many streams on one socket", () => {
  test("several HTTP/1.1 WatchAgent streams all receive their opening page", async () => {
    // THE EARLY HEAD AND RESPONSE COMPRESSION CANNOT BOTH BE TRUE. The shim
    // writes the response head itself the moment it accepts a stream, and the
    // adapter's own later `writeHead` — the one that would have announced
    // `connect-content-encoding` — is absorbed. So a compressed envelope would
    // reach a client that was never told how to read it, and connect-go's own
    // client says exactly that: "received compressed envelope, but do not know
    // how to decompress".
    //
    // It only bites once a page grows past the compression threshold, which is
    // why it showed up as later streams failing while the first few worked.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    // Grow the book so an opening page is comfortably over any threshold.
    for (const turn of ["t1", "t2", "t3", "t4"]) {
      await shim.clients.h1.startTurn(startTurnRequest({ turn, text: "!md" }));
    }

    const streams = [0, 1, 2, 3, 4, 5].map(() =>
      openStream((options) => shim.clients.h1.watchAgent(watchAgentRequest(), options)),
    );
    const pages = await Promise.all(streams.map(async (stream) => stream.next()));

    for (const page of pages) expect(watchAgentPage(page).entries.length).toBeGreaterThan(0);
    for (const stream of streams) stream.close();
  });
});
