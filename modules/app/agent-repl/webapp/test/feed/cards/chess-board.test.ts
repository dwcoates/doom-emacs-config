// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { ConnectError, Code, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  InspectChessBoardSquareResponseSchema,
  type InspectChessBoardSquareRequest,
  type InspectChessBoardSquareResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_inspect_chess_board_square_pb";
import { FeedChessBoardSchema, type FeedChessBoard } from "../../../../proto/gen/ts/frontend/v1/chess_board_pb";
import { FeedRowSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { stopTicking } from "../../../src/feed/ticking.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { chessWidgetLoader, type ChessWidgetMountOptions } from "../../../src/feed/cards/chess-widget-loader.js";
import {
  boardKey,
  CHESS_BOARD_KEY_ATTRIBUTE,
  drawFeedChessBoard,
  SQUARE_REFUSALS,
  WIDGET_LOAD_FAILED_TEXT,
} from "../../../src/feed/cards/chess-board.js";
import { testAppContext } from "../../rpc/app-context.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";

// THE WIDGET BUNDLE IS FAKED: a mount records its options and its handle's
// calls, and the module either loads or fails as the test says. The loader
// is spied on, not module-mocked, because the suite shares one module graph
// per worker.
interface FakeMount {
  host: HTMLElement;
  options: ChessWidgetMountOptions;
  shown: Uint8Array[];
  unmounted: boolean;
}
const mounts: FakeMount[] = [];
const loader = { fail: false, pending: undefined as undefined | Promise<void> };

function installFakeWidget(): void {
  vi.spyOn(chessWidgetLoader, "load").mockImplementation(async () => {
    if (loader.pending !== undefined) await loader.pending;
    if (loader.fail) throw new Error("bundle 404");
    return {
      mountCeeWebWidget: (host: HTMLElement, options: ChessWidgetMountOptions) => {
        const mount: FakeMount = { host, options, shown: [], unmounted: false };
        mounts.push(mount);
        return {
          showSquareEvents: (bytes: Uint8Array) => mount.shown.push(bytes),
          unmount: () => {
            mount.unmounted = true;
          },
        };
      },
    };
  });
}

const SINK: FailureSink = { report: () => {}, retract: () => {} };
const HEADING = "Chess board · CEE session agent-a";

/** Answers the next square click with, in order. */
let answers: Array<() => Promise<InspectChessBoardSquareResponse>> = [];
/** Every square click the daemon was asked. */
let asked: InspectChessBoardSquareRequest[] = [];

function rowContext(previous?: HTMLElement): RowContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      inspectChessBoardSquare: (req) => {
        asked.push(req);
        const next = answers.shift();
        if (next === undefined) throw new Error("no answer scripted");
        return next();
      },
    });
  });
  return {
    ctx: testAppContext({
      client: createAgentReplClient(transport),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker: createTicker(1000),
      failures: SINK,
      composerEnabled: false,
    }),
    feed: "root",
    row: create(FeedRowSchema, {}),
    revealRow: async () => false,
    previous,
  };
}

function preparing(step: string): FeedChessBoard {
  return create(FeedChessBoardSchema, {
    heading: { text: HEADING },
    state: { case: "preparing", value: { step: { text: step } } },
  });
}

function ready(bytes = new Uint8Array([0x0a, 0x00]), stamp = "abc"): FeedChessBoard {
  return create(FeedChessBoardSchema, {
    heading: { text: HEADING },
    state: {
      case: "ready",
      value: {
        widget: { ceeWebWidget: bytes },
        bundle: { scriptUrl: `/chess-widget/${stamp}/cee-web-widget.js`, stylesheetUrl: `/chess-widget/${stamp}/cee-web-widget.css` },
        squareToken: { value: "v1.token" },
        startPosition: { gamePoint: 7n },
      },
    },
  });
}

/** Let loaded modules mount and settled answers land. */
async function drain(): Promise<void> {
  for (let i = 0; i < 10; i += 1) await new Promise((resolve) => setTimeout(resolve, 0));
}

function success(bytes: number[]): () => Promise<InspectChessBoardSquareResponse> {
  return async () =>
    create(InspectChessBoardSquareResponseSchema, {
      result: { case: "success", value: { getSquareEventsResponse: new Uint8Array(bytes) } },
    });
}

/** A scripted answer the test releases when it chooses. */
function deferred(bytes: number[]): { answer: () => Promise<InspectChessBoardSquareResponse>; release: () => void } {
  let release: () => void = () => {};
  const gate = new Promise<void>((resolve) => {
    release = resolve;
  });
  return {
    answer: async () => {
      await gate;
      return success(bytes)();
    },
    release: () => release(),
  };
}

beforeEach(() => {
  mounts.length = 0;
  answers = [];
  asked = [];
  loader.fail = false;
  loader.pending = undefined;
  document.head.replaceChildren();
  installFakeWidget();
});

afterEach(() => {
  vi.restoreAllMocks();
});

describe("drawFeedChessBoard: the board's states", () => {
  it("draws a preparing board's heading and step", () => {
    // Act.
    const el = drawFeedChessBoard(preparing("Building the chess widget…"), rowContext());

    // Assert.
    expect([el.getAttribute("data-state"), el.querySelector(".agentic-heading")?.textContent, el.querySelector(".chess-board-step")?.textContent]).toEqual([
      "preparing",
      HEADING,
      "Building the chess widget…",
    ]);
  });

  it("draws an unavailable board's reason", () => {
    // Arrange.
    const board = create(FeedChessBoardSchema, {
      heading: { text: HEADING },
      state: { case: "unavailable", value: { reason: { text: "CEE session agent-a no longer holds game g-1." } } },
    });

    // Act.
    const el = drawFeedChessBoard(board, rowContext());

    // Assert.
    expect(el.querySelector(".chess-board-unavailable")?.textContent).toBe("CEE session agent-a no longer holds game g-1.");
  });

  it("refuses a board with no heading as a malformed view", () => {
    // Arrange.
    const board = create(FeedChessBoardSchema, { state: { case: "preparing", value: { step: { text: "x" } } } });

    // Act, Assert.
    expect(() => drawFeedChessBoard(board, rowContext())).toThrow(MalformedView);
  });

  it("refuses a board with no state as a malformed view", () => {
    // Arrange.
    const board = create(FeedChessBoardSchema, { heading: { text: HEADING } });

    // Act, Assert.
    expect(() => drawFeedChessBoard(board, rowContext())).toThrow(MalformedView);
  });
});

describe("drawFeedChessBoard: mounting the widget", () => {
  it("mounts the widget from the served bytes", async () => {
    // Act.
    drawFeedChessBoard(ready(new Uint8Array([1, 2, 3])), rowContext());
    await drain();

    // Assert.
    expect([...mounts[0].options.widgetBytes]).toEqual([1, 2, 3]);
  });

  it("loads the board's stylesheet", () => {
    // Act.
    drawFeedChessBoard(ready(), rowContext());

    // Assert.
    expect(document.head.querySelector("link[data-chess-widget]")?.getAttribute("href")).toBe("/chess-widget/abc/cee-web-widget.css");
  });

  it("keeps its previous element when the same board is drawn again", async () => {
    // Arrange.
    const first = drawFeedChessBoard(ready(), rowContext());
    await drain();

    // Act.
    const second = drawFeedChessBoard(ready(), rowContext(first));
    await drain();

    // Assert.
    expect([second === first, mounts.length]).toEqual([true, 1]);
  });

  it("draws a fresh element for a board whose data changed", async () => {
    // Arrange.
    const first = drawFeedChessBoard(ready(new Uint8Array([1])), rowContext());

    // Act.
    const second = drawFeedChessBoard(ready(new Uint8Array([2])), rowContext(first));

    // Assert.
    expect(second).not.toBe(first);
  });

  it("unmounts the widget when its element is discarded", async () => {
    // Arrange.
    const el = drawFeedChessBoard(ready(), rowContext());
    await drain();

    // Act.
    stopTicking(el);

    // Assert.
    expect(mounts[0].unmounted).toBe(true);
  });

  it("never mounts a board discarded before its bundle arrived", async () => {
    // Arrange.
    let release: () => void = () => {};
    loader.pending = new Promise<void>((resolve) => {
      release = resolve;
    });
    const el = drawFeedChessBoard(ready(), rowContext());

    // Act.
    stopTicking(el);
    release();
    await drain();

    // Assert.
    expect(mounts).toHaveLength(0);
  });

  it("says the widget could not be loaded, and logs it at error", async () => {
    // Arrange.
    const capture = captureLogRecords();
    loader.fail = true;

    // Act.
    const el = drawFeedChessBoard(ready(), rowContext());
    await drain();

    // Assert.
    expect(el.querySelector(".chess-board-status")?.textContent).toBe(WIDGET_LOAD_FAILED_TEXT);
    const record = await forwardedRecord(capture, "feed.cards.chess-board.load-failed");
    expect(record.level.case).toBe("error");
  });

  it("marks a ready board with its key", () => {
    // Act.
    const el = drawFeedChessBoard(ready(new Uint8Array([9])), rowContext());

    // Assert.
    expect(el.getAttribute(CHESS_BOARD_KEY_ATTRIBUTE)).toBe(boardKey("/chess-widget/abc/cee-web-widget.js", "v1.token", new Uint8Array([9])));
  });
});

describe("drawFeedChessBoard: square clicks", () => {
  it("asks for the square at the start position with the board's token", async () => {
    // Arrange.
    answers.push(success([8, 28]));
    drawFeedChessBoard(ready(), rowContext());
    await drain();

    // Act.
    mounts[0].options.onSquareSelect(28);
    await drain();

    // Assert.
    expect([asked[0].board?.value, asked[0].gamePoint, asked[0].square]).toEqual(["v1.token", 7n, 28]);
  });

  it("hands the answer back to the widget whole", async () => {
    // Arrange.
    answers.push(success([8, 28]));
    drawFeedChessBoard(ready(), rowContext());
    await drain();

    // Act.
    mounts[0].options.onSquareSelect(28);
    await drain();

    // Assert.
    expect(mounts[0].shown.map((bytes) => [...bytes])).toEqual([[8, 28]]);
  });

  it("asks at the position the widget last reported", async () => {
    // Arrange.
    answers.push(success([1]));
    drawFeedChessBoard(ready(), rowContext());
    await drain();
    mounts[0].options.onPositionChange(42);

    // Act.
    mounts[0].options.onSquareSelect(3);
    await drain();

    // Assert.
    expect(asked[0].gamePoint).toBe(42n);
  });

  it("drops an answer a later click superseded", async () => {
    // Arrange.
    const slow = deferred([1]);
    answers.push(slow.answer, success([2]));
    drawFeedChessBoard(ready(), rowContext());
    await drain();

    // Act.
    mounts[0].options.onSquareSelect(1);
    mounts[0].options.onSquareSelect(2);
    await drain();
    slow.release();
    await drain();

    // Assert.
    expect(mounts[0].shown.map((bytes) => [...bytes])).toEqual([[2]]);
  });

  it("drops an answer a navigation superseded", async () => {
    // Arrange.
    const slow = deferred([1]);
    answers.push(slow.answer);
    drawFeedChessBoard(ready(), rowContext());
    await drain();

    // Act.
    mounts[0].options.onSquareSelect(1);
    mounts[0].options.onPositionChange(9);
    slow.release();
    await drain();

    // Assert.
    expect(mounts[0].shown).toHaveLength(0);
  });

  it.each([
    ["sessionGone", SQUARE_REFUSALS.sessionGone],
    ["backendUnreachable", SQUARE_REFUSALS.backendUnreachable],
  ] as const)("says why a %s click got no answer", async (arm, text) => {
    // Arrange.
    answers.push(async () =>
      create(InspectChessBoardSquareResponseSchema, {
        result: { case: "error", value: { cause: { case: arm, value: { detail: "d" } } } },
      }),
    );
    const el = drawFeedChessBoard(ready(), rowContext());
    await drain();

    // Act.
    mounts[0].options.onSquareSelect(1);
    await drain();

    // Assert.
    expect(el.querySelector(".chess-board-status")?.textContent).toBe(text);
  });

  it("says a click could not reach the daemon, and logs it at error", async () => {
    // Arrange.
    const capture = captureLogRecords();
    answers.push(async () => {
      throw new ConnectError("down", Code.Unavailable);
    });
    const el = drawFeedChessBoard(ready(), rowContext());
    await drain();

    // Act.
    mounts[0].options.onSquareSelect(1);
    await drain();

    // Assert.
    expect(el.querySelector(".chess-board-status")?.textContent).toBe(SQUARE_REFUSALS.transport);
    const record = await forwardedRecord(capture, "feed.cards.chess-board.square-failed");
    expect(record.level.case).toBe("error");
  });

  it("hides the status line again when a later click is answered", async () => {
    // Arrange.
    answers.push(
      async () =>
        create(InspectChessBoardSquareResponseSchema, {
          result: { case: "error", value: { cause: { case: "sessionGone", value: { detail: "d" } } } },
        }),
      success([5]),
    );
    const el = drawFeedChessBoard(ready(), rowContext());
    await drain();
    mounts[0].options.onSquareSelect(1);
    await drain();

    // Act.
    mounts[0].options.onSquareSelect(2);
    await drain();

    // Assert.
    expect((el.querySelector(".chess-board-status") as HTMLElement).hidden).toBe(true);
  });
});

describe("boardKey", () => {
  it("differs for different widget data", () => {
    // Act, Assert.
    expect(boardKey("/s.js", "t", new Uint8Array([1]))).not.toBe(boardKey("/s.js", "t", new Uint8Array([2])));
  });

  it("differs for a different bundle", () => {
    // Act, Assert.
    expect(boardKey("/a.js", "t", new Uint8Array([1]))).not.toBe(boardKey("/b.js", "t", new Uint8Array([1])));
  });
});
