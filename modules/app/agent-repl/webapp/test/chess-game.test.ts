import { describe, expect, it } from "vitest";
import {
  CHESS_GAME_MARKER,
  chessGameContainerHtml,
  chessGameMarkerPath,
  isPartialChessGameMarker,
} from "../src/chess-game.js";

describe("chessGameMarkerPath", () => {
  it("returns the path between prefix and closing arrow", () => {
    // Arrange
    const line = `${CHESS_GAME_MARKER}/ws/.claude/emacs/cee-web-widget/chess-game-ab12.pgn <---`;
    // Act + Assert
    expect(chessGameMarkerPath(line)).toBe("/ws/.claude/emacs/cee-web-widget/chess-game-ab12.pgn");
  });

  it("accepts a marker without the closing arrow", () => {
    // Arrange + Act + Assert
    expect(chessGameMarkerPath(`${CHESS_GAME_MARKER}/tmp/chess-game-x.fen`)).toBe("/tmp/chess-game-x.fen");
  });

  it("trims whitespace around the path", () => {
    // Arrange + Act + Assert
    expect(chessGameMarkerPath(`${CHESS_GAME_MARKER}  /tmp/chess-game-x.pgn  <---`)).toBe("/tmp/chess-game-x.pgn");
  });
});

describe("isPartialChessGameMarker", () => {
  it("holds for a strict prefix of the marker", () => {
    // Arrange + Act + Assert
    expect(isPartialChessGameMarker("---> agent-repl-che")).toBe(true);
  });

  it("rejects a complete marker line", () => {
    // Arrange + Act + Assert
    expect(isPartialChessGameMarker(`${CHESS_GAME_MARKER}/tmp/f.pgn`)).toBe(false);
  });

  it("rejects unrelated text", () => {
    // Arrange + Act + Assert
    expect(isPartialChessGameMarker("hello world")).toBe(false);
  });

  it("rejects the empty line", () => {
    // Arrange + Act + Assert
    expect(isPartialChessGameMarker("")).toBe(false);
  });
});

describe("chessGameContainerHtml", () => {
  it("carries the path in a data attribute with the processing indicator", () => {
    // Arrange + Act
    const html = chessGameContainerHtml("/tmp/chess-game-x.pgn");
    // Assert
    expect(html).toContain(`data-game-file="/tmp/chess-game-x.pgn"`);
    expect(html).toContain(`class="thinking-spinner"`);
    expect(html).toContain("processing");
  });

  it("trails the processing word with the animated ellipsis rather than a static one", () => {
    // Arrange + Act — the dots cycle beside the arc, so the row reads live.
    const html = chessGameContainerHtml("/tmp/chess-game-x.pgn");
    // Assert — the animated span sits after the processing word.
    expect(html).toContain("animated-ellipsis");
  });

  it("escapes markup in the path", () => {
    // Arrange + Act + Assert
    expect(chessGameContainerHtml(`/tmp/<img>"x.pgn`)).not.toContain("<img>");
  });
});

import { afterEach } from "vitest";
import {
  type ChessGameConfig,
  type ChessWidgetHandle,
  type ChessWidgetMountOpts,
  type GameContainer,
  CHESS_NAV_HOOK,
  type ChessStepDirection,
  chessGameFileUrl,
  chessGameKind,
  installChessNavHook,
  splitChessGameSegments,
  stepActiveChessGame,
  configureChessGames,
  hydrateChessGames,
  parseSessionPointer,
  releaseChessGames,
} from "../src/chess-game.js";

describe("chessGameKind", () => {
  it.each([
    ["/a/chess-game-x.pgn", "pgn"],
    ["/a/chess-game-x.fen", "fen"],
    ["/a/chess-game-x.session", "session"],
  ])("classifies %s as %s", (path, kind) => {
    // Arrange + Act + Assert
    expect(chessGameKind(path)).toBe(kind);
  });

  it("rejects an unsupported extension", () => {
    // Arrange + Act + Assert
    expect(chessGameKind("/a/chess-game-x.tcn")).toBeNull();
  });
});

describe("parseSessionPointer", () => {
  it("parses backend URL and session id lines", () => {
    // Arrange + Act + Assert
    expect(parseSessionPointer("http://127.0.0.1:4577\nshell-12\n")).toEqual({
      backend: "http://127.0.0.1:4577",
      session: "shell-12",
    });
  });

  it("rejects a file whose first line is no URL", () => {
    // Arrange + Act + Assert
    expect(parseSessionPointer("shell-12\nhttp://127.0.0.1:4577")).toBeNull();
  });

  it("rejects a file missing the session id line", () => {
    // Arrange + Act + Assert
    expect(parseSessionPointer("http://127.0.0.1:4577")).toBeNull();
  });
});

describe("chessGameFileUrl", () => {
  it("routes through the session's chess-game endpoint with the path encoded", () => {
    // Arrange + Act + Assert
    expect(chessGameFileUrl("http://d:1", "s_ab", "/ws/a b/chess-game-x.pgn")).toBe(
      "http://d:1/sessions/s_ab/chess-game?path=%2Fws%2Fa%20b%2Fchess-game-x.pgn",
    );
  });
});

interface FakeContainer extends GameContainer {
  dataset: { gameFile?: string; chessHydrated?: string };
  click(): void;
}

function fakeContainer(path: string, feedKey = "k1"): FakeContainer {
  const listeners: Array<() => void> = [];
  return {
    isConnected: true,
    innerHTML: "spinner",
    dataset: { gameFile: path },
    closest: () => ({ getAttribute: () => feedKey }),
    addEventListener: (_type: string, listener: () => void) => {
      listeners.push(listener);
    },
    click: () => listeners.forEach((l) => l()),
  };
}

function rootOf(...els: FakeContainer[]) {
  return { querySelectorAll: () => els };
}

function flush(): Promise<void> {
  return new Promise((r) => setTimeout(r, 0));
}

describe("hydrateChessGames", () => {
  interface Mounted {
    container: unknown;
    opts: ChessWidgetMountOpts;
    unmounted: boolean;
  }
  let mounted: Mounted[];
  let fetched: string[];
  let cfg: ChessGameConfig;

  function arrange(fetchText: (url: string) => Promise<string> = async (url) => {
    fetched.push(url);
    return "1. e4 *";
  }): void {
    mounted = [];
    fetched = [];
    cfg = {
      base: "http://d:1",
      session: () => "s_1",
      loadMount: async () => (container, opts) => {
        const m: Mounted = { container, opts, unmounted: false };
        mounted.push(m);
        const handle: ChessWidgetHandle = {
          unmount: () => {
            m.unmounted = true;
          },
          serializeState: () => opts.state,
        };
        return handle;
      },
      fetchText,
    };
    configureChessGames(cfg);
  }

  afterEach(() => configureChessGames(null));

  it("mounts a pgn payload with the fetched source", async () => {
    // Arrange
    arrange();
    const el = fakeContainer("/ws/chess-game-a.pgn");
    // Act
    hydrateChessGames(rootOf(el));
    await flush();
    // Assert
    expect(mounted).toHaveLength(1);
    expect(mounted[0].container).toBe(el);
    expect(mounted[0].opts.kind).toBe("pgn");
    expect(mounted[0].opts.source).toBe("1. e4 *");
    expect(fetched[0]).toContain("/sessions/s_1/chess-game?path=");
  });

  it("mounts a session pointer with the backend attached", async () => {
    // Arrange
    arrange(async () => "http://127.0.0.1:4577\nshell-9");
    const el = fakeContainer("/ws/chess-game-a.session");
    // Act
    hydrateChessGames(rootOf(el));
    await flush();
    // Assert
    expect(mounted[0].opts).toMatchObject({
      kind: "session",
      source: "shell-9",
      backend: { url: "http://127.0.0.1:4577" },
    });
  });

  it("renders an in-frame error for an unsupported extension", async () => {
    // Arrange
    arrange();
    const el = fakeContainer("/ws/chess-game-a.tcn");
    // Act
    hydrateChessGames(rootOf(el));
    await flush();
    // Assert
    expect(el.innerHTML).toContain("chess-game-error");
    expect(mounted).toHaveLength(0);
  });

  it("renders the daemon's error in frame when the fetch fails", async () => {
    // Arrange
    arrange(async () => {
      throw new Error("game file not found");
    });
    const el = fakeContainer("/ws/chess-game-a.pgn");
    // Act
    hydrateChessGames(rootOf(el));
    await flush();
    // Assert
    expect(el.innerHTML).toContain("game file not found");
  });

  it("renders a capability error in frame when the bundle fails to load", async () => {
    // Arrange
    arrange();
    cfg.loadMount = async () => {
      throw new Error("widget assets are not served");
    };
    configureChessGames(cfg);
    const el = fakeContainer("/ws/chess-game-a.pgn");
    // Act
    hydrateChessGames(rootOf(el));
    await flush();
    // Assert
    expect(el.innerHTML).toContain("widget assets are not served");
  });

  it("passes the previously serialized state to a re-mount", async () => {
    // Arrange
    arrange();
    const first = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(first));
    await flush();
    mounted[0].opts.onStateChange?.({ moves: ["e2e4"] });
    // Act — the bubble re-rendered: a FRESH container for the same item.
    const second = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(second));
    await flush();
    // Assert
    expect(mounted[1].opts.state).toEqual({ moves: ["e2e4"] });
  });

  it("does not mount an already-hydrated container twice", async () => {
    // Arrange
    arrange();
    const el = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(el));
    await flush();
    // Act
    hydrateChessGames(rootOf(el));
    await flush();
    // Assert
    expect(mounted).toHaveLength(1);
  });

  it("skips mounting into a container detached while the payload loaded", async () => {
    // Arrange
    arrange();
    const el = fakeContainer("/ws/chess-game-a.pgn");
    // Act
    hydrateChessGames(rootOf(el));
    el.isConnected = false;
    await flush();
    // Assert
    expect(mounted).toHaveLength(0);
  });

  it("throws when the hydrator was never configured", () => {
    // Arrange
    configureChessGames(null);
    const el = fakeContainer("/ws/chess-game-a.pgn");
    // Act + Assert
    expect(() => hydrateChessGames(rootOf(el))).toThrow(/not configured/);
  });
});

describe("releaseChessGames", () => {
  afterEach(() => configureChessGames(null));

  it("unmounts the live widget of a container", async () => {
    // Arrange
    let unmounted = false;
    configureChessGames({
      base: "http://d:1",
      session: () => "s_1",
      loadMount: async () => () => ({ unmount: () => (unmounted = true) as unknown as void, serializeState: () => null }),
      fetchText: async () => "1. e4 *",
    });
    const el = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(el));
    await new Promise((r) => setTimeout(r, 0));
    // Act
    releaseChessGames(rootOf(el));
    // Assert
    expect(unmounted).toBe(true);
  });
});


describe("splitChessGameSegments", () => {
  const marker = "---> agent-repl-chess-game-file: /ws/chess-game-a.pgn <---";

  it("returns one text segment when no marker is present", () => {
    // Arrange + Act + Assert
    expect(splitChessGameSegments("a\nb", false)).toEqual([{ text: "a\nb" }]);
  });

  it("splits text around a marker in order", () => {
    // Arrange + Act + Assert
    expect(splitChessGameSegments(`intro\n${marker}\noutro`, false)).toEqual([
      { text: "intro" },
      { path: "/ws/chess-game-a.pgn" },
      { text: "outro" },
    ]);
  });

  it("handles multiple markers", () => {
    // Arrange + Act
    const segs = splitChessGameSegments(`${marker}\nmid\n${marker}`, false);
    // Assert
    expect(segs).toEqual([
      { path: "/ws/chess-game-a.pgn" },
      { text: "mid" },
      { path: "/ws/chess-game-a.pgn" },
    ]);
  });

  it("treats an indented marker as ordinary text", () => {
    // Arrange + Act + Assert
    expect(splitChessGameSegments(`  ${marker}`, false)).toEqual([{ text: `  ${marker}` }]);
  });

  it("leaves a marker inside a code fence as literal text", () => {
    // Arrange + Act + Assert
    expect(splitChessGameSegments("```\n" + marker + "\n```", false)).toEqual([
      { text: "```\n" + marker + "\n```" },
    ]);
  });

  it("drops a trailing partial marker while streaming", () => {
    // Arrange + Act + Assert
    expect(splitChessGameSegments("done\n---> agent-repl-che", true)).toEqual([{ text: "done" }]);
  });

  it("keeps a trailing partial marker when the block is done", () => {
    // Arrange + Act + Assert
    expect(splitChessGameSegments("done\n---> agent-repl-che", false)).toEqual([
      { text: "done\n---> agent-repl-che" },
    ]);
  });
});


describe("feed re-pinning around board mounts", () => {
  afterEach(() => configureChessGames(null));

  function arrangePin(pinned: boolean): { parks: number[] } {
    const parks: number[] = [];
    configureChessGames({
      base: "http://d:1",
      session: () => "s_1",
      isPinned: () => pinned,
      parkFeed: () => parks.push(1),
      loadMount: async () => () => ({ unmount: () => {}, serializeState: () => null }),
      fetchText: async () => "1. e4 *",
    });
    return { parks };
  }

  it("re-parks a pinned feed after the board mounts", async () => {
    // Arrange
    const { parks } = arrangePin(true);
    const el = fakeContainer("/ws/chess-game-a.pgn");
    // Act
    hydrateChessGames(rootOf(el));
    await flush();
    // Assert
    expect(parks).toHaveLength(1);
  });

  it("leaves an unpinned feed alone", async () => {
    // Arrange
    const { parks } = arrangePin(false);
    const el = fakeContainer("/ws/chess-game-a.pgn");
    // Act
    hydrateChessGames(rootOf(el));
    await flush();
    // Assert
    expect(parks).toHaveLength(0);
  });

  it("does not re-park when the mount fails", async () => {
    // Arrange
    const { parks } = arrangePin(true);
    configureChessGames({
      base: "http://d:1",
      session: () => "s_1",
      isPinned: () => true,
      parkFeed: () => parks.push(1),
      loadMount: async () => {
        throw new Error("assets missing");
      },
      fetchText: async () => "1. e4 *",
    });
    const el = fakeContainer("/ws/chess-game-a.pgn");
    // Act
    hydrateChessGames(rootOf(el));
    await flush();
    // Assert
    expect(parks).toHaveLength(0);
  });
});

describe("out-of-band navigation", () => {
  afterEach(() => configureChessGames(null));

  function arrangeNav(withStep: boolean): string[] {
    const steps: string[] = [];
    configureChessGames({
      base: "http://d:1",
      session: () => "s_1",
      loadMount: async () => () => ({
        unmount: () => {},
        serializeState: () => null,
        ...(withStep ? { step: (d: ChessStepDirection) => steps.push(d) } : {}),
      }),
      fetchText: async () => "1. e4 *",
    });
    return steps;
  }

  it("steps the last-clicked board through the window hook", async () => {
    // Arrange
    const steps = arrangeNav(true);
    const el = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(el));
    await flush();
    el.click();
    const target: Record<string, unknown> = {};
    installChessNavHook(target);
    const hook = target[CHESS_NAV_HOOK] as (d: string) => boolean;
    // Act + Assert
    expect(hook("forward")).toBe(true);
    expect(hook("back")).toBe(true);
    expect(steps).toEqual(["forward", "back"]);
  });

  it("does nothing before any board was clicked", async () => {
    // Arrange
    const steps = arrangeNav(true);
    const el = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(el));
    await flush();
    // Act + Assert
    expect(stepActiveChessGame("forward")).toBe(false);
    expect(steps).toEqual([]);
  });

  it("reports false when the widget build predates the step extension", async () => {
    // Arrange
    arrangeNav(false);
    const el = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(el));
    await flush();
    el.click();
    // Act + Assert
    expect(stepActiveChessGame("forward")).toBe(false);
  });

  it("keeps stepping after a re-render swaps the container", async () => {
    // Arrange — click the first container, then simulate the innerHTML
    // rewrite: release it and hydrate a FRESH container (same item).
    const steps = arrangeNav(true);
    const first = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(first));
    await flush();
    first.click();
    releaseChessGames(rootOf(first));
    const second = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(second));
    await flush();
    // Act + Assert
    expect(stepActiveChessGame("back")).toBe(true);
    expect(steps).toEqual(["back"]);
  });

  it("stops stepping a released board", async () => {
    // Arrange
    arrangeNav(true);
    const el = fakeContainer("/ws/chess-game-a.pgn");
    hydrateChessGames(rootOf(el));
    await flush();
    el.click();
    releaseChessGames(rootOf(el));
    // Act + Assert
    expect(stepActiveChessGame("forward")).toBe(false);
  });
});
