// @vitest-environment node
import { describe, expect, it } from "vitest";
import { appLayerChunks, appLayers, type LayerModule, type ModuleGraph } from "../app-layers";

/** A module of SIZE bytes importing IMPORTS. */
function mod(id: string, size: number, ...imports: string[]): LayerModule {
  return { id, size, imports };
}

/** The layer of every module, as a plain object for one comparison. */
function layered(modules: LayerModule[], budget: number): Record<string, number> {
  return Object.fromEntries(appLayers(modules, budget));
}

describe("appLayers", () => {
  it("puts every module in layer 0 when they fit the budget together", () => {
    // Arrange
    const modules = [mod("a", 10, "b"), mod("b", 10)];
    // Act
    const got = layered(modules, 100);
    // Assert
    expect(got).toEqual({ a: 0, b: 0 });
  });

  it("puts a dependency in a lower layer than its importer once the budget is spent", () => {
    // Arrange -- a imports b imports c, each filling the budget.
    const modules = [mod("a", 100, "b"), mod("b", 100, "c"), mod("c", 100)];
    // Act
    const got = layered(modules, 100);
    // Assert
    expect(got).toEqual({ c: 0, b: 1, a: 2 });
  });

  it("keeps an import cycle whole in one layer", () => {
    // Arrange -- b and c import each other, and the cycle overflows one layer's budget.
    const modules = [mod("a", 60, "b"), mod("b", 60, "c"), mod("c", 60, "b")];
    // Act
    const got = layered(modules, 100);
    // Assert
    expect([got.b === got.c, got.a > got.b]).toEqual([true, true]);
  });

  it("gives a component larger than the budget a layer of its own", () => {
    // Arrange
    const modules = [mod("a", 10, "big"), mod("big", 500), mod("c", 10)];
    // Act
    const got = layered(modules, 100);
    // Assert -- nothing shares the oversized module's layer.
    expect(Object.keys(got).filter((id) => got[id] === got.big)).toEqual(["big"]);
  });

  it("never lets a module import a module in a higher layer", () => {
    // Arrange -- a tangle of shared imports and two cycles.
    const modules = [
      mod("main", 30, "feed", "sidebar", "log"),
      mod("feed", 80, "cards", "log", "scroll"),
      mod("cards", 80, "feed", "dom"),
      mod("sidebar", 70, "row", "log"),
      mod("row", 40, "sidebar", "dom"),
      mod("scroll", 50, "log", "dom"),
      mod("log", 20),
      mod("dom", 20, "log"),
    ];
    // Act
    const got = appLayers(modules, 100);
    // Assert
    const upward = modules.flatMap((m) => m.imports.filter((to) => got.get(to)! > got.get(m.id)!).map((to) => `${m.id}->${to}`));
    expect(upward).toEqual([]);
  });

  it("ignores an import of a module it was not given", () => {
    // Arrange
    const modules = [mod("a", 10, "/node_modules/x.js")];
    // Act
    const got = layered(modules, 100);
    // Assert
    expect(got).toEqual({ a: 0 });
  });

  it.each([0, -1, Number.NaN])("refuses a budget of %s", (budget) => {
    // Act / Assert
    expect(() => appLayers([mod("a", 1)], budget)).toThrow(/budget must be positive/);
  });
});

/** A Rollup-shaped graph of MODULES; an entry is marked with `entry: true`. */
function graphOf(
  modules: Record<string, { imports?: string[]; entry?: boolean; code?: string | null }>,
): ModuleGraph {
  return {
    getModuleIds: () => Object.keys(modules)[Symbol.iterator](),
    getModuleInfo: (id) => {
      const m = modules[id];
      if (m === undefined) return null;
      return { isEntry: m.entry === true, importedIds: m.imports ?? [], code: m.code === undefined ? "x" : m.code };
    },
  };
}

const isApp = (id: string): boolean => id.startsWith("/src/");

describe("appLayerChunks", () => {
  it("names every app module the entry's script statically reaches by its layer", () => {
    // Arrange
    const graph = graphOf({
      "/index.html": { entry: true, imports: ["/src/main.ts"] },
      "/src/main.ts": { imports: ["/src/feed.ts"] },
      "/src/feed.ts": { imports: ["/src/log.ts"] },
      "/src/log.ts": {},
    });
    // Act
    const got = Object.fromEntries(appLayerChunks(graph, isApp, 100));
    // Assert
    expect(got).toEqual({ "/src/feed.ts": "app-0", "/src/log.ts": "app-0" });
  });

  it("leaves the entry's own script in the entry chunk", () => {
    // Arrange
    const graph = graphOf({
      "/index.html": { entry: true, imports: ["/src/main.ts"] },
      "/src/main.ts": { imports: ["/src/log.ts"] },
      "/src/log.ts": {},
    });
    // Act
    const got = appLayerChunks(graph, isApp, 100);
    // Assert
    expect(got.has("/src/main.ts")).toBe(false);
  });

  it("leaves a module the entry reaches only lazily to rollup", () => {
    // Arrange -- importedIds carries static imports only, so a lazy module is never in it.
    const graph = graphOf({
      "/index.html": { entry: true, imports: ["/src/main.ts"] },
      "/src/main.ts": { imports: ["/src/log.ts"] },
      "/src/log.ts": {},
      "/src/host/main.ts": { imports: ["/src/log.ts"] },
    });
    // Act
    const got = appLayerChunks(graph, isApp, 100);
    // Assert
    expect(got.has("/src/host/main.ts")).toBe(false);
  });

  it("leaves a module that is not app source to rollup", () => {
    // Arrange
    const graph = graphOf({
      "/index.html": { entry: true, imports: ["/src/main.ts"] },
      "/src/main.ts": { imports: ["/node_modules/x.js"] },
      "/node_modules/x.js": {},
    });
    // Act
    const got = appLayerChunks(graph, isApp, 100);
    // Assert
    expect(got.size).toBe(0);
  });

  it("names nothing for a graph with no entry", () => {
    // Act
    const got = appLayerChunks(graphOf({ "/src/a.ts": {} }), isApp, 100);
    // Assert
    expect(got.size).toBe(0);
  });

  it("throws for a reached module rollup holds no info for", () => {
    // Arrange
    const graph = graphOf({
      "/index.html": { entry: true, imports: ["/src/main.ts"] },
      "/src/main.ts": { imports: ["/src/gone.ts"] },
    });
    // Act / Assert
    expect(() => appLayerChunks(graph, isApp, 100)).toThrow(/no module info for \/src\/gone\.ts/);
  });

  it("throws for a reached module rollup holds no code for", () => {
    // Arrange
    const graph = graphOf({
      "/index.html": { entry: true, imports: ["/src/main.ts"] },
      "/src/main.ts": { imports: ["/src/empty.ts"] },
      "/src/empty.ts": { code: null },
    });
    // Act / Assert
    expect(() => appLayerChunks(graph, isApp, 100)).toThrow(/no code for \/src\/empty\.ts/);
  });
});
