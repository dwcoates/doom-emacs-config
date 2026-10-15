/**
 * THE APP CODE IS SPLIT INTO LAYERED CHUNKS, so no chunk grows past Vite's
 * 500 kB size advisory however much app code is added (vite.config.ts).
 *
 * WHY LAYERS. The webapp's own source used to ride in the entry chunk, and by
 * 2026-10-15 it sat 160 bytes under the advisory: any change could tip it
 * over. Splitting it by folder is no answer, because the folders import each
 * other in both directions, and a split along a cycle makes chunks that import
 * each other, which Rollup warns about (and the build is held to zero
 * warnings, test/vite-config.test.ts).
 *
 * A LAYER ONLY EVER IMPORTS LAYERS BELOW IT, so chunks can never import each
 * other in a cycle. The modules' import cycles (strongly connected components)
 * are each kept whole, the components are taken dependencies first, and they
 * are packed in that order into layers of at most `budget` bytes of
 * pre-minification code. Every module a component imports sits in an earlier
 * component, so in the same layer or a lower one. As the code grows, the count
 * of layers grows; no layer does.
 *
 * Only modules the entry reaches through STATIC imports are layered: a module
 * reached only through a dynamic `import()` is meant to load lazily, and is
 * left to Rollup. The entry's own scripts (the modules the entry imports
 * directly: index.html's `src/main.ts`) stay in the entry chunk, which
 * imports every layer above them, so the entry still reaches every chunk.
 */

/** One app module, as the layering reads it. */
export interface LayerModule {
  id: string;
  /** The app modules it statically imports. Any other id is ignored. */
  imports: readonly string[];
  /** Its code's size, in bytes, before minification. */
  size: number;
}

/**
 * The layer index of each module: components of MODULES packed dependencies
 * first into layers of at most BUDGET bytes. A component larger than BUDGET
 * gets a layer of its own, which the build's size check then reports.
 */
export function appLayers(modules: readonly LayerModule[], budget: number): Map<string, number> {
  if (!(budget > 0)) throw new Error(`app-layers: the layer budget must be positive (got ${budget})`);
  const layers = new Map<string, number>();
  let layer = 0;
  let filled = 0;
  for (const component of componentsDependenciesFirst(modules)) {
    const size = component.reduce((sum, m) => sum + m.size, 0);
    if (filled > 0 && filled + size > budget) {
      layer += 1;
      filled = 0;
    }
    filled += size;
    for (const m of component) layers.set(m.id, layer);
  }
  return layers;
}

/**
 * The strongly connected components of MODULES (Tarjan's algorithm), each
 * emitted only after every component it imports: dependencies first.
 */
function componentsDependenciesFirst(modules: readonly LayerModule[]): LayerModule[][] {
  const byId = new Map(modules.map((m) => [m.id, m]));
  const index = new Map<string, number>();
  const low = new Map<string, number>();
  const onStack = new Set<string>();
  const stack: LayerModule[] = [];
  const components: LayerModule[][] = [];
  let next = 0;

  // Iterative, so a long import chain cannot overflow the call stack.
  for (const root of modules) {
    if (index.has(root.id)) continue;
    const frames: Array<{ m: LayerModule; edge: number }> = [];
    const enter = (m: LayerModule): void => {
      index.set(m.id, next);
      low.set(m.id, next);
      next += 1;
      stack.push(m);
      onStack.add(m.id);
      frames.push({ m, edge: 0 });
    };
    enter(root);
    while (frames.length > 0) {
      const frame = frames[frames.length - 1];
      const imports = frame.m.imports;
      if (frame.edge < imports.length) {
        const to = byId.get(imports[frame.edge]);
        frame.edge += 1;
        if (to === undefined) continue;
        if (!index.has(to.id)) {
          enter(to);
        } else if (onStack.has(to.id)) {
          low.set(frame.m.id, Math.min(low.get(frame.m.id)!, index.get(to.id)!));
        }
        continue;
      }
      frames.pop();
      const parent = frames[frames.length - 1];
      if (parent !== undefined) {
        low.set(parent.m.id, Math.min(low.get(parent.m.id)!, low.get(frame.m.id)!));
      }
      if (low.get(frame.m.id) !== index.get(frame.m.id)) continue;
      const component: LayerModule[] = [];
      for (;;) {
        const member = stack.pop()!;
        onStack.delete(member.id);
        component.push(member);
        if (member.id === frame.m.id) break;
      }
      components.push(component);
    }
  }
  return components;
}

/** What `appLayerChunks` reads off Rollup's chunking context. */
export interface ModuleGraph {
  getModuleIds(): IterableIterator<string>;
  getModuleInfo(id: string): {
    isEntry: boolean;
    importedIds: readonly string[];
    code: string | null;
  } | null;
}

/**
 * The layer chunk name (`app-<n>`) of every app module the entry statically
 * reaches, read off GRAPH: IS_APP says which ids are app source, BUDGET is the
 * layer budget. The entry, the entry's own scripts and every other module get
 * no name.
 */
export function appLayerChunks(
  graph: ModuleGraph,
  isApp: (id: string) => boolean,
  budget: number,
): Map<string, string> {
  const entries = [...graph.getModuleIds()].filter((id) => graph.getModuleInfo(id)?.isEntry === true);
  const scripts = new Set(entries.flatMap((id) => graph.getModuleInfo(id)?.importedIds ?? []));
  const reached = new Map<string, LayerModule>();
  const pending = [...scripts].flatMap((id) => graph.getModuleInfo(id)?.importedIds ?? []);
  for (let id = pending.pop(); id !== undefined; id = pending.pop()) {
    if (reached.has(id) || !isApp(id) || scripts.has(id) || entries.includes(id)) continue;
    const info = graph.getModuleInfo(id);
    if (info === null) throw new Error(`app-layers: rollup has no module info for ${id}`);
    if (info.code === null) throw new Error(`app-layers: rollup holds no code for ${id}`);
    const imports = info.importedIds.filter(isApp);
    reached.set(id, { id, imports, size: info.code.length });
    pending.push(...imports);
  }
  const names = new Map<string, string>();
  for (const [id, layer] of appLayers([...reached.values()], budget)) names.set(id, `app-${layer}`);
  return names;
}
