import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { viteCacheDir } from "../vite-cache";
import buildConfig from "../vite.config";
import unitConfig from "../vitest.config";
import integrationConfig from "../vitest.integration.config";
import webappLayerConfig from "../vitest.webapp-layer.config";
import webkitConfig from "../vitest.webkit.config";

/**
 * THE CACHE MUST NOT LIVE IN `node_modules`, and every config in this package
 * must say so.
 *
 * Vite's default `cacheDir` is `node_modules/.vite`. In the e2e sandbox
 * container `webapp/node_modules` is a symlink into a READ-ONLY image layer,
 * so a config that takes the default cannot create its cache at all: that is
 * what made all eleven `TestWebappLayer*` areas fail inside the container
 * while passing on the host. The failure is a filesystem error from vitest's
 * startup, nowhere near the config that caused it, so the property is
 * asserted here — at the config — rather than left to be rediscovered.
 *
 * See ../vite-cache.ts for why the package directory is the right home.
 */

const webappDir = path.dirname(fileURLToPath(new URL("../vite-cache.ts", import.meta.url)));

describe("the shared vite cache directory", () => {
  it("is outside node_modules, which is read-only in the e2e sandbox", () => {
    const segments = viteCacheDir.split(path.sep);

    expect(segments).not.toContain("node_modules");
  });

  it("is inside the webapp package, which is writable in both the sandbox and the host", () => {
    const relative = path.relative(webappDir, viteCacheDir);

    expect(relative).toBe(".vite-cache");
  });
});

// EVERY config, not just the webapp layer's. The layer is where the read-only
// tree bit first, but the sandbox runs the unit and integration projects and
// `vite build` against the same linked `node_modules`, so one config left on
// the default is one more way to break the container.
const configs: { name: string; config: { cacheDir?: string } }[] = [
  { name: "vite.config.ts", config: buildConfig },
  { name: "vitest.config.ts", config: unitConfig },
  { name: "vitest.integration.config.ts", config: integrationConfig },
  { name: "vitest.webapp-layer.config.ts", config: webappLayerConfig },
  { name: "vitest.webkit.config.ts", config: webkitConfig },
];

describe.each(configs)("$name", ({ config }) => {
  it("sets its cacheDir to the shared one rather than taking vite's node_modules default", () => {
    expect(config.cacheDir).toBe(viteCacheDir);
  });
});
