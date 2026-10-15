import { describe, expect, it } from "vitest";
import integrationConfig from "../vitest.integration.config";
import webappLayerConfig from "../vitest.webapp-layer.config";

/**
 * THE MOUNT-ONCE-PER-FILE PROJECTS SHARE ONE WORKER, AND START EACH FILE CLEAN.
 *
 * The integration and webapp-layer projects each mount their app once per
 * file, in a `beforeAll`, and run un-isolated (`isolate: false`): the
 * integration run to stop paying a fresh jsdom and app import per file, the
 * webapp layer so the warm driver's first area test does not re-evaluate the
 * module graph inside the Go side's silence bound. Sharing is safe only
 * because each loads `test/setup-shared-integration.ts`, resetting the clock
 * and the page before a file's own `beforeAll`. A project that drops the
 * hygiene fails here rather than as an order dependency between files.
 */
const projects = [
  { name: "vitest.integration.config.ts", config: integrationConfig },
  { name: "vitest.webapp-layer.config.ts", config: webappLayerConfig },
];

describe.each(projects)("$name", ({ config }) => {
  it("runs its files in one shared worker", () => {
    // Arrange / Act
    const isolate = config.test?.isolate;
    // Assert
    expect(isolate).toBe(false);
  });

  it("loads the per-file hygiene", () => {
    // Arrange / Act
    const setupFiles = [config.test?.setupFiles ?? []].flat();
    // Assert
    expect(setupFiles).toContain("./test/setup-shared-integration.ts");
  });
});
