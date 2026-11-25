// @vitest-environment node
/**
 * The layering and the cycle check walk one component walk (app-layers.ts).
 */
import { describe, expect, it } from "vitest";
import appLayersSource from "../app-layers.ts?raw";
import { codeOf, functionBody } from "../test/source-text.js";

describe("importCycles", () => {
  it("walks the one component walk the layering walks", () => {
    // Arrange
    const code = codeOf(appLayersSource);
    // Act
    const walkers = ["appLayers", "importCycles"].filter((fn) => functionBody(code, fn).includes("componentsDependenciesFirst("));
    // Assert
    expect(walkers).toEqual(["appLayers", "importCycles"]);
  });
});
