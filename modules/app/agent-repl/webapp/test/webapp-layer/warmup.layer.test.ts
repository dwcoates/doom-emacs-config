/**
 * THE LAYER DRIVER'S WARM-UP (layer-server.mjs). The driver runs this file
 * once, at its own start and before any area's timed run, so vite's
 * transform and the worker's evaluation of the module graph every area
 * imports -- the app through the integration harness, the drive helpers and
 * the real-daemon bridge -- are paid while no area's bound is running. It
 * needs no world and no daemon.
 */
import { expect, it } from "vitest";
import * as harness from "../integration/harness";
import * as drive from "./drive";
import * as realDaemon from "./real-daemon";

it("loads the module graph every area imports", () => {
  expect(typeof harness.startAppAgainst).toBe("function");
  expect(typeof drive.bootLayer).toBe("function");
  expect(typeof realDaemon.realDaemon).toBe("function");
});
