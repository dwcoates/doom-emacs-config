/**
 * The page module the rail's wall-phase WebKit suite bundles: the REAL
 * `installWallPhase` and a real presence, under the webapp's own logger (which the module writes
 * through), exposed on `window.WallPhase`.
 */
import { installPageLogger } from "./page-log";
import { installWallPhase } from "../../src/sidebar/wall-phase.js";
import { createPresence } from "../../src/presence.js";

installPageLogger("wall-phase");

export { createPresence, installWallPhase };
