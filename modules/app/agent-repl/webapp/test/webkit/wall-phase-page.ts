/**
 * The page module the rail's wall-phase WebKit suite bundles: the REAL
 * `installWallPhase`, under the webapp's own logger (which the module writes
 * through), exposed on `window.WallPhase`.
 */
import { installPageLogger } from "./page-log";
import { installWallPhase } from "../../src/sidebar/wall-phase.js";

installPageLogger("wall-phase");

export { installWallPhase };
