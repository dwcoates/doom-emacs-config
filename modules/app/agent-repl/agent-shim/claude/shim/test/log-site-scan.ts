/**
 * The shim's static scan of its log sites (logging-contract.md "Log sites"):
 * the shared scan (agent-shim/logging/ts/log-site-scan.ts) over the shim's
 * call shape, `log.info(fields, template, values)` on a logger
 * `bindLog("scope", ...)` built.
 */
import ts from "typescript";
import { logSiteScan } from "../../../logging/ts/log-site-scan.js";

export type { ScanResult, ScanSource, ScannedSite } from "../../../logging/ts/log-site-scan.js";

/** The shim's scan: its runtime (src/log.ts `SHIM_LOG_RUNTIME`) and call shape. */
export const scanLogSites = logSiteScan(ts, {
  runtime: "shim",
  product: "the shim",
  loggingScope: "shim.logging",
  methods: ["debug", "info", "warn", "error", "logVerbose"],
  builder: "bindLog",
  templateArg: 1,
  // A call whose template argument is no literal writes a LogMessage
  // (ShimLogMethod's second signature), whose site is the `.message(...)`
  // call that made it; the compiler refuses a string that is not a literal.
  literalTemplateRequired: false,
  logModuleCalls: [{ name: "internal", templateArg: 2 }, { name: "emergencyStderr", templateArg: 0 }],
});
