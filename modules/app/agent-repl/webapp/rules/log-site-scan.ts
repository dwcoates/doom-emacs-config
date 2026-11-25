/**
 * The webapp's static scan of its log sites (logging-contract.md "Log
 * sites"): the shared scan (agent-shim/logging/ts/log-site-scan.ts) over the
 * webapp's call shape, `log.info(template, fields, values)` on a logger
 * `createLogger("scope")` built.
 */
import ts from "typescript";
import { logSiteScan } from "../../agent-shim/logging/ts/log-site-scan.js";

export type { ScanResult, ScanSource, ScannedSite } from "../../agent-shim/logging/ts/log-site-scan.js";

/** The webapp's scan: its runtime (src/log.ts `WEBAPP_LOG_RUNTIME`) and call shape. */
export const scanLogSites = logSiteScan(ts, {
  runtime: "webapp",
  product: "the webapp",
  loggingScope: "webapp.log",
  methods: ["debug", "info", "warn", "error"],
  builder: "createLogger",
  templateArg: 0,
  literalTemplateRequired: true,
  logModuleCalls: [{ name: "internal", templateArg: 2 }],
});
