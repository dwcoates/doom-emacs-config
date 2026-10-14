// build-store-server.mjs — bundle the fake store's process entry
// (store-server-main.ts) into STORE_SERVER_OUTFILE, with the shim's own bundle
// options (build-options.mjs). The sidecar's mocked-vendor harness runs the
// result as the store behind its shim.
import { build } from "esbuild";
import { fileURLToPath } from "node:url";
import path from "node:path";
import { bundleOptions } from "../../build-options.mjs";

const dir = path.dirname(fileURLToPath(import.meta.url));
const outfile = process.env.STORE_SERVER_OUTFILE;
if (!outfile) {
  process.stderr.write("build-store-server: STORE_SERVER_OUTFILE is required\n");
  process.exit(2);
}

await build({ ...bundleOptions(path.join(dir, "store-server-main.ts"), outfile), logLevel: "warning" });
