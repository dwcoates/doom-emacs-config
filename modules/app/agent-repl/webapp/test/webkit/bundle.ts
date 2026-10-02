/**
 * Bundle a WebKit test's page module into ONE classic script, so it runs from
 * `setContent` with no server. Shared by every webkit suite that drives the
 * REAL modules rather than hand-written markup.
 */
import { build, type Rollup } from "vite";
import { protobufRuntimeAliases } from "../../protobuf-runtime-aliases";

/** ENTRY's bundle as an iife that assigns its exports to `window[NAME]`. */
export async function bundlePage(entry: string, name: string): Promise<string> {
  const out = await build({
    configFile: false,
    logLevel: "silent",
    resolve: { alias: protobufRuntimeAliases },
    build: {
      write: false,
      minify: false,
      lib: { entry, formats: ["iife"], name },
    },
  });
  const outputs = (Array.isArray(out) ? out : [out]) as Rollup.RollupOutput[];
  const chunk = outputs.flatMap((o) => o.output).find((o) => o.type === "chunk");
  if (chunk === undefined || chunk.type !== "chunk") throw new Error("the page bundle has no script chunk");
  return chunk.code;
}
