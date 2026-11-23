// source-files.mjs — the files a build READS, for the targets whose build
// tool is not Go. Called by lib-deploy-stamp.sh (the one table of source sets);
// never run by hand.
//
//   node source-files.mjs esbuild --repo R --package P [--html F]
//        [--entry F]... [--entry-dir D --ext .ts,.tsx]... [--extra F]...
//   node source-files.mjs swift --repo R --package P    (swift's `package
//        describe --type json` on stdin)
//
// Prints one repo-relative path per line, sorted and unique. Exits nonzero,
// printing nothing on stdout, when the set cannot be determined: the caller
// then treats the target as stale.
//
// WHY AN IMPORT GRAPH AND NOT A DIRECTORY. A directory lists what is near the
// build, not what the build reads: the shim's directory holds its test suite,
// and the webapp's holds a dozen configs for other tools. The graph is the
// bundler's own account of its inputs, taken from the same parser
// (esbuild) the shim is bundled by, so it follows the same imports, dynamic
// imports and `.js`-to-`.ts` rewrites. Computing it needs no build: esbuild
// runs with `write: false`, and bare package imports stay external, because
// node_modules is pinned by the lockfile, which is listed as an input itself.
//
// THE WEBAPP'S ENTRIES INCLUDE ITS WHOLE `src/` AND `test/` TREES, because its
// build (`npm run build`) typechecks the program tsconfig.json includes before
// bundling: a type error in a test fails the build, so a test file is an input
// of the build even though no bundle contains it.
import { createRequire } from "node:module";
import fs from "node:fs";
import path from "node:path";

const JS_EXTENSIONS = new Set([".js", ".mjs", ".cjs"]);
const DECLARATION_SIBLINGS = { ".js": ".d.ts", ".mjs": ".d.mts", ".cjs": ".d.cts" };

function parseArgs(argv) {
  const opts = { entry: [], entryDir: [], ext: [], extra: [], html: [] };
  for (let i = 0; i < argv.length; i += 2) {
    const key = argv[i];
    const value = argv[i + 1];
    if (!key?.startsWith("--") || value === undefined) {
      throw new Error(`source-files: malformed argument ${key ?? "<none>"}`);
    }
    const name = key.slice(2).replace(/-([a-z])/g, (_, c) => c.toUpperCase());
    if (Array.isArray(opts[name])) opts[name].push(value);
    else opts[name] = value;
  }
  if (!opts.repo || !opts.package) throw new Error("source-files: --repo and --package are required");
  opts.repo = fs.realpathSync(opts.repo);
  opts.package = fs.realpathSync(opts.package);
  return opts;
}

/** Every file under `dir` whose name ends in one of `exts`, skipping node_modules. */
function walk(dir, exts) {
  const found = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      if (entry.name !== "node_modules") found.push(...walk(full, exts));
    } else if (exts.some((ext) => entry.name.endsWith(ext))) {
      found.push(full);
    }
  }
  return found;
}

/** The module scripts an index.html loads, as paths under the package. */
function htmlEntries(pkg, htmlFile) {
  const html = fs.readFileSync(path.join(pkg, htmlFile), "utf8");
  const entries = [];
  for (const tag of html.matchAll(/<script\b[^>]*>/g)) {
    if (!/\btype\s*=\s*["']module["']/.test(tag[0])) continue;
    const src = /\bsrc\s*=\s*["']([^"']+)["']/.exec(tag[0]);
    if (src) entries.push(path.join(pkg, src[1].replace(/^\//, "")));
  }
  return entries;
}

/**
 * Query suffixes (`x.svg?raw`, `x.png?inline`) are Vite's; esbuild would look
 * for a file with the `?` in its name. Resolving to the bare file makes the
 * graph name the file the bundler actually reads, and loading it as an empty
 * asset keeps a `?raw` import of a source file from being parsed as code.
 */
const queryPlugin = {
  name: "strip-query",
  setup(build) {
    build.onResolve({ filter: /\?[A-Za-z-]+$/ }, (args) => ({
      path: path.resolve(args.resolveDir, args.path.replace(/\?[A-Za-z-]+$/, "")),
      namespace: "asset",
    }));
    // The file is read as text or bytes, never as code, whatever its extension.
    build.onLoad({ filter: /.*/, namespace: "asset" }, () => ({ contents: "", loader: "empty" }));
  },
};

async function esbuildFiles(opts) {
  const requirePkg = createRequire(path.join(opts.package, "package.json"));
  const { build } = requirePkg("esbuild");
  const entries = new Set(opts.entry.map((e) => path.join(opts.package, e)));
  for (const html of opts.html) for (const e of htmlEntries(opts.package, html)) entries.add(e);
  for (const dir of opts.entryDir) {
    const exts = opts.ext.length > 0 ? opts.ext.flatMap((e) => e.split(",")) : [".ts", ".tsx"];
    for (const file of walk(path.join(opts.package, dir), exts)) entries.add(file);
  }
  if (entries.size === 0) throw new Error("source-files: no entry points");
  const result = await build({
    // ONE synthetic entry importing every real one: separate entries would
    // each generate their own bundle of the shared graph, which for the
    // webapp's thousand test and source files is most of the run's time.
    stdin: {
      contents: [...entries].map((entry) => `import ${JSON.stringify(entry)};`).join("\n"),
      resolveDir: opts.package,
      loader: "js",
      sourcefile: "source-files-entry.js",
    },
    outfile: path.join(opts.package, ".source-files-unwritten.js"),
    absWorkingDir: opts.package,
    bundle: true,
    write: false,
    metafile: true,
    packages: "external",
    format: "esm",
    jsx: "automatic",
    logLevel: "silent",
    plugins: [queryPlugin],
    loader: { ".svg": "empty", ".png": "empty", ".woff": "empty", ".woff2": "empty", ".ttf": "empty" },
  });
  const files = new Set();
  for (const input of Object.keys(result.metafile.inputs)) {
    if (input.includes("node_modules/") || input === "source-files-entry.js") continue;
    files.add(path.resolve(opts.package, input.replace(/^asset:/, "")));
  }
  for (const extra of opts.extra) files.add(path.join(opts.package, extra));
  for (const file of [...files]) {
    const sibling = DECLARATION_SIBLINGS[path.extname(file)];
    if (JS_EXTENSIONS.has(path.extname(file)) && fs.existsSync(file.slice(0, -path.extname(file).length) + sibling)) {
      files.add(file.slice(0, -path.extname(file).length) + sibling);
    }
  }
  return files;
}

/** The sources of every non-test SwiftPM target, resolved through symlinks. */
function swiftFiles(opts) {
  const description = JSON.parse(fs.readFileSync(0, "utf8"));
  const files = new Set();
  for (const target of description.targets) {
    if (target.type === "test") continue;
    for (const source of target.sources) {
      files.add(path.join(opts.package, target.path, source));
    }
  }
  for (const extra of opts.extra) files.add(path.join(opts.package, extra));
  return files;
}

async function main() {
  const [mode, ...rest] = process.argv.slice(2);
  const opts = parseArgs(rest);
  let files;
  if (mode === "esbuild") files = await esbuildFiles(opts);
  else if (mode === "swift") files = swiftFiles(opts);
  else throw new Error(`source-files: unknown mode ${mode}`);
  const relative = new Set();
  for (const file of files) {
    // realpath: a symlinked source directory (the launcher's generated
    // bindings) names the files git tracks, not the link.
    const real = fs.realpathSync(file);
    if (!real.startsWith(opts.repo + path.sep)) {
      throw new Error(`source-files: ${real} is outside the checkout ${opts.repo}`);
    }
    relative.add(path.relative(opts.repo, real));
  }
  process.stdout.write([...relative].sort().join("\n") + "\n");
}

main().catch((error) => {
  process.stderr.write(`${error.stack ?? error}\n`);
  process.exit(1);
});
