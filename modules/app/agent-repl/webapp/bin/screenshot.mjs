/**
 * screenshot.mjs — headless capture of the live claude-repl webapp.
 *
 * Boots the built webapp against a fresh fake-mode daemon session,
 * drives two real turns (an inline-markdown echo and the `!md`
 * showcase), and writes a PNG of the conversation feed. Proves the
 * markdown engine renders end to end through the real store/render
 * pipeline, not a reconstruction.
 *
 * Prerequisites (all already produced by the normal build):
 *   - agent-shim/claude-shim/dist/main.js
 *                              (cd ../agent-shim/claude-shim && npm run build)
 *   - webapp/dist              (npm run build in webapp/)
 *   - a claude-repld binary    (cd ../daemon && go build ./cmd/claude-repld)
 *   - Playwright Chromium      (.agents-sandbox installs chromium-headless-shell)
 *
 * Usage:
 *   node bin/screenshot.mjs [--theme light|dark] [--out FILE] \
 *       [--daemon HOST:PORT] [--base-url URL]
 *
 * With --base-url the script assumes a webapp is already served there
 * and skips spawning a daemon (point it at a running dev server or a
 * `-webapp dist` daemon). Otherwise it needs --daemon-bin.
 */
import { spawn } from "node:child_process";
import { once } from "node:events";
import { access } from "node:fs/promises";
import { createRequire } from "node:module";
import { setTimeout as sleep } from "node:timers/promises";

function parseArgs(argv) {
  const args = {
    theme: "dark",
    out: null,
    daemon: "127.0.0.1:18790",
    baseUrl: null,
    daemonBin: null,
    shim: "../agent-shim/claude-shim/dist/main.js",
    webapp: "dist",
  };
  for (let i = 0; i < argv.length; i++) {
    const next = () => argv[++i];
    switch (argv[i]) {
      case "--theme": args.theme = next(); break;
      case "--out": args.out = next(); break;
      case "--daemon": args.daemon = next(); break;
      case "--base-url": args.baseUrl = next(); break;
      case "--daemon-bin": args.daemonBin = next(); break;
      case "--shim": args.shim = next(); break;
      case "--webapp": args.webapp = next(); break;
      default: throw new Error(`unknown argument: ${argv[i]}`);
    }
  }
  args.out ??= `screenshot-${args.theme}.png`;
  return args;
}

/** Resolve Playwright's chromium regardless of local vs global install. */
function loadChromium() {
  const candidates = [
    "playwright",
    "playwright-core",
    "/opt/acttoolcache/node/24.18.0/x64/lib/node_modules/playwright/index.mjs",
  ];
  const require = createRequire(import.meta.url);
  for (const spec of candidates) {
    try {
      return require(spec).chromium;
    } catch {
      /* try next */
    }
  }
  throw new Error("playwright not found; install it or run inside the sandbox");
}

async function waitForHttp(url, attempts = 30) {
  for (let i = 0; i < attempts; i++) {
    try {
      const res = await fetch(url);
      if (res.ok || res.status === 404) return;
    } catch {
      /* not up yet */
    }
    await sleep(250);
  }
  throw new Error(`timed out waiting for ${url}`);
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const chromium = loadChromium();

  let daemon = null;
  let baseUrl = args.baseUrl;
  if (!baseUrl) {
    if (!args.daemonBin) {
      throw new Error("--base-url or --daemon-bin is required");
    }
    await access(args.daemonBin);
    const [host, port] = args.daemon.split(":");
    daemon = spawn(
      args.daemonBin,
      ["-addr", args.daemon, "-shim", args.shim, "-fake", "-webapp", args.webapp],
      { stdio: "inherit" },
    );
    baseUrl = `http://${host}:${port}`;
    await waitForHttp(`${baseUrl}/sessions`);
  }

  const browser = await chromium.launch({
    executablePath: process.env.PLAYWRIGHT_CHROMIUM || undefined,
    args: ["--no-sandbox"],
  });
  try {
    const page = await browser.newPage({
      viewport: { width: 900, height: 1100 },
      colorScheme: args.theme === "dark" ? "dark" : "light",
    });
    const errors = [];
    page.on("console", (m) => m.type() === "error" && errors.push(m.text()));
    page.on("pageerror", (e) => errors.push(String(e)));

    await page.goto(`${baseUrl}/?fake=1`, { waitUntil: "networkidle" });
    await page.waitForSelector("#conn-status.ok", { timeout: 10000 });

    const send = async (text) => {
      await page.fill("#composer-input", text);
      await page.click("#send-btn");
    };
    await send("yo show me **markdown** with `code` and a [link](https://example.com)");
    await page.waitForSelector(".result", { timeout: 10000 });
    await send("!md");
    await page.waitForSelector(".md h1", { timeout: 10000 });
    await page.waitForFunction(
      () => document.querySelectorAll(".result").length >= 2,
      { timeout: 10000 },
    );

    const feed = await page.$("#feed");
    await feed.screenshot({ path: args.out });

    if (errors.length > 0) {
      throw new Error(`page reported console errors: ${errors.join("; ")}`);
    }
    console.log(`wrote ${args.out} (${args.theme} theme)`);
  } finally {
    await browser.close();
    if (daemon) {
      daemon.kill();
      await once(daemon, "exit").catch(() => {});
    }
  }
}

main().then(
  () => process.exit(0),
  (err) => {
    console.error(`screenshot failed: ${err.message}`);
    process.exit(1);
  },
);
