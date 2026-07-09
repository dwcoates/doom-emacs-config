# webapp/bin

## screenshot.mjs

Headless capture of the live webapp rendering a real conversation
(inline markdown echo + the `!md` showcase) through the actual daemon,
fake shim, and store/render pipeline. Use it to eyeball the markdown
engine or to regression-check the feed's look.

```bash
# from webapp/, after building shim + webapp + the daemon binary:
#   (cd ../shim && npm run build)
#   npm run build
#   (cd ../daemon && go build -o /tmp/claude-repld ./cmd/claude-repld)

PLAYWRIGHT_CHROMIUM=$(node -e "process.stdout.write(require('playwright').chromium.executablePath())") \
  node bin/screenshot.mjs --theme dark --daemon-bin /tmp/claude-repld --out feed-dark.png
```

Flags: `--theme light|dark`, `--out FILE`, `--daemon HOST:PORT`,
`--daemon-bin PATH` (spawns its own fake-mode daemon), or `--base-url
URL` to shoot an already-running server instead. Exits non-zero if the
page logs any console error.

In the sandbox, Chromium lives at
`$PLAYWRIGHT_BROWSERS_PATH/chromium_headless_shell-*/chrome-headless-shell-linux64/chrome-headless-shell`;
pass it via `PLAYWRIGHT_CHROMIUM`.
