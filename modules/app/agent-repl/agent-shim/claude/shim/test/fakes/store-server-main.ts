/**
 * The fake store as a process, for suites outside this package that run the
 * shim and need a store behind it without running the real one (the
 * sidecar's mocked-vendor harness). It serves startFakeStore on the socket
 * named by `--socket`, prints `ready` once accepting, and closes on SIGTERM.
 *
 * Built by test/fakes/build-store-server.mjs, with the same bundle options as
 * the shim itself (build-options.mjs).
 */
import { startFakeStore } from "./store-server.js";

const at = process.argv.indexOf("--socket");
const socketPath = at >= 0 ? process.argv[at + 1] : undefined;
if (socketPath === undefined || socketPath === "") {
  process.stderr.write("store-server-main: --socket <path> is required\n");
  process.exit(2);
}

const store = await startFakeStore(socketPath);

const close = (): void => {
  store.close().then(
    () => process.exit(0),
    (err: unknown) => {
      process.stderr.write(`store-server-main: closing the fake store failed: ${String(err)}\n`);
      process.exit(1);
    },
  );
};
// THE HANDLERS STAND BEFORE `ready` IS SAID: a parent that signals the moment
// it reads `ready` otherwise reached a process still on the default action,
// which died of the signal instead of closing (2026-10-15 merge gate).
process.once("SIGTERM", close);
process.once("SIGINT", close);
process.stdout.write("ready\n");
