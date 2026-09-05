import { fileURLToPath } from "node:url";

/**
 * WHERE VITE AND VITEST KEEP THEIR WRITABLE CACHE — one definition, imported
 * by every config in this package (the build config and all three test
 * projects), because four copies of a path is four chances to drift.
 *
 * WHY IT IS NOT THE DEFAULT. Vite's default `cacheDir` is
 * `node_modules/.vite`, and that default assumes `node_modules` is a writable
 * directory this package owns. Here it is a SYMLINK to a shared tree in both
 * environments, and in one of them that tree is read-only:
 *
 *   * IN THE E2E SANDBOX CONTAINER, `webapp/node_modules` points into
 *     `/sandbox/deps/webapp/node_modules`, an image layer (see
 *     e2e/sandbox/Dockerfile's "BAKE the node dependency trees"). Baking is
 *     deliberate: materializing the shim's and the webapp's trees per run cost
 *     ~730 MiB of tmpfs RAM and OOM-killed concurrent sandboxes, while an
 *     image layer is shared and paid for once. An image layer is also
 *     read-only, so vitest's first act — creating its cache directory — failed
 *     with a filesystem error, and all eleven `TestWebappLayer*` areas failed
 *     inside the container while passing on the host. A layer that cannot run
 *     in the environment it exists for is not a layer.
 *
 *   * ON THE HOST, `webapp/node_modules` points into the shared node-store
 *     that bin/build-frontend.sh garbage-collects, so the cache was shared
 *     between worktrees and could be collected out from under a live run.
 *
 * WHY HERE, and not a copy of node_modules or a temp directory. Copying the
 * baked tree per run (`SANDBOX_NODE_MODULES=copy`) buys the write back at the
 * price the bake exists to avoid — 108 MiB of tmpfs for this package alone.
 * A temp directory would be per-run in both places, throwing away the host's
 * incremental cache. The package directory is writable in BOTH places (a real
 * directory on the host, the tmpfs working copy in the container), is a
 * sibling of `node_modules` on the same filesystem so no host run gets
 * slower, and is per-worktree rather than shared. It is gitignored, and the
 * sandbox's rsync excludes it so a host cache never travels into a container.
 *
 * test/vite-cache.test.ts holds every config to this: a config that fell back
 * to the default would put the cache back inside `node_modules` and break the
 * sandbox again, silently, so the property is asserted rather than trusted.
 */
export const viteCacheDir = fileURLToPath(new URL(".vite-cache", import.meta.url));
