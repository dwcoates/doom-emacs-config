/**
 * build-identity.ts — the ONE reader of the bundle's build identity.
 *
 * `bin/build-frontend.sh` computes the source revision once, exports it to the
 * bundler as SHIM_BUILD_SHA, and writes the SAME value to `dist/.built-sha`.
 * build.mjs substitutes `process.env.SHIM_BUILD_SHA` at build time, so a built
 * bundle carries a literal here and the two agree by construction rather than
 * by two computations happening to match.
 *
 * OUTSIDE A BUNDLE — `tsc --noEmit`, vitest, a `node src/...` run — there is no
 * substitution and the env read yields undefined. That is reported as "" and
 * NOT papered over: the daemon reads an empty identity as UNKNOWN, which is
 * never a mismatch and therefore never a bounce. An unknown identity honestly
 * reported is the correct answer; a fabricated one would make the daemon bounce
 * a healthy shim, or refuse to bounce a stale one.
 */
export function shimBuildSha(): string {
  return process.env.SHIM_BUILD_SHA ?? "";
}
