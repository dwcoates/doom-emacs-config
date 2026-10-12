/**
 * engine/vendor-paths.ts — where the vendor files a conversation on disk.
 *
 * The one home of the vendor's project-directory layout, for the engine, the
 * fake vendor and the tests alike:
 *
 *   $CLAUDE_CONFIG_DIR/projects/<cwd-slug>/<vendor-session-id>.jsonl
 *
 * A second copy of the slug rule would look for a directory the vendor never
 * created the day the two drifted.
 */
import path from "node:path";

/**
 * Slugify an absolute directory the way the vendor names its project folder.
 *
 * EVERY byte that is not `[A-Za-z0-9]` becomes `-`; case is preserved and
 * existing dashes are untouched. This is stronger than "replace `/` and `.`":
 * an underscore is replaced too, which matters because macOS temp directories
 * (`/private/var/folders/_m/...`) are full of them.
 *
 * Verified against the live `~/.claude/projects` tree:
 * - `/Users/x/.config/y`      → `-Users-x--config-y`
 * - `/private/var/folders/_m/x` → `-private-var-folders--m-x`
 */
export function cwdSlug(absoluteDir: string): string {
  return absoluteDir.replace(/[^A-Za-z0-9]/g, "-");
}

/** The vendor's project directory for one working directory. */
export function projectDir(configDir: string, cwd: string): string {
  return path.join(configDir, "projects", cwdSlug(cwd));
}

/** The session transcript the vendor writes for one conversation. */
export function transcriptPath(configDir: string, cwd: string, vendorSessionId: string): string {
  return path.join(projectDir(configDir, cwd), `${vendorSessionId}.jsonl`);
}
