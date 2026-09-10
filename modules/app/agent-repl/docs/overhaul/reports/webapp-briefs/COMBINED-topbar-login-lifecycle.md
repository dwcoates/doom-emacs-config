# Combined brief: topbar-login + lifecycle (one agent, worktree topbar-login)

Execute BOTH prompt files in this directory, in this order: `lifecycle.md`
first (it extends src/failure/overlay.ts with `suppress` and src/rpc/context.ts
with `quiesce`, keeping their tests green), then `topbar-login.md`. They are
disjoint in files otherwise (src/lifecycle/*, src/topbar/*, src/login/*).
Commit atomically per module. Finish with `npx vitest run test/lifecycle
test/topbar test/login test/rpc test/failure` and `npm run typecheck` green.
One report covering both scopes.
