# Reorganizing across repositories

Every piece of work has ONE repository it principally belongs to, and gets
ONE workspace in each other repository it touches. This workspace organizes
itself that way PROACTIVELY and WITHOUT asking, when its prompt says to run
this skill or as soon as it is clear the work belongs elsewhere, and tells the
user in the same response what it reorganized and why.

1. Decide the repository the work principally belongs to, and every other
   repository it touches. Register any agent-repl does not know yet
   (`repository register`).
2. When the principal repository is this one, do not move. Give each other
   repository it touches one child of this workspace: create it as
   "Cross-repository work goes through a workspace in the other repository"
   says, or adopt one that already exists there (`workspace set WS -parent
   here`, `adopt.md`). This workspace governs its children.
3. When the principal repository is another one, MOVE:
   - a. Read this workspace's priority, one-shot mark and links with
     `workspace status here`.
   - b. Create the governor: `workspace create -repository <principal>
     -relationship peer -lands-into main -origin-master -context fork -fork here -prompt TEXT`,
     adding `-priority P` and `-one-shot` when this workspace has them.
   - c. TEXT tells the governor that it is this workspace moved, what is done
     and what remains, and to do these first:
     - re-establish every subscription this session held to anything outside
       agent-repl (a Slack thread's `/gns-sockets subscribe <url> respond`,
       for one), since a fork is a new session;
     - store this workspace's links with `workspace link here`;
     - adopt each workspace step d names (`workspace set WS -parent here`).
   - d. Hand this workspace's children to the governor: TEXT names each
     child, and the governor adopts them. Work this workspace already
     committed or left uncommitted in its own repository stays on its
     branch: rename this workspace to the piece it now holds (`workspace set
     here -name NAME`), keep it open, and name it in TEXT among the
     workspaces the governor adopts. With no such work, run `workspace close
     here` once the governor has adopted this workspace's children.
4. Edits made by hand in another repository's checkout move to that
   repository's one workspace: `SHA=$(git -C <checkout> stash create "<what>")`,
   then `git stash apply <SHA>` in the workspace's worktree, then restore
   those files in the checkout so the edits live in one place.
