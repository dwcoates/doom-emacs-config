# The repository lifecycle config and its stages

- **What happens at each point of a workspace's life is its repository's,
  never the caller's.** The repository commits `.agent-repl/lifecycle.yaml`
  at its main checkout root. `.agent-repl/deploy.yaml` is no longer read.
  - A repository with no such file refuses `workspace finished` and
    `workspace async-merge` (`no_deploy_config`, naming lifecycle.yaml). A
    create there is fine and runs only the built-in defaults.
  - A file that does not parse or leaves a required field unset is refused
    naming the field: `deploy_config_invalid` for a deploy, and
    `lifecycle_config_invalid` for a create, before anything is built.
  - The file is read at every use, so an edit applies to the next create or
    deploy.
- A workspace meets FIVE STAGES in order. The deploy stages run in the
  deploying workspace's own feed bubble, one tab per hook, one deploy at a
  time in queue order unless `concurrent:` names them:

| stage | holds | runs | may be concurrent |
|---|---|---|---|
| `creation` | hooks, per kind | once, when the workspace first comes online | no |
| `finish` | hooks, per kind | when the workspace calls `workspace finished`, once that turn ends, before the deploy is put in the queue | no |
| `pre_deploy` | hooks, per kind, then the optional built-in `local_merge` | before anything lands | yes, without a `local_merge` |
| `deploy` | one hook per kind, optional | after the local merge landed | yes, with `pre_deploy` |
| `post_deploy` | hooks, per kind | last | no |

- `queue_drained:` follows them (`lifecycle-failures-queue.md`), for no kind of workspace.
