# Creation model allowlist

The model picker shown when a workspace is created (`SPC TAB C-o` and every
other picking create) offered a fixed Emacs-side list (`opus`, `sonnet`,
`haiku`) and accepted any typed text. The owner asked that it offer only the
models the creating agent-repl login (work or personal) knows, and accept
nothing else.

## Assumptions (autonomous design)

- The models a login knows are the catalog its last session served, which
  the daemon already keeps per account root (`account_model_catalogs`,
  `wsm.AccountModelCatalog`). Evidence tier: existing code, plus the live
  `~/.claude-emacs/wsm.db`, which holds catalogs for both `~/.claude` and
  `~/.claude-chesscom`.
- The login a new workspace spends as is the path routing of its worktree
  (`account.Resolver.ConfigDirFor(record.Dir)`, `internal/workspace/create.go`).
  Every new worktree is a direct child of `workspace.WorktreeParent(repoDir)`,
  and routing is directory containment with equality counted as "under", so
  `ConfigDirFor(WorktreeParent(repoDir))` answers the same login as any new
  worktree's directory. Both the listing and the create's check use that one
  helper, so they cannot disagree with each other.
- Systems involved: proto, daemon (rpc, create verb), Emacs (rpc, wire,
  picker). The webapp and the shim are untouched.

## Iteration sequence

Endpoint inventory (one new read, one new refusal pair on an existing
endpoint), then the two shapes. The canonical `oneof result { success; error }`
applies.

## Landed changes

### `agentrepl.v1.ListCreationModels`

- WHAT: a new read taking a `workspace.v1.RepositoryRef` and answering the
  creating login's `config_dir` plus either its catalog (`known`, a list of
  `conversation.v1.ModelOption`) or `unknown` when no session on that login has
  served one. `unknown_repository` is the only error.
- WHY: the picker must offer exactly the login's models, and only the daemon
  knows which login a create lands on and what it last served.
- An unknown catalog is a domain answer (success arm), not a failure.
- Consequence accepted: a login that has never run a session offers no model
  to pick; the create still runs under the daemon's default.

### `CreateWorkspaceError.model_not_in_catalog` / `.model_catalog_unknown`

- WHAT: CreateWorkspace refuses a requested model that is not an option's
  `model.name` in the creating login's catalog, or any model at all on a login
  with no catalog. Both arms carry the requested model and the login; the
  first carries the catalog too.
- WHY: "should only accept those models as input" holds for every client
  (Emacs, the `agent-repld workspace create -model` CLI, command files), not
  only the Emacs picker, so the check lives in the create verb.
- The check runs before the naming call and before git is touched, beside the
  other "checked before anything is spent" refusals.
- `CreateWorkspaceRequest.model` stays a `string` rather than becoming the
  `conversation.v1.AgentModel` echo token: the CLI's `-model` is hand-typed
  text, so the field is user input that the daemon validates, not a token a
  client can only echo. The refusal is what makes an invented value fail.
