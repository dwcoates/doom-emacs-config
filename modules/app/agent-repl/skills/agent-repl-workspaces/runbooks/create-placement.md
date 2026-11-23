# Creating a workspace: filing it where it is born

- AN INTAKE FILES THE WORKSPACE IT CREATES. A workspace that creates another
  decides where it belongs and says so in the create, so the new workspace is
  born in its subsection and is never asked to file itself.
  - `-subsection NAME` files it into an existing subsection of its
    repository.
  - `-new-subsection NAME -gather WS,WS,...` files it into a new subsection
    that also gathers the named open, unsectioned, top-level workspaces of the
    repository (never a main workspace). `-gather` is optional.
  - A placement is refused for a child, for a create asked for by the user
    (`-as-user`) and beside an async merge, and for a subsection or a gathered
    workspace the repository does not have. Nothing is created then, and the
    refusal names the arm (`placement_on_child`, `unknown_placement_subsection`,
    `gather_unknown_workspace`, `gather_not_gatherable`, and the others).
  - A create with a placement does not add the filing note to the first
    message.
