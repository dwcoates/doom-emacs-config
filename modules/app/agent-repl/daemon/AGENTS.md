# daemon/ — REBUILD IN PROGRESS

The daemon is being rebuilt from scratch against the frozen contract. The
previous contents of this file described the deleted tree and are gone.

- Architecture, package map, seams and conventions: `ARCHITECTURE.md`.
- Integration suite specification: `integration/SPEC.md`.
- Unlanded error arms ledger: `ERROR-ARMS.md`.
- Build and test: `go build ./... && go vet ./... && go test ./...` from this
  directory; the integration suite runs with `go test -tags integration ./integration/...`
  (teamlead only during the rebuild).

This file is rewritten for the new architecture in the rebuild's final wave.
