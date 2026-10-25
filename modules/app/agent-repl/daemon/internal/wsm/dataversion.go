package wsm

import (
	"context"

	"agent-repld/internal/dlog"
)

// DataVersion answers `PRAGMA data_version` on the handle's one connection.
//
// IT IS THE ONLY CROSS-PROCESS SIGNAL THE STATE FILE GIVES. Two daemons share
// this file across a handover and have no channel between them, so a fact one
// of them commits -- the user's selection above all -- reaches the other only
// when the other re-reads. SQLite bumps data_version on a connection whenever
// any OTHER connection commits, and the store holds exactly one connection
// (SetMaxOpenConns(1)), so a changed answer means "another process wrote", and
// an unchanged one means nothing did.
func (s *store) DataVersion(ctx context.Context) (int64, error) {
	var version int64
	err := s.read(ctx, "daemon.wsm.data_version", dlog.Context{}, func(ctx context.Context) error {
		return s.q(ctx).QueryRowContext(ctx, `PRAGMA data_version`).Scan(&version)
	})
	return version, err
}
