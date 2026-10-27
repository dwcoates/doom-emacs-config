package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
)

// accountDefaultModelsDDL is the layout-84 addition: the model each LOGIN
// (account root, a Claude config dir) starts its new sessions under when
// nobody names one, as the reader chose it with a model selector row's
// "Make default" (SetDefaultModel).
//
// ONE ROW PER ACCOUNT ROOT, keyed by its path, because the default belongs to
// the login and not to any workspace: every workspace on the root reads the
// same row. An ABSENT row is "nobody chose one", and the daemon's built-in
// default for that login applies.
//
// A new table, so the step is additive: the build before it never names it.
const accountDefaultModelsDDL = `
CREATE TABLE account_default_models (
  config_dir TEXT PRIMARY KEY,
  model      TEXT NOT NULL
);
`

// PutAccountDefaultModel records MODEL as the default of the account root
// CONFIG_DIR, replacing any default it had. A blank root or model is a bug in
// the caller (the server validates both before any verb runs), so it is
// refused loudly rather than persisted.
func (s *store) PutAccountDefaultModel(ctx context.Context, configDir, model string) error {
	const op = "daemon.wsm.put_account_default_model"
	fields := dlog.Context{"config_dir": configDir, "model": model}
	if strings.TrimSpace(configDir) == "" || strings.TrimSpace(model) == "" {
		err := fmt.Errorf("wsm: an account default model needs a root and a model (root %q, model %q)", configDir, model)
		s.log.Error(op, "refused a blank account root or model", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO account_default_models (config_dir, model) VALUES (?, ?)
			 ON CONFLICT(config_dir) DO UPDATE SET model = excluded.model`,
			configDir, model)
		return err
	})
}

// AccountDefaultModel loads the default the reader chose for the account root
// CONFIG_DIR, reporting false when nobody chose one. An absent row is NOT an
// error; a stored blank model is a corrupt record (DecodeError).
func (s *store) AccountDefaultModel(ctx context.Context, configDir string) (string, bool, error) {
	var model string
	found := false
	err := s.read(ctx, "daemon.wsm.account_default_model", dlog.Context{"config_dir": configDir}, func(ctx context.Context) error {
		var stored string
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT model FROM account_default_models WHERE config_dir = ?`, configDir).Scan(&stored)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		if strings.TrimSpace(stored) == "" {
			return &DecodeError{Table: "account_default_models", Row: configDir, Field: "model", Err: fmt.Errorf("a stored default names a model")}
		}
		model, found = stored, true
		return nil
	})
	return model, found, err
}
