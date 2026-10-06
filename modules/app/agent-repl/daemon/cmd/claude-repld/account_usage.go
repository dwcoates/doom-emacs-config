package main

import (
	"context"

	"claude-repld/internal/wsm"
)

// accountUsageWriter is the state store's account-usage write.
type accountUsageWriter interface {
	SetAccountUsage(ctx context.Context, usage wsm.AccountUsage) error
}

// accountUsageSink keeps an account root's usage evidence durable for the
// footer (footer.WithAccountUsageSink). The store records its own failure,
// and the footer records the error it is handed.
func accountUsageSink(db accountUsageWriter) func(wsm.AccountUsage) error {
	return func(usage wsm.AccountUsage) error {
		return db.SetAccountUsage(context.Background(), usage)
	}
}
