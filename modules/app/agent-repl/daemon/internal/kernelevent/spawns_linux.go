package kernelevent

import (
	"context"
	"errors"
)

// ErrSpawnsUnsupported is NewSpawns's error on Linux, where no unprivileged
// process-start event exists (the proc connector needs CAP_NET_ADMIN).
var ErrSpawnsUnsupported = errors.New("kernelevent: Linux reports no process start to an unprivileged follower")

// Spawns is Darwin's; on Linux it is never built (NewSpawns refuses).
type Spawns struct{}

// NewSpawns refuses on Linux.
func NewSpawns() (*Spawns, error) { return nil, ErrSpawnsUnsupported }

// FollowForks is never reached on Linux.
func (s *Spawns) FollowForks(int) (bool, error) { return false, ErrSpawnsUnsupported }

// FollowExec is never reached on Linux.
func (s *Spawns) FollowExec(int) (bool, error) { return false, ErrSpawnsUnsupported }

// Wait is never reached on Linux.
func (s *Spawns) Wait(context.Context) error { return ErrSpawnsUnsupported }

// Close is never reached on Linux.
func (s *Spawns) Close() {}
