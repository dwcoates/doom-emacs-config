package drain

import "claude-repld/internal/clock"

// Clock is the controller's view of time; see internal/clock.
type Clock = clock.Clock

// SystemClock is the production Clock.
type SystemClock = clock.System
