//go:build !race

package db

// raceEnabled says this test binary is instrumented by the race detector. See
// racedetector_on_test.go for why a latency bound is only asserted when it is
// not.
const raceEnabled = false
