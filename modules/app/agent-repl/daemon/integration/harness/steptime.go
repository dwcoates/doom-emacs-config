package harness

import (
	"fmt"
	"io"
	"time"
)

// TimedStep runs one setup step of a suite's TestMain and writes how long it
// took to w as one line, "<prefix>: <name> took <ms>ms", failed or not, so a
// slow prebuild or boot names its slow step from the run log alone.
func TimedStep[T any](w io.Writer, prefix, name string, step func() (T, error)) (T, error) {
	began := time.Now()
	v, err := step()
	outcome := ""
	if err != nil {
		outcome = " (failed)"
	}
	fmt.Fprintf(w, "%s: %s took %dms%s\n", prefix, name, time.Since(began).Milliseconds(), outcome)
	return v, err
}
