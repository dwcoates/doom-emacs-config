package deploy

import (
	"context"
	"fmt"
	"strings"
)

// DefaultLSRegister is macOS's LaunchServices registration tool.
const DefaultLSRegister = "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister"

// LSRegister registers an app bundle with LaunchServices (`lsregister -f`),
// so macOS attributes the bundle's notifications, icon and Spotlight entry to
// the bundle now installed rather than one it cached earlier.
type LSRegister struct {
	// Binary is the lsregister to run (DefaultLSRegister in production).
	Binary string
	// Dir is the working directory the command runs in.
	Dir    string
	Runner Runner
}

// Register registers bundle, failing with lsregister's own words.
func (r LSRegister) Register(ctx context.Context, bundle string) error {
	argv := []string{r.Binary, "-f", bundle}
	out, code, err := r.Runner.Run(ctx, r.Dir, argv)
	if err != nil {
		return fmt.Errorf("deploy: run %s: %w", strings.Join(argv, " "), err)
	}
	if code != 0 {
		return fmt.Errorf("deploy: %s exited %d: %s", strings.Join(argv, " "), code, strings.TrimSpace(out))
	}
	return nil
}
