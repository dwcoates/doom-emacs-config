package persistentwifi

import (
	"bufio"
	"context"
	"fmt"
	"net"
	"path/filepath"
	"strings"
	"sync"
)

// A HEARD HOST FACT reaches the controller through a watcher of one shape:
// Watch subscribes, THEN reads, and hands heard the state read followed by
// every change, in order and on one goroutine, until ctx ends. Besides the
// operating system's own watcher, every heard fact has the same two fake
// forms, which live here once: a line socket a test harness serves
// (watchSocket), and a state fixed for the daemon's life (watchFixed). The
// environment variable naming a fake is resolved by fakeFact, and the
// watcher it names picked by pickWatcher.

// watchSocket is the watch over a Unix socket a harness serves: the server
// writes each connection the fact's state when it connects, then every
// change, one name per line. EACH WATCH HAS A CONNECTION OF ITS OWN, so two
// daemons watching at once (a handover's predecessor and successor) never
// take each other's lines, and closing a watch's own connection is what
// wakes its read when ctx ends. fact names the fact for the errors.
func watchSocket[T any](ctx context.Context, path, fact string, parse func(string) (T, error), heard func(T)) error {
	var dialer net.Dialer
	conn, err := dialer.DialContext(ctx, "unix", path)
	if err != nil {
		if ctx.Err() != nil {
			return nil
		}
		return fmt.Errorf("connect to the %s socket: %w", fact, err)
	}
	stopped := make(chan struct{})
	var closer sync.WaitGroup
	closer.Add(1)
	// inflight: joined: the function that starts it waits for it.
	go func() {
		defer closer.Done()
		select {
		case <-ctx.Done():
		case <-stopped:
		}
		_ = conn.Close()
	}()
	defer closer.Wait()
	defer close(stopped)
	sc := bufio.NewScanner(conn)
	for sc.Scan() {
		v, err := parse(sc.Text())
		if err != nil {
			return err
		}
		heard(v)
	}
	if ctx.Err() != nil {
		return nil
	}
	if err := sc.Err(); err != nil {
		return fmt.Errorf("read the %s socket: %w", fact, err)
	}
	return fmt.Errorf("the %s socket ended", fact)
}

// watchFixed is the watch of a fact heard once, as v, that never changes.
func watchFixed[T any](ctx context.Context, v T, heard func(T)) error {
	heard(v)
	<-ctx.Done()
	return nil
}

// parseState reads one state's name among states, the names a fake's lines
// and its fixed value use. fact names the fact for the error.
func parseState[T fmt.Stringer](line, fact string, states ...T) (T, error) {
	for _, s := range states {
		if strings.TrimSpace(line) == s.String() {
			return s, nil
		}
	}
	var zero T
	return zero, fmt.Errorf("%q names no %s state", line, fact)
}

// fakeFact checks one heard fact's fake (env's value, a state name or an
// absolute socket path) against the fake host tools (EnvToolsDir's dir):
// fake tools without a fake fact are REFUSED, since the real fact would then
// move what the fake tools only pretend to set, and a fake fact without fake
// tools likewise, since it would move the real machine.
func fakeFact[T any](env, value, dir string, parse func(string) (T, error)) error {
	_, parseErr := parse(value)
	switch {
	case dir != "" && value == "":
		return fmt.Errorf("persistentwifi: %s is set without %s; fake host tools never hear the real machine", EnvToolsDir, env)
	case value != "" && dir == "":
		return fmt.Errorf("persistentwifi: %s is set without %s; a fake fact never moves the real machine", env, EnvToolsDir)
	case value != "" && parseErr != nil && !filepath.IsAbs(value):
		return fmt.Errorf("persistentwifi: %s=%q is neither a state nor an absolute socket path", env, value)
	}
	return nil
}

// pickWatcher is the watcher a fake's value names: empty is the operating
// system's (system), a state name a fixed watcher, anything else (an
// absolute path, fakeFact's check) a socket's.
func pickWatcher[T, W any](fake string, parse func(string) (T, error), system func() W, fixed func(T) W, socket func(string) W) W {
	if fake == "" {
		return system()
	}
	if v, err := parse(fake); err == nil {
		return fixed(v)
	}
	return socket(fake)
}
