package main

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"syscall"
	"time"

	"agentrepl/logging/sockethold"
	"agentrepl/shim-store/internal/logging"
	"agentrepl/shim-store/internal/server"
)

// holderTakeBound bounds taking one listener from the socket holder: the
// holder's start, when launchd is still spawning it (measured 10ms at worst
// over five starts, the timer's resolution, at load average 3.9-4.4 on
// 2026-11-18 11:25), and the exchange itself (under 1ms). Each Take's time is
// recorded at INFO, so the bound is re-sized from real starts.
const holderTakeBound = 30 * time.Millisecond

// The names the store's holder holds its sockets under (requirements.json,
// socket-holder-store).
const (
	holderStoreSocket = "store"
	holderReadSocket  = "store-read"
)

// storeListeners answers the producers' and the read-only listeners: taken
// from the holder at HOLDER when one is named, the store's own bound sockets
// otherwise. A holder that cannot be reached fails the boot; the store never
// binds its own socket instead, because a bound socket closes with the store.
func storeListeners(holder, socketPath, readSocketPath string, log, readLog *logging.Logger) (net.Listener, net.Listener, error) {
	if holder == "" {
		ln, err := server.Listen(socketPath, log.With(logging.Fields{Component: "server"}))
		if err != nil {
			return nil, nil, err
		}
		readLn, err := server.Listen(readSocketPath, readLog)
		if err != nil {
			if closeErr := ln.Close(); closeErr != nil {
				log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: socketPath}, "closing the producers' socket after the read-only socket failed to bind failed: %v", closeErr)
				return nil, nil, errors.Join(err, closeErr)
			}
			return nil, nil, err
		}
		return ln, readLn, nil
	}
	ln, err := takeFromHolder(holder, holderStoreSocket, log)
	if err != nil {
		return nil, nil, err
	}
	readLn, err := takeFromHolder(holder, holderReadSocket, readLog)
	if err != nil {
		if closeErr := ln.Close(); closeErr != nil {
			log.Log(logging.Fields{Operation: "store.listen", Level: "error", Socket: holder}, "closing the producers' listener after the read-only one could not be taken failed: %v", closeErr)
			return nil, nil, errors.Join(err, closeErr)
		}
		return nil, nil, err
	}
	return ln, readLn, nil
}

// takeFromHolder takes the listener NAME from the holder at HOLDER, bounded
// by holderTakeBound, recording the take and its time.
func takeFromHolder(holder, name string, log *logging.Logger) (net.Listener, error) {
	ctx, cancel := context.WithTimeout(context.Background(), holderTakeBound)
	defer cancel()
	began := time.Now()
	ln, err := sockethold.Take(ctx, holder, name)
	took := time.Since(began)
	if err != nil {
		log.Log(logging.Fields{Operation: "store.listen.holder", Level: "error", Socket: holder},
			"the store could not take its %q listener from the socket holder within %s (measured 10ms) after %s: %v", name, holderTakeBound, took, err)
		return nil, fmt.Errorf("shim-store: take the %q listener from the holder %s: %w", name, holder, err)
	}
	log.Log(logging.Fields{Operation: "store.listen.holder", Socket: holder}, "took the %q listener from the socket holder in %s", name, took)
	return ln, nil
}

// lockDatabase holds the exclusive kernel lock that makes this the one store
// serving DBPATH, and answers its release. A second store refuses to start.
func lockDatabase(dbPath string, log *logging.Logger) (func(), error) {
	path := dbPath + ".store.lock"
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o600)
	if err != nil {
		log.Log(logging.Fields{Operation: "store.lock", Level: "error"}, "opening the database's store lock %s failed: %v", path, err)
		return nil, fmt.Errorf("shim-store: open the store lock %s: %w", path, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		f.Close()
		if errors.Is(err, syscall.EWOULDBLOCK) {
			log.Log(logging.Fields{Operation: "store.lock", Level: "error"}, "another store already serves the database %s; refusing to start a second", dbPath)
			return nil, fmt.Errorf("shim-store: another store already serves %s", dbPath)
		}
		log.Log(logging.Fields{Operation: "store.lock", Level: "error"}, "taking the database's store lock %s failed: %v", path, err)
		return nil, fmt.Errorf("shim-store: lock %s: %w", path, err)
	}
	return func() { f.Close() }, nil
}
