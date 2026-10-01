package main

import (
	"errors"
	"fmt"
	"strconv"
)

// Argv is the shim's spawn contract, as the fake parses it:
//
//	<main.js> --listen <uds> --store-socket <uds> --log-fd 3 [--spawn-gate-fd 4] [--fake]
//
// The daemon runs `node <main.js> ...`; the harness substitutes this binary
// for node, so argv[1] is the module path and the flags follow.
type Argv struct {
	MainJS      string
	Listen      string
	StoreSocket string
	LogFD       int
	// SpawnGateFD is the descriptor the shim waits on before it binds, -1
	// when the launcher gates nothing.
	SpawnGateFD int
	Fake        bool
}

// ParseArgv parses the shim's argument vector (without argv[0]). Every field
// the contract fixes is required; an unknown flag is an error, never ignored.
func ParseArgv(args []string) (Argv, error) {
	var a Argv
	a.LogFD = -1
	a.SpawnGateFD = -1
	for i := 0; i < len(args); i++ {
		switch arg := args[i]; arg {
		case "--listen", "--store-socket", "--log-fd", "--spawn-gate-fd":
			if i+1 >= len(args) {
				return Argv{}, fmt.Errorf("fakeshim: %s wants a value", arg)
			}
			i++
			switch arg {
			case "--listen":
				a.Listen = args[i]
			case "--store-socket":
				a.StoreSocket = args[i]
			case "--log-fd":
				fd, err := strconv.Atoi(args[i])
				if err != nil {
					return Argv{}, fmt.Errorf("fakeshim: --log-fd %q: %w", args[i], err)
				}
				a.LogFD = fd
			case "--spawn-gate-fd":
				fd, err := strconv.Atoi(args[i])
				if err != nil {
					return Argv{}, fmt.Errorf("fakeshim: --spawn-gate-fd %q: %w", args[i], err)
				}
				a.SpawnGateFD = fd
			}
		case "--fake":
			a.Fake = true
		default:
			if len(arg) > 1 && arg[0] == '-' {
				return Argv{}, fmt.Errorf("fakeshim: unknown flag %q", arg)
			}
			if a.MainJS != "" {
				return Argv{}, fmt.Errorf("fakeshim: unexpected positional %q", arg)
			}
			a.MainJS = arg
		}
	}
	if a.MainJS == "" {
		return Argv{}, errors.New("fakeshim: the shim module path is missing")
	}
	if a.Listen == "" {
		return Argv{}, errors.New("fakeshim: --listen is missing")
	}
	if a.StoreSocket == "" {
		return Argv{}, errors.New("fakeshim: --store-socket is missing")
	}
	if a.LogFD < 0 {
		return Argv{}, errors.New("fakeshim: --log-fd is missing")
	}
	return a, nil
}
