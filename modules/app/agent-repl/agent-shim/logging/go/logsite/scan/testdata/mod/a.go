package mod

import "example.com/mod/sub"

type Context map[string]any

type logger struct{}

func (logger) Error(op string, message string, ctx Context) {}

const prefix = "tick "

type T struct{}

func (t *T) Run(l logger, n int) {
	l.Error("op", "plain", nil)
	l.Error("op", prefix+"{n}", Context{"n": n})
	go func() {
		l.Error("op", "in a closure {shim}", Context{"shim": 1})
	}()
	l.Error("op", sub.Shared, nil)
}

func (t T) Value(l logger, ctx Context) {
	l.Error("op", "missing {gone}", Context{"here": 1})
	l.Error("op", "dynamic {x}", ctx)
	l.Error("op", "unparsed {", nil)
	l.Error("op", "twice", nil)
	l.Error("op", "twice", nil)
	msg := "not a constant"
	l.Error("op", msg, nil)
}

var _ = func(l logger) { l.Error("op", "at package level", nil) }
