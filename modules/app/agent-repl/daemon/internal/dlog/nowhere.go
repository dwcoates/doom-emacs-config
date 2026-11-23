package dlog

import "agentrepl/logging/logsite"

// Nowhere answers a Logger that records nothing: the default of a component
// that logs nowhere until its owner hands it a real Logger (a store handle
// opened without one). Every Logger lives in this package (Logger.logAt), so
// a component outside it takes this one rather than writing its own.
func Nowhere() Logger { return nowhere{} }

type nowhere struct{}

func (nowhere) Debug(string, template, Context)               {}
func (nowhere) Info(string, template, Context)                {}
func (nowhere) Warn(string, template, Context)                {}
func (nowhere) Error(string, template, Context)               {}
func (nowhere) Log(string, string, logsite.Message, Context)  {}
func (nowhere) logAt(string, string, string, string, Context) {}
func (n nowhere) With(Context) Logger                         { return n }
