package main

import (
	"errors"
	"strings"
)

// repeatedFlag collects every value of a flag given more than once, in order,
// refusing a blank one as "an empty <noun>".
type repeatedFlag struct {
	values *[]string
	noun   string
}

func (f repeatedFlag) String() string { return "" }
func (f repeatedFlag) Set(value string) error {
	if strings.TrimSpace(value) == "" {
		return errors.New("an empty " + f.noun)
	}
	*f.values = append(*f.values, value)
	return nil
}
