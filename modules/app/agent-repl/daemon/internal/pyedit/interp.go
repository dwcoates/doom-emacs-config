package pyedit

import (
	"fmt"
	"strings"
)

// The interpreter's values. Each is something the program can only have
// built out of literals, so every value the interpreter holds is KNOWN; a
// construct whose value it would have to guess is an error instead.
type value interface{ valueNode() }

type (
	// strVal is a string whose content is known.
	strVal string
	// pathVal is a pathlib path to a known file name.
	pathVal struct{ path string }
	// textVal is a file's content, read from path, with the replacements the
	// program has made to it since, in order.
	textVal struct {
		path string
		reps []Replacement
	}
	// fileVal is a file opened by open().
	fileVal struct {
		path  string
		write bool
	}
	// seqVal is a list or a tuple.
	seqVal []value
	// moduleVal is an imported module.
	moduleVal string
	// funcVal is a callable the interpreter knows: "pathlib.Path", "open",
	// "print".
	funcVal string
	// opaqueVal is a value nothing may be built from (a count, a write's
	// return): it may be compared in an assert and printed, nothing more.
	opaqueVal struct{}
	// defVal is a function the program defined, with its defaults as they
	// were evaluated at the definition, and the names its body binds locally.
	defVal struct {
		def      *defStmt
		defaults []value
		locals   map[string]bool
	}
)

func (strVal) valueNode()    {}
func (pathVal) valueNode()   {}
func (textVal) valueNode()   {}
func (fileVal) valueNode()   {}
func (seqVal) valueNode()    {}
func (moduleVal) valueNode() {}
func (funcVal) valueNode()   {}
func (opaqueVal) valueNode() {}
func (defVal) valueNode()    {}

type interp struct {
	// env is the module's namespace, builtins included.
	env     map[string]value
	frame   *frame
	calling map[string]bool
	edits   []FileEdit
	written map[string]bool
}

// frame is a running function's scope, by python's rule: a name the body
// binds is local throughout the body unless declared global, and reading a
// local before it is bound is an error, never a fall-through to the global.
type frame struct {
	vars    map[string]value
	locals  map[string]bool
	globals map[string]bool
}

func newInterp() *interp {
	return &interp{
		env: map[string]value{
			"open":  funcVal("open"),
			"print": funcVal("print"),
		},
		calling: map[string]bool{},
		written: map[string]bool{},
	}
}

// lookup reads a name in the current scope.
func (in *interp) lookup(name string) (value, error) {
	if f := in.frame; f != nil && f.locals[name] && !f.globals[name] {
		v, ok := f.vars[name]
		if !ok {
			return nil, fmt.Errorf("the local %q read before it is bound", name)
		}
		return v, nil
	}
	v, ok := in.env[name]
	if !ok {
		return nil, fmt.Errorf("the unbound name %q", name)
	}
	return v, nil
}

// assign binds a name in the current scope.
func (in *interp) assign(name string, v value) {
	if f := in.frame; f != nil && !f.globals[name] {
		f.vars[name] = v
		return
	}
	in.env[name] = v
}

// boundNames collects the names a function body binds: assignment, loop and
// with targets, imports, and definitions.
func boundNames(body []stmt, into map[string]bool) {
	for _, s := range body {
		switch s := s.(type) {
		case assignStmt:
			for _, t := range s.targets {
				targetNames(t, into)
			}
		case forStmt:
			targetNames(s.target, into)
			boundNames(s.body, into)
		case withStmt:
			for _, item := range s.items {
				if item.as != "" {
					into[item.as] = true
				}
			}
			boundNames(s.body, into)
		case importStmt:
			for _, name := range s.order {
				into[name] = true
			}
		case defStmt:
			into[s.name] = true
		}
	}
}

func targetNames(t expr, into map[string]bool) {
	switch t := t.(type) {
	case nameExpr:
		into[t.id] = true
	case seqExpr:
		for _, elt := range t.elts {
			targetNames(elt, into)
		}
	}
}

// run interprets a program, answering the edits its writes state.
func run(body []stmt) ([]FileEdit, error) {
	in := newInterp()
	if err := in.block(body); err != nil {
		return nil, err
	}
	return in.edits, nil
}

func (in *interp) block(body []stmt) error {
	for _, s := range body {
		if err := in.stmt(s); err != nil {
			return err
		}
	}
	return nil
}

func (in *interp) stmt(s stmt) error {
	switch s := s.(type) {
	case passStmt:
		return nil
	case importStmt:
		for _, name := range s.order {
			switch what := s.binds[name]; what {
			case "from:pathlib.Path":
				in.assign(name, funcVal("pathlib.Path"))
			default:
				in.assign(name, moduleVal(what))
			}
		}
		return nil
	case assertStmt:
		if err := in.guard(s.test); err != nil {
			return err
		}
		if s.msg != nil {
			// The message is evaluated only when the assert fails, which
			// stops the program either way.
			return in.pure(s.msg, false)
		}
		return nil
	case exprStmt:
		if call, ok := s.x.(callExpr); ok {
			if fn, ok := call.fn.(nameExpr); ok {
				if v, err := in.lookup(fn.id); err == nil && v == funcVal("print") {
					for _, arg := range call.args {
						if err := in.pure(arg, true); err != nil {
							return err
						}
					}
					for _, kw := range call.kw {
						if err := in.pure(kw.value, true); err != nil {
							return err
						}
					}
					return nil
				}
			}
		}
		_, err := in.eval(s.x)
		return err
	case assignStmt:
		v, err := in.eval(s.value)
		if err != nil {
			return err
		}
		for _, target := range s.targets {
			if err := in.bind(target, v); err != nil {
				return err
			}
		}
		return nil
	case forStmt:
		iter, err := in.eval(s.iter)
		if err != nil {
			return err
		}
		items, ok := iter.(seqVal)
		if !ok {
			return fmt.Errorf("a loop over something other than a literal list")
		}
		for _, item := range items {
			if err := in.bind(s.target, item); err != nil {
				return err
			}
			if err := in.block(s.body); err != nil {
				return err
			}
		}
		return nil
	case withStmt:
		for _, item := range s.items {
			v, err := in.eval(item.ctx)
			if err != nil {
				return err
			}
			if _, ok := v.(fileVal); !ok {
				return fmt.Errorf("a with over something other than open()")
			}
			if item.as != "" {
				in.assign(item.as, v)
			}
		}
		return in.block(s.body)
	case defStmt:
		if in.frame != nil {
			return fmt.Errorf("a nested function")
		}
		fn := defVal{def: &s, locals: map[string]bool{}}
		for _, par := range s.params {
			fn.locals[par.name] = true
			if par.def == nil {
				fn.defaults = append(fn.defaults, nil)
				continue
			}
			v, err := in.eval(par.def)
			if err != nil {
				return err
			}
			fn.defaults = append(fn.defaults, v)
		}
		boundNames(s.body, fn.locals)
		in.assign(s.name, fn)
		return nil
	}
	return fmt.Errorf("a statement the subset refuses")
}

func (in *interp) bind(target expr, v value) error {
	switch t := target.(type) {
	case nameExpr:
		in.assign(t.id, v)
		return nil
	case seqExpr:
		items, ok := v.(seqVal)
		if !ok || len(items) != len(t.elts) {
			return fmt.Errorf("an unpacking of a value with the wrong shape")
		}
		for i, elt := range t.elts {
			if err := in.bind(elt, items[i]); err != nil {
				return err
			}
		}
		return nil
	}
	return fmt.Errorf("an assignment to something other than a name")
}

// guard admits an assert's test: membership of a known string in a text or
// string, a text's count compared with a number, their negations, and "and"
// of guards. A guard only ever stops the program, so its outcome is not
// needed; what matters is that evaluating it changes nothing.
func (in *interp) guard(test expr) error {
	switch t := test.(type) {
	case notExpr:
		return in.guard(t.x)
	case binExpr:
		switch t.op {
		case "and", "or":
			if err := in.guard(t.l); err != nil {
				return err
			}
			return in.guard(t.r)
		case "in", "not in":
			needle, err := in.eval(t.l)
			if err != nil {
				return err
			}
			if _, ok := needle.(strVal); !ok {
				return fmt.Errorf("a membership test of something other than a string")
			}
			hay, err := in.eval(t.r)
			if err != nil {
				return err
			}
			switch hay.(type) {
			case textVal, strVal:
				return nil
			}
			return fmt.Errorf("a membership test in something other than text")
		case "==", "!=", "<", "<=", ">", ">=":
			if err := in.pure(t.l, true); err != nil {
				return err
			}
			return in.pure(t.r, true)
		}
	}
	return fmt.Errorf("an assert the subset refuses")
}

// pureCalls are the methods and builtins a printed or asserted value may
// call: each only reads.
var pureCalls = map[string]bool{
	"count": true, "len": true, "repr": true, "str": true, "find": true,
	"startswith": true, "endswith": true, "strip": true, "splitlines": true,
	"read_text": true,
}

// impureWords are names whose appearance inside an f-string means it may do
// something other than read.
var impureWords = []string{"write", "open", "exec", "eval", "__import__", "unlink", "remove",
	"rename", "system", "run", "Popen", "setattr", "rmtree", "mkdir", "touch", "replace", ":=", "exit"}

// pure admits an expression that only reads: literals, names, attributes,
// subscripts, and the read-only calls. When bound, every name it reads must
// be bound, because the expression is certain to be evaluated and an unbound
// name would stop the program there.
func (in *interp) pure(x expr, bound bool) error {
	switch x := x.(type) {
	case strExpr:
		if x.tok.fstring {
			for _, word := range impureWords {
				if strings.Contains(x.tok.raw, word) {
					return fmt.Errorf("an f-string that may do more than read")
				}
			}
		}
		return nil
	case numExpr:
		return nil
	case nameExpr:
		if bound {
			_, err := in.lookup(x.id)
			return err
		}
		return nil
	case subscriptExpr:
		if err := in.pure(x.x, bound); err != nil {
			return err
		}
		for _, part := range x.parts {
			if part == nil {
				continue
			}
			if err := in.pure(part, bound); err != nil {
				return err
			}
		}
		return nil
	case attrExpr:
		return in.pure(x.x, bound)
	case seqExpr:
		for _, elt := range x.elts {
			if err := in.pure(elt, bound); err != nil {
				return err
			}
		}
		return nil
	case binExpr:
		if err := in.pure(x.l, bound); err != nil {
			return err
		}
		return in.pure(x.r, bound)
	case notExpr:
		return in.pure(x.x, bound)
	case callExpr:
		name := ""
		switch fn := x.fn.(type) {
		case nameExpr:
			name = fn.id
		case attrExpr:
			name = fn.name
			if err := in.pure(fn.x, bound); err != nil {
				return err
			}
		}
		if !pureCalls[name] {
			return fmt.Errorf("a call to %q where only reads belong", name)
		}
		for _, arg := range x.args {
			if err := in.pure(arg, bound); err != nil {
				return err
			}
		}
		for _, kw := range x.kw {
			if err := in.pure(kw.value, bound); err != nil {
				return err
			}
		}
		return nil
	}
	return fmt.Errorf("an expression the subset refuses")
}

func (in *interp) eval(x expr) (value, error) {
	switch x := x.(type) {
	case strExpr:
		if x.tok.fstring {
			return nil, fmt.Errorf("an f-string where a known string belongs")
		}
		return strVal(x.tok.text), nil
	case numExpr:
		return opaqueVal{}, nil
	case nameExpr:
		return in.lookup(x.id)
	case seqExpr:
		items := make(seqVal, 0, len(x.elts))
		for _, elt := range x.elts {
			v, err := in.eval(elt)
			if err != nil {
				return nil, err
			}
			items = append(items, v)
		}
		return items, nil
	case binExpr:
		if x.op != "+" {
			return nil, fmt.Errorf("the operator %q outside an assert", x.op)
		}
		l, err := in.eval(x.l)
		if err != nil {
			return nil, err
		}
		r, err := in.eval(x.r)
		if err != nil {
			return nil, err
		}
		ls, lok := l.(strVal)
		rs, rok := r.(strVal)
		if !lok || !rok {
			return nil, fmt.Errorf("a + of something other than two strings")
		}
		return ls + rs, nil
	case attrExpr:
		base, err := in.eval(x.x)
		if err != nil {
			return nil, err
		}
		if base == moduleVal("module:pathlib") && x.name == "Path" {
			return funcVal("pathlib.Path"), nil
		}
		return nil, fmt.Errorf("the attribute %q outside a call", x.name)
	case callExpr:
		return in.call(x)
	}
	return nil, fmt.Errorf("an expression the subset refuses")
}

// encodingOnly admits a read or write whose only keyword is encoding.
func encodingOnly(call callExpr) error {
	for _, kw := range call.kw {
		if kw.name != "encoding" {
			return fmt.Errorf("the keyword %q", kw.name)
		}
		if _, ok := kw.value.(strExpr); !ok {
			return fmt.Errorf("an encoding that is not a literal")
		}
	}
	return nil
}

func (in *interp) args(call callExpr) ([]value, error) {
	out := make([]value, 0, len(call.args))
	for _, arg := range call.args {
		v, err := in.eval(arg)
		if err != nil {
			return nil, err
		}
		out = append(out, v)
	}
	return out, nil
}

// pathOf reads a value naming a file: a string or a pathlib path.
func pathOf(v value) (string, error) {
	switch v := v.(type) {
	case strVal:
		return string(v), nil
	case pathVal:
		return v.path, nil
	}
	return "", fmt.Errorf("a file named by something other than a known path")
}

func (in *interp) call(call callExpr) (value, error) {
	if attr, ok := call.fn.(attrExpr); ok {
		if base, err := in.eval(attr.x); err == nil {
			if _, isModule := base.(moduleVal); !isModule {
				return in.method(base, attr.name, call)
			}
		} else {
			return nil, err
		}
	}
	fn, err := in.eval(call.fn)
	if err != nil {
		return nil, err
	}
	args, err := in.args(call)
	if err != nil {
		return nil, err
	}
	if def, ok := fn.(defVal); ok {
		return in.invoke(def, args, call)
	}
	switch fn {
	case funcVal("pathlib.Path"):
		if len(args) != 1 || len(call.kw) > 0 {
			return nil, fmt.Errorf("a Path() of other than one argument")
		}
		path, err := pathOf(args[0])
		if err != nil {
			return nil, err
		}
		return pathVal{path: path}, nil
	case funcVal("open"):
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("an open() of %d arguments", len(args))
		}
		if err := encodingOnly(call); err != nil {
			return nil, err
		}
		path, err := pathOf(args[0])
		if err != nil {
			return nil, err
		}
		mode := strVal("r")
		if len(args) == 2 {
			m, ok := args[1].(strVal)
			if !ok {
				return nil, fmt.Errorf("an open() mode that is not a known string")
			}
			mode = m
		}
		switch mode {
		case "r", "rt":
			return fileVal{path: path}, nil
		case "w", "wt":
			return fileVal{path: path, write: true}, nil
		}
		return nil, fmt.Errorf("the open() mode %q", string(mode))
	}
	return nil, fmt.Errorf("a call the subset refuses")
}

func (in *interp) method(base value, name string, call callExpr) (value, error) {
	args, err := in.args(call)
	if err != nil {
		return nil, err
	}
	switch b := base.(type) {
	case pathVal:
		switch name {
		case "read_text":
			if len(args) > 0 {
				return nil, fmt.Errorf("a read_text() with positional arguments")
			}
			if err := encodingOnly(call); err != nil {
				return nil, err
			}
			return textVal{path: b.path}, nil
		case "write_text":
			if len(args) != 1 {
				return nil, fmt.Errorf("a write_text() of %d arguments", len(args))
			}
			if err := encodingOnly(call); err != nil {
				return nil, err
			}
			return opaqueVal{}, in.write(b.path, args[0])
		}
	case fileVal:
		if len(call.kw) > 0 {
			return nil, fmt.Errorf("a keyword on a file method")
		}
		switch {
		case name == "read" && !b.write && len(args) == 0:
			return textVal{path: b.path}, nil
		case name == "write" && b.write && len(args) == 1:
			return opaqueVal{}, in.write(b.path, args[0])
		}
	case textVal:
		if name == "replace" {
			old, new, err := replaceArgs(args, call)
			if err != nil {
				return nil, err
			}
			if old == "" {
				return nil, fmt.Errorf("a replacement of the empty string")
			}
			reps := append(append([]Replacement(nil), b.reps...), Replacement{Old: old, New: new, First: len(args) == 3})
			if old == new {
				reps = b.reps
			}
			return textVal{path: b.path, reps: reps}, nil
		}
	case strVal:
		if name == "replace" {
			old, new, err := replaceArgs(args, call)
			if err != nil {
				return nil, err
			}
			n := -1
			if len(args) == 3 {
				n = 1
			}
			return strVal(strings.Replace(string(b), old, new, n)), nil
		}
	}
	return nil, fmt.Errorf("the method %q the subset refuses", name)
}

// replaceArgs reads str.replace's (old, new[, 1]): two known strings and, at
// most, a count of exactly one.
func replaceArgs(args []value, call callExpr) (string, string, error) {
	if len(call.kw) > 0 || len(args) < 2 || len(args) > 3 {
		return "", "", fmt.Errorf("a replace() of other than (old, new[, 1])")
	}
	if len(args) == 3 {
		count, ok := call.args[2].(numExpr)
		if !ok || count.text != "1" {
			return "", "", fmt.Errorf("a replace() count other than 1")
		}
	}
	old, ok := args[0].(strVal)
	if !ok {
		return "", "", fmt.Errorf("a replace() of something other than a known string")
	}
	new, ok := args[1].(strVal)
	if !ok {
		return "", "", fmt.Errorf("a replace() by something other than a known string")
	}
	return string(old), string(new), nil
}

// write records a file write. Only a text read from the same file is an
// edit: a write of anything else replaces content the program never states.
func (in *interp) write(path string, content value) error {
	text, ok := content.(textVal)
	if !ok {
		return fmt.Errorf("a write of something other than the file's own edited text")
	}
	if text.path != path {
		return fmt.Errorf("a write of %q's text into %q", text.path, path)
	}
	if in.written[path] {
		return fmt.Errorf("a second write to %q", path)
	}
	in.written[path] = true
	if len(text.reps) > 0 {
		in.edits = append(in.edits, FileEdit{Path: path, Replacements: text.reps})
	}
	return nil
}

// invoke runs a call to a function the program defined, binding its
// arguments by python's rule: positional first, then keywords, then the
// defaults; a missing, repeated or unknown argument is an error, as it is in
// python. A function the program is already running is refused, so a
// recursive program is declined rather than unrolled.
func (in *interp) invoke(fn defVal, args []value, call callExpr) (value, error) {
	d := fn.def
	if in.calling[d.name] {
		return nil, fmt.Errorf("a recursive call to %q", d.name)
	}
	if len(args) > len(d.params) {
		return nil, fmt.Errorf("a call to %q with %d arguments for %d parameters", d.name, len(args), len(d.params))
	}
	f := &frame{vars: map[string]value{}, locals: fn.locals, globals: d.globals}
	for i, arg := range args {
		f.vars[d.params[i].name] = arg
	}
	for _, kw := range call.kw {
		known := false
		for _, par := range d.params {
			known = known || par.name == kw.name
		}
		if !known {
			return nil, fmt.Errorf("a call to %q with the unknown keyword %q", d.name, kw.name)
		}
		if _, dup := f.vars[kw.name]; dup {
			return nil, fmt.Errorf("a call to %q that binds %q twice", d.name, kw.name)
		}
		v, err := in.eval(kw.value)
		if err != nil {
			return nil, err
		}
		f.vars[kw.name] = v
	}
	for i, par := range d.params {
		if _, ok := f.vars[par.name]; ok {
			continue
		}
		if fn.defaults[i] == nil {
			return nil, fmt.Errorf("a call to %q missing %q", d.name, par.name)
		}
		f.vars[par.name] = fn.defaults[i]
	}
	outer := in.frame
	in.frame = f
	in.calling[d.name] = true
	err := in.block(d.body)
	in.frame = outer
	delete(in.calling, d.name)
	if err != nil {
		return nil, err
	}
	return opaqueVal{}, nil
}
