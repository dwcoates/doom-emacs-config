package pyedit

import "fmt"

// The python subset's syntax tree. It is WIDER than what the interpreter
// accepts (any call, any attribute, any subscript), so that a program is first
// read whole and then judged; it is NARROWER than python wherever a construct
// could change control flow (if, while, try, raise, return, del, augmented
// assignment, lambdas, comprehensions, a nested def): those fail the parse.

type expr interface{ exprNode() }

type (
	strExpr  struct{ tok token }
	numExpr  struct{ text string }
	nameExpr struct{ id string }
	attrExpr struct {
		x    expr
		name string
	}
	callExpr struct {
		fn   expr
		args []expr
		kw   []keyword
	}
	seqExpr struct {
		elts []expr
	}
	binExpr struct {
		op   string
		l, r expr
	}
	notExpr struct{ x expr }
	// subscriptExpr is x[...]: an index or a slice, each part nil when the
	// slice leaves it out. Only ever read (an assert's message, a print).
	subscriptExpr struct {
		x     expr
		parts []expr
	}
)

type keyword struct {
	name  string
	value expr
}

func (strExpr) exprNode()       {}
func (numExpr) exprNode()       {}
func (nameExpr) exprNode()      {}
func (attrExpr) exprNode()      {}
func (callExpr) exprNode()      {}
func (seqExpr) exprNode()       {}
func (binExpr) exprNode()       {}
func (notExpr) exprNode()       {}
func (subscriptExpr) exprNode() {}

type stmt interface{ stmtNode() }

type (
	importStmt struct {
		// binds maps each bound name to what it names: "module:<m>" or
		// "from:<m>.<name>".
		binds map[string]string
		order []string
	}
	assignStmt struct {
		targets []expr
		value   expr
	}
	exprStmt   struct{ x expr }
	assertStmt struct {
		test expr
		msg  expr
	}
	forStmt struct {
		target expr
		iter   expr
		body   []stmt
	}
	withStmt struct {
		items []withItem
		body  []stmt
	}
	passStmt struct{}
	// defStmt is a function definition: positional parameters, each with an
	// optional default, and a body whose leading global statements are
	// collected into globals.
	defStmt struct {
		name    string
		params  []param
		globals map[string]bool
		body    []stmt
	}
)

type param struct {
	name string
	def  expr
}

type withItem struct {
	ctx expr
	as  string
}

func (importStmt) stmtNode() {}
func (assignStmt) stmtNode() {}
func (exprStmt) stmtNode()   {}
func (assertStmt) stmtNode() {}
func (forStmt) stmtNode()    {}
func (withStmt) stmtNode()   {}
func (passStmt) stmtNode()   {}
func (defStmt) stmtNode()    {}

type parser struct {
	toks []token
	pos  int
}

// parse reads a python program in the subset.
func parse(src string) ([]stmt, error) {
	toks, err := lex(src)
	if err != nil {
		return nil, err
	}
	p := &parser{toks: toks}
	var body []stmt
	for p.peek().kind != tokEOF {
		stmts, err := p.statement()
		if err != nil {
			return nil, err
		}
		body = append(body, stmts...)
	}
	return body, nil
}

func (p *parser) peek() token { return p.toks[p.pos] }

func (p *parser) next() token {
	t := p.toks[p.pos]
	if t.kind != tokEOF {
		p.pos++
	}
	return t
}

func (p *parser) isOp(op string) bool {
	t := p.peek()
	return t.kind == tokOp && t.text == op
}

func (p *parser) isName(name string) bool {
	t := p.peek()
	return t.kind == tokName && t.text == name
}

func (p *parser) expectOp(op string) error {
	if !p.isOp(op) {
		return p.errorf("expected %q", op)
	}
	p.next()
	return nil
}

func (p *parser) errorf(format string, args ...any) error {
	t := p.peek()
	return fmt.Errorf("line %d: %s (at %q)", t.line, fmt.Sprintf(format, args...), t.text)
}

// keywords that open a statement the subset refuses.
var refusedKeywords = map[string]bool{
	"if": true, "elif": true, "else": true, "while": true, "class": true,
	"try": true, "except": true, "finally": true, "raise": true, "return": true,
	"del": true, "nonlocal": true, "lambda": true, "yield": true,
	"async": true, "await": true, "break": true, "continue": true, "match": true,
	"exec": true,
}

// statement reads one logical line (several when ';'-separated) or one
// compound statement.
func (p *parser) statement() ([]stmt, error) {
	t := p.peek()
	if t.kind == tokName && refusedKeywords[t.text] {
		return nil, p.errorf("a %q statement", t.text)
	}
	if p.isName("for") {
		s, err := p.forStatement()
		return []stmt{s}, err
	}
	if p.isName("with") {
		s, err := p.withStatement()
		return []stmt{s}, err
	}
	if p.isName("def") {
		s, err := p.defStatement()
		return []stmt{s}, err
	}
	if p.isName("global") {
		return nil, p.errorf("a global statement outside a function's opening lines")
	}
	var out []stmt
	for {
		s, err := p.simple()
		if err != nil {
			return nil, err
		}
		out = append(out, s)
		if !p.isOp(";") {
			break
		}
		p.next()
		if p.peek().kind == tokNewline {
			break
		}
	}
	if p.peek().kind != tokNewline {
		return nil, p.errorf("expected the end of the line")
	}
	p.next()
	return out, nil
}

func (p *parser) simple() (stmt, error) {
	t := p.peek()
	if t.kind == tokName && refusedKeywords[t.text] {
		return nil, p.errorf("a %q statement", t.text)
	}
	switch {
	case p.isName("pass"):
		p.next()
		return passStmt{}, nil
	case p.isName("import"), p.isName("from"):
		return p.importStatement()
	case p.isName("assert"):
		p.next()
		test, err := p.expression()
		if err != nil {
			return nil, err
		}
		s := assertStmt{test: test}
		if p.isOp(",") {
			p.next()
			if s.msg, err = p.expression(); err != nil {
				return nil, err
			}
		}
		return s, nil
	}
	first, err := p.exprList()
	if err != nil {
		return nil, err
	}
	if !p.isOp("=") {
		if p.peek().kind == tokOp && p.peek().text != ";" {
			return nil, p.errorf("an operator the subset refuses")
		}
		return exprStmt{x: first}, nil
	}
	targets := []expr{first}
	var value expr
	for p.isOp("=") {
		p.next()
		if value, err = p.exprList(); err != nil {
			return nil, err
		}
		targets = append(targets, value)
	}
	targets = targets[:len(targets)-1]
	for _, target := range targets {
		if err := checkTarget(target); err != nil {
			return nil, err
		}
	}
	return assignStmt{targets: targets, value: value}, nil
}

// checkTarget admits a name or a tuple of names as an assignment target.
func checkTarget(target expr) error {
	switch t := target.(type) {
	case nameExpr:
		return nil
	case seqExpr:
		for _, elt := range t.elts {
			if _, ok := elt.(nameExpr); !ok {
				return fmt.Errorf("an assignment to something other than a name")
			}
		}
		return nil
	}
	return fmt.Errorf("an assignment to something other than a name")
}

func (p *parser) importStatement() (stmt, error) {
	s := importStmt{binds: map[string]string{}}
	bind := func(name, what string) {
		if _, seen := s.binds[name]; !seen {
			s.order = append(s.order, name)
		}
		s.binds[name] = what
	}
	if p.isName("import") {
		p.next()
		for {
			module, err := p.dotted()
			if err != nil {
				return nil, err
			}
			name := module
			if i := indexByte(module, '.'); i >= 0 {
				name = module[:i]
			}
			if p.isName("as") {
				p.next()
				if name, err = p.name(); err != nil {
					return nil, err
				}
				bind(name, "module:"+module)
			} else {
				bind(name, "module:"+name)
			}
			if !p.isOp(",") {
				return s, nil
			}
			p.next()
		}
	}
	p.next()
	module, err := p.dotted()
	if err != nil {
		return nil, err
	}
	if !p.isName("import") {
		return nil, p.errorf("expected import")
	}
	p.next()
	paren := p.isOp("(")
	if paren {
		p.next()
	}
	for {
		if p.isOp("*") {
			return nil, p.errorf("a star import")
		}
		name, err := p.name()
		if err != nil {
			return nil, err
		}
		bound := name
		if p.isName("as") {
			p.next()
			if bound, err = p.name(); err != nil {
				return nil, err
			}
		}
		bind(bound, "from:"+module+"."+name)
		if !p.isOp(",") {
			break
		}
		p.next()
		if paren && p.isOp(")") {
			break
		}
	}
	if paren {
		if err := p.expectOp(")"); err != nil {
			return nil, err
		}
	}
	return s, nil
}

func indexByte(s string, c byte) int {
	for i := 0; i < len(s); i++ {
		if s[i] == c {
			return i
		}
	}
	return -1
}

func (p *parser) dotted() (string, error) {
	name, err := p.name()
	if err != nil {
		return "", err
	}
	for p.isOp(".") {
		p.next()
		part, err := p.name()
		if err != nil {
			return "", err
		}
		name += "." + part
	}
	return name, nil
}

func (p *parser) name() (string, error) {
	t := p.peek()
	if t.kind != tokName {
		return "", p.errorf("expected a name")
	}
	p.next()
	return t.text, nil
}

func (p *parser) block() ([]stmt, error) {
	if err := p.expectOp(":"); err != nil {
		return nil, err
	}
	if p.peek().kind != tokNewline {
		// A one-line body: "for a, b in pairs: s = s.replace(a, b)".
		var out []stmt
		for {
			s, err := p.simple()
			if err != nil {
				return nil, err
			}
			out = append(out, s)
			if !p.isOp(";") {
				break
			}
			p.next()
		}
		if p.peek().kind != tokNewline {
			return nil, p.errorf("expected the end of the line")
		}
		p.next()
		return out, nil
	}
	p.next()
	if p.peek().kind != tokIndent {
		return nil, p.errorf("expected an indented block")
	}
	p.next()
	var body []stmt
	for p.peek().kind != tokDedent {
		if p.peek().kind == tokEOF {
			return nil, p.errorf("an unterminated block")
		}
		stmts, err := p.statement()
		if err != nil {
			return nil, err
		}
		body = append(body, stmts...)
	}
	p.next()
	return body, nil
}

func (p *parser) forStatement() (stmt, error) {
	p.next()
	target, err := p.targetList()
	if err != nil {
		return nil, err
	}
	if err := checkTarget(target); err != nil {
		return nil, err
	}
	if !p.isName("in") {
		return nil, p.errorf("expected in")
	}
	p.next()
	iter, err := p.exprList()
	if err != nil {
		return nil, err
	}
	body, err := p.block()
	if err != nil {
		return nil, err
	}
	if p.isName("else") {
		return nil, p.errorf("a for-else")
	}
	return forStmt{target: target, iter: iter, body: body}, nil
}

// targetList reads a for loop's target, which stops at "in" rather than
// reading it as a comparison.
func (p *parser) targetList() (expr, error) {
	var elts []expr
	paren := p.isOp("(")
	if paren {
		p.next()
	}
	for {
		name, err := p.name()
		if err != nil {
			return nil, err
		}
		elts = append(elts, nameExpr{id: name})
		if !p.isOp(",") {
			break
		}
		p.next()
		if paren && p.isOp(")") {
			break
		}
	}
	if paren {
		if err := p.expectOp(")"); err != nil {
			return nil, err
		}
	}
	if len(elts) == 1 && !paren {
		return elts[0], nil
	}
	return seqExpr{elts: elts}, nil
}

// defStatement reads a function definition. Its global statements must open
// its body, which is where python requires them to precede every use.
func (p *parser) defStatement() (stmt, error) {
	p.next()
	name, err := p.name()
	if err != nil {
		return nil, err
	}
	if err := p.expectOp("("); err != nil {
		return nil, err
	}
	d := defStmt{name: name, globals: map[string]bool{}}
	for !p.isOp(")") {
		pname, err := p.name()
		if err != nil {
			return nil, err
		}
		par := param{name: pname}
		if p.isOp("=") {
			p.next()
			if par.def, err = p.expression(); err != nil {
				return nil, err
			}
		} else if len(d.params) > 0 && d.params[len(d.params)-1].def != nil {
			return nil, p.errorf("a parameter without a default after one with")
		}
		d.params = append(d.params, par)
		if !p.isOp(",") {
			break
		}
		p.next()
	}
	if err := p.expectOp(")"); err != nil {
		return nil, err
	}
	if err := p.expectOp(":"); err != nil {
		return nil, err
	}
	if p.peek().kind != tokNewline {
		return nil, p.errorf("a one-line function")
	}
	p.next()
	if p.peek().kind != tokIndent {
		return nil, p.errorf("expected an indented block")
	}
	p.next()
	for p.isName("global") {
		p.next()
		for {
			g, err := p.name()
			if err != nil {
				return nil, err
			}
			d.globals[g] = true
			if !p.isOp(",") {
				break
			}
			p.next()
		}
		if p.isOp(";") {
			p.next()
			if p.peek().kind == tokNewline {
				p.next()
			}
			continue
		}
		if p.peek().kind != tokNewline {
			return nil, p.errorf("expected the end of the line")
		}
		p.next()
	}
	for p.peek().kind != tokDedent {
		if p.peek().kind == tokEOF {
			return nil, p.errorf("an unterminated block")
		}
		if p.isName("def") {
			return nil, p.errorf("a nested function")
		}
		stmts, err := p.statement()
		if err != nil {
			return nil, err
		}
		d.body = append(d.body, stmts...)
	}
	p.next()
	return d, nil
}

func (p *parser) withStatement() (stmt, error) {
	p.next()
	var items []withItem
	for {
		ctx, err := p.expression()
		if err != nil {
			return nil, err
		}
		item := withItem{ctx: ctx}
		if p.isName("as") {
			p.next()
			if item.as, err = p.name(); err != nil {
				return nil, err
			}
		}
		items = append(items, item)
		if !p.isOp(",") {
			break
		}
		p.next()
	}
	body, err := p.block()
	if err != nil {
		return nil, err
	}
	return withStmt{items: items, body: body}, nil
}

// exprList reads one expression, or a bare tuple of them ("a, b").
func (p *parser) exprList() (expr, error) {
	first, err := p.expression()
	if err != nil {
		return nil, err
	}
	if !p.isOp(",") {
		return first, nil
	}
	elts := []expr{first}
	for p.isOp(",") {
		p.next()
		if p.endsList() {
			break
		}
		e, err := p.expression()
		if err != nil {
			return nil, err
		}
		elts = append(elts, e)
	}
	return seqExpr{elts: elts}, nil
}

func (p *parser) endsList() bool {
	t := p.peek()
	return t.kind == tokNewline || t.kind == tokEOF || t.kind == tokOp && (t.text == "=" || t.text == ";" || t.text == ")" || t.text == "]" || t.text == ":")
}

// expression reads the subset's expressions: or/and/not, comparisons, "+",
// and primaries with attribute and call trailers.
func (p *parser) expression() (expr, error) {
	if p.isName("lambda") {
		return nil, p.errorf("a lambda")
	}
	return p.boolOp("or")
}

func (p *parser) boolOp(op string) (expr, error) {
	sub := func() (expr, error) {
		if op == "or" {
			return p.boolOp("and")
		}
		return p.notOp()
	}
	l, err := sub()
	if err != nil {
		return nil, err
	}
	for p.isName(op) {
		p.next()
		r, err := sub()
		if err != nil {
			return nil, err
		}
		l = binExpr{op: op, l: l, r: r}
	}
	if p.isName("if") {
		return nil, p.errorf("a conditional expression")
	}
	return l, nil
}

func (p *parser) notOp() (expr, error) {
	if p.isName("not") {
		p.next()
		x, err := p.notOp()
		if err != nil {
			return nil, err
		}
		return notExpr{x: x}, nil
	}
	return p.comparison()
}

var comparisonOps = map[string]bool{"==": true, "!=": true, "<": true, "<=": true, ">": true, ">=": true}

func (p *parser) comparison() (expr, error) {
	l, err := p.sum()
	if err != nil {
		return nil, err
	}
	for {
		var op string
		switch {
		case p.peek().kind == tokOp && comparisonOps[p.peek().text]:
			op = p.next().text
		case p.isName("in"):
			p.next()
			op = "in"
		case p.isName("not") && p.toks[p.pos+1].kind == tokName && p.toks[p.pos+1].text == "in":
			p.next()
			p.next()
			op = "not in"
		case p.isName("is"):
			return nil, p.errorf("an is comparison")
		default:
			return l, nil
		}
		r, err := p.sum()
		if err != nil {
			return nil, err
		}
		l = binExpr{op: op, l: l, r: r}
	}
}

func (p *parser) sum() (expr, error) {
	l, err := p.primary()
	if err != nil {
		return nil, err
	}
	for p.isOp("+") || p.isOp("-") {
		op := p.next().text
		r, err := p.primary()
		if err != nil {
			return nil, err
		}
		l = binExpr{op: op, l: l, r: r}
	}
	return l, nil
}

func (p *parser) primary() (expr, error) {
	x, err := p.atom()
	if err != nil {
		return nil, err
	}
	for {
		switch {
		case p.isOp("."):
			p.next()
			name, err := p.name()
			if err != nil {
				return nil, err
			}
			x = attrExpr{x: x, name: name}
		case p.isOp("("):
			p.next()
			call := callExpr{fn: x}
			for !p.isOp(")") {
				if p.isOp("*") || p.isOp("**") {
					return nil, p.errorf("a starred argument")
				}
				t := p.peek()
				if t.kind == tokName && p.toks[p.pos+1].kind == tokOp && p.toks[p.pos+1].text == "=" {
					p.next()
					p.next()
					value, err := p.expression()
					if err != nil {
						return nil, err
					}
					call.kw = append(call.kw, keyword{name: t.text, value: value})
				} else {
					if len(call.kw) > 0 {
						return nil, p.errorf("a positional argument after a keyword")
					}
					arg, err := p.expression()
					if err != nil {
						return nil, err
					}
					if p.isName("for") {
						return nil, p.errorf("a generator expression")
					}
					call.args = append(call.args, arg)
				}
				if !p.isOp(",") {
					break
				}
				p.next()
			}
			if err := p.expectOp(")"); err != nil {
				return nil, err
			}
			x = call
		case p.isOp("["):
			p.next()
			sub := subscriptExpr{x: x}
			for {
				if p.isOp(":") || p.isOp("]") {
					sub.parts = append(sub.parts, nil)
				} else {
					part, err := p.expression()
					if err != nil {
						return nil, err
					}
					sub.parts = append(sub.parts, part)
				}
				if !p.isOp(":") {
					break
				}
				p.next()
			}
			if err := p.expectOp("]"); err != nil {
				return nil, err
			}
			x = sub
		default:
			return x, nil
		}
	}
}

func (p *parser) atom() (expr, error) {
	t := p.peek()
	switch t.kind {
	case tokString:
		p.next()
		s := strExpr{tok: t}
		// Adjacent literals are one string.
		for p.peek().kind == tokString {
			n := p.next()
			s.tok.fstring = s.tok.fstring || n.fstring
			s.tok.text += n.text
			s.tok.raw += " " + n.raw
		}
		return s, nil
	case tokNumber:
		p.next()
		return numExpr{text: t.text}, nil
	case tokName:
		if refusedKeywords[t.text] || t.text == "for" || t.text == "in" || t.text == "not" {
			return nil, p.errorf("a keyword where a value belongs")
		}
		p.next()
		return nameExpr{id: t.text}, nil
	case tokOp:
		switch t.text {
		case "(", "[":
			closer := map[string]string{"(": ")", "[": "]"}[t.text]
			p.next()
			var elts []expr
			trailingComma := false
			for !p.isOp(closer) {
				e, err := p.expression()
				if err != nil {
					return nil, err
				}
				if p.isName("for") {
					return nil, p.errorf("a comprehension")
				}
				elts = append(elts, e)
				trailingComma = false
				if !p.isOp(",") {
					break
				}
				p.next()
				trailingComma = true
			}
			if err := p.expectOp(closer); err != nil {
				return nil, err
			}
			if t.text == "(" && len(elts) == 1 && !trailingComma {
				return elts[0], nil
			}
			return seqExpr{elts: elts}, nil
		}
	}
	return nil, p.errorf("an expression the subset refuses")
}
