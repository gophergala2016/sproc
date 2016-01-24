// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package parse builds parse trees for templates as defined by text/template
// and html/template. Clients should use those packages to construct templates
// rather than this one, which provides shared internal data structures not
// intended for general use.
package sproc

import (
	"fmt"
	"runtime"
	"strconv"
	"strings"
)

// Tree is the representation of a single parsed template.
type Tree struct {
	Name      string          // name of the template represented by the tree.
	ParseName string          // name of the top-level template during parsing, for error messages.
	Root      *ActionNode     // top-level root of the tree.
	Defs      map[string]Node // top-level definitions
	text      string          // text parsed to create the template (or its parent)
	// Parsing only; cleared after parse.
	funcs     []map[string]interface{}
	lex       *lexer
	token     [3]item // three-token lookahead for parser.
	peekCount int
	vars      []string // variables defined at the moment.
	streams   []string
}

// Copy returns a copy of the Tree. Any parsing state is discarded.
func (t *Tree) Copy() *Tree {
	if t == nil {
		return nil
	}
	return &Tree{
		Name:      t.Name,
		ParseName: t.ParseName,
		Root:      t.Root.CopyAction(),
		text:      t.text,
		// FIXME: add defs
	}
}

// Parse returns a map from template name to parse.Tree, created by parsing the
// templates described in the argument string. The top-level template will be
// given the specified name. If an error is encountered, parsing stops and an
// empty map is returned with the error.
func Parse(name, text string, funcs ...map[string]interface{}) (tree *Tree, err error) {
	t := New(name)
	t.text = text
	return t.Parse(text, funcs...)
}

// next returns the next token.
func (t *Tree) next() item {
	if t.peekCount > 0 {
		t.peekCount--
	} else {
		t.token[0] = t.lex.nextItem()
	}
	return t.token[t.peekCount]
}

// backup backs the input stream up one token.
func (t *Tree) backup() {
	t.peekCount++
}

// backup2 backs the input stream up two tokens.
// The zeroth token is already there.
func (t *Tree) backup2(t1 item) {
	t.token[1] = t1
	t.peekCount = 2
}

// backup3 backs the input stream up three tokens
// The zeroth token is already there.
func (t *Tree) backup3(t2, t1 item) { // Reverse order: we're pushing back.
	t.token[1] = t1
	t.token[2] = t2
	t.peekCount = 3
}

// peek returns but does not consume the next token.
func (t *Tree) peek() item {
	if t.peekCount > 0 {
		return t.token[t.peekCount-1]
	}
	t.peekCount = 1
	t.token[0] = t.lex.nextItem()
	return t.token[0]
}

// nextNonSpace returns the next non-space token.
func (t *Tree) nextNonSpace() (token item) {
	for {
		token = t.next()
		if token.typ != itemSpace {
			break
		}
	}
	return token
}

// peekNonSpace returns but does not consume the next non-space token.
func (t *Tree) peekNonSpace() (token item) {
	for {
		token = t.next()
		if token.typ != itemSpace {
			break
		}
	}
	t.backup()
	return token
}

// Parsing.

// New allocates a new parse tree with the given name.
func New(name string, funcs ...map[string]interface{}) *Tree {
	return &Tree{
		Name:  name,
		Defs:  make(map[string]Node),
		funcs: funcs,
	}
}

// ErrorContext returns a textual representation of the location of the node in the input text.
// The receiver is only used when the node does not have a pointer to the tree inside,
// which can occur in old code.
func (t *Tree) ErrorContext(n Node) (location, context string) {
	pos := int(n.Position())
	tree := n.tree()
	if tree == nil {
		tree = t
	}
	text := tree.text[:pos]
	byteNum := strings.LastIndex(text, "\n")
	if byteNum == -1 {
		byteNum = pos // On first line.
	} else {
		byteNum++ // After the newline.
		byteNum = pos - byteNum
	}
	lineNum := 1 + strings.Count(text, "\n")
	context = n.String()
	if len(context) > 20 {
		context = fmt.Sprintf("%.20s...", context)
	}
	return fmt.Sprintf("%s:%d:%d", tree.ParseName, lineNum, byteNum), context
}

// errorf formats the error and terminates processing.
func (t *Tree) errorf(format string, args ...interface{}) {
	t.Root = nil
	format = fmt.Sprintf("script: %s:%d: %s", t.ParseName, t.lex.lineNumber(), format)
	panic(fmt.Errorf(format, args...))
}

// error terminates processing.
func (t *Tree) error(err error) {
	t.errorf("%s", err)
}

// expect consumes the next token and guarantees it has the required type.
func (t *Tree) expect(expected itemType, context string) item {
	token := t.nextNonSpace()
	if token.typ != expected {
		t.unexpected(token, context)
	}
	return token
}

// expectOneOf consumes the next token and guarantees it has one of the required types.
func (t *Tree) expectOneOf(expected1, expected2 itemType, context string) item {
	token := t.nextNonSpace()
	if token.typ != expected1 && token.typ != expected2 {
		t.unexpected(token, context)
	}
	return token
}

// unexpected complains about the token and terminates processing.
func (t *Tree) unexpected(token item, context string) {
	t.errorf("unexpected %s in %s", token, context)
}

var recoverbuf = make([]byte, 4*1024*1024)

// recover is the handler that turns panics into returns from the top level of Parse.
func (t *Tree) recover(errp *error) {
	e := recover()
	if e != nil {
		if _, ok := e.(runtime.Error); ok {
			n := runtime.Stack(recoverbuf, true)
			fmt.Println(recoverbuf[:n])
			panic(e)
		}
		if t != nil {
			t.lex.drain()
			t.stopParse()
		}
		*errp = e.(error)
	}
	return
}

// startParse initializes the parser, using the lexer.
func (t *Tree) startParse(funcs []map[string]interface{}, lex *lexer) {
	t.Root = nil
	t.lex = lex
	t.vars = []string{}
	t.streams = []string{"@"}
	t.funcs = funcs
}

// stopParse terminates parsing.
func (t *Tree) stopParse() {
	t.lex = nil
	t.vars = nil
	t.streams = nil
	t.funcs = nil
}

// Parse parses the template definition string to construct a representation of
// the template for execution. If either action delimiter string is empty, the
// default ("{{" or "}}") is used. Embedded template definitions are added to
// the treeSet map.
func (t *Tree) Parse(text string, funcs ...map[string]interface{}) (tree *Tree, err error) {
	defer t.recover(&err)
	t.ParseName = t.Name
	t.startParse(funcs, lex(t.Name, text))
	t.text = text
	t.parseTopLevel()
	t.stopParse()
	return t, nil
}

// add adds tree to the treeSet.
func (t *Tree) add(treeSet map[string]*Tree) {
	tree := treeSet[t.Name]
	if tree == nil || IsEmptyTree(tree.Root) {
		treeSet[t.Name] = t
		return
	}
	if !IsEmptyTree(t.Root) {
		t.errorf("template: multiple definition of template %q", t.Name)
	}
}

// IsEmptyTree reports whether this tree (node) is empty of everything but space.
func IsEmptyTree(n Node) bool {
	switch n := n.(type) {
	case nil:
		return true
	case *ActionNode:
	case *IfNode:
	case *ListNode:
		for _, node := range n.Nodes {
			if !IsEmptyTree(node) {
				return false
			}
		}
		return true
	//case *RangeNode:
	//case *TemplateNode:
	//case *TextNode:
	//return len(bytes.TrimSpace(n.Text)) == 0
	//case *WithNode:
	default:
		panic("unknown node: " + n.String())
	}
	return false
}

// parse is the top-level parser for a template, essentially the same
// as itemList except it also parses {{define}} actions.
// It runs to EOF.
func (t *Tree) parseTopLevel() {
	t.Root = t.action()
}

func (t *Tree) parseDefinition() (string, *FuncNode) {
	println("::definition")
	const context = "definition"

	switch typ := t.next().typ; typ {
	case itemFunc:
		// parse func
		proto := t.parseProto()
		block := t.parseBlock()
		id := "func " + proto.Name
		return id, t.newFunc(t.peek().pos, proto, block)
	default:
		t.unexpected(t.peek(), context)
	}

	return "", nil
}

// proto:
//  name ('(' arg type ')')* ':>' ret '{' body '}'
func (t *Tree) parseProto() *ProtoNode {
	const context = "proto"

	name := t.expect(itemIdentifier, context)
	vars := []*ParameterNode{}

	for {
		switch typ := t.next().typ; typ {
		case itemLeftParen:
			// parse param
			parName := t.expect(itemIdentifier, context)
			parType := t.expect(itemIdentifier, context)
			t.expect(itemRightParen, context)
			vars = append(vars, t.newParameter(t.peekNonSpace().pos, parName.val, parType.val))
		case itemColonRAB:
			retVal := t.expect(itemIdentifier, context)
			return t.newProto(t.peekNonSpace().pos, name.val, vars, retVal.val)
		case itemSpace:
			continue
		default:
			t.unexpected(t.peek(), context)
		}
	}
	return nil
}

// block:
// '{' list '}'
// has variables
func (t *Tree) parseBlock() *BlockNode {
	const context = "block"
	t.expect(itemLeftBracket, context)
	list := t.action()
	t.expect(itemRightBracket, context)
	return t.newBlock(t.peek().pos, list, nil)
}

// parseDefinition parses a {{define}} ...  {{end}} template definition and
// installs the definition in the treeSet map.  The "define" keyword has already
// been scanned.
/*
func (t *Tree) parseDefinition(treeSet map[string]*Tree) {
	const context = "define clause"
	name := t.expectOneOf(itemString, itemRawString, context)
	var err error
	t.Name, err = strconv.Unquote(name.val)
	if err != nil {
		t.error(err)
	}
	t.expect(itemRightDelim, context)
	var end Node
	t.Root, end = t.itemList()
	if end.Type() != nodeEnd {
		t.errorf("unexpected %s in %s", end, context)
	}
	t.add(treeSet)
	t.stopParse()
}
*/

// itemList:
//	textOrAction*
// Terminates at {{end}} or {{else}}, returned separately.
func (t *Tree) itemList() (list *ListNode, next Node) {
	list = t.newList(t.peekNonSpace().pos)
	for t.peekNonSpace().typ != itemEOF {
		n := t.textOrAction()
		switch n.Type() {
		case nodeElse:
			return list, n
		}
		list.append(n)
	}
	t.errorf("unexpected EOF")
	return
}

// textOrAction:
//	text | action
// FIXME: we does not have text
func (t *Tree) textOrAction() Node {
	switch token := t.nextNonSpace(); token.typ {
	default:
		t.unexpected(token, "input")
	}
	return nil
}

// Action:
//	pipeline (";" pipeline)*
// Left delim is past. Now get actions.
// First word could be a keyword such as range.
func (t *Tree) action() (n *ActionNode) {
	pipes := []*PipeNode{}
	for {
		if t.peekNonSpace().typ == itemEOF {
			goto END
		}

		if t.peekNonSpace().typ == itemEndline {
			// skip empty lines
			t.next()
			continue
		}

		switch t.peekNonSpace().typ {
		case itemFunc:
			// TODO: disallow function definition inside other bodies
			// define a func
			println("::action: new definition, token:", t.peek().typ.String(), "val:", t.peek().val)
			id, funct := t.parseDefinition()
			t.Defs[id] = funct
		case itemIdentifier:
			// parse a pipeline
			println("::action: new pipeline, token:", t.peek().typ.String(), "val:", t.peek().val)
			pipe := t.pipeline("command")
			pipes = append(pipes, pipe)
		default:
			println("::action: wtf token:", t.peek().typ.String(), "val:", t.peek().val)
		}

		for {
			switch token := t.next().typ; {
			case token == itemRightParen, token == itemRightBracket:
				// we completed our nested action, return
				t.backup()
				goto END
			case token == itemSemicolon, token == itemEndline:
				// proceed and parse new pipe
				continue
			case token == itemEOF:
				// at end of file, return
				t.backup()
				goto END
			default:
				t.backup()
			}
			break
		}
		/*
			switch token := t.nextNonSpace(); token.typ {
			case itemIf:
				return t.ifControl()
			case itemElse:
				return t.elseControl()
				//case itemEnd:
				//	return t.endControl()
				//case itemRange:
				//	return t.rangeControl()
				//case itemTemplate:
				//return t.templateControl()
				//case itemWith:
				//	return t.withControl()
			}
			t.backup()
		*/
	}

END:

	return t.newAction(t.peek().pos, t.lex.lineNumber(), pipes)
	// Do not pop variables; they persist until "end".
	//return t.newAction(t.peek().pos, t.lex.lineNumber(), t.pipeline("command"))
}

// Pipeline:
//	command ('|' command)*
func (t *Tree) pipeline(context string) (pipe *PipeNode) {
	pos := t.peekNonSpace().pos
	pipe = t.newPipeline(pos, t.lex.lineNumber())

	println("::pipeline: new at", pos)

	for {
		switch token := t.nextNonSpace(); token.typ {
		case itemRightBracket, itemRightParen, itemSemicolon, itemEndline:
			// At this point, the pipeline is complete
			t.checkPipeline(pipe, context)
			//if token.typ == itemRightParen || token.typ == itemRightBracket {
			t.backup()
			//}
			return
			/*
				case itemBool, itemIdentifier,
					itemNumber, itemString, itemVariable, itemLeftParen:
					t.backup()
					pipe.append(t.command())
			*/

		// TODO: add possibility to start command with parent
		// effectively making dynamic dispatch
		case itemIdentifier:
			t.backup()
			pipe.append(t.command())
		default:
			t.unexpected(token, context)
		}
	}
}

func (t *Tree) checkPipeline(pipe *PipeNode, context string) {
	// Reject empty pipelines
	if len(pipe.Cmds) == 0 {
		t.errorf("missing value for %s", context)
	}
	// Only the first command of a pipeline can start with a non executable operand
	for i, c := range pipe.Cmds[1:] {
		switch c.Args[0].Type() {
		case NodeBool, NodeNumber, NodeString:
			// With A|B|C, pipeline stage 2 is B
			t.errorf("non executable command in pipeline stage %d", i+2)
		}
	}
}

func (t *Tree) parseControl(allowElseIf bool, context string) (pos Pos, line int, pipe *PipeNode, list, elseList *ListNode) {
	defer t.popVars(len(t.vars))
	line = t.lex.lineNumber()
	pipe = t.pipeline(context)
	var next Node
	list, next = t.itemList()
	switch next.Type() {
	case nodeElse:
		if allowElseIf {
			// Special case for "else if". If the "else" is followed immediately by an "if",
			// the elseControl will have left the "if" token pending. Treat
			//	{{if a}}_{{else if b}}_{{end}}
			// as
			//	{{if a}}_{{else}}{{if b}}_{{end}}{{end}}.
			// To do this, parse the if as usual and stop at it {{end}}; the subsequent{{end}}
			// is assumed. This technique works even for long if-else-if chains.
			// TODO: Should we allow else-if in with and range?
			if t.peek().typ == itemIf {
				t.next() // Consume the "if" token.
				elseList = t.newList(next.Position())
				elseList.append(t.ifControl())
				// Do not consume the next item - only one {{end}} required.
				break
			}
		}
		elseList, next = t.itemList()
		t.errorf("expected end; found %s", next)
	}
	return pipe.Position(), line, pipe, list, elseList
}

// If:
//	{{if pipeline}} itemList {{end}}
//	{{if pipeline}} itemList {{else}} itemList {{end}}
// If keyword is past.
func (t *Tree) ifControl() Node {
	return t.newIf(t.parseControl(true, "if"))
}

// Range:
//	{{range pipeline}} itemList {{end}}
//	{{range pipeline}} itemList {{else}} itemList {{end}}
// Range keyword is past.
/*
func (t *Tree) rangeControl() Node {
	return t.newRange(t.parseControl(false, "range"))
}

// With:
//	{{with pipeline}} itemList {{end}}
//	{{with pipeline}} itemList {{else}} itemList {{end}}
// If keyword is past.
func (t *Tree) withControl() Node {
	return t.newWith(t.parseControl(false, "with"))
}

// End:
//	{{end}}
// End keyword is past.
func (t *Tree) endControl() Node {
	return t.newEnd(t.expect(itemRightDelim, "end").pos)
}

*/
// Else:
//	{{else}}
// Else keyword is past.
func (t *Tree) elseControl() Node {
	// Special case for "else if".
	peek := t.peekNonSpace()
	if peek.typ == itemIf {
		// We see "{{else if ... " but in effect rewrite it to {{else}}{{if ... ".
		return t.newElse(peek.pos, t.lex.lineNumber())
	}
	return t.newElse(t.expect(itemRightBracket, "else").pos, t.lex.lineNumber())
}

// Template:
//	{{template stringValue pipeline}}
// Template keyword is past.  The name must be something that can evaluate
// to a string.
/*
func (t *Tree) templateControl() Node {
	var name string
	token := t.nextNonSpace()
	switch token.typ {
	case itemString, itemRawString:
		s, err := strconv.Unquote(token.val)
		if err != nil {
			t.error(err)
		}
		name = s
	default:
		t.unexpected(token, "template invocation")
	}
	var pipe *PipeNode
	if t.nextNonSpace().typ != itemRightDelim {
		t.backup()
		// Do not pop variables; they persist until "end".
		pipe = t.pipeline("template")
	}
	return t.newTemplate(token.pos, t.lex.lineNumber(), name, pipe)
}
*/

// command:
//	operand (space operand)*
// space-separated arguments up to a pipeline character.
// we consume the pipe character but leave the right delim to terminate the action.
func (t *Tree) command() *CommandNode {
	cmd := t.newCommand(t.peekNonSpace().pos)
	for {
		t.peekNonSpace() // skip leading spaces.
		operand := t.operand()
		if operand != nil {
			cmd.append(operand)
		}
		println("::command: operand", operand)
		switch token := t.next(); token.typ {
		case itemSpace:
			continue
		case itemError:
			t.errorf("%s", token.val)
		case itemRightBracket, itemRightParen, itemEndline, itemSemicolon:
			t.backup()
		case itemPipe:
		default:
			t.errorf("unexpected %s in operand", token)
		}
		break
	}
	if len(cmd.Args) == 0 {
		t.errorf("empty command")
	}
	return cmd
}

// operand:
//	term .Field*
// An operand is a space-separated component of a command,
// a term possibly followed by field accesses.
// A nil return means the next item is not an operand.
func (t *Tree) operand() Node {
	node := t.term()
	if node == nil {
		return nil
	}
	// FIXME: complete wtf
	if t.peek().typ == itemField {
		chain := t.newChain(t.peek().pos, node)
		for t.peek().typ == itemField {
			chain.Add(t.next().val)
		}
		// Compatibility with original API: If the term is of type NodeField
		// or NodeVariable, just put more fields on the original.
		// Otherwise, keep the Chain node.
		// Obvious parsing errors involving literal values are detected here.
		// More complex error cases will have to be handled at execution time.
		switch node.Type() {
		case NodeField:
			node = t.newField(chain.Position(), chain.String())
		case NodeVariable:
			node = t.newVariable(chain.Position(), chain.String())
		case NodeStream:
			node = t.newStream(chain.Position(), chain.String())
		case NodeBool, NodeString, NodeNumber:
			t.errorf("unexpected . after term %q", node.String())
		default:
			node = chain
		}
	}
	return node
}

// term:
//	literal (number, string, nil, boolean)
//	function (identifier)
//	.
//	.Field
//	$
//	'(' pipeline ')'
// A term is a simple "expression".
// A nil return means the next item is not a term.
func (t *Tree) term() Node {
	switch token := t.nextNonSpace(); token.typ {
	case itemError:
		t.errorf("%s", token.val)
	case itemIdentifier: // XXX
		/*
			if !t.hasFunction(token.val) {
				t.errorf("function %q not defined", token.val)
			}
		*/
		return NewIdentifier(token.val).SetTree(t).SetPos(token.pos)
		/*
			case itemDot:
				return t.newDot(token.pos)
			case itemNil:
				return t.newNil(token.pos)
		*/
	case itemVariable:
		return t.useVar(token.pos, token.val) // XXX
	case itemStream:
		return t.useStream(token.pos, token.val)
	case itemField:
		return t.newField(token.pos, token.val)
	case itemBool:
		return t.newBool(token.pos, token.val == "true")
	case itemNumber:
		number, err := t.newNumber(token.pos, token.val, token.typ)
		if err != nil {
			t.error(err)
		}
		return number
	case itemLeftParen:
		action := t.action()
		if token := t.next(); token.typ != itemRightParen {
			t.errorf("unclosed right paren: unexpected %s", token)
		}
		return action
	case itemLeftBracket:
		t.backup()
		block := t.parseBlock()
		return block
	case itemString:
		s, err := strconv.Unquote(token.val)
		if err != nil {
			t.error(err)
		}
		return t.newString(token.pos, token.val, s)
	}
	t.backup()
	return nil
}

// hasFunction reports if a function name exists in the Tree's maps.
func (t *Tree) hasFunction(name string) bool {
	for _, funcMap := range t.funcs {
		if funcMap == nil {
			continue
		}
		if funcMap[name] != nil {
			return true
		}
	}
	return false
}

// popVars trims the variable list to the specified length
func (t *Tree) popVars(n int) {
	t.vars = t.vars[:n]
}

// useVar returns a node for a variable reference. It errors if the
// variable is not defined.
func (t *Tree) useVar(pos Pos, name string) Node {
	v := t.newVariable(pos, name)
	for _, varName := range t.vars {
		if varName == v.Ident[0] {
			return v
		}
	}
	t.errorf("undefined variable %q", v.Ident[0])
	return nil
}

func (t *Tree) useStream(pos Pos, name string) Node {
	v := t.newStream(pos, name)
	for _, varName := range t.streams {
		if varName == v.Ident[0] {
			return v
		}
	}
	t.errorf("undefined stream %q", v.Ident[0])
	return nil
}
