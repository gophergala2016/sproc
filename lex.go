package sproc

// borrowed from stdlib text/template

// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// item represents a token or text string returned from the scanner.
type item struct {
	typ itemType // The type of this item.
	pos Pos      // The starting position, in bytes, of this item in the input string.
	val string   // The value of this item.
}

func (i item) String() string {
	switch {
	case i.typ == itemEOF:
		return "EOF"
	case i.typ == itemError:
		return i.val
	case i.typ > itemKeyword:
		return fmt.Sprintf("<%s>", i.val)
	case len(i.val) > 10:
		return fmt.Sprintf("%.10q...", i.val)
	}
	return fmt.Sprintf("%q", i.val)
}

// itemType identifies the type of lex items.
type itemType int

const (
	itemError        itemType = iota // error occurred; value is text of error
	itemBool                         // boolean constant
	itemChar                         // printable ASCII character; grab bag for comma etc.
	itemCharConstant                 // character constant
	//itemComplex                      // complex constant (1+2i); imaginary is just a number
	//itemColonEquals                  // colon-equals (':=') introducing a declaration
	itemColonRAB // daw (colon-right-angle-bracket) ':>' for return
	itemSemicolon
	itemEndline
	itemEOF
	itemField      // alphanumeric identifier starting with '.'
	itemIdentifier // alphanumeric identifier not starting with '.'
	//itemLeftDelim  // left action delimiter
	itemLeftParen    // '(' inside action
	itemRightParen   // ')' inside action
	itemLeftBracket  // '{'
	itemRightBracket // '}'
	itemNumber       // simple number, including imaginary
	itemPipe         // pipe symbol
	//itemRawString // raw quoted string (includes quotes)
	//itemRightDelim // right action delimiter
	itemSpace  // run of spaces separating arguments
	itemString // quoted string (includes quotes)
	//itemText       // plain text
	itemVariable // variable starting with '$', such as '$' or  '$1' or '$hello'
	itemStream   // name starting with '@'
	// Keywords appear after all the rest.

	itemKeyword // used only to delimit the keywords

	//itemDot      // the cursor, spelled '.'
	//itemDefine   // define keyword
	itemElse // else keyword
	//itemEnd      // end keyword
	itemIf    // if keyword
	itemNil   // the untyped nil constant, easiest to treat as a keyword
	itemRange // range keyword
	//itemTemplate // template keyword
	itemWith // with keyword

	itemDefinition // used to delimit definitions

	itemFunc // func keyword
)

var key = map[string]itemType{
	//".":        itemDot,
	//"define":   itemDefine,
	//"template": itemTemplate,
	"func": itemFunc,
	"else": itemElse,
	//"end":   itemEnd,
	"if":    itemIf,
	"range": itemRange,
	"nil":   itemNil,
	"with":  itemWith,
}

const eof = -1

// stateFn represents the state of the scanner as a function that returns the next state.
type stateFn func(*lexer) stateFn

// lexer holds the state of the scanner.
type lexer struct {
	name  string // the name of the input; used only for error reports
	input string // the string being scanned
	//leftDelim  string    // start of action
	//rightDelim string    // end of action
	state        stateFn   // the next lexing function to enter
	pos          Pos       // current position in the input
	start        Pos       // start position of this item
	width        Pos       // width of last rune read from input
	lastPos      Pos       // position of most recent item returned by nextItem
	items        chan item // channel of scanned items
	parenDepth   int       // nesting depth of ( ) exprs
	bracketDepth int       // nesting depth of { } exprs // TODO: pairing with ( )
}

// next returns the next rune in the input.
func (l *lexer) next() rune {
	if int(l.pos) >= len(l.input) {
		l.width = 0
		return eof
	}
	r, w := utf8.DecodeRuneInString(l.input[l.pos:])
	l.width = Pos(w)
	l.pos += l.width
	return r
}

// peek returns but does not consume the next rune in the input.
func (l *lexer) peek() rune {
	r := l.next()
	l.backup()
	return r
}

// backup steps back one rune. Can only be called once per call of next.
func (l *lexer) backup() {
	l.pos -= l.width
}

// emit passes an item back to the client.
func (l *lexer) emit(t itemType) {
	l.items <- item{t, l.start, l.input[l.start:l.pos]}
	l.start = l.pos
}

// ignore skips over the pending input before this point.
func (l *lexer) ignore() {
	l.start = l.pos
}

// accept consumes the next rune if it's from the valid set.
func (l *lexer) accept(valid string) bool {
	if strings.IndexRune(valid, l.next()) >= 0 {
		return true
	}
	l.backup()
	return false
}

// acceptRun consumes a run of runes from the valid set.
func (l *lexer) acceptRun(valid string) {
	for strings.IndexRune(valid, l.next()) >= 0 {
	}
	l.backup()
}

// lineNumber reports which line we're on, based on the position of
// the previous item returned by nextItem. Doing it this way
// means we don't have to worry about peek double counting.
func (l *lexer) lineNumber() int {
	return 1 + strings.Count(l.input[:l.lastPos], "\n")
}

// errorf returns an error token and terminates the scan by passing
// back a nil pointer that will be the next state, terminating l.nextItem.
func (l *lexer) errorf(format string, args ...interface{}) stateFn {
	l.items <- item{itemError, l.start, fmt.Sprintf(format, args...)}
	return nil
}

// nextItem returns the next item from the input.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) nextItem() item {
	item := <-l.items
	l.lastPos = item.pos
	return item
}

// drain drains the output so the lexing goroutine will exit.
// Called by the parser, not in the lexing goroutine.
func (l *lexer) drain() {
	for range l.items {
	}
}

// lex creates a new scanner for the input string.
func lex(name, input string) *lexer {
	/*
		if left == "" {
			left = leftDelim
		}
		if right == "" {
			right = rightDelim
		}
	*/
	l := &lexer{
		name:  name,
		input: input,
		//	leftDelim:  left,
		//	rightDelim: right,
		items: make(chan item),
	}
	go l.run()
	return l
}

// run runs the state machine for the lexer.
func (l *lexer) run() {
	for l.state = lexTopLevel; l.state != nil; {
		l.state = l.state(l)
	}
	close(l.items)
}

// state functions

const (
//leftDelim    = "{{"
//rightDelim   = "}}"
//leftComment  = "/*"
//rightComment = "*/"
)

// =============================== FIXME lexText must be replaced and eof handling should be done
// lexText scans until an opening action delimiter, "{{".
/*
func lexText(l *lexer) stateFn {
	for {
		if strings.HasPrefix(l.input[l.pos:], l.leftDelim) {
			if l.pos > l.start {
				l.emit(itemText)
			}
			return lexLeftDelim
		}
		if l.next() == eof {
			break
		}
	}
	// Correctly reached EOF.
	if l.pos > l.start {
		l.emit(itemText)
	}
	l.emit(itemEOF)
	return nil
}
*/

// FIXME: this must be rewired
// lexLeftDelim scans the left delimiter, which is known to be present.
/*
func lexLeftDelim(l *lexer) stateFn {
	l.pos += Pos(len(l.leftDelim))
	if strings.HasPrefix(l.input[l.pos:], leftComment) {
		return lexComment
	}
	l.emit(itemLeftDelim)
	l.parenDepth = 0
	return lexInsideAction
}
*/

// lexComment scans a comment. The left comment marker is known to be present.
/*
func lexComment(l *lexer) stateFn {
	l.pos += Pos(len(leftComment))
	i := strings.Index(l.input[l.pos:], rightComment)
	if i < 0 {
		return l.errorf("unclosed comment")
	}
	l.pos += Pos(i + len(rightComment))
	l.ignore()
	return lexText
}
*/

// lexRightDelim scans the right delimiter, which is known to be present.
/*
func lexRightDelim(l *lexer) stateFn {
	l.pos += Pos(len(l.rightDelim))
	l.emit(itemRightDelim)
	return lexText
}
*/

// lexTopLevel scans the top level elements
func lexTopLevel(l *lexer) stateFn {
	// Either number, quoted string, or identifier.
	// Spaces separate arguments; runs of spaces turn into itemSpace.
	// Pipe symbols separate and are emitted.
	switch r := l.next(); {
	case r == eof || isEndOfLine(r):
		// FIXME: handle end of line
		if l.parenDepth > 0 || l.bracketDepth > 0 {
			return l.errorf("unclosed action")
		}

		l.emit(itemEndline)
		if r == eof {
			// stop the lexer
			l.emit(itemEOF)
			return nil
		}
	case isSpace(r):
		return lexSpace
	case r == ':':
		if l.next() != '>' {
			return l.errorf("expected :>")
		}
		l.emit(itemColonRAB)
	case r == '|':
		l.emit(itemPipe)
	case r == ';':
		l.emit(itemSemicolon)
	case r == '"':
		return lexQuote
		/*
			case r == '`':
				return lexRawQuote
		*/

	case r == '$':
		return lexVariable
	case r == '@':
		return lexStream

	case r == '#':
		return lexComment

		/*
			case r == '\'':
				return lexChar
		*/

		// FIXME: can be useful as example
		/*
			case r == '.':
				// special look-ahead for ".field" so we don't break l.backup().
				if l.pos < Pos(len(l.input)) {
					r := l.input[l.pos]
					if r < '0' || '9' < r {
						return lexField
					}
				}
				fallthrough // '.' can start a number.
		*/

	case r == '+' || r == '-' || ('0' <= r && r <= '9'):
		l.backup()
		return lexNumber
	case isAlphaNumeric(r):
		l.backup()
		return lexIdentifier
	case r == '(':
		l.emit(itemLeftParen)
		l.parenDepth++
	case r == ')':
		l.emit(itemRightParen)
		l.parenDepth--
		if l.parenDepth < 0 {
			return l.errorf("unexpected right paren %#U", r)
		}
	case r == '{':
		l.emit(itemLeftBracket)
		l.bracketDepth++
	case r == '}':
		l.emit(itemRightBracket)
		l.bracketDepth--
		if l.bracketDepth < 0 {
			return l.errorf("unexpected right bracket %#U", r)
		}
	case r <= unicode.MaxASCII && unicode.IsPrint(r):
		l.emit(itemChar)
		return lexTopLevel
	default:
		return l.errorf("unrecognized character in action: %#U", r)
	}
	return lexTopLevel
}

// lexSpace scans a run of space characters.
// One space has already been seen.
func lexSpace(l *lexer) stateFn {
	for isSpace(l.peek()) {
		l.next()
	}
	l.emit(itemSpace)
	return lexTopLevel
}

func lexComment(l *lexer) stateFn {
	for isEndOfLine(l.peek()) {
		l.next()
	}
	l.ignore()
	return lexTopLevel
}

// lexIdentifier scans an alphanumeric.
func lexIdentifier(l *lexer) stateFn {
Loop:
	for {
		switch r := l.next(); {
		case isAlphaNumeric(r):
			// absorb.
		default:
			l.backup()
			word := l.input[l.start:l.pos]
			if !l.atTerminator() {
				return l.errorf("bad character %#U", r)
			}
			switch {
			case key[word] > itemKeyword:
				l.emit(key[word])
				/*
					case word[0] == '.':
						l.emit(itemField)
				*/
			case word == "true", word == "false":
				l.emit(itemBool)
			default:
				l.emit(itemIdentifier)
			}
			break Loop
		}
	}
	return lexTopLevel
}

// lexField scans a field: .Alphanumeric.
// The . has been scanned.
/*
func lexField(l *lexer) stateFn {
	return lexFieldOrVariable(l, itemField)
}
*/

// lexVariable scans a Variable: $Alphanumeric.
// The $ has been scanned.
func lexVariable(l *lexer) stateFn {
	return lexStreamOrVariable(l, itemVariable)
}

// lexVariable scans a Stream: @Alphanumeric.
// The @ has been scanned.
func lexStream(l *lexer) stateFn {
	return lexStreamOrVariable(l, itemStream)
}

// lexVariable scans a field or variable: [@$]Alphanumeric.
// The @ or $ has been scanned.
func lexStreamOrVariable(l *lexer, typ itemType) stateFn {
	if l.atTerminator() { // Nothing interesting follows -> "@" or "$".
		if typ == itemVariable {
			// FIXME: error?
			l.emit(itemVariable)
		} else {
			l.emit(itemStream)
		}
		return lexTopLevel
	}

	var r rune
	for {
		r = l.next()
		if !isAlphaNumeric(r) {
			l.backup()
			break
		}
	}

	if !l.atTerminator() {
		return l.errorf("bad character %#U", r)
	}

	l.emit(typ)
	return lexTopLevel
}

// atTerminator reports whether the input is at valid termination character to
// appear after an identifier. Breaks .X.Y into two pieces. Also catches cases
// like "$x+2" not being acceptable without a space, in case we decide one
// day to implement arithmetic.
func (l *lexer) atTerminator() bool {
	r := l.peek()
	if isSpace(r) || isEndOfLine(r) {
		return true
	}

	switch r {
	case eof, '.', ',', '|', ':', ')', '(', '}', '{', ';':
		return true
	}

	return false
}

// lexChar scans a character constant. The initial quote is already
// scanned. Syntax checking is done by the parser.
/*
func lexChar(l *lexer) stateFn {
Loop:
	for {
		switch l.next() {
		case '\\':
			if r := l.next(); r != eof && r != '\n' {
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("unterminated character constant")
		case '\'':
			break Loop
		}
	}
	l.emit(itemCharConstant)
	return lexInsideAction
}
*/

// lexNumber scans a number: decimal, octal, hex, float, or imaginary. This
// isn't a perfect number scanner - for instance it accepts "." and "0x0.2"
// and "089" - but when it's wrong the input is invalid and the parser (via
// strconv) will notice.
func lexNumber(l *lexer) stateFn {
	if !l.scanNumber() {
		return l.errorf("bad number syntax: %q", l.input[l.start:l.pos])
	}
	l.emit(itemNumber)
	return lexTopLevel
}

func (l *lexer) scanNumber() bool {
	// Optional leading sign.
	l.accept("+-")
	// Is it hex?
	digits := "0123456789"
	if l.accept("0") && l.accept("xX") {
		digits = "0123456789abcdefABCDEF"
	}
	l.acceptRun(digits)
	if l.accept(".") {
		l.acceptRun(digits)
	}
	if l.accept("eE") {
		l.accept("+-")
		l.acceptRun("0123456789")
	}
	// Next thing mustn't be alphanumeric.
	if isAlphaNumeric(l.peek()) {
		l.next()
		return false
	}
	return true
}

// lexQuote scans a quoted string.
func lexQuote(l *lexer) stateFn {
Loop:
	for {
		switch l.next() {
		case '\\':
			if r := l.next(); r != eof && r != '\n' {
				break
			}
			fallthrough
		case eof, '\n':
			return l.errorf("unterminated quoted string")
		case '"':
			break Loop
		}
	}
	l.emit(itemString)
	return lexTopLevel
}

// lexRawQuote scans a raw quoted string.
/*
func lexRawQuote(l *lexer) stateFn {
Loop:
	for {
		switch l.next() {
		case eof:
			return l.errorf("unterminated raw quoted string")
		case '`':
			break Loop
		}
	}
	l.emit(itemRawString)
	return lexInsideAction
}
*/

// isSpace reports whether r is a space character.
func isSpace(r rune) bool {
	return r == ' ' || r == '\t'
}

// isEndOfLine reports whether r is an end-of-line character.
func isEndOfLine(r rune) bool {
	return r == '\r' || r == '\n'
}

// isAlphaNumeric reports whether r is an alphabetic, digit, or underscore.
func isAlphaNumeric(r rune) bool {
	return r == '_' || unicode.IsLetter(r) || unicode.IsDigit(r)
}
