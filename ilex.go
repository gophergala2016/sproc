package sproc

type Lexface struct {
	lexer *lexer
}

func NewLexface(input string) *Lexface {
	lexer := lex("sproc-lexer", input)
	return &Lexface{lexer}
}

func (lf *Lexface) Next() (string, string, bool) {
	item, ok := <-lf.lexer.items
	if !ok {
		return "", "", false
	}

	return item.String(), item.typ.String(), true
}
