package sproc

import (
	"os"
)

type Interpretator struct {
	funcs map[string]*Function
	root  *Block

	namedStreams map[string]*Stream
}

func NewInterpretator() *Interpretator {
	return &Interpretator{
		funcs:        make(map[string]*Function),
		namedStreams: make(map[string]*Stream),
	}
}

func (intr *Interpretator) Run(tree *Tree) error {
	b := intr.prepare(tree)
	intr.openStdStreams()
	return intr.executeBlock(b)
}

func (intr *Interpretator) prepare(tree *Tree) *Block {
	for _, src := range tree.Defs {
		fnnode := src.(*FuncNode)
		fn := newFunction()
		fn.buildProto(fnnode.Proto)
		intr.funcs[fnnode.Proto.Name] = fn
	}

	for _, src := range tree.Defs {
		fnnode := src.(*FuncNode)
		name := fnnode.Proto.Name
		buildFunction(intr, name, fnnode)
	}

	return nil
}

func (intr *Interpretator) LookupExec(name string) *Function {
	return intr.funcs[name]
}

func (intr *Interpretator) AddStream(name string, stream *Stream) {
	// FIXME: add overwrite protection
	intr.namedStreams[name] = stream
}

func (intr *Interpretator) GetStream(name string) *Stream {
	if stream, ok := intr.namedStreams[name]; ok {
		return stream
	}

	stream := NewStream()
	intr.AddStream(name, stream)
	return stream
}

func (intr *Interpretator) openStdStreams() {
	stdin := NewStream()
	go ReadLinesToStream(os.Stdin, stdin.In())
	intr.AddStream("stdin", stdin)

	stdout := NewStream()
	go WriteLinesFromStream(os.Stdout, stdout.Out())
	intr.AddStream("stdout", stdout)
}

func (intr *Interpretator) executeBlock(b *Block) error {
	return nil
}
