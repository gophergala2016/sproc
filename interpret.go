package sproc

import (
	"fmt"
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
	intr.loadBuiltins()
	b := intr.prepare(tree)
	intr.openStdStreams()
	b.UseStreams(intr.GetStream("stdin").Out(), intr.GetStream("stdout").In())

	fmt.Printf(":: about to execute block %#v\n", b)

	return executeBlock(intr, b)
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

	block := buildBlock(intr, tree.Root.Pipes, nil)
	return block
}

func (intr *Interpretator) LookupExec(name string) *Function {
	return intr.funcs[name]
}

func (intr *Interpretator) AddNativeFunc(nfn NativeFunc, name string, params []*Parameter) {
	fn := newFunction()
	fn.isNative = true
	fn.native = nfn
	fn.params = params
	fn.name = name

	intr.funcs[name] = fn
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

func (intr *Interpretator) loadBuiltins() {
	intr.AddNativeFunc(NativeCat, "cat", nil)
}

func (intr *Interpretator) openStdStreams() {
	stdin := NewStream()
	go ReadLinesToStream(os.Stdin, stdin.In())
	intr.AddStream("stdin", stdin)

	stdout := NewStream()
	go WriteLinesFromStream(os.Stdout, stdout.Out())
	intr.AddStream("stdout", stdout)
}

func executeBlock(intr *Interpretator, block *Block) error {
	for _, pipe := range block.pipes {
		// TODO: waitgroup?
		pipe.InheritIO(&block.IOBase)
		go executePipe(intr, pipe)
	}

	return nil
}

func executePipe(intr *Interpretator, pipe *Pipeline) {
	if len(pipe.cmds) == 1 {
		cmd := pipe.cmds[0]
		cmd.InheritIO(&pipe.IOBase)
		go executeCommand(intr, cmd, pipe.vars)
		return
	}

	var nextInput *StreamSlot = pipe.stdin
	var currentOutput *StreamSlot
	var currentInput *StreamSlot

	for idx, cmd := range pipe.cmds {
		currentInput = nextInput

		if idx == (len(pipe.cmds) - 1) {
			// last entry writes to pipe out
			currentOutput = pipe.stdout
		} else {
			pipeStream := NewStream()
			currentOutput = pipeStream.In()
			nextInput = pipeStream.Out()
		}

		cmd.UseStreams(currentInput, currentOutput)
		go executeCommand(intr, cmd, pipe.vars)
	}
}

func executeCommand(intr *Interpretator, cmd *Command, vars *VarList) {
	// first, evaluate arguments. It can take a while (and some input)
	cmd.evaluateArgs(vars)
	// then invoke a function
	cmd.dfunc.Invoke(intr, &cmd.IOBase, cmd.vals)
}
