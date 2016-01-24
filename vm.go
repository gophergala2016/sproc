package sproc

type Type int

const (
	TypeInvalid Type = iota
	TypeInt
	TypeFloat
	TypeString
	TypeStream
	TypePipeline
	TypeProc
	TypeRef
)

type Block struct {
	pipes []*Pipeline
}

func buildBlock(intr *Interpretator, pipenodes []*PipeNode, vars *VarList) *Block {
	pipes := []*Pipeline{}

	for _, p := range pipenodes {
		pipe := buildPipeline(intr, p, vars)
		pipes = append(pipes, pipe)
	}

	return &Block{pipes}
}

type Pipeline struct {
	cmds []*Command
	vars *VarList
}

func buildPipeline(intr *Interpretator, pnode *PipeNode, vars *VarList) *Pipeline {
	cmds := []*Command{}
	for _, cmdnode := range pnode.Cmds {
		cmd := buildCommand(intr, cmdnode, vars)
		cmds = append(cmds, cmd)
	}

	return &Pipeline{cmds, vars}
}

type Command struct {
	dfunc *Function
	vals  []*Value

	// TODO: stdin/stdout streams
}

func buildCommand(intr *Interpretator, cnode *CommandNode, vars *VarList) *Command {
	execNode := cnode.Args[0].(*IdentifierNode)
	fn := intr.LookupExec(execNode.Ident)
	if fn == nil {
		panic("nil fn in cmd")
	}

	if len(fn.params) != len(cnode.Args[1:]) {
		panic("arg count mismatch")
	}

	// prepare args
	vals := []*Value{}

	for _, carg := range cnode.Args[1:] {
		switch carg.Type() {
		case NodeNumber:
			node := carg.(*NumberNode)
			val := &Value{}
			if node.IsInt {
				val.typ = TypeInt
				val.intVal = node.Int64
			} else if node.IsFloat {
				val.typ = TypeFloat
				val.floatVal = node.Float64
			} else {
				panic("not a numeric value in numeric node")
			}

			vals = append(vals, val)
		case NodeString:
			node := carg.(*StringNode)
			val := &Value{typ: TypeString, stringVal: node.Text}
			vals = append(vals, val)
		case NodeVariable:
			node := carg.(*VariableNode)
			vr := vars.Lookup(node.Ident[0])
			if vr == nil {
				panic("unknown variable")
			}
			val := &Value{typ: TypeRef, vref: vr}
			vals = append(vals, val)
		case NodeAction:
			// TODO immediate call
		case NodeBlock:
			// TODO proc
		case NodeStream:
			// TODO stream
		default:
			panic("unknown type")
		}
	}

	return &Command{fn, vals}
}

type Parameter struct {
	name string
	typ  Type
}

func newParameter(name, typ string) *Parameter {
	return &Parameter{name, getType(typ)}
}

func params2varlist(params []*Parameter) *VarList {
	var vlist *VarList
	for _, param := range params {
		v := &Variable{param.name, Value{typ: param.typ}}
		vlist = AddVar(v, vlist)
	}
	return vlist
}

type Variable struct {
	name  string
	value Value
}

type VarList struct {
	v    *Variable
	next *VarList
}

func AddVar(v *Variable, vars *VarList) *VarList {
	return &VarList{v, vars}
}

func (v *VarList) Lookup(name string) *Variable {
	for {
		if v == nil {
			return nil
		}

		if v.v != nil && v.v.name == name {
			return v.v
		}

		v = v.next
	}
	return nil
}

type Value struct {
	typ       Type
	intVal    int64
	floatVal  float64
	stringVal string

	vref *Variable
	// TODO: streamVal
}

type Function struct {
	name   string
	params []*Parameter
	body   *Block
}

func newFunction() *Function {
	return &Function{}
}

func (fn *Function) buildProto(proto *ProtoNode) {
	params := []*Parameter{}
	for _, parg := range proto.Vars {
		arg := newParameter(parg.Name, parg.PType)
		params = append(params, arg)
	}

	fn.name = proto.Name
	fn.params = params
}

func buildFunction(intr *Interpretator, name string, node *FuncNode) {
	fn := intr.funcs[name]
	// parse body (pipeline), with args as vars
	args := params2varlist(fn.params)
	fn.body = buildBlock(intr, node.Block.Body.Pipes, args)
}

func getType(typ string) Type {
	switch typ {
	case "int":
		return TypeInt
	case "float":
		return TypeFloat
	case "string":
		return TypeString
	case "stream":
		return TypeStream
	default:
		return TypeInvalid
	}
}
