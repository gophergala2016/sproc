// generated by stringer -type NodeType; DO NOT EDIT

package sproc

import "fmt"

const _NodeType_name = "NodeActionNodeBoolNodeChainNodeCommandNodeBlockNodeFuncNodeProtoNodeParameternodeElseNodeFieldNodeIdentifierNodeIfNodeListNodeNumberNodePipeNodeStringNodeVariableNodeStream"

var _NodeType_index = [...]uint8{0, 10, 18, 27, 38, 47, 55, 64, 77, 85, 94, 108, 114, 122, 132, 140, 150, 162, 172}

func (i NodeType) String() string {
	if i < 0 || i >= NodeType(len(_NodeType_index)-1) {
		return fmt.Sprintf("NodeType(%d)", i)
	}
	return _NodeType_name[_NodeType_index[i]:_NodeType_index[i+1]]
}