package main

import (
	"fmt"
	"github.com/gophergala2016/sproc"
	"os"
)

/*
func main() {
	lf := sproc.NewLexface("test 1 2 3")
	for {
		str, typ, ok := lf.Next()
		if !ok {
			break
		}

		fmt.Println(typ, str)
	}
}

//*/

//*
func main() {
	if len(os.Args) != 2 {
		fmt.Println("add some text")
		return
	}
	tree, err := sproc.Parse("test", os.Args[1])
	fmt.Println("parsed", err)

	if tree == nil {
		fmt.Println("tree is nil!")
		return
	}

	if tree.Root == nil {
		fmt.Println("tree.Root is nil!")
		return
	}

	fmt.Println(tree.Root.Repr(""))
}

//*/
