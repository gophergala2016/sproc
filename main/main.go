package main

import (
	"fmt"
	"github.com/gophergala2016/sproc"
	"os"
	"time"
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

/*
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

	fmt.Println("tree:")
	fmt.Println(tree.Root.Repr(""))
	fmt.Println("funcs:")
	for _, def := range tree.Defs {
		fmt.Println(def.Repr(""))
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

	fmt.Println("tree:")
	fmt.Println(tree.Root.Repr(""))
	fmt.Println("funcs:")
	for _, def := range tree.Defs {
		fmt.Println(def.Repr(""))
	}

	fmt.Println("---")
	fmt.Println("executing:")

	interp := sproc.NewInterpretator()
	interp.Run(tree)

	time.Sleep(2 * time.Second)
}

//*/
