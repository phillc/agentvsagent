package main

import (
  "fmt"
	"git.apache.org/thrift.git/lib/go/thrift"
	"./lib/hearts/hearts"
)

func main() {
  fmt.Printf("Hello", thrift.NewTBinaryProtocolFactoryDefault())
}

