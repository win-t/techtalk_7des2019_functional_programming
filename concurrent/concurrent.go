package main

import (
	"fmt"
	"time"
)

func main() {

	go func() {
		for {
			time.Sleep(1000 * time.Millisecond)
			fmt.Println("Hello World 1")
		}
	}()

	go func() {
		for {
			time.Sleep(700 * time.Millisecond)
			fmt.Println("Hello World 2")
		}
	}()

	for {
		time.Sleep(1000 * time.Millisecond)
	}
}
