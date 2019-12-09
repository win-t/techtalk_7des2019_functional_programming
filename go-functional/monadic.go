// +build ignore

package main

import (
	"errors"
	"fmt"
)

// normal: add5
func add5(x int) int {
	return x + 5
}

// normal: 3 / x
func threeDivX(x int) int {
	return 3 / x
}

// normal composition, the (.) operator
// for simplicity, i don't this function accept tuple of function
// instead of return function that return another function (High order function), like in simple.go
func compose(f func(int) int, g func(int) int) func(int) int {
	return func(x int) int {
		return f(g(x))
	}
}

// golang doesn't have type polymorphism (yet, golang 2 will have generic)
// so we need to create type for every monad we need
// this "monad" struct equivalent to "Either Error Int" in haskell
type monad struct {
	value int
	err   error
}

// monad style: add5
func add5Monad(x int) monad {
	return monad{x + 5, nil}
}

// monad style: 3 / x
func threeDivXMonad(x int) monad {
	if x == 0 {
		return monad{0, errors.New("division by zero")}
	}
	return monad{x / 3, nil}
}

// equivalent to "return" in haskell
func idMonad(x int) monad { return monad{x, nil} }

// equivalent to "<=<" in haskell
func composeMonad(f func(int) monad, g func(int) monad) func(int) monad {
	return func(x int) monad {
		r1 := g(x)
		if r1.err != nil {
			return monad{0, r1.err}
		}

		r2 := f(r1.value)
		if r2.err != nil {
			return monad{0, r2.err}
		}

		return monad{r2.value, nil}
	}
}

// helper for (<=<) composition of haskell
type monadComposer struct{ f func(int) monad }

func monadComposerOf(f func(int) monad) monadComposer { return monadComposer{f} }

func (c monadComposer) dot(f func(int) monad) monadComposer {
	return monadComposer{composeMonad(c.f, f)}
}

// in other language (>=>) operator also called flatMap
func (c monadComposer) flatMap(f func(int) monad) monadComposer {
	return monadComposer{composeMonad(f, c.f)}
}

func (c monadComposer) do(x int) monad {
	return c.f(x)
}

func printMonad(m monad) {
	if m.err != nil {
		fmt.Println("err:", m.err.Error())
	} else {
		fmt.Println(m.value)
	}
}
func main() {
	// normal := compose(threeDivX, add5)(-5)
	// fmt.Println(normal)

	// will error
	printMonad(composeMonad(threeDivXMonad, add5Monad)(-5))

	// composition style

	// will error
	printMonad(monadComposerOf(threeDivXMonad).
		dot(add5Monad).
		do(-5),
	)

	// monadic composition also called flatMap
	// you can compose the multiple into single action, and call it later
	// the action is short circuit

	action := monadComposerOf(idMonad).
		flatMap(func(x int) monad { return monad{x * 5, nil} }). // (1)
		flatMap(func(x int) monad { return monad{x - 5, nil} }). // (2)
		flatMap(threeDivXMonad).                                 // (3)
		flatMap(add5Monad).                                      // (4)
		flatMap(func(x int) monad { return monad{x + 10, nil} }) // (5)

	// will print 21
	printMonad(action.do(5))

	// will error on (3), but because of using
	// monadid style (4) and (5) will not be called (short circuit)
	printMonad(action.do(1))
}
