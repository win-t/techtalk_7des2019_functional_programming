package main

import "fmt"

// you don't have to use "var"
// here, I just want to emphasize their type
// every function take "exactly" one argument, and return "exactly" one result
var (
	add func(int) func(int) int
	sub func(int) func(int) int
	mul func(int) func(int) int
	div func(int) func(int) int

	flip    func(func(int) func(int) int) func(int) func(int) int
	compose func(func(int) int) func(func(int) int) func(int) int

	filter func(func(int) bool) func([]int) []int
	fmap   func(func(int) int) func([]int) []int
	fold   func(func(int) func(int) int) func(int) func([]int) int
)

func init() {
	add = func(x int) func(int) int {
		return func(y int) int {
			return x + y
		}
	}

	sub = func(x int) func(int) int {
		return func(y int) int {
			return x - y
		}
	}

	mul = func(x int) func(int) int {
		return func(y int) int {
			return x * y
		}
	}

	flip = func(f func(int) func(int) int) func(int) func(int) int {
		return func(x int) func(int) int {
			return func(y int) int {
				return f(y)(x)
			}
		}
	}

	compose = func(f func(int) int) func(func(int) int) func(int) int {
		return func(g func(int) int) func(int) int {
			return func(x int) int {
				return f(g(x))
			}
		}
	}

	filter = func(p func(int) bool) func([]int) []int {
		return func(xs []int) []int {
			var ret []int
			for _, x := range xs {
				if p(x) {
					ret = append(ret, x)
				}
			}
			return ret
		}
	}

	fmap = func(f func(int) int) func([]int) []int {
		return func(xs []int) []int {
			var ret []int
			for _, x := range xs {
				ret = append(ret, f(x))
			}
			return ret
		}
	}

	fold = func(f func(int) func(int) int) func(int) func([]int) int {
		return func(z int) func([]int) int {
			return func(xs []int) int {
				if len(xs) == 0 {
					return z
				}
				tmp := f(z)(xs[0])
				for i := 1; i < len(xs); i++ {
					tmp = f(tmp)(xs[i])
				}
				return tmp
			}
		}
	}

}

func genRange(min int, step int, max int) []int {
	var ret []int
	for val := min; val <= max; val += step {
		ret = append(ret, val)
	}
	return ret
}

// helper for (.) composition of haskell
type composer struct{ f func(int) int }

func composerOf(f func(int) int) composer { return composer{f} }

func (c composer) dot(f func(int) int) composer {
	return composer{compose(c.f)(f)}
}

func (c composer) tod(f func(int) int) composer {
	// we can't use flip(compose)(c.f)(f), because go doesn't support generic
	return composer{compose(f)(c.f)}
}

func (c composer) do(x int) int {
	return c.f(x)
}

func main() {
	// add 5 3
	fmt.Println(add(5)(3))

	// add5 = add 5
	add5 := add(5)
	fmt.Println(add5(3))

	// ((+5) . (*3)) 10
	thenAdd5 := compose(add5)
	fmt.Println(compose(add5)(mul(3))(10))

	// ((+5) . (+5) . (*3)) 10
	fmt.Println(thenAdd5(thenAdd5(mul(3)))(10))
	fmt.Println(composerOf(add5).dot(add5).dot(mul(3)).do(10))

	// ((+5) . (*3) . flip (-) 4) 10
	fmt.Println(composerOf(add5).dot(mul(3)).dot(flip(sub)(4)).do(10))
	fmt.Println(composerOf(flip(sub)(4)).tod(mul(3)).tod(add5).do(10))

	// filter (\x -> x `mod` 2 == 0) [1..10]
	isEven := func(x int) bool { return x%2 == 0 }
	fmt.Println(filter(isEven)(genRange(1, 1, 10)))

	// fmap (+5) [1,3..10]
	fmt.Println(fmap(add(5))(genRange(1, 2, 10)))

	// let sum = fold (+) 0
	//  in sum [1..0]
	sum := fold(add)(0)
	fmt.Println(sum(genRange(1, 1, 10)))

	// fold (*) 1 [1..0]
	product := fold(mul)(1)
	fmt.Println(product(genRange(1, 1, 10)))

	// sum (filter (\x -> x `mod` 2 /= 0) (fmap (*5) [1..5]))
	fmt.Println(sum(filter(func(x int) bool { return x%2 != 0 })(fmap(mul(5))(genRange(1, 1, 5)))))
}
