// there is bug in this code because of
// golang compiler doesn't have the concept of immutability
// programmer should know every detail of their code
// compiler will not help you in this matter

package main

import (
	"fmt"
	"sort"
)

type score struct {
	name  string
	value int
}

func printScore(s score) { fmt.Println(s.name, s.value) }

// getMinInList have side effect
// and the caller maybe not aware of it
// when caller need to aware of the called function, that will make overall code hard to maintain
func getMinInList(scores []score) score {
	sort.Slice(scores, func(i, j int) bool { return scores[i].value < scores[j].value })
	return scores[0]
}

func getFirstInList(scores []score) score { return scores[0] }

func getLastInList(scores []score) score { return scores[len(scores)-1] }

func main() {
	scores := []score{
		score{name: "Alice", value: 70},
		score{name: "Bob", value: 60},
		score{name: "Carlie", value: 80},
	}

	printScore(getMinInList(scores))
	printScore(getFirstInList(scores))
	printScore(getLastInList(scores))
}
