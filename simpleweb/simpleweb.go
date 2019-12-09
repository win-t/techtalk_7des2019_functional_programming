package main

import (
	"fmt"
	"net/http"
	"os"
)

func app(w http.ResponseWriter, r *http.Request) {
	switch r.URL.EscapedPath() {
	case "/hello":
		w.Header().Set("Content-Type", "text/plain")
		w.WriteHeader(200)
		fmt.Fprintln(w, "Hello World")

	default:
		w.Header().Set("Content-Type", "text/plain")
		w.WriteHeader(404)
		fmt.Fprintln(w, "404 NOT FOUND")
	}
}

func main() {
	if err := http.ListenAndServe(":8080", http.HandlerFunc(app)); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}
