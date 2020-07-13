package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"unicode/utf8"
)

var in = flag.String("f", "", "input file")
var out = flag.String("o", "", "output file")

type word struct {
	forView  string
	forInput string
}

func (w word) validate() bool {
	if len(w.forView) < 1 || len(w.forInput) < 1 {
		return false
	}
	if utf8.RuneCountInString(w.forView) != utf8.RuneCountInString(w.forInput) {
		return false
	}
	return true
}

func readTextWords(file string) []word {
	f, err := os.Open(file)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()
	input := bufio.NewScanner(f)
	var words []word
	for {
		var w word
		if input.Scan() {
			w.forView = input.Text()
		} else {
			break
		}
		if input.Scan() {
			w.forInput = input.Text()
		} else {
			log.Fatal("need InputText")
		}
		if input.Scan() {
			// 空行のはず
			if strings.Trim(input.Text(), " 　\t") != "" {
				log.Fatal("need space line")
			}
		}
		if !w.validate() {
			fmt.Printf("%v\n", w)
			fmt.Printf("%d, %d\n", len(w.forView), len(w.forInput))
			log.Fatal("ViewText and InputText must be same length")
		}
		words = append(words, w)
	}
	return words
}

func writeElmWords(file string, words []word) error {
	f, err := os.Create(file)
	if err != nil {
		return err
	}
	defer f.Close()

	fmt.Fprintf(f, "module Emb.%s exposing (list)\n", basename(file))
	fmt.Fprint(f, "\n\n")
	fmt.Fprint(f, "list : List ( String, String )\n")
	fmt.Fprint(f, "list =\n")

	fmt.Fprint(f, "    [")
	for i, word := range words {
		if i != 0 {
			fmt.Fprint(f, "    ,")
		}
		fmt.Fprintf(f, " ( \"%s\"\n", word.forView)
		fmt.Fprintf(f, "      , \"%s\"\n", word.forInput)
		fmt.Fprint(f, "      )\n")

	}
	fmt.Fprint(f, "    ]")

	return nil

}

func main() {
	flag.Parse()
	if len(os.Args) <= 2 {
		flag.Usage()
		os.Exit(1)
	}

	words := readTextWords(*in)

	fmt.Println(words)

	if *out == "" {
		*out = basename(*in) + ".elm"
	}
	fmt.Println(*out)

	err := writeElmWords(*out, words)
	if err != nil {
		log.Fatal(err)
	}
}

func basename(s string) string {
	s = filepath.Base(s)
	if dot := strings.LastIndex(s, "."); dot >= 0 {
		s = s[:dot]
	}
	return s
}
