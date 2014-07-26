package main

import (
	"flag"
	"fmt"
  "strconv"
	"time"
  "os"
)

func main() {
  defer errshutdown()
  flag.Parse()

  unixts, err := strconv.ParseInt(flag.Arg(0), 10, 64)
  errhandle(err, "first arg should be an int")

	fmt.Printf("%v\n", time.Unix(unixts, 0).UTC().Format(time.RFC3339))
}

func errhandle(e error, msg string) {
  if e == nil { return }
  panic(msg)
}

func errshutdown() {
  r := recover()
  if r == nil { return }

  fmt.Fprintln(os.Stderr, r)
  os.Exit(1)
}
