package main

import (
	"encoding/json"
	"example.com/deku_interop"
	"fmt"
)

type message struct {
	Action string
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	state_transition := func(input []byte) {
		var message message
		fmt.Printf("State transition received %s\n", string(input))
		err := json.Unmarshal(input, &message)
		check(err)
		counter_bytes := deku_interop.Get("counter")
		var counter *int
		json.Unmarshal(counter_bytes, &counter)
		var new_counter int
		switch message.Action {
		case "Increment":
			new_counter = *counter + 1
			deku_interop.Set("counter", new_counter)
			fmt.Printf("Incremented counter %d\n", new_counter)
		case "Decrement":
			new_counter = *counter - 1
			deku_interop.Set("counter", new_counter)
		}
	}
	deku_interop.Main(state_transition)
}
