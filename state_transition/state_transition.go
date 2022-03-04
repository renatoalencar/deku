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

func log(message string) {
	colored := fmt.Sprintf("\x1b[%dm%s\x1b[0m\n", 31, message)
	fmt.Printf(colored)
}

func main() {
	state_transition := func(input []byte) {
		var message message
		log(fmt.Sprintf("State transition received %s", string(input)))
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
			log(fmt.Sprintf("Incremented counter %d", new_counter))
		case "Decrement":
			if *counter > 0 {
				new_counter = *counter - 1
				deku_interop.Set("counter", new_counter)
				log(fmt.Sprintf("Decremented counter %d", new_counter))
			} else {
				log("Skipping counter increase")
			}
		}
	}
	deku_interop.Main(state_transition)
}
