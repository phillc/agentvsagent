package main

func passCards() string {
	return "hello"
}

func playCard() string {
	return "hey"
}

func main() {
	play(passCards, playCard)
}
