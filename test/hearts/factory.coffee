Arena = require "../../hearts/arena"
exports.createGame = (options={}) ->
  arena = options.arena || new Arena()

  players = [
    arena.createPlayer()
    arena.createPlayer()
    arena.createPlayer()
    arena.createPlayer()
  ]

  game = arena.createGame(players)

  game

