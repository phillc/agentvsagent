Arena = require "../lib/arena"
HeartsFactory = require "../lib/hearts/factory"

exports.createArena = createArena = (options={}) ->
  factory = options.factory || new HeartsFactory()
  new Arena(factory)

exports.createGame = createGame = (options={}) ->
  arena = options.arena || createArena(options)

  players = [
    arena.createPlayer()
    arena.createPlayer()
    arena.createPlayer()
    arena.createPlayer()
  ]

  game = arena.createGame(players)

  game

