exports.createGame = (options) ->
  arena = options.arena

  players = [
    arena.createPlayer()
    arena.createPlayer()
    arena.createPlayer()
    arena.createPlayer()
  ]

  game = arena.createGame(players)

  game

