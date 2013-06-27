transport = new Thrift.Transport("/game/tic_tac_toe/service.json")
protocol  = new Thrift.Protocol(transport)
client    = new AgentVsAgent.TicTacToeClient(protocol)

client.enter_arena new AgentVsAgent.EntryRequest(), (response) =>
  window.ticket = response.ticket
  if window.ticket
    client.get_game_info window.ticket, (gameInfo) =>
      console.log("ok, start")

window.make_move = (move) ->
  client.make_move window.ticket, move, (moveResult) ->
    if moveResult.status != AgentVsAgent.GameStatus.NEXT_MOVE
      console.log("It is over..")
      game_result = client.get_game_result ticket
      console.log(game_result)
    else
      console.log("Move again!")

