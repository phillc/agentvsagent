spawn = require('child_process').spawn
net = require('net')
machina = require('machina')()

exports.run = (game, command, {port, host}) ->
  console.log "command: ", command
  parts = command.split(" ")
  child = spawn(parts[0], parts.slice(1))

  console.log('connecting to', host, port)
  client = new net.Socket()

  state = new State(client: client, child: child)

  child.stdout.on 'data', (data) ->
    console.log('stdout: ' + data)
    client?.write(data)

  child.stderr.on 'data', (data) ->
    console.log('stderr: ' + data)

  child.on 'close', (code) ->
    console.log('child process exited with code ' + code)
    client?.end()

  client.connect port, host, ->
    console.log('CONNECTED TO: ' + host + ':' + port)
    client.write(JSON.stringify({
      message: "requestingEntry"
      data:
        name: "anonymous"
        game: game
    }))
    client.write("\n")

  buffer = ''
  client.on 'data', (data) ->
    lines = data.toString().split("\n")
    lines[0] = buffer + lines[0]
    buffer = ''
    if lines[lines.length - 1] != ""
      buffer = lines[lines.length - 1]
      lines.splice(lines.length - 1, 1)

    for line in lines
      if line != ''
        state.handle "line", line

  client.on 'end', ->
    child.kill()

State = machina.Fsm.extend
  initialize: ({@child, @client}) ->

  initialState: "entering"
  states:
    entering:
      line: (line) ->
        console.log("entering line:", line)
        @client.write(JSON.stringify({
          message: "requestingGame"
        }))
        @client.write("\n")
        @transition("playing")

    playing:
      line: (line) ->
        console.log("playing line:", line)
        @child.stdin.write(line)
        @child.stdin.write("\n")
