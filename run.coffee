spawn = require('child_process').spawn
net = require('net')

exports.run = (command) ->
  console.log "command: ", command
  parts = command.split(" ")
  child = spawn(parts[0], parts.slice(1))

  child.stdout.on 'data', (data) ->
    console.log('stdout: ' + data)
    client?.write(data)

  child.stderr.on 'data', (data) ->
    console.log('stderr: ' + data)

  child.on 'close', (code) ->
    console.log('child process exited with code ' + code)
    client?.end()

  client = new net.Socket()
  host = process.env.AVA_HOST || "127.0.0.1"
  port = process.env.AVA_PORT || 4002

  client.connect port, host, ->
    console.log('CONNECTED TO: ' + host + ':' + port)
    client.write('ava run connected')

