function game() {
  var transport = new Thrift.Transport("/game/hearts/service");
  var protocol  = new Thrift.Protocol(transport);
  var client    = new HeartsClient(protocol);

  try {
    result = client.enter_arena();
    $('#result').val(result);
    $('#result').css('color', 'black');
  } catch(ouch){
    $('#result').val(ouch.why);
    $('#result').css('color', 'red');
  }
}
// $(game);


var transport = new Thrift.Transport("/game/hearts/service");
var protocol  = new Thrift.Protocol(transport);
var client    = new HeartsClient(protocol);
