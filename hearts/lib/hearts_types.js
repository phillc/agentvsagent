//
// Autogenerated by Thrift Compiler (0.9.0)
//
// DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
//
var Thrift = require('thrift').Thrift;
var ttypes = module.exports = {};
ttypes.Suit = {
'CLUBS' : 21,
'DIAMONDS' : 22,
'SPADES' : 23,
'HEARTS' : 24
};
ttypes.Rank = {
'TWO' : 2,
'THREE' : 3,
'FOUR' : 4,
'FIVE' : 5,
'SIZE' : 6,
'SEVEN' : 7,
'EIGHT' : 8,
'NINE' : 9,
'TEN' : 10,
'JACK' : 11,
'QUEEN' : 12,
'KING' : 13,
'ACE' : 1
};
Card = module.exports.Card = function(args) {
  this.suit = null;
  this.rank = null;
  if (args) {
    if (args.suit !== undefined) {
      this.suit = args.suit;
    }
    if (args.rank !== undefined) {
      this.rank = args.rank;
    }
  }
};
Card.prototype = {};
Card.prototype.read = function(input) {
  input.readStructBegin();
  while (true)
  {
    var ret = input.readFieldBegin();
    var fname = ret.fname;
    var ftype = ret.ftype;
    var fid = ret.fid;
    if (ftype == Thrift.Type.STOP) {
      break;
    }
    switch (fid)
    {
      case 1:
      if (ftype == Thrift.Type.I32) {
        this.suit = input.readI32();
      } else {
        input.skip(ftype);
      }
      break;
      case 2:
      if (ftype == Thrift.Type.I32) {
        this.rank = input.readI32();
      } else {
        input.skip(ftype);
      }
      break;
      default:
        input.skip(ftype);
    }
    input.readFieldEnd();
  }
  input.readStructEnd();
  return;
};

Card.prototype.write = function(output) {
  output.writeStructBegin('Card');
  if (this.suit !== null && this.suit !== undefined) {
    output.writeFieldBegin('suit', Thrift.Type.I32, 1);
    output.writeI32(this.suit);
    output.writeFieldEnd();
  }
  if (this.rank !== null && this.rank !== undefined) {
    output.writeFieldBegin('rank', Thrift.Type.I32, 2);
    output.writeI32(this.rank);
    output.writeFieldEnd();
  }
  output.writeFieldStop();
  output.writeStructEnd();
  return;
};

Agent = module.exports.Agent = function(args) {
  this.gameId = null;
  this.agentId = null;
  if (args) {
    if (args.gameId !== undefined) {
      this.gameId = args.gameId;
    }
    if (args.agentId !== undefined) {
      this.agentId = args.agentId;
    }
  }
};
Agent.prototype = {};
Agent.prototype.read = function(input) {
  input.readStructBegin();
  while (true)
  {
    var ret = input.readFieldBegin();
    var fname = ret.fname;
    var ftype = ret.ftype;
    var fid = ret.fid;
    if (ftype == Thrift.Type.STOP) {
      break;
    }
    switch (fid)
    {
      case 1:
      if (ftype == Thrift.Type.STRING) {
        this.gameId = input.readString();
      } else {
        input.skip(ftype);
      }
      break;
      case 2:
      if (ftype == Thrift.Type.STRING) {
        this.agentId = input.readString();
      } else {
        input.skip(ftype);
      }
      break;
      default:
        input.skip(ftype);
    }
    input.readFieldEnd();
  }
  input.readStructEnd();
  return;
};

Agent.prototype.write = function(output) {
  output.writeStructBegin('Agent');
  if (this.gameId !== null && this.gameId !== undefined) {
    output.writeFieldBegin('gameId', Thrift.Type.STRING, 1);
    output.writeString(this.gameId);
    output.writeFieldEnd();
  }
  if (this.agentId !== null && this.agentId !== undefined) {
    output.writeFieldBegin('agentId', Thrift.Type.STRING, 2);
    output.writeString(this.agentId);
    output.writeFieldEnd();
  }
  output.writeFieldStop();
  output.writeStructEnd();
  return;
};

