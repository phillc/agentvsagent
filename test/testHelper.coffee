chai = require("chai")
chaiAsPromised = require("chai-as-promised")
chai.use(chaiAsPromised)

should = require("should")

global.should = should
global.expect = chai.expect

