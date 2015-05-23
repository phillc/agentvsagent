chai = require("chai")
chaiAsPromised = require("chai-as-promised")
chai.use(chaiAsPromised)

global.expect = chai.expect

require("coffee-script/register");
global.Factory = require("./factory")

