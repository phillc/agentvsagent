chai = require("chai")
chaiAsPromised = require("chai-as-promised")
chai.use(chaiAsPromised)

global.expect = chai.expect

require("coffee-script");
global.Factory = require("./factory")

