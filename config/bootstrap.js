/**
 * Bootstrap
 *
 * An asynchronous boostrap function that runs before your Sails app gets lifted.
 * This gives you an opportunity to set up your data model, run jobs, or perform some special logic.
 *
 * For more information on bootstrapping your app, check out:
 * http://sailsjs.org/#documentation
 */

module.exports.bootstrap = function (cb) {
  if (sails.config.ava) {
    sails.config.port = sails.config.ava.port //no, environment variable does not win
    if (sails.config.ava.http) {
      cb();
    } else {
      cb("Not starting http server");
    }
  } else {
    cb("Not starting http server");
  }
};
