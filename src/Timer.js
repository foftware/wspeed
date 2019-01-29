"use strict";

exports.startTimer = function (ms) {
  return function (fn) {
    return function () {
      return setInterval(fn, ms);
    };
  };
};


exports.stopTimer = function (id) {
  return function () {
    clearInterval(id);
  };
};
