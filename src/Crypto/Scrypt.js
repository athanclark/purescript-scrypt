"use strict";

var scrypt = require('scrypt-js');

exports.scryptImpl = function scryptImpl (params) {
  scrypt(params.password, params.salt, params.n, params.r, params.p, params.dkLen, function callback (error, progress, key) {
    if (error) {
      params.onError(error);
    } else if (key) {
      params.onComplete(key);
    } else {
      params.onProgress(progress);
    }
  });
};
