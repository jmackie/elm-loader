'use strict';

exports.chdir = function(dir) {
    return function() {
        process.chdir(dir);
    };
};
