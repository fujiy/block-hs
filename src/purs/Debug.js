
exports.console_log = console.log
exports.console_log_with = function(s){ return function(a){ console.log(s, a) } }
exports.console_error = function(e){console.error(e); throw "error"}
