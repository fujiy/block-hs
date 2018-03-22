
exports.console_log = console.log
exports.console_log_with = function(s){ return function(a){ console.log(s, a) } }
