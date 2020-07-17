'use strict';

exports.everyImpl = (ms, toMsg) => send => {
	const i = setInterval(() => send(toMsg(Date.now())), ms);
	return () => clearInterval(i);
};

exports.wait = ms => (fulfill, reject) => {
	setTimeout(fulfill, ms);
};

exports.stdinImpl = toMsg => send => {
	process.stdin.setEncoding('utf8');
	const listener = d => send(toMsg(d.trim()));
	process.stdin.on('data', listener);
	return () => process.stdin.off(`data`, listener);
};

exports.exit = () => process.exit();
