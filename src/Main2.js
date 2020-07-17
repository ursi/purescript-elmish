const fs = require(`fs`);
const fsp = fs.promises;

exports.wait = ms => fulfill => {
	setTimeout(fulfill, ms);
};

exports.waitFail = ms => (_, reject) => {
	setTimeout(reject, ms);
};

exports.consoleLog = a => {
	console.log(a);
	return a;
};

exports.logShowIdImpl = show => a => {
	console.log(show(a));
	return a;
}
