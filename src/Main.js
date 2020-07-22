exports.waitImpl = ms => fulfill => {
	setTimeout(fulfill, ms);
};

exports.everyImpl = ms => send => {
	const i = setInterval(() => send(Date.now()), ms);
	return () => clearInterval(i);
};
