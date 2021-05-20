exports.everyImpl = ms => toA => send => {
	const i = setInterval(() => send(toA(Date.now())), ms);
	return () => clearInterval(i);
};

exports.every_Impl = ms => a => send => {
	const i = setInterval(() => send(a), ms);
	return () => clearInterval(i);
};
