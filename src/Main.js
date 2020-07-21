exports.wait = ms => fulfill => {
	setTimeout(fulfill, ms);
};
