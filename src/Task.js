exports.fromPromiseImpl = makeTask => effectUnit => promiseThunk => {
	return makeTask(aC => xC => () => {
		promiseThunk().then(aC, xC);
		return effectUnit;
	});
};
