'use strict';

exports.mapImpl = f => t => (fulfill, reject, onError) => {
	t(a => fulfill(f(a)), reject, onError);
};

exports.applyImpl = tf => ta => (fulfill, reject, onError) => {
	tf(
		f => {
			try {
				ta(a => fulfill(f(a)), reject, onError)
			} catch (error) {
				onError(error);
			}
		},

		reject,
		onError
	);
};

exports.pureImpl = a => fulfill => fulfill(a);

exports.bindImpl = t => f => (fulfill, reject, onError) => {
	t(
		a => {
			try {
				f(a)(fulfill, reject, onError)
			} catch (error) {
				onError(error);
			}
		},

		reject,
		onError
	);
};

exports.fromEffect = e => fulfill => fulfill(e());

exports.mapError = f => t => (fulfill, reject, onError) => {
	t(fulfill, x => reject(f(x)), onError);
};

exports.onErrorImpl = f => t => (fulfill, reject, onError) => {
	t(
		fulfill,
		x => {
			try {
				f(x)(fulfill, reject, onError);
			} catch (error) {
				onError(Error)
			}
		},

		onError
	);
};

exports.reportImpl = Left => Right => onError => handler => t => () => {
	try {
		t(a => handler(Right(a)), x => handler(Left(x)), onError);
	} catch (error) {
		onError(error);
	}
};

exports.logError = x => {
	console.error(x);
};

exports.unsafeCaptureImpl = (handler, t) => () => {
	t(handler);
};

exports.fromPromiseImpl = toPromise => arg => (fulfill, reject) => {
	toPromise(arg).then(fulfill, e => reject(String(e)));
};

exports.parallel = tasks => (fulfill, reject, onError) => {
	const result = [];

	let completed = 0;

	const fulfillLocal = i => a => {
		result[i] = a;
		if (++completed === tasks.length) fulfill(result);
	};

	tasks.forEach((t, i) => t(fulfillLocal(i), reject, onError));
};
