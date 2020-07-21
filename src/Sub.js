// exports.none = {
// 	id: `none`,
// 	args: [],
// 	sub() {return () => {};}
// };

const createSub = (id, args, subImpl) => {
	return {
		id,
		args: args,
		sub: subImpl(...args.map(a => a.arg))
	};
};

exports.mapSubImplImpl = f => subImpl => send => subImpl(a => send(f(a)));

exports.toSub0 = id => subImpl => {
	return createSub(id, [], subImpl);
};

exports.toSub1 = id => a1 => subImpl => {
	return createSub(id, [a1], subImpl);
};

exports.toSub2 = id => a1 => a2 => subImpl => {
	return createSub(id, [a1, a2], subImpl);
};

exports.toSub3 = id => a1 => a2 => a3 => subImpl => {
	return createSub(id, [a1, a2, a3], subImpl);
};

exports.toSub4 = id => a1 => a2 => a3 => a4 => subImpl => {
	return createSub(id, [a1, a2, a3, a4], subImpl);
};

exports.toSub5 = id => a1 => a2 => a3 => a4 => a5 => subImpl => {
	return createSub(id, [a1, a2, a3, a4, a5], subImpl);
};

// exports.appendSubImplImpl = s1 => s2 => send

exports.everyImpl = ms => send => {
	const i = setInterval(send(Date.now()), ms);
	return () => clearInterval(i);
};
