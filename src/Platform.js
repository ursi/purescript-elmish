'use strict';

let update;

const subs = {
	// array of cancel functions
	cancel:  [],
	// array of {id, args, cancel}
	current: []
};

exports.setUpdate = f => () => update = f;

exports.sendMsg = msg => update(msg);

exports.worker = init => flags => {
	update = msg => {
		model = init.update(msg)(model)();
		handleSubscriptions(init.subscriptions(model));
	};

	model = init.init(flags)();
	handleSubscriptions(init.subscriptions(model));
};

const handleSubscriptions = newSubs => {
	// cancel all subs with empty ids
	subs.cancel.forEach(c => c());
	subs.cancel = [];

	// remove all subs with empty ids from new subs
	// add them to subs.cancel
	// set execute them
	let i = 0;
	while (i < newSubs.length) {
		const {id} = newSubs[i];
		if (id === ``) {
			subs.cancel.push = newSub.sub(sendMsg);
			newSubs.splice(i, 1);
		} else {
			i++;
		}
	}

	// cancel a current sub if it's not included in new subs
	// delete a new sub if it's the same as a current sub
	i = 0;
	while (i < subs.current.length) {
		const sub = subs.current[i];
		const index = newSubs.findIndex(subsEqual(sub));
		if (index === -1) {
			subs.current.splice(i, 1);
			sub.cancel();
		} else {
			newSubs.splice(index, 1);
			i++;
		}
	}

	// add all the new subs that are left to the current subs
	subs.current = subs.current.concat(
		newSubs.map(sub => {
			sub.cancel = sub.sub(sendMsg);
			return sub;
		})
	);
};

const subsEqual = a => b => {
	if (a.id === b.id) {
		return a.args.every((v, i) =>  v.eq(v.arg)(b.args[i].arg));
	} else {
		return false;
	}
};
