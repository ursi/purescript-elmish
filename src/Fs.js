'use strict';

const fs = require(`fs`);

const fsp = fs.promises;

exports.exists = pth => fulfill => {
	fulfill(fs.existsSync(pth));
};

exports.mkdirPromise = fsp.mkdir;

exports.readdirPromise = fsp.readdir;

exports.unlinkPromise = fsp.unlink;

exports.writeFilePromise = pth => contents => {
	return fsp.writeFile(pth, contents);
};

exports.readFilePromise = pth => {
	return fsp.readFile(pth, `utf8`);
};
