'use strict';

const fs = require(`fs`);

const fsp = fs.promises;

exports.exists = pth => fulfill => {
	fulfill(fs.existsSync(pth));
};

exports.mkdirPromise = name => () => fsp.mkdir(name);

exports.readdirPromise = dir => () => fsp.readdir(dir);

exports.unlinkPromise = path => () => fsp.unlink(path);

exports.writeFilePromise = pth => contents => () => {
	return fsp.writeFile(pth, contents);
};

exports.readFilePromise = pth => () => {
	return fsp.readFile(pth, `utf8`);
};
