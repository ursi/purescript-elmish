'use strict';

const fs = require(`fs`);

const fsp = fs.promises;

export const exists = pth => fulfill => {
	fulfill(fs.existsSync(pth));
};

export const mkdirPromise = name => () => fsp.mkdir(name);

export const readdirPromise = dir => () => fsp.readdir(dir);

export const unlinkPromise = path => () => fsp.unlink(path);

export const writeFilePromise = pth => contents => () => {
	return fsp.writeFile(pth, contents);
};

export const readFilePromise = pth => () => {
	return fsp.readFile(pth, `utf8`);
};
