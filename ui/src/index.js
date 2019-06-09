'use strict';

require("./assets/styles/index.scss");

const { Elm } = require('./Main');
const mountNode = document.createElement('div')
document.body.prepend(mountNode)

var app = Elm.Main.init({
    node: mountNode
});
