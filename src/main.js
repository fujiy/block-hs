import jQuery from 'jquery'
// import popper from 'popper.js'
// import bootstrap from 'bootstrap'
// import * as mdc from 'material-components-web';


// customElements.define('main-module', class extends HTMLDivElement {constructor(){super()}}, {extends: 'div'})

import riot from 'riot'

import html from 'index.html'
import style from 'main.sass'

import Editor from './editor/editor.tag'

// window.console_log = console.log

console.log('load');
// console.log();

riot.mount('editor')
