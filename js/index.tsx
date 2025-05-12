////////////////////////////////////////////////////////////////
// Load a bunch of CSS
import '../css/app.scss';
import { dom, library } from '@fortawesome/fontawesome-svg-core';
import { fas } from '@fortawesome/free-solid-svg-icons';
library.add(fas);
dom.i2svg();

////////////////////////////////////////////////////////////////
// Display a banner
import { version } from '../package.json';
console.log("This is\n" +
"  ▀██▄   ▄██▀ ██ █████     █████ ▄███████████████████▄    ███\n" + 
"    ▀██▄██▀   ██▐██ ▐██   ██▌ ██▌██                 ██▌  ██▀██\n" + 
"      ███     ██▐██  ██▌ ▐██  ██▌▐█████████ ▄████████▀  ██▀ ▀██\n" + 
"    ▄██▀██▄   ██▐██  ▐██ ██▌  ██▌██        ▐█▌  ▀██▄   ██▀   ▀██\n" + 
"  ▄██▀   ▀██▄ ██▐██   ▀███▀   ██▌▀█████████▐█▌    ▀██▄██▀     ▀██\n" +
"version",version);

////////////////////////////////////////////////////////////////
// the elm architecture lifecycle, via snabbdom

import Snabbdom from 'snabbdom-pragma';

import {
  init,
  classModule,
  propsModule,
  styleModule,
  eventListenersModule,
  VNode
} from "snabbdom";

var patch = init([ // Init patch function with chosen modules
  classModule, // makes it easy to toggle classes
  propsModule, // for setting properties on DOM elements
  styleModule, // handles styling on elements with support for animations
  eventListenersModule, // attaches event listeners
]);

// the initial container
var vnode;
let state;

function repaint() {
  if (vnode === undefined)
    vnode = patch(document.body, app.view({state, dispatch}) as VNode);
  else
    vnode = patch(vnode, app.view({state, dispatch}) as VNode);
}

window.onpopstate = function() {
  dispatch( ['navigate-to', window.location.pathname] );
};

import app from './app';

function debounce(func, wait) {
  var timeout;
  return function() {
    var later = function() {
      timeout = null;
      func();
    };
    clearTimeout(timeout);
    timeout = setTimeout(later, wait);
  };
};

const repaintSlowly = debounce( repaint, 10 );

function update(stateAndCommand) {
  state = stateAndCommand[0];

  //window.requestAnimationFrame( repaint );
  repaintSlowly();
  
  let command = stateAndCommand[1];

  (async () => {
    for await (const message of command() ) {
      dispatch(message);
      //window.requestAnimationFrame( repaint );
      repaintSlowly();
    }
  })();
}

export function dispatch(message) {
  update( app.update(message, state) );
}

update( app.init() );


