import Snabbdom from 'snabbdom-pragma';
import Cmd from './cmd';
import logo from '../public/logo/logo.svg';
import { Link } from './router';

const brandStyle = { maxHeight: "1rem", opacity: "0.5", verticalAlign: "baseline"};
function Brand() { return <img src={logo} style={brandStyle}/>; }

export function view( { state, dispatch } ) {
  return <nav class={{navbar:true, "fixed-top":true, "navbar-expand-md":true, "navbar-light":true, "bg-light": true}}>
    <Link class={{"navbar-brand":true}} dispatch={dispatch} href="/"><Brand/></Link>
    <button class={{"navbar-toggler":true}} type="button" data-toggle="collapse" data-target="#navbarNav" aria-controls="navbarNav" aria-expanded="false" aria-label="Toggle navigation">
    <span class={{"navbar-toggler-icon":true}}></span>
    </button>
    <div class={{collapse:true, "navbar-collapse":true}} id="navbarNav">
    <ul class={{"navbar-nav":true}}>
    <li class={{"nav-item":true}}>
    <Link class={{"nav-link":true}} dispatch={dispatch} href="/help">Help</Link>
    </li>
    <li class={{"nav-item":true}}>
    <a class={{"nav-link":true}} href="#">Features</a>
    </li>
    </ul>
    </div>
    </nav>;
}

export default view;
